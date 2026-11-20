/**
 * Path integrator. Per pixel sample it generates a jittered camera ray, then
 * follows up to MAX_BOUNCES bounces:
 * - next-event estimation on the studio lights (key and fill cones, and the
 *   rectangular softboxes; shadow rays through the BVH), combined with BSDF
 *   sampling by the power heuristic (MIS). Escaping rays that hit a light add
 *   the other half. Lights are invisible to primary rays: the backdrop is the
 *   plain sky, and only bounced rays see the softboxes and the horizon band.
 * - transparent plastic is a delta dielectric. Beer–Lambert tint uses the
 *   distance travelled inside (ray-tracked medium, refracted by IOR 1.49).
 * - Russian roulette from bounce RR_START; a firefly clamp on indirect
 *   contributions (luminance FIREFLY_CLAMP).
 * - Transparent renders are shadow-catcher output: a primary ray that hits
 *   the ground writes black with alpha = 1 − (lit fraction), a primary ray
 *   that escapes to the sky writes alpha 0.
 *
 * Colour is accumulated premultiplied: `out` holds (r·a, g·a, b·a, a).
 * Pixel rows are rendered through `renderRowSample`, which takes one jittered
 * sample per pixel. `captureGuide` writes first-hit albedo and normal for the
 * denoiser. No allocation happens per sample.
 */
import { intersect, occluded, createHit } from './bvh.js'
import {
  MATERIAL,
  createFrame,
  createEval,
  createSample,
  createDielectricSample,
  evalSurface,
  sampleSurface,
  sampleDielectric,
  setFrame,
} from './materials.js'
import { environmentRadiance, lightPdf, sampleLight, skyRadiance } from './scene.js'

export const MAX_BOUNCES = 6
const RR_START = 3
const RR_MAX_SURVIVAL = 0.95
export const FIREFLY_CLAMP = 10
/** Offset (LDU) applied along the normal before spawning a ray, to avoid self-hits. */
const SURFACE_EPS = 0.02
/** Reference path length (LDU) over which the transparent tint reaches its full colour. */
const BRICK_HEIGHT = 24
const GROUND_NORMAL_Y = 1

/** Allocates a ray record `{ ox, oy, oz, dx, dy, dz, tMin, tMax }`. */
export function createRay() {
  return { ox: 0, oy: 0, oz: 0, dx: 0, dy: 0, dz: 0, tMin: 0, tMax: Infinity }
}

// Scratch state. Each worker (and each test) runs its own synchronous loop.
const frame = createFrame()
const evalOut = createEval()
const sampleOut = createSample()
const dielOut = createDielectricSample()
const hitScratch = createHit()
const ray = createRay()
const shadowRay = createRay()
const beta = new Float64Array(3)
const contrib = new Float64Array(3)
const lightDir = new Float64Array(3)
const surfNormal = new Float64Array(3)
const skyRad = new Float64Array(3)
const hitPos = new Float64Array(3)
const albedoRow = new Float64Array(3)
const woScratch = new Float64Array(3)
const dirScratch = new Float64Array(3)
const offsetScratch = new Float64Array(3)

/**
 * Camera ray through pixel (px, py) at sub-pixel offset (jx, jy) in [0, 1),
 * written into `out`. Supports optional crop sub-frustum and thin-lens macro
 * depth of field when apertureRadius > 0.
 */
export function primaryRay(scene, px, py, jx, jy, out, rng) {
  const cam = scene.camera
  const crop = scene.crop
  const u = crop ? crop.x + ((px + jx) / scene.width) * crop.width : (px + jx) / scene.width
  const v = crop ? crop.y + ((py + jy) / scene.height) * crop.height : (py + jy) / scene.height
  const ndcX = (2 * u - 1) * cam.aspect * cam.tanHalfY
  const ndcY = (1 - 2 * v) * cam.tanHalfY
  const f = cam.forward
  const r = cam.right
  const up = cam.up
  let dx = f[0] + r[0] * ndcX + up[0] * ndcY
  let dy = f[1] + r[1] * ndcX + up[1] * ndcY
  let dz = f[2] + r[2] * ndcX + up[2] * ndcY
  const len = Math.sqrt(dx * dx + dy * dy + dz * dz)
  dx /= len
  dy /= len
  dz /= len

  let ox = cam.origin[0]
  let oy = cam.origin[1]
  let oz = cam.origin[2]

  const ap = cam.apertureRadius
  if (ap > 0 && rng) {
    const cosF = dx * f[0] + dy * f[1] + dz * f[2]
    const tf = cosF > 1e-6 ? cam.focalDistance / cosF : cam.focalDistance
    const fx = ox + dx * tf
    const fy = oy + dy * tf
    const fz = oz + dz * tf

    const u1 = rng.next()
    const u2 = rng.next()
    const rad = ap * Math.sqrt(u1)
    const phi = 2 * Math.PI * u2
    const lx = rad * Math.cos(phi)
    const ly = rad * Math.sin(phi)

    ox += r[0] * lx + up[0] * ly
    oy += r[1] * lx + up[1] * ly
    oz += r[2] * lx + up[2] * ly

    dx = fx - ox
    dy = fy - oy
    dz = fz - oz
    const flen = Math.hypot(dx, dy, dz)
    dx /= flen
    dy /= flen
    dz /= flen
  }

  out.ox = ox
  out.oy = oy
  out.oz = oz
  out.dx = dx
  out.dy = dy
  out.dz = dz
  out.tMin = 0
  out.tMax = Infinity
  return out
}

/** Random direction toward `light`, written into `out`. Returns its density per steradian. */
function sampleLightDir(light, rng, out) {
  const u1 = rng.next()
  const u2 = rng.next()
  return sampleLight(light, u1, u2, out)
}

function setShadowRay(origin, dir) {
  shadowRay.ox = origin[0]
  shadowRay.oy = origin[1]
  shadowRay.oz = origin[2]
  shadowRay.dx = dir[0]
  shadowRay.dy = dir[1]
  shadowRay.dz = dir[2]
  shadowRay.tMin = 0
  shadowRay.tMax = Infinity
  return shadowRay
}

/** Distance along the ray to the ground plane, or Infinity when it does not hit it ahead. */
function groundDistance(scene, r) {
  if (Math.abs(r.dy) < 1e-12) return Infinity
  const t = (scene.groundY - r.oy) / r.dy
  return t > 1e-9 ? t : Infinity
}

/** Adds an RGB contribution (premultiplied output), clamping fireflies on indirect bounces. */
function addClamped(out, r, g, b, indirect) {
  let scale = 1
  if (indirect) {
    const lum = 0.2126 * r + 0.7152 * g + 0.0722 * b
    if (lum > FIREFLY_CLAMP) scale = FIREFLY_CLAMP / lum
  }
  out[0] += r * scale
  out[1] += g * scale
  out[2] += b * scale
}

/**
 * Lit fraction of the ground point `p` (normal up), in [0, 1]. Estimated from
 * one sample per light cone plus one cosine-weighted sky direction, each
 * weighted by its unshadowed irradiance.
 */
function groundLitFraction(scene, rng, p) {
  let num = 0
  let den = 0
  const origin = [p[0], p[1] + SURFACE_EPS, p[2]]
  for (const light of scene.lights) {
    const pl = sampleLightDir(light, rng, lightDir)
    const cosL = lightDir[1]
    if (cosL <= 0) continue
    // Estimate of the light's irradiance from one sample: L·cos / pdf, with L = E / solid angle.
    const e = ((light.irradiance / light.solidAngle) * cosL) / pl
    den += e
    if (!occluded(scene.bvh, setShadowRay(origin, lightDir))) num += e
  }
  const u1 = rng.next()
  const u2 = rng.next()
  const r = Math.sqrt(u1)
  const phi = 2 * Math.PI * u2
  lightDir[0] = r * Math.cos(phi)
  lightDir[1] = Math.sqrt(Math.max(0, 1 - u1))
  lightDir[2] = r * Math.sin(phi)
  den += scene.skyIrradiance
  if (!occluded(scene.bvh, setShadowRay(origin, lightDir))) num += scene.skyIrradiance
  return den > 0 ? num / den : 1
}

/**
 * Interpolates the smoothed vertex normal at `hit`, aligned to face incoming ray `r`.
 */
function shadingNormal(scene, hit, r, out) {
  const slot = hit.slot
  const o = slot * 9
  const tris = scene.bvh.tris
  const ax = tris[o]
  const ay = tris[o + 1]
  const az = tris[o + 2]
  const e1x = tris[o + 3]
  const e1y = tris[o + 4]
  const e1z = tris[o + 5]
  const e2x = tris[o + 6]
  const e2y = tris[o + 7]
  const e2z = tris[o + 8]
  let gnx = e1y * e2z - e1z * e2y
  let gny = e1z * e2x - e1x * e2z
  let gnz = e1x * e2y - e1y * e2x
  const gnl = Math.sqrt(gnx * gnx + gny * gny + gnz * gnz) || 1
  gnx /= gnl
  gny /= gnl
  gnz /= gnl
  const sign = gnx * r.dx + gny * r.dy + gnz * r.dz > 0 ? -1 : 1
  gnx *= sign
  gny *= sign
  gnz *= sign

  let nx = gnx
  let ny = gny
  let nz = gnz

  const vn = scene.vertexNormals
  if (vn) {
    const u = hit.u
    const v = hit.v
    const w = 1 - u - v
    let snx = sign * (w * vn[o] + u * vn[o + 3] + v * vn[o + 6])
    let sny = sign * (w * vn[o + 1] + u * vn[o + 4] + v * vn[o + 7])
    let snz = sign * (w * vn[o + 2] + u * vn[o + 5] + v * vn[o + 8])
    const nl = Math.sqrt(snx * snx + sny * sny + snz * snz) || 1
    snx /= nl
    sny /= nl
    snz /= nl
    if (snx * r.dx + sny * r.dy + snz * r.dz < -1e-5) {
      nx = snx
      ny = sny
      nz = snz
    }
  }

  out[0] = nx
  out[1] = ny
  out[2] = nz
  return out
}

function lightScaleForMaterial(light, mat) {
  return mat === MATERIAL.PLASTIC || mat === MATERIAL.RUBBER ? (light.plasticScale ?? 1) : 1
}

function hash3d(i, j, k) {
  let n = (i * 374761393 + j * 668265263 + k * 1274126177) | 0
  n = Math.imul(n ^ (n >>> 13), 1274126177)
  return ((n ^ (n >>> 16)) >>> 0) / 4294967296
}

function valueNoise3d(x, y, z) {
  const ix = Math.floor(x) | 0
  const iy = Math.floor(y) | 0
  const iz = Math.floor(z) | 0
  const fx = x - ix
  const fy = y - iy
  const fz = z - iz
  const wx = fx * fx * fx * (fx * (fx * 6 - 15) + 10)
  const wy = fy * fy * fy * (fy * (fy * 6 - 15) + 10)
  const wz = fz * fz * fz * (fz * (fz * 6 - 15) + 10)

  const c000 = hash3d(ix, iy, iz)
  const c100 = hash3d(ix + 1, iy, iz)
  const c010 = hash3d(ix, iy + 1, iz)
  const c110 = hash3d(ix + 1, iy + 1, iz)
  const c001 = hash3d(ix, iy, iz + 1)
  const c101 = hash3d(ix + 1, iy, iz + 1)
  const c011 = hash3d(ix, iy + 1, iz + 1)
  const c111 = hash3d(ix + 1, iy + 1, iz + 1)

  const x00 = c000 + wx * (c100 - c000)
  const x10 = c010 + wx * (c110 - c010)
  const x01 = c001 + wx * (c101 - c001)
  const x11 = c011 + wx * (c111 - c011)

  const y0 = x00 + wy * (x10 - x00)
  const y1 = x01 + wy * (x11 - x01)

  return y0 + wz * (y1 - y0)
}

/**
 * Procedural injection-mold micro-grain for matte plastic preset.
 */
export function applySurfaceTexture(shader = 'glossy', pos, normal, albedo) {
  if (shader !== 'matte') return

  const nx = normal[0]
  const ny = normal[1]
  const nz = normal[2]

  const helperY = Math.abs(ny) < 0.9 ? 1 : 0
  const helperX = helperY === 1 ? 0 : 1
  let tx = ny * 0 - nz * helperY
  let ty = nz * helperX - nx * 0
  let tz = nx * helperY - ny * helperX
  const tlen = Math.hypot(tx, ty, tz) || 1
  tx /= tlen
  ty /= tlen
  tz /= tlen
  const bx = ny * tz - nz * ty
  const by = nz * tx - nx * tz
  const bz = nx * ty - ny * tx

  const px = pos[0]
  const py = pos[1]
  const pz = pos[2]

  const grainX = (valueNoise3d(px * 1.6 + 13.1, py * 1.6 + 7.3, pz * 1.6 + 3.7) - 0.5) * 0.015
  const grainY = (valueNoise3d(px * 1.6 + 31.7, py * 1.6 + 19.3, pz * 1.6 + 11.5) - 0.5) * 0.015

  let pnx = nx + grainX * tx + grainY * bx
  let pny = ny + grainX * ty + grainY * by
  let pnz = nz + grainX * tz + grainY * bz
  const pnl = Math.hypot(pnx, pny, pnz) || 1
  normal[0] = pnx / pnl
  normal[1] = pny / pnl
  normal[2] = pnz / pnl
}

/**
 * Accumulates one path for primary ray `primary` into `out` (premultiplied
 * RGBA). Returns `out`.
 */
export function tracePath(scene, rng, primary, out) {
  out[0] = out[1] = out[2] = 0
  out[3] = 1
  beta[0] = beta[1] = beta[2] = 1
  ray.ox = primary.ox
  ray.oy = primary.oy
  ray.oz = primary.oz
  ray.dx = primary.dx
  ray.dy = primary.dy
  ray.dz = primary.dz
  ray.tMin = 0
  ray.tMax = Infinity

  let specular = true
  let lastPdf = 0
  let lastMat = MATERIAL.MATTE
  let inside = false
  let insideDist = 0
  const bvh = scene.bvh

  for (let bounce = 0; bounce <= MAX_BOUNCES; bounce++) {
    const indirect = bounce > 0
    const hit = intersect(bvh, ray, hitScratch)
    const tModel = hit ? hit.t : Infinity
    const tGround = groundDistance(scene, ray)

    if (tModel === Infinity && tGround === Infinity) {
      // Escaped to the sky.
      if (bounce === 0 && scene.transparent) {
        out[0] = out[1] = out[2] = out[3] = 0
        return out
      }
      // Primary rays see the plain backdrop; bounced rays see the reflection environment.
      if (indirect) environmentRadiance(ray.dy, skyRad)
      else skyRadiance(ray.dy, skyRad)
      addClamped(out, beta[0] * skyRad[0], beta[1] * skyRad[1], beta[2] * skyRad[2], indirect)
      if (indirect) {
        // Light hits found by BSDF sampling: MIS against the light's NEE.
        for (const light of scene.lights) {
          const pl = lightPdf(light, ray.dx, ray.dy, ray.dz)
          if (!(pl > 0)) continue
          // BSDF-side MIS weight: the light's NEE already holds the complementary share.
          const w = specular ? 1 : (lastPdf * lastPdf) / (pl * pl + lastPdf * lastPdf)
          const le = (light.irradiance / light.solidAngle) * w * lightScaleForMaterial(light, lastMat)
          addClamped(out, beta[0] * le, beta[1] * le, beta[2] * le, true)
        }
      }
      return out
    }

    let mat
    let alb
    const pos = hitPos
    let tHit
    if (tGround < tModel) {
      tHit = tGround
      pos[0] = ray.ox + ray.dx * tHit
      pos[1] = ray.oy + ray.dy * tHit
      pos[2] = ray.oz + ray.dz * tHit
      if (bounce === 0 && scene.transparent) {
        out[0] = out[1] = out[2] = 0
        out[3] = 1 - groundLitFraction(scene, rng, pos)
        return out
      }
      const ny = ray.dy < 0 ? GROUND_NORMAL_Y : -GROUND_NORMAL_Y
      surfNormal[0] = 0
      surfNormal[1] = ny
      surfNormal[2] = 0
      setFrame(frame, 0, ny, 0)
      mat = MATERIAL.MATTE
      alb = scene.groundAlbedo
      inside = false
    } else {
      tHit = tModel
      const slot = hit.slot
      pos[0] = ray.ox + ray.dx * tHit
      pos[1] = ray.oy + ray.dy * tHit
      pos[2] = ray.oz + ray.dz * tHit
      shadingNormal(scene, hit, ray, surfNormal)
      mat = scene.material[slot]
      albedoRow[0] = scene.albedo[slot * 3]
      albedoRow[1] = scene.albedo[slot * 3 + 1]
      albedoRow[2] = scene.albedo[slot * 3 + 2]
      alb = albedoRow

      if (mat === MATERIAL.PLASTIC && scene.shader && scene.shader !== 'glossy') {
        applySurfaceTexture(scene.shader, pos, surfNormal, alb)
      }
      const nx = surfNormal[0]
      const ny = surfNormal[1]
      const nz = surfNormal[2]

      if (mat === MATERIAL.TRANSPARENT) {
        // Delta dielectric: track the medium so exiting rays take the tint.
        if (inside) insideDist += tHit
        dirScratch[0] = ray.dx
        dirScratch[1] = ray.dy
        dirScratch[2] = ray.dz
        sampleDielectric(dirScratch, surfNormal, inside, rng, dielOut)
        if (dielOut.transmit) {
          if (inside) {
            const k = insideDist / BRICK_HEIGHT
            beta[0] *= Math.pow(alb[0], k)
            beta[1] *= Math.pow(alb[1], k)
            beta[2] *= Math.pow(alb[2], k)
            inside = false
            insideDist = 0
          } else {
            inside = true
            insideDist = 0
          }
        }
        specular = true
        lastMat = mat
        spawn(ray, pos, surfNormal, dielOut.dir)
        if (!applyRoulette(bounce, rng)) break
        continue
      }

      inside = false
      setFrame(frame, nx, ny, nz)
    }

    // Opaque surface (or ground): next-event estimation on each light cone.
    const wo = woScratch
    wo[0] = -ray.dx
    wo[1] = -ray.dy
    wo[2] = -ray.dz
    for (const light of scene.lights) {
      const pl = sampleLightDir(light, rng, lightDir)
      const cosL = frame.n[0] * lightDir[0] + frame.n[1] * lightDir[1] + frame.n[2] * lightDir[2]
      if (cosL <= 0) continue
      if (occluded(bvh, setShadowRay(offsetAlong(pos, surfNormal), lightDir))) continue
      evalSurface(mat, alb, frame, wo, lightDir, evalOut, scene.shader)
      if (!(evalOut.pdf > 0)) continue
      const w = (pl * pl) / (pl * pl + evalOut.pdf * evalOut.pdf)
      // f · L · cos · w / pdf, with L = E / solid angle.
      const k = (((light.irradiance / light.solidAngle) * cosL * w) / pl) * lightScaleForMaterial(light, mat)
      addClamped(out, beta[0] * evalOut.f[0] * k, beta[1] * evalOut.f[1] * k, beta[2] * evalOut.f[2] * k, indirect)
    }

    // BSDF sampling for the continuation.
    if (!sampleSurface(mat, alb, frame, wo, rng, sampleOut, scene.shader)) break
    beta[0] *= sampleOut.weight[0]
    beta[1] *= sampleOut.weight[1]
    beta[2] *= sampleOut.weight[2]
    lastPdf = sampleOut.pdf
    lastMat = mat
    specular = false
    spawn(ray, pos, surfNormal, sampleOut.wi)
    if (!applyRoulette(bounce, rng)) break
  }
  return out
}

/** Points `ray` at direction `dir` from `pos`, nudged off the surface along `n`. */
function spawn(r, pos, n, dir) {
  const side = dir[0] * n[0] + dir[1] * n[1] + dir[2] * n[2] < 0 ? -SURFACE_EPS : SURFACE_EPS
  r.ox = pos[0] + n[0] * side
  r.oy = pos[1] + n[1] * side
  r.oz = pos[2] + n[2] * side
  r.dx = dir[0]
  r.dy = dir[1]
  r.dz = dir[2]
  r.tMin = 0
  r.tMax = Infinity
}

function offsetAlong(pos, n) {
  offsetScratch[0] = pos[0] + n[0] * SURFACE_EPS
  offsetScratch[1] = pos[1] + n[1] * SURFACE_EPS
  offsetScratch[2] = pos[2] + n[2] * SURFACE_EPS
  return offsetScratch
}

/**
 * Russian roulette once the path is past RR_START. Scales beta so the
 * estimator stays unbiased. Returns false when the path terminates.
 */
function applyRoulette(bounce, rng) {
  if (bounce < RR_START) return true
  const q = Math.min(RR_MAX_SURVIVAL, Math.max(beta[0], beta[1], beta[2]))
  if (!(q > 0) || rng.next() > q) return false
  beta[0] /= q
  beta[1] /= q
  beta[2] /= q
  return true
}

/**
 * Adds one jittered sample of every pixel in row `y` into `sums`, which holds
 * `width * 4` floats (premultiplied RGBA) for that row.
 */
export function renderRowSample(scene, rng, y, sums, pixel = new Float64Array(4)) {
  for (let x = 0; x < scene.width; x++) {
    primaryRay(scene, x, y, rng.next(), rng.next(), ray, rng)
    const v = tracePath(scene, rng, ray, pixel)
    const o = x * 4
    sums[o] += v[0]
    sums[o + 1] += v[1]
    sums[o + 2] += v[2]
    sums[o + 3] += v[3]
  }
}

/**
 * First-hit guide for row `y`: `albedo` (3 floats per pixel, linear) and
 * `normal` (3 floats per pixel), taken at the pixel centre. Misses carry the
 * sky radiance and the view direction. When `depth` is provided (Float32Array),
 * `depth[x]` receives the Euclidean hit distance or Infinity.
 */
export function captureGuide(scene, y, albedo, normal, depth) {
  const bvh = scene.bvh
  for (let x = 0; x < scene.width; x++) {
    primaryRay(scene, x, y, 0.5, 0.5, ray)
    const hit = intersect(bvh, ray, hitScratch)
    const hitGround = groundDistance(scene, ray)
    const tModel = hit ? hit.t : Infinity
    if (depth) {
      depth[x] = Math.min(tModel, hitGround)
    }
    const o = x * 3
    if (tModel === Infinity && hitGround === Infinity) {
      skyRadiance(ray.dy, skyRad)
      albedo[o] = skyRad[0]
      albedo[o + 1] = skyRad[1]
      albedo[o + 2] = skyRad[2]
      normal[o] = ray.dx
      normal[o + 1] = ray.dy
      normal[o + 2] = ray.dz
    } else if (hitGround < tModel) {
      albedo[o] = scene.groundAlbedo[0]
      albedo[o + 1] = scene.groundAlbedo[1]
      albedo[o + 2] = scene.groundAlbedo[2]
      normal[o] = 0
      normal[o + 1] = ray.dy < 0 ? 1 : -1
      normal[o + 2] = 0
    } else {
      albedoRow[0] = scene.albedo[hit.slot * 3]
      albedoRow[1] = scene.albedo[hit.slot * 3 + 1]
      albedoRow[2] = scene.albedo[hit.slot * 3 + 2]
      shadingNormal(scene, hit, ray, surfNormal)
      if (scene.shader && scene.shader !== 'glossy' && scene.material[hit.slot] === MATERIAL.PLASTIC) {
        const hPos = [ray.ox + ray.dx * tModel, ray.oy + ray.dy * tModel, ray.oz + ray.dz * tModel]
        applySurfaceTexture(scene.shader, hPos, surfNormal, albedoRow)
      }
      albedo[o] = albedoRow[0]
      albedo[o + 1] = albedoRow[1]
      albedo[o + 2] = albedoRow[2]
      normal[o] = surfNormal[0]
      normal[o + 1] = surfNormal[1]
      normal[o + 2] = surfNormal[2]
    }
  }
}
