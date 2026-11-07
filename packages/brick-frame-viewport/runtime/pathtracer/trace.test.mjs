import assert from 'node:assert/strict'
import test from 'node:test'
import { GROUND_ALBEDO, prepareScene } from './scene.js'
import { createRay, captureGuide, primaryRay, renderRowSample, tracePath } from './trace.js'
import { createRng } from './rng.js'
import { createHit, intersect } from './bvh.js'
import { denoise } from './denoise.js'
import { toRgba8 } from './output.js'

const norm = (v) => {
  const l = Math.hypot(v[0], v[1], v[2])
  return [v[0] / l, v[1] / l, v[2] / l]
}
const dot = (a, b) => a[0] * b[0] + a[1] * b[1] + a[2] * b[2]

/** Two triangles forming a 120 × 120 LDU white square at height y. */
function squareAt(y) {
  const h = 60
  const a = [-h, y, -h]
  const b = [h, y, -h]
  const c = [h, y, h]
  const d = [-h, y, h]
  return [...a, ...b, ...c, 1, 1, 1, 1, 0, ...a, ...c, ...d, 1, 1, 1, 1, 0]
}

function sceneFor(width, height, { triangles, camera, transparent = false }) {
  return prepareScene({
    width,
    height,
    transparent,
    camera: camera ?? { position: [0, 80, 300], target: [0, 30, 0], up: [0, 1, 0], fovYDegrees: 45, zoom: 1 },
    triangles: Float32Array.from(triangles ?? squareAt(40)),
  })
}

test('the centre pixel ray points at the camera target', () => {
  const scene = sceneFor(32, 18, {})
  // Sub-pixel offset of 1 lands exactly on the image centre (16, 9).
  const ray = primaryRay(scene, 15, 8, 1, 1, createRay())
  const want = norm([0, 30 - 80, -300 + 0].map((v, i) => [0, 30, 0][i] - [0, 80, 300][i]))
  const got = norm([ray.dx, ray.dy, ray.dz])
  assert.ok(Math.abs(dot(got, want) - 1) < 1e-9, `centre dir ${got} vs ${want}`)
  assert.equal(ray.ox, 0)
  assert.equal(ray.oy, 80)
  assert.equal(ray.oz, 300)
})

test('a 1920x1080 render is not stretched: horizontal tan is the aspect times the vertical tan', () => {
  const scene = sceneFor(1920, 1080, {})
  const cam = scene.camera
  const left = primaryRay(scene, 0, 540, 0, 0.5, createRay())
  const top = primaryRay(scene, 960, 0, 0.5, 0, createRay())
  const fwd = cam.forward
  const tanH = dot([left.dx, left.dy, left.dz], cam.right) / dot([left.dx, left.dy, left.dz], fwd)
  const tanV = dot([top.dx, top.dy, top.dz], cam.up) / dot([top.dx, top.dy, top.dz], fwd)
  assert.ok(Math.abs(Math.abs(tanV) - cam.tanHalfY) < 1e-3, `vertical tan ${tanV}`)
  assert.ok(Math.abs(Math.abs(tanH) / Math.abs(tanV) - 1920 / 1080) < 1e-3, `ratio ${tanH / tanV}`)
  assert.ok(Math.abs(tanH) > Math.abs(tanV), 'horizontal FOV should be the wider one')
})

test('prepareScene places the ground just below the lowest vertex', () => {
  const scene = sceneFor(8, 8, { triangles: [...squareAt(40).slice(0, 14)], })
  assert.ok(Math.abs(scene.groundY - (40 - 0.01)) < 1e-6)
})

test('colours are converted from sRGB to linear for shading', () => {
  const tri = [0, 0, 0, 1, 0, 0, 0, 0, 1, 0.5, 0.5, 0.5, 1, 0]
  const scene = sceneFor(8, 8, { triangles: tri })
  assert.ok(Math.abs(scene.albedo[0] - 0.214) < 0.01)
})

test('an end-to-end tiny render is finite, lit, and shadowed under the square', () => {
  const width = 32
  const height = 18
  const scene = sceneFor(width, height, {})
  const rng = createRng(7)
  const sums = new Float32Array(width * height * 4)
  const ray = createRay()
  const passes = 16
  for (let pass = 0; pass < passes; pass++) {
    for (let y = 0; y < height; y++) {
      renderRowSample(scene, rng, y, sums.subarray(y * width * 4, (y + 1) * width * 4))
    }
  }
  const albedo = new Float32Array(width * height * 3)
  const normal = new Float32Array(width * height * 3)
  for (let y = 0; y < height; y++) {
    captureGuide(scene, y, albedo.subarray(y * width * 3, (y + 1) * width * 3), normal.subarray(y * width * 3, (y + 1) * width * 3))
  }
  const color = new Float32Array(width * height * 3)
  const alpha = new Float32Array(width * height)
  for (let p = 0; p < width * height; p++) {
    const a = sums[p * 4 + 3] / passes
    alpha[p] = a
    for (let k = 0; k < 3; k++) color[p * 3 + k] = sums[p * 4 + k] / passes / Math.max(a, 1e-9)
  }
  for (const v of color) assert.ok(Number.isFinite(v))
  for (const v of alpha) assert.ok(Number.isFinite(v) && v >= 0 && v <= 1)

  // Ground pixels are those whose first hit is the grey ground albedo, facing up.
  let lo = Infinity
  let hi = 0
  let groundPixels = 0
  for (let p = 0; p < width * height; p++) {
    const isGround = Math.abs(albedo[p * 3] - GROUND_ALBEDO[0]) < 1e-6 && normal[p * 3 + 1] > 0.99
    if (!isGround) continue
    groundPixels++
    const lum = 0.2126 * color[p * 3] + 0.7152 * color[p * 3 + 1] + 0.0722 * color[p * 3 + 2]
    lo = Math.min(lo, lum)
    hi = Math.max(hi, lum)
  }
  assert.ok(groundPixels > 50, `need ground pixels, got ${groundPixels}`)
  assert.ok(hi > 0.05, 'lit ground should be non-black')
  assert.ok(lo < 0.9 * hi, `shadowed ground (${lo}) should be darker than lit ground (${hi})`)

  const denoised = denoise(color, alpha, albedo, normal, width, height, 2)
  const rgba = toRgba8(denoised.color, denoised.alpha, width, height)
  assert.equal(rgba.length, width * height * 4)
})

test('transparent renders: sky is alpha 0, and the lit scene keeps alpha 1 on the model', () => {
  const width = 32
  const height = 18
  const scene = sceneFor(width, height, { transparent: true })
  const rng = createRng(3)
  const out = new Float64Array(4)
  const ray = createRay()
  // Top-left pixel looks at the sky above the ground plane.
  primaryRay(scene, 0, 0, 0.5, 0.5, ray)
  tracePath(scene, rng, ray, out)
  assert.equal(out[3], 0)
  assert.equal(out[0], 0)
  // Find a pixel whose primary ray hits the square's interior.
  let target = null
  for (let y = 0; y < height && !target; y++) {
    for (let x = 0; x < width && !target; x++) {
      primaryRay(scene, x, y, 0.5, 0.5, ray)
      const hit = intersect(scene.bvh, ray, createHit())
      if (hit && hit.t < 1e6 && scene.material[hit.slot] === 0) target = [x, y]
    }
  }
  assert.ok(target, 'some pixel should see the square')
  primaryRay(scene, target[0], target[1], 0.5, 0.5, ray)
  let covered = 0
  for (let i = 0; i < 8; i++) {
    tracePath(scene, rng, ray, out)
    covered += out[3]
  }
  assert.ok(covered / 8 > 0.99, `model pixel alpha ${covered / 8}`)
})

test('an opaque render paints the sky in the top rows', () => {
  const scene = sceneFor(32, 18, {})
  const out = new Float64Array(4)
  const ray = createRay()
  primaryRay(scene, 0, 0, 0.5, 0.5, ray)
  tracePath(scene, createRng(5), ray, out)
  assert.equal(out[3], 1)
  assert.ok(out[0] > 0.2 && out[2] > 0.2, `sky should be visible, got ${out}`)
})

/**
 * Irradiance a light gives an upward-facing surface. Cones are taken at their
 * axis; a rectangle is integrated over its solid angle on a midpoint grid
 * (dω = dx·dy / |p|³ on the plane at unit distance).
 */
function upIrradiance(light) {
  if (light.kind !== 'rect') return light.irradiance * Math.max(0, light.dir[1])
  const n = 200
  const dx = (2 * light.ha) / n
  const dy = (2 * light.hb) / n
  let sum = 0
  for (let i = 0; i < n; i++) {
    const x = -light.ha + (i + 0.5) * dx
    for (let j = 0; j < n; j++) {
      const y = -light.hb + (j + 0.5) * dy
      const len = Math.sqrt(1 + x * x + y * y)
      const cosUp = (light.dir[1] + light.t[1] * x + light.b[1] * y) / len
      if (cosUp > 0) sum += (cosUp / (len * len * len)) * dx * dy
    }
  }
  return (light.irradiance / light.solidAngle) * sum
}

test('direct and sky light on the ground match the analytic Lambert estimate (MIS is unbiased)', () => {
  // A distant dummy triangle makes the scene ground-only.
  const scene = prepareScene({
    width: 8,
    height: 8,
    transparent: false,
    camera: { position: [170, 150, 190], target: [10, 20, -10], up: [0, 1, 0], fovYDegrees: 45 },
    triangles: [1e5, 0, 1e5, 1e5 + 1, 0, 1e5, 1e5, 0, 1e5 + 1, 1, 1, 1, 1, 0],
  })
  const rng = createRng(3)
  const out = new Float64Array(4)
  const ray = createRay()
  primaryRay(scene, 4, 7, 0.5, 0.5, ray)
  const n = 20000
  let sum = 0
  for (let i = 0; i < n; i++) {
    tracePath(scene, rng, ray, out)
    sum += (out[0] + out[1] + out[2]) / 3
  }
  // Lambert: albedo/pi · E·cos per light, plus albedo times the mean sky radiance (sky irradiance / pi).
  const direct = scene.lights.reduce((acc, l) => acc + upIrradiance(l) / Math.PI, 0)
  const albedoMean = (GROUND_ALBEDO[0] + GROUND_ALBEDO[1] + GROUND_ALBEDO[2]) / 3
  const expected = albedoMean * (direct + scene.skyIrradiance / Math.PI)
  const measured = sum / n
  assert.ok(Math.abs(measured - expected) / expected < 0.03, `ground ${measured} vs analytic ${expected}`)
})

test('horizontal Red (#B40000) and Black (#1B2A34) plastic bricks retain rich saturated colour without softbox washout', () => {
  function renderHorizontalPlastic(rSrgb, gSrgb, bSrgb) {
    const h = 60
    const y = 30
    const a = [-h, y, -h]
    const b = [h, y, -h]
    const c = [h, y, h]
    const d = [-h, y, h]
    const triangles = [
      ...a, ...b, ...c, rSrgb, gSrgb, bSrgb, 1, 0,
      ...a, ...c, ...d, rSrgb, gSrgb, bSrgb, 1, 0,
    ]
    const scene = sceneFor(8, 8, { triangles })
    const rng = createRng(19)
    const ray = primaryRay(scene, 3, 3, 1, 1, createRay())
    const out = new Float64Array(4)
    const sum = new Float32Array(3)
    const samples = 512
    for (let i = 0; i < samples; i++) {
      tracePath(scene, rng, ray, out)
      sum[0] += out[0] / samples
      sum[1] += out[1] / samples
      sum[2] += out[2] / samples
    }
    return toRgba8(sum, new Float32Array([1]), 1, 1)
  }

  // Red #B40000 = (180/255, 0, 0)
  const redRgba = renderHorizontalPlastic(180 / 255, 0, 0)
  assert.ok(redRgba[0] >= 145, `Red brick R channel should be strong, got ${redRgba[0]}`)
  assert.ok(redRgba[1] < 65 && redRgba[2] < 65, `Red brick G/B should not bleach to pink, got (${redRgba[0]}, ${redRgba[1]}, ${redRgba[2]})`)
  assert.ok(redRgba[0] > 2.5 * redRgba[1], `Red brick R/G ratio should be >= 2.5x, got (${redRgba[0]}, ${redRgba[1]}, ${redRgba[2]})`)

  // Black #1B2A34 = (27/255, 42/255, 52/255)
  const blackRgba = renderHorizontalPlastic(27 / 255, 42 / 255, 52 / 255)
  assert.ok(
    blackRgba[0] < 75 && blackRgba[1] < 85 && blackRgba[2] < 95,
    `Black brick should stay dark instead of washing out to grey, got (${blackRgba[0]}, ${blackRgba[1]}, ${blackRgba[2]})`,
  )
})

test('smooth vertex normals interpolate across 16-sided cylindrical studs while keeping 90-degree rims sharp', () => {
  // Build two adjacent facets of a 16-sided cylinder of radius 6 (height 0..4) plus a flat top cap triangle at y = 4.
  const r = 6
  const segs = 16
  const pt = (k, y) => {
    const a = (k * 2 * Math.PI) / segs
    return [r * Math.sin(a), y, r * Math.cos(a)]
  }
  const p0b = pt(0, 0)
  const p1b = pt(1, 0)
  const p2b = pt(2, 0)
  const p0t = pt(0, 4)
  const p1t = pt(1, 4)
  const p2t = pt(2, 4)
  const centerTop = [0, 4, 0]
  const col = [0.8, 0.1, 0.1, 1, 0]
  const triangles = [
    // Facet 0
    ...p0b, ...p1b, ...p1t, ...col,
    ...p0b, ...p1t, ...p0t, ...col,
    // Facet 1 (shares edge p1b-p1t at 22.5° < 35°)
    ...p1b, ...p2b, ...p2t, ...col,
    ...p1b, ...p2t, ...p1t, ...col,
    // Top cap (shares edge p0t-p1t at 90° > 35°)
    ...centerTop, ...p0t, ...p1t, ...col,
  ]
  const scene = sceneFor(8, 8, { triangles })
  // Find the top cap slot (face normal +Y)
  for (let slot = 0; slot < scene.bvh.count; slot++) {
    const vn = scene.vertexNormals.subarray(slot * 9, (slot + 1) * 9)
    if (vn[1] > 0.9) {
      // All 3 corners of the top cap must stay strictly vertical (0, 1, 0)
      assert.ok(vn[1] > 0.999 && vn[4] > 0.999 && vn[7] > 0.999, `top cap normal leaked wall normal: ${vn}`)
    }
  }
  // Ray aimed across the shared vertical edge p1 at height y = 2 should produce a smooth normal aligned with p1's radial direction
  const radial1 = norm([p1b[0], 0, p1b[2]])
  const ray = {
    ox: p1b[0] * 3,
    oy: 2,
    oz: p1b[2] * 3,
    dx: -radial1[0],
    dy: 0,
    dz: -radial1[2],
    tMin: 0,
    tMax: Infinity,
  }
  const hit = intersect(scene.bvh, ray, createHit())
  assert.ok(hit, 'ray should hit the stud cylinder wall near vertex 1')
  const o = hit.slot * 9
  const vn = scene.vertexNormals
  const w = 1 - hit.u - hit.v
  const nx = w * vn[o] + hit.u * vn[o + 3] + hit.v * vn[o + 6]
  const ny = w * vn[o + 1] + hit.u * vn[o + 4] + hit.v * vn[o + 7]
  const nz = w * vn[o + 2] + hit.u * vn[o + 5] + hit.v * vn[o + 8]
  const nInterp = norm([nx, ny, nz])
  assert.ok(
    dot(nInterp, radial1) > 0.998,
    `smoothed cylinder normal at shared edge should match radial normal (${dot(nInterp, radial1)})`,
  )
})

test('prepareScene respects custom lightDirection when provided', () => {
  const customDir = [0, 1, 0]
  const scene = prepareScene({
    width: 8,
    height: 8,
    transparent: false,
    lightDirection: customDir,
    camera: { position: [0, 80, 300], target: [0, 30, 0], up: [0, 1, 0], fovYDegrees: 45 },
    triangles: Float32Array.from(squareAt(40)),
  })
  const key = scene.lights[0]
  assert.equal(key.kind, 'cone')
  assert.ok(Math.abs(key.dir[0]) < 1e-6 && Math.abs(key.dir[1] - 1) < 1e-6 && Math.abs(key.dir[2]) < 1e-6)
})
