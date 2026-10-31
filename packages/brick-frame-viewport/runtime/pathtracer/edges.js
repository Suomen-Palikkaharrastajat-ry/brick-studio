/**
 * Sharp edge feature extraction and depth-tested line rasterization.
 *
 * Extracts crease feature edges from triangle geometry (e.g. brick contours
 * and seams between adjacent parts) and renders crisp, antialiased, depth-tested
 * dark borders into the linear RGB color buffer.
 */

const STRIDE = 14

/**
 * Quantizes 3D coordinates to millildu (0.001 LDU) to match coincident vertices.
 */
function quantizeCoord(val) {
  return Math.round(val * 1000) + 0
}

function vertexKey(x, y, z) {
  return `${quantizeCoord(x)},${quantizeCoord(y)},${quantizeCoord(z)}`
}

function dot3(a, b) {
  return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/**
 * Extracts sharp feature edges from flat triangle array (stride 14).
 * Crease edges with adjacent face normal angle >= minAngleDegrees are included.
 * Coplanar quad diagonals (dot ~ 1.0) are excluded.
 * If options.includeBoundaries is true, open mesh boundary edges (1 face) are also included.
 */
export function extractSharpEdges(triangles, minAngleDegrees = 30, options = {}) {
  if (!triangles || triangles.length < STRIDE) return []
  const { includeBoundaries = false } = options
  const thresholdCos = Math.cos((minAngleDegrees * Math.PI) / 180)

  // Map from canonical edgeKey to { p0, p1, normals: [] }
  const edgeMap = new Map()

  const count = Math.floor(triangles.length / STRIDE)
  for (let i = 0; i < count; i++) {
    const base = i * STRIDE
    const ax = triangles[base]
    const ay = triangles[base + 1]
    const az = triangles[base + 2]
    const bx = triangles[base + 3]
    const by = triangles[base + 4]
    const bz = triangles[base + 5]
    const cx = triangles[base + 6]
    const cy = triangles[base + 7]
    const cz = triangles[base + 8]

    // Face normal from cross product of edge vectors
    const e1x = bx - ax
    const e1y = by - ay
    const e1z = bz - az
    const e2x = cx - ax
    const e2y = cy - ay
    const e2z = cz - az

    const nx = e1y * e2z - e1z * e2y
    const ny = e1z * e2x - e1x * e2z
    const nz = e1x * e2y - e1y * e2x
    const len = Math.hypot(nx, ny, nz)
    if (len < 1e-9) continue // degenerate triangle
    const normal = [nx / len, ny / len, nz / len]

    const ka = vertexKey(ax, ay, az)
    const kb = vertexKey(bx, by, bz)
    const kc = vertexKey(cx, cy, cz)

    const triEdges = [
      { p0: [ax, ay, az], p1: [bx, by, bz], ka, kb },
      { p0: [bx, by, bz], p1: [cx, cy, cz], ka: kb, kb: kc },
      { p0: [cx, cy, cz], p1: [ax, ay, az], ka: kc, kb: ka },
    ]

    for (const te of triEdges) {
      if (te.ka === te.kb) continue // degenerate edge

      let edgeKey
      let canonicalP0
      let canonicalP1
      if (te.ka < te.kb) {
        edgeKey = `${te.ka}|${te.kb}`
        canonicalP0 = te.p0
        canonicalP1 = te.p1
      } else {
        edgeKey = `${te.kb}|${te.ka}`
        canonicalP0 = te.p1
        canonicalP1 = te.p0
      }

      let entry = edgeMap.get(edgeKey)
      if (!entry) {
        entry = { p0: canonicalP0, p1: canonicalP1, normals: [] }
        edgeMap.set(edgeKey, entry)
      }

      // Record normal if not nearly identical to an already recorded normal on this edge
      if (!entry.normals.some((n) => dot3(n, normal) > 0.9999)) {
        entry.normals.push(normal)
      }
    }
  }

  const sharpEdges = []
  for (const entry of edgeMap.values()) {
    const normals = entry.normals
    if (normals.length === 0) continue

    if (normals.length === 1) {
      if (includeBoundaries) {
        sharpEdges.push({ p0: entry.p0, p1: entry.p1 })
      }
      continue
    }

    // Check if any pair of adjacent face normals meets the crease threshold
    let isCrease = false
    for (let i = 0; i < normals.length; i++) {
      for (let j = i + 1; j < normals.length; j++) {
        if (dot3(normals[i], normals[j]) < thresholdCos) {
          isCrease = true
          break
        }
      }
      if (isCrease) break
    }

    if (isCrease) {
      sharpEdges.push({ p0: entry.p0, p1: entry.p1 })
    }
  }

  return sharpEdges
}

/**
 * Projects a 3D point in render coordinates to camera space and screen space.
 */
function projectPoint(p, camera, width, height) {
  const vx = p[0] - camera.origin[0]
  const vy = p[1] - camera.origin[1]
  const vz = p[2] - camera.origin[2]
  const cz = vx * camera.forward[0] + vy * camera.forward[1] + vz * camera.forward[2]
  const cx = vx * camera.right[0] + vy * camera.right[1] + vz * camera.right[2]
  const cy = vx * camera.up[0] + vy * camera.up[1] + vz * camera.up[2]
  const dist = Math.hypot(vx, vy, vz)
  const ndcX = cx / (cz * camera.aspect * camera.tanHalfY)
  const ndcY = cy / (cz * camera.tanHalfY)
  const sx = (ndcX + 1) * 0.5 * width
  const sy = (1 - ndcY) * 0.5 * height
  return { sx, sy, cz, dist }
}

/**
 * Rasterizes feature edges with subpixel antialiasing and depth testing into colorBuffer.
 */
export function rasterizeEdges(edges, camera, depthBuffer, width, height, colorBuffer, options = {}) {
  if (!edges || edges.length === 0 || !camera || !colorBuffer) return colorBuffer

  const nearPlane = options.nearPlane ?? 0.1
  const lineWidth = options.lineWidth ?? 1.5
  const edgeColor = options.edgeColor ?? [0.02, 0.02, 0.02]
  const depthBias = options.depthBias ?? 0.05
  const depthBiasFactor = options.depthBiasFactor ?? 1.005

  const radius = lineWidth * 0.5
  const feather = 0.5
  const maxCoverage = new Float32Array(width * height)

  for (const edge of edges) {
    let p0 = edge.p0
    let p1 = edge.p1

    const vx0 = p0[0] - camera.origin[0]
    const vy0 = p0[1] - camera.origin[1]
    const vz0 = p0[2] - camera.origin[2]
    const z0 = vx0 * camera.forward[0] + vy0 * camera.forward[1] + vz0 * camera.forward[2]

    const vx1 = p1[0] - camera.origin[0]
    const vy1 = p1[1] - camera.origin[1]
    const vz1 = p1[2] - camera.origin[2]
    const z1 = vx1 * camera.forward[0] + vy1 * camera.forward[1] + vz1 * camera.forward[2]

    if (z0 < nearPlane && z1 < nearPlane) continue

    if (z0 < nearPlane) {
      const t = (nearPlane - z0) / (z1 - z0)
      p0 = [p0[0] + t * (p1[0] - p0[0]), p0[1] + t * (p1[1] - p0[1]), p0[2] + t * (p1[2] - p0[2])]
    } else if (z1 < nearPlane) {
      const t = (nearPlane - z0) / (z1 - z0)
      p1 = [p0[0] + t * (p1[0] - p0[0]), p0[1] + t * (p1[1] - p0[1]), p0[2] + t * (p1[2] - p0[2])]
    }

    const pr0 = projectPoint(p0, camera, width, height)
    const pr1 = projectPoint(p1, camera, width, height)

    const pad = radius + feather
    const minX = Math.max(0, Math.floor(Math.min(pr0.sx, pr1.sx) - pad))
    const maxX = Math.min(width - 1, Math.ceil(Math.max(pr0.sx, pr1.sx) + pad))
    const minY = Math.max(0, Math.floor(Math.min(pr0.sy, pr1.sy) - pad))
    const maxY = Math.min(height - 1, Math.ceil(Math.max(pr0.sy, pr1.sy) + pad))

    if (minX > maxX || minY > maxY) continue

    const dx = pr1.sx - pr0.sx
    const dy = pr1.sy - pr0.sy
    const lenSq = dx * dx + dy * dy

    for (let py = minY; py <= maxY; py++) {
      const cy = py + 0.5
      for (let px = minX; px <= maxX; px++) {
        const cx = px + 0.5
        let t = 0
        if (lenSq > 1e-8) {
          t = ((cx - pr0.sx) * dx + (cy - pr0.sy) * dy) / lenSq
          if (t < 0) t = 0
          else if (t > 1) t = 1
        }

        const qx = pr0.sx + t * dx
        const qy = pr0.sy + t * dy
        const d = Math.hypot(cx - qx, cy - qy)

        if (d >= radius + feather) continue

        let coverage = 1
        if (d > radius - feather) {
          coverage = (radius + feather - d) / (2 * feather)
        }
        coverage = Math.max(0, Math.min(1, coverage))
        if (coverage <= 0) continue

        const pixelIdx = py * width + px

        // Depth occlusion check against depthBuffer (euclidean ray distance)
        if (depthBuffer) {
          const sx = p0[0] + t * (p1[0] - p0[0])
          const sy = p0[1] + t * (p1[1] - p0[1])
          const sz = p0[2] + t * (p1[2] - p0[2])
          const strokeDist = Math.hypot(
            sx - camera.origin[0],
            sy - camera.origin[1],
            sz - camera.origin[2],
          )
          const sceneDepth = depthBuffer[pixelIdx]
          if (strokeDist > sceneDepth * depthBiasFactor + depthBias) {
            continue
          }
        }

        if (coverage > maxCoverage[pixelIdx]) {
          const prev = maxCoverage[pixelIdx]
          const deltaAlpha = (coverage - prev) / Math.max(1e-6, 1 - prev)
          maxCoverage[pixelIdx] = coverage
          const cIdx = pixelIdx * 3
          colorBuffer[cIdx] = colorBuffer[cIdx] * (1 - deltaAlpha) + edgeColor[0] * deltaAlpha
          colorBuffer[cIdx + 1] = colorBuffer[cIdx + 1] * (1 - deltaAlpha) + edgeColor[1] * deltaAlpha
          colorBuffer[cIdx + 2] = colorBuffer[cIdx + 2] * (1 - deltaAlpha) + edgeColor[2] * deltaAlpha
        }
      }
    }
  }

  return colorBuffer
}
