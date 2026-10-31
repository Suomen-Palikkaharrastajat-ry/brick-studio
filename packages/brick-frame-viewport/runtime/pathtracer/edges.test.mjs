import assert from 'node:assert/strict'
import test from 'node:test'
import { extractSharpEdges, rasterizeEdges } from './edges.js'
import { makeCamera, DEFAULT_ZOOM } from './scene.js'

function makeBox(minX, minY, minZ, maxX, maxY, maxZ) {
  const p000 = [minX, minY, minZ]
  const p001 = [minX, minY, maxZ]
  const p010 = [minX, maxY, minZ]
  const p011 = [minX, maxY, maxZ]
  const p100 = [maxX, minY, minZ]
  const p101 = [maxX, minY, maxZ]
  const p110 = [maxX, maxY, minZ]
  const p111 = [maxX, maxY, maxZ]

  const quad = (a, b, c, d) => [
    ...a, ...b, ...c, 1, 0, 0, 1, 0,
    ...a, ...c, ...d, 1, 0, 0, 1, 0,
  ]

  return [
    // Top (+Y)
    ...quad(p010, p011, p111, p110),
    // Bottom (-Y)
    ...quad(p000, p100, p101, p001),
    // Left (-X)
    ...quad(p000, p001, p011, p010),
    // Right (+X)
    ...quad(p100, p110, p111, p101),
    // Front (+Z)
    ...quad(p001, p101, p111, p011),
    // Back (-Z)
    ...quad(p000, p010, p110, p100),
  ]
}

test('coplanar triangles produce 0 edges', () => {
  const h = 60
  const a = [-h, 40, -h]
  const b = [h, 40, -h]
  const c = [h, 40, h]
  const d = [-h, 40, h]
  const coplanarTris = new Float32Array([
    ...a, ...b, ...c, 1, 1, 1, 1, 0,
    ...a, ...c, ...d, 1, 1, 1, 1, 0,
  ])
  const edges = extractSharpEdges(coplanarTris)
  assert.equal(edges.length, 0, 'coplanar quad diagonal should not be extracted as crease')
})

test('90-degree adjacent boxes produce crease edges along their seams', () => {
  // Box 1 from x in [-20, 0], Box 2 from x in [0, 20], both touching along x = 0 at y = 24
  const box1 = makeBox(-20, 0, -20, 0, 24, 20)
  const box2 = makeBox(0, 0, -20, 20, 24, 20)
  const triangles = new Float32Array([...box1, ...box2])

  const edges = extractSharpEdges(triangles)
  assert.ok(edges.length > 0, 'should extract sharp edges from 3D boxes')

  // Find the top seam at x = 0, y = 24 extending along z from -20 to 20
  const seamEdge = edges.find((e) => {
    const isAtSeam =
      Math.abs(e.p0[0]) < 1e-3 &&
      Math.abs(e.p1[0]) < 1e-3 &&
      Math.abs(e.p0[1] - 24) < 1e-3 &&
      Math.abs(e.p1[1] - 24) < 1e-3
    const spansZ =
      (Math.abs(e.p0[2] - -20) < 1e-3 && Math.abs(e.p1[2] - 20) < 1e-3) ||
      (Math.abs(e.p0[2] - 20) < 1e-3 && Math.abs(e.p1[2] - -20) < 1e-3)
    return isAtSeam && spansZ
  })

  assert.ok(seamEdge, 'seam along x=0, y=24 between adjacent boxes must be extracted as a sharp edge')
})

test('rasterization clips occluded edges and draws visible edges', () => {
  const width = 32
  const height = 32
  const camera = makeCamera(
    { position: [0, 0, 50], target: [0, 0, 0], up: [0, 1, 0], fovYDegrees: 45, zoom: 1 },
    width,
    height,
  )

  // Surface at z = 0 (distance 50 from camera at z = 50)
  const depthBuffer = new Float32Array(width * height).fill(50.0)

  // Visible edge at z = 0 (distance 50) crossing screen center horizontally
  const visibleEdge = [{ p0: [-10, 0, 0], p1: [10, 0, 0] }]
  const colorBufferVisible = new Float32Array(width * height * 3).fill(1.0)
  rasterizeEdges(visibleEdge, camera, depthBuffer, width, height, colorBufferVisible)

  const centerIdx = (16 * width + 16) * 3
  assert.ok(
    colorBufferVisible[centerIdx] < 0.5,
    `visible edge must be drawn and darken center pixel (got ${colorBufferVisible[centerIdx]})`,
  )

  // Occluded edge at z = -20 (distance 70), behind the surface at depth 50
  const occludedEdge = [{ p0: [-10, 0, -20], p1: [10, 0, -20] }]
  const colorBufferOccluded = new Float32Array(width * height * 3).fill(1.0)
  rasterizeEdges(occludedEdge, camera, depthBuffer, width, height, colorBufferOccluded)

  assert.equal(
    colorBufferOccluded[centerIdx],
    1.0,
    'occluded edge behind depth buffer must be clipped/hidden',
  )

  // Edge entirely behind the camera (e.g. z = 60, camera is at z = 50 looking at z = 0)
  const behindEdge = [{ p0: [-10, 0, 60], p1: [10, 0, 60] }]
  const colorBufferBehind = new Float32Array(width * height * 3).fill(1.0)
  rasterizeEdges(behindEdge, camera, depthBuffer, width, height, colorBufferBehind)
  assert.equal(colorBufferBehind[centerIdx], 1.0, 'edge behind camera near plane must be rejected')
})

test('50% zoom scales camera NDC appropriately', () => {
  const width = 100
  const height = 100
  const contractDefault = {
    position: [0, 0, 50],
    target: [0, 0, 0],
    up: [0, 1, 0],
    fovYDegrees: 45,
  }
  const contract1x = {
    ...contractDefault,
    zoom: 1.0,
  }

  const camDefault = makeCamera(contractDefault, width, height)
  const cam1x = makeCamera(contract1x, width, height)

  assert.equal(DEFAULT_ZOOM, 1.5)
  assert.ok(
    Math.abs(cam1x.tanHalfY / camDefault.tanHalfY - 1.5) < 1e-9,
    `default camera tanHalfY should be scaled down by 1.5 (got ratio ${cam1x.tanHalfY / camDefault.tanHalfY})`,
  )

  // Project an off-center point [5, 5, 0] (cz = 50)
  // ndc = camCoord / (cz * tanHalfY)
  // Smaller tanHalfY means larger NDC (zoomed in view of object)
  const p = [5, 5, 0]
  const cz = 50
  const ndcX1x = p[0] / (cz * cam1x.aspect * cam1x.tanHalfY)
  const ndcXDefault = p[0] / (cz * camDefault.aspect * camDefault.tanHalfY)

  assert.ok(
    Math.abs(ndcXDefault / ndcX1x - 1.5) < 1e-9,
    `NDC coordinates should be scaled up by exactly 1.5x under 50% zoom (got ratio ${ndcXDefault / ndcX1x})`,
  )
})
