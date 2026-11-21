import assert from 'node:assert/strict'
import test from 'node:test'
import {
  WEBP_QUALITY,
  MIN_FRAME_MS,
  animationEtaMs,
  animationFrameCount,
  animationMotion,
  animationSamplesOverride,
  copyWebglCanvasTo2d,
  dataUrlToBlob,
  encodeWebp,
  fittedSize,
  frameDurationsMs,
  initSnapshots,
  stillProgressReporter,
} from './snapshot.js'
import { samplesPerPixelFor } from './pathtracer/index.js'

test('fittedSize preserves aspect ratio within maxWidth and maxHeight', () => {
  assert.deepEqual(fittedSize(1920, 1080, 960, 960), { width: 960, height: 540 })
  assert.deepEqual(fittedSize(1000, 2000, 500, 500), { width: 250, height: 500 })
  assert.deepEqual(fittedSize(800, 600, 0, 0), { width: 800, height: 600 })
})

test('dataUrlToBlob decodes base64 PNG data URLs into a typed Blob', async () => {
  const bytes = new Uint8Array([137, 80, 78, 71, 13, 10, 26, 10])
  const b64 = Buffer.from(bytes).toString('base64')
  const blob = dataUrlToBlob(`data:image/png;base64,${b64}`)
  assert.ok(blob instanceof Blob)
  assert.equal(blob.type, 'image/png')
  const buf = new Uint8Array(await blob.arrayBuffer())
  assert.deepEqual(Array.from(buf), Array.from(bytes))
})

test('dataUrlToBlob decodes URL-encoded data URLs and returns null for invalid input', async () => {
  const blob = dataUrlToBlob('data:text/plain,hello%20world')
  assert.ok(blob instanceof Blob)
  assert.equal(blob.type, 'text/plain')
  assert.equal(await blob.text(), 'hello world')
  assert.equal(dataUrlToBlob('https://example.com/image.png'), null)
  assert.equal(dataUrlToBlob('data:image/png'), null)
  assert.equal(dataUrlToBlob(null), null)
})

test('saveSnapshot downloads via a blob: object URL and defers revocation for iOS Safari', () => {
  const prevDoc = globalThis.document
  const createdLinks = []
  const appended = []
  let removedCount = 0

  globalThis.document = {
    createElement(tag) {
      assert.equal(tag, 'a')
      const link = {
        href: '',
        download: '',
        style: {},
        clicked: false,
        click() {
          this.clicked = true
        },
        remove() {
          removedCount++
        },
      }
      createdLinks.push(link)
      return link
    },
    body: {
      appendChild(el) {
        appended.push(el)
      },
    },
  }

  try {
    let savedHandler = null
    initSnapshots({
      saveSnapshot: {
        subscribe(fn) {
          savedHandler = fn
        },
      },
    })
    assert.equal(typeof savedHandler, 'function')

    const b64 = Buffer.from([137, 80, 78, 71]).toString('base64')
    savedHandler({
      dataUrl: `data:image/png;base64,${b64}`,
      filename: 'model.png',
    })

    assert.equal(createdLinks.length, 1)
    const link = createdLinks[0]
    assert.ok(link.href.startsWith('blob:'), `expected blob: URL for iOS Safari compatibility, got ${link.href}`)
    assert.equal(link.download, 'model.png')
    assert.equal(link.style.display, 'none')
    assert.equal(link.clicked, true)
    assert.equal(appended.length, 1)
    assert.equal(
      removedCount,
      0,
      'link removal and URL revocation must be deferred so iOS Safari can complete its async download prompt',
    )
    if (typeof URL.revokeObjectURL === 'function') {
      URL.revokeObjectURL(link.href)
    }
  } finally {
    globalThis.document = prevDoc
  }
})

test('copyWebglCanvasTo2d reads pixels via gl.readPixels and flips rows vertically', () => {
  const prevDoc = globalThis.document
  const prevImageData = globalThis.ImageData
  let readPixelsCalled = false
  let putImageDataCalled = false
  let drawImageCalled = false

  globalThis.ImageData = class MockImageData {
    constructor(data, width, height) {
      this.data = data
      this.width = width
      this.height = height
    }
  }

  const mockSrcCanvas = {
    width: 0,
    height: 0,
    getContext(type) {
      if (type === '2d') {
        return {
          putImageData(imgData, x, y) {
            putImageDataCalled = true
            assert.equal(x, 0)
            assert.equal(y, 0)
            // Bottom row in WebGL (y=0) should be at top row in 2D (y=0)
            // Let's verify row flipping:
            // WebGL had [10, 20, 30, 255] at bottom row (index 0..3)
            // and [40, 50, 60, 255] at top row (index 4..7)
            // After flip: top row (index 0..3) is [40, 50, 60, 255]
            assert.equal(imgData.data[0], 40)
            assert.equal(imgData.data[1], 50)
            assert.equal(imgData.data[4], 10)
            assert.equal(imgData.data[5], 20)
          },
        }
      }
      return null
    },
  }

  globalThis.document = {
    createElement(tag) {
      if (tag === 'canvas') return mockSrcCanvas
      return {}
    },
    documentElement: {},
  }

  try {
    const mockWebgl = {
      drawingBufferWidth: 1,
      drawingBufferHeight: 2,
      readPixels(x, y, w, h, format, type, dst) {
        readPixelsCalled = true
        assert.equal(format, 0x1908)
        assert.equal(type, 0x1401)
        // Row 0 (bottom row):
        dst[0] = 10
        dst[1] = 20
        dst[2] = 30
        dst[3] = 255
        // Row 1 (top row):
        dst[4] = 40
        dst[5] = 50
        dst[6] = 60
        dst[7] = 255
      },
      RGBA: 0x1908,
      UNSIGNED_BYTE: 0x1401,
    }

    const mockCanvas = {
      getContext(type) {
        if (type === 'webgl2') return mockWebgl
        return null
      },
    }

    const targetCtx = {
      fillRect() {},
      drawImage(source, x, y, w, h) {
        drawImageCalled = true
        assert.equal(source, mockSrcCanvas)
        assert.equal(w, 100)
        assert.equal(h, 200)
      },
    }

    copyWebglCanvasTo2d(mockCanvas, targetCtx, 100, 200, false)
    assert.ok(readPixelsCalled, 'readPixels should be called')
    assert.ok(putImageDataCalled, 'putImageData should be called')
    assert.ok(drawImageCalled, 'drawImage should be called')
  } finally {
    globalThis.document = prevDoc
    globalThis.ImageData = prevImageData
  }
})

test('copyWebglCanvasTo2d falls back to drawImage when canvas lacks WebGL context', () => {
  let fallbackDrawn = false
  const mockCanvas = {
    getContext() {
      return null
    },
  }
  const targetCtx = {
    fillRect() {},
    drawImage(source, x, y, w, h) {
      fallbackDrawn = true
      assert.equal(source, mockCanvas)
      assert.equal(w, 80)
      assert.equal(h, 60)
    },
  }

  copyWebglCanvasTo2d(mockCanvas, targetCtx, 80, 60, true)
  assert.ok(fallbackDrawn, 'fallback drawImage should be called')
})

test('3D projection (u, v) in perspective-aligned cropping preview matches pathtracer primary ray NDC', () => {
  const targetW = 1920
  const targetH = 1080
  const targetAspect = targetW / targetH
  const fovYDegrees = 45
  const tanHalfY = Math.tan(((fovYDegrees / 2) * Math.PI) / 180)

  // Test across multiple window dimensions: ultrawide, 16:9, 4:3, portrait
  const windowSizes = [
    { srcW: 2560, srcH: 1080 },
    { srcW: 1920, srcH: 1080 },
    { srcW: 1024, srcH: 768 },
    { srcW: 800, srcH: 1000 },
  ]

  // Arbitrary 3D camera coordinates in front of camera
  const testPoints = [
    [0, 0, 100],
    [25, -15, 120],
    [-40, 30, 80],
  ]

  for (const { srcW, srcH } of windowSizes) {
    const srcAspect = srcW / srcH
    const drawnW = targetH * srcAspect
    const offsetX = (targetW - drawnW) / 2

    for (const [cx, cy, cz] of testPoints) {
      // Path tracer primary ray projection (16:9 baseAspect, 1.0x zoom)
      const ptNdcX = cx / (cz * targetAspect * tanHalfY)
      const ptNdcY = cy / (cz * tanHalfY)
      const ptU = (ptNdcX + 1) / 2
      const ptV = (1 - ptNdcY) / 2

      // WebGL modeler canvas projection
      const webglNdcX = cx / (cz * srcAspect * tanHalfY)
      const webglNdcY = cy / (cz * tanHalfY)
      const webglU = (webglNdcX + 1) / 2
      const webglV = (1 - webglNdcY) / 2

      // Preview canvas composite projection
      const previewX = offsetX + webglU * drawnW
      const previewY = webglV * targetH
      const previewU = previewX / targetW
      const previewV = previewY / targetH

      assert.ok(
        Math.abs(previewU - ptU) < 1e-9,
        `Horizontal coordinate previewU (${previewU}) must match path tracer ptU (${ptU}) for window ${srcW}x${srcH}`,
      )
      assert.ok(
        Math.abs(previewV - ptV) < 1e-9,
        `Vertical coordinate previewV (${previewV}) must match path tracer ptV (${ptV}) for window ${srcW}x${srcH}`,
      )
    }
  }
})

test('encodeWebp asks the canvas for WebP at WEBP_QUALITY', async () => {
  const prevDoc = globalThis.document
  const prevImageData = globalThis.ImageData
  const calls = []
  globalThis.ImageData = class MockImageData {
    constructor(data, width, height) {
      this.data = data
      this.width = width
      this.height = height
    }
  }
  globalThis.document = {
    createElement(tag) {
      assert.equal(tag, 'canvas')
      return {
        width: 0,
        height: 0,
        getContext: () => ({ putImageData() {} }),
        toDataURL(type, quality) {
          calls.push({ type, quality })
          return 'data:image/webp;base64,AAAA'
        },
      }
    },
  }
  try {
    assert.equal(WEBP_QUALITY, 0.92)
    assert.equal(await encodeWebp(new Uint8ClampedArray(4), 1, 1), 'data:image/webp;base64,AAAA')
    assert.deepEqual(calls, [{ type: 'image/webp', quality: 0.92 }])
  } finally {
    globalThis.document = prevDoc
    globalThis.ImageData = prevImageData
  }
})

test('animationSamplesOverride rounds and clamps an explicit sample count, and ignores bad values', () => {
  assert.equal(animationSamplesOverride(64), 64)
  assert.equal(animationSamplesOverride(6.6), 7)
  assert.equal(animationSamplesOverride(1000), 256)
  assert.equal(animationSamplesOverride(0), null)
  assert.equal(animationSamplesOverride(undefined), null)
  assert.equal(animationSamplesOverride('x'), null)
})

/** A minimal single-frame RIFF WEBP (VP8) as a data URL, enough for the muxer. */
function mockWebpDataUrl(width, height) {
  const vp8 = [0x9d, 0x01, 0x2a, width & 0xff, (width >> 8) & 0xff, height & 0xff, (height >> 8) & 0xff, 0x00]
  const chunk = [0x56, 0x50, 0x38, 0x20, vp8.length, 0, 0, 0, ...vp8]
  const size = 4 + chunk.length
  const file = [0x52, 0x49, 0x46, 0x46, size & 0xff, size >> 8, 0, 0, 0x57, 0x45, 0x42, 0x50, ...chunk]
  return `data:image/webp;base64,${Buffer.from(file).toString('base64')}`
}

const ANIM_CAMERA = { position: [0, 100, 300], target: [0, 0, 0], up: [0, 1, 0], fovYDegrees: 45 }
const TRI = [0, 0, 0, 10, 0, 0, 0, 0, 10, 1, 1, 1, 1, 0]

/**
 * A fake render session factory. Each frame reports two progress steps and
 * resolves, unless `hang` is set: then it waits for the abort signal.
 */
function fakeSessions({ hang = false, width = 64, height = 48 } = {}) {
  const sessions = []
  const create = (request, options) => {
    const session = {
      request,
      options,
      width,
      height,
      frames: [],
      closed: 0,
      close() {
        session.closed++
      },
      async renderFrame(frame) {
        session.frames.push(frame)
        if (hang) {
          return new Promise((_, reject) => {
            options.signal.addEventListener('abort', () => reject(Object.assign(new Error('cancelled'), { name: 'AbortError' })))
          })
        }
        const n = frame.samplesPerPixel
        frame.onProgress?.({ fraction: 0, samples: 0, stage: 'preparing' })
        frame.onProgress?.({ fraction: 0.5, samples: Math.floor(n / 2) })
        frame.onProgress?.({ fraction: 1, samples: n })
        return { dataUrl: mockWebpDataUrl(64, 48), width: 64, height: 48, samples: n }
      },
    }
    sessions.push(session)
    return session
  }
  return { sessions, create }
}

/** Ports recording what JS sends to Elm, plus the captureRender and cancelSnapshot handlers. */
function animationPorts() {
  const sent = { progress: [], ready: [], failed: [] }
  const handlers = {}
  const ports = {
    captureRender: { subscribe: (fn) => (handlers.capture = fn) },
    cancelSnapshot: { subscribe: (fn) => (handlers.cancel = fn) },
    snapshotProgress: { send: (p) => sent.progress.push(p) },
    snapshotReady: { send: (r) => sent.ready.push(r) },
    snapshotFailed: { send: (m) => sent.failed.push(m) },
  }
  return { ports, sent, handlers }
}

const settle = () => new Promise((resolve) => setTimeout(resolve, 10))

test('a turntable animation renders every frame at the size-based N and reports per-frame progress', async () => {
  const { sessions, create } = fakeSessions()
  const { ports, sent, handlers } = animationPorts()
  initSnapshots(ports, { createRenderSession: create })
  handlers.capture({
    width: 64,
    height: 48,
    camera: ANIM_CAMERA,
    triangles: TRI,
    timeBudgetMs: 6000,
    filename: 'model.png',
    animation: { fps: 10, durationSeconds: 0.5 },
  })
  await settle()
  assert.equal(sessions.length, 1, 'one session for the whole animation')
  const [session] = sessions
  const n = samplesPerPixelFor(64, 48)
  assert.equal(session.frames.length, 5)
  assert.ok(session.frames.every((f) => f.samplesPerPixel === n), 'every frame gets the same N')
  const positions = session.frames.map((f) => f.camera.position.join(','))
  assert.equal(new Set(positions).size, 5, 'the camera turns')
  assert.ok(session.frames.every((f) => f.extraTriangles === undefined))
  assert.equal(session.request.groundMinY, undefined)
  assert.ok(session.closed >= 1)
  assert.equal(sent.failed.length, 0)
  assert.equal(sent.ready.length, 1)
  assert.equal(sent.ready[0].filename, 'model.webp')
  assert.match(sent.ready[0].dataUrl, /^data:image\/webp;base64,/)
  const fractions = sent.progress.map((p) => p.fraction)
  for (let i = 1; i < fractions.length; i++) assert.ok(fractions[i] >= fractions[i - 1], `fractions ${fractions}`)
  const last = sent.progress[sent.progress.length - 1]
  assert.deepEqual(
    { ...last, elapsedMs: undefined },
    { fraction: 1, samples: n, stage: 'assembling', elapsedMs: undefined, etaMs: null, frame: 5, totalFrames: 5, targetSamples: n },
  )
  assert.deepEqual(sent.progress[0].stage, 'preparing', 'the first report says the scene is being prepared')
  assert.ok(
    sent.progress.every((p) => p.frame === 1 || p.stage !== 'preparing'),
    'only the first frame reports preparing',
  )
  assert.equal(sent.progress[0].etaMs, null, 'no estimate before any work is done')
  assert.ok(sent.progress.every((p) => Number.isInteger(p.elapsedMs) && p.elapsedMs >= 0))
  const midFrames = sent.progress.filter((p) => p.frame >= 2 && p.stage !== 'assembling')
  assert.ok(midFrames.every((p) => Number.isInteger(p.etaMs) && p.etaMs >= 0), 'an estimate once a frame is done')
  assert.ok(sent.progress.every((p) => p.samples <= n), 'samples is the current frame\'s samples per pixel')
  // A frame's sampling fills only part of its slot; denoise and encode take the rest.
  const midFrame = sent.progress.find((p) => p.frame === 1 && p.samples === n)
  assert.ok(midFrame.fraction < 1 / 5, `frame 0 sampled is ${midFrame.fraction}`)
})

test('the animation sample count follows the final frame size and ignores the time budget', async () => {
  const counts = []
  for (const timeBudgetMs of [6000, 60000, undefined]) {
    // The session reports the size after the crop; N follows that size.
    const { sessions, create } = fakeSessions({ width: 854, height: 480 })
    const { ports, handlers } = animationPorts()
    initSnapshots(ports, { createRenderSession: create })
    handlers.capture({ width: 1280, height: 720, camera: ANIM_CAMERA, triangles: TRI, timeBudgetMs, animation: { fps: 10, durationSeconds: 0.5 } })
    await settle()
    counts.push(sessions[0].frames[0].samplesPerPixel)
  }
  assert.deepEqual(counts, [10, 10, 10])
})

test('a motor animation passes each frame its gear triangles, keeps the camera, and pins the ground', async () => {
  const { sessions, create } = fakeSessions()
  const { ports, sent, handlers } = animationPorts()
  initSnapshots(ports, { createRenderSession: create })
  const gear = (y) => [0, y, 0, 5, y, 0, 0, y, 5, 1, 0, 0, 1, 0]
  const extras = [gear(3), gear(-2), []]
  handlers.capture({
    width: 64,
    height: 48,
    camera: ANIM_CAMERA,
    triangles: TRI,
    timeBudgetMs: 6000,
    animation: { fps: 10, durationSeconds: 2, framesExtraTriangles: extras, samplesPerPixel: 6 },
  })
  await settle()
  const [session] = sessions
  assert.equal(session.frames.length, 3)
  assert.deepEqual(session.frames.map((f) => f.extraTriangles), extras)
  assert.ok(session.frames.every((f) => f.camera === ANIM_CAMERA))
  assert.equal(session.request.groundMinY, -2)
  assert.ok(session.frames.every((f) => f.samplesPerPixel === 6), 'the explicit override wins')
  assert.equal(sent.ready.length, 1)
})

test('cancelling an animation closes its session and sends nothing more', async () => {
  const { sessions, create } = fakeSessions({ hang: true })
  const { ports, sent, handlers } = animationPorts()
  initSnapshots(ports, { createRenderSession: create })
  handlers.capture({ width: 64, height: 48, camera: ANIM_CAMERA, triangles: TRI, animation: { fps: 10, durationSeconds: 1 } })
  await settle()
  assert.ok(sessions[0].options.signal instanceof AbortSignal)
  handlers.cancel()
  await settle()
  assert.ok(sessions[0].options.signal.aborted)
  assert.ok(sessions[0].closed >= 1)
  assert.equal(sent.ready.length, 0)
  assert.equal(sent.failed.length, 0)
})

test('a new render supersedes an animation in flight and closes its session', async () => {
  const hanging = fakeSessions({ hang: true })
  const { ports, sent, handlers } = animationPorts()
  initSnapshots(ports, { createRenderSession: hanging.create })
  const request = { width: 64, height: 48, camera: ANIM_CAMERA, triangles: TRI, animation: { fps: 10, durationSeconds: 1 } }
  handlers.capture(request)
  await settle()
  handlers.capture(request)
  await settle()
  assert.equal(hanging.sessions.length, 2)
  assert.ok(hanging.sessions[0].options.signal.aborted)
  assert.ok(hanging.sessions[0].closed >= 1)
  assert.equal(hanging.sessions[1].closed, 0, 'the new render is still running')
  handlers.cancel()
  await settle()
  assert.equal(sent.failed.length, 0)
})

test('animationMotion takes an explicit motion, and otherwise infers it from the frames', () => {
  assert.equal(animationMotion({ motion: 'motor' }), 'motor')
  assert.equal(animationMotion({ motion: 'motor', framesExtraTriangles: [] }), 'motor')
  assert.equal(animationMotion({ motion: 'turntable', framesExtraTriangles: [[1]] }), 'turntable')
  assert.equal(animationMotion({ framesExtraTriangles: [[], []] }), 'motor')
  assert.equal(animationMotion({ framesExtraTriangles: [] }), 'turntable')
  assert.equal(animationMotion({}), 'turntable')
  assert.equal(animationMotion({ motion: 'spin', framesExtraTriangles: [[1]] }), 'motor')
  assert.equal(animationMotion(undefined), 'turntable')
})

/** Renders one animation request through a fake session and returns the session and what was sent. */
async function renderAnimation(animation) {
  const { sessions, create } = fakeSessions()
  const { ports, sent, handlers } = animationPorts()
  initSnapshots(ports, { createRenderSession: create })
  handlers.capture({ width: 64, height: 48, camera: ANIM_CAMERA, triangles: TRI, animation })
  await settle()
  return { session: sessions[0], sent }
}

test('explicit motor motion keeps the camera still and gives each frame its own triangles', async () => {
  const gear = [0, -4, 0, 5, -4, 0, 0, -4, 5, 1, 0, 0, 1, 0]
  const { session, sent } = await renderAnimation({ fps: 10, durationSeconds: 2, motion: 'motor', framesExtraTriangles: [gear, [], gear] })
  assert.equal(session.frames.length, 3, 'one frame per entry')
  assert.ok(session.frames.every((f) => f.camera === ANIM_CAMERA))
  assert.deepEqual(session.frames.map((f) => f.extraTriangles), [gear, [], gear])
  assert.equal(session.request.groundMinY, -4)
  assert.equal(sent.ready.length, 1)
  assert.equal(sent.failed.length, 0)
})

test('explicit motor motion with only empty frames renders the static model with a still camera', async () => {
  const { session, sent } = await renderAnimation({ fps: 10, durationSeconds: 2, motion: 'motor', framesExtraTriangles: [[], [], [], []] })
  assert.equal(session.frames.length, 4)
  assert.ok(session.frames.every((f) => f.camera === ANIM_CAMERA))
  assert.ok(session.frames.every((f) => Array.isArray(f.extraTriangles) && f.extraTriangles.length === 0))
  assert.equal(sent.ready.length, 1)
  assert.equal(sent.failed.length, 0)
})

test('explicit motor motion without any frame list still keeps the camera still', async () => {
  const { session } = await renderAnimation({ fps: 10, durationSeconds: 0.5, motion: 'motor' })
  assert.equal(session.frames.length, 5)
  assert.ok(session.frames.every((f) => f.camera === ANIM_CAMERA && f.extraTriangles === undefined))
})

test('explicit turntable motion turns the camera a full circle and ignores any frame triangles', async () => {
  const { session } = await renderAnimation({ fps: 10, durationSeconds: 0.5, motion: 'turntable', framesExtraTriangles: [TRI, TRI] })
  assert.equal(session.frames.length, 5, 'fps × duration frames, not one per entry')
  assert.ok(session.frames.every((f) => f.extraTriangles === undefined))
  assert.equal(session.request.groundMinY, undefined)
  const positions = session.frames.map((f) => f.camera.position)
  assert.equal(new Set(positions.map((p) => p.join(','))).size, 5)
  // Frame 0 is the original view; the steps are equal parts of 360°.
  assert.deepEqual(positions[0].map((v) => Math.round(v * 1e6) / 1e6), ANIM_CAMERA.position)
  const angle = (p) => Math.atan2(p[0], p[2])
  const step = (2 * Math.PI) / 5
  for (let f = 1; f < 5; f++) {
    const turned = (angle(positions[f]) - angle(positions[0]) + 4 * Math.PI) % (2 * Math.PI)
    assert.ok(Math.abs(turned - f * step) < 1e-9, `frame ${f} turned ${turned}`)
  }
})

test('without motion, an empty frame list falls back to a turntable', async () => {
  const { session } = await renderAnimation({ fps: 10, durationSeconds: 0.5, framesExtraTriangles: [] })
  assert.equal(session.frames.length, 5)
  assert.equal(new Set(session.frames.map((f) => f.camera.position.join(','))).size, 5)
})

test('stillProgressReporter adds elapsed time and the time left in the budget plus the finish', () => {
  let t = 1000
  const sent = []
  const report = stillProgressReporter((p) => sent.push(p), { width: 100, height: 100, timeBudgetMs: 8000 }, () => t)
  t = 3000
  report({ fraction: 0.25, samples: 3, stage: 'sampling' })
  // 6 s of the 8 s budget left, plus 100 × 100 px of finishing.
  assert.deepEqual(sent[0], { fraction: 0.25, samples: 3, stage: 'sampling', elapsedMs: 2000, etaMs: 6000 + 25 })
  t = 9500
  report({ fraction: 1, samples: 20, stage: 'finishing' })
  assert.equal(sent[1].etaMs, 25, 'finishing leaves only the finish estimate')
  t = 9600
  report({ fraction: 1, samples: 20, stage: 'finishing' })
  assert.equal(sent[2].etaMs, 0, 'and counts it down')
  assert.equal(sent[2].elapsedMs, 8600)
})

test('stillProgressReporter clamps the budget like renderSnapshot', () => {
  const sent = []
  const report = stillProgressReporter((p) => sent.push(p), { width: 0, height: 0 }, () => 0)
  report({ fraction: 0, samples: 0, stage: 'preparing' })
  assert.equal(sent[0].etaMs, 30000, 'the default 30 s budget when none is given')
})

test('animationEtaMs estimates from finished frames, skipping the scene-building first one', () => {
  assert.equal(animationEtaMs({ frameMs: [], totalFrames: 10, frame: 0, frameFraction: 0.01, frameElapsedMs: 500 }), null)
  // Frame 0 half done in 2 s: 4 s a frame, 9.5 frames left.
  assert.equal(animationEtaMs({ frameMs: [], totalFrames: 10, frame: 0, frameFraction: 0.5, frameElapsedMs: 2000 }), 38000)
  // Only frame 0 done: its time is all there is.
  assert.equal(animationEtaMs({ frameMs: [5000], totalFrames: 10, frame: 1, frameFraction: 0, frameElapsedMs: 0 }), 45000)
  // Frames 1 and 2 took 3 s each; frame 0's 5 s is left out. 6.5 frames left.
  assert.equal(animationEtaMs({ frameMs: [5000, 3000, 3000], totalFrames: 10, frame: 3, frameFraction: 0.5, frameElapsedMs: 1500 }), 19500)
  assert.equal(animationEtaMs({ frameMs: [3000, 3000], totalFrames: 2, frame: 2, frameFraction: 0, frameElapsedMs: 0 }), 0)
})

/** The frame durations (ms) of an animated WebP data URL, read from its ANMF chunks. */
function anmfDurations(dataUrl) {
  const bytes = Buffer.from(dataUrl.slice(dataUrl.indexOf(',') + 1), 'base64')
  assert.equal(bytes.toString('latin1', 0, 4), 'RIFF')
  assert.equal(bytes.toString('latin1', 8, 12), 'WEBP')
  const durations = []
  let i = 12
  while (i + 8 <= bytes.length) {
    const tag = bytes.toString('latin1', i, i + 4)
    const size = bytes.readUInt32LE(i + 4)
    if (tag === 'ANMF') durations.push(bytes.readUIntLE(i + 8 + 12, 3))
    i += 8 + size + (size % 2)
  }
  return durations
}

const sum = (xs) => xs.reduce((a, b) => a + b, 0)

test('frameDurationsMs spreads the duration over the frames in whole ms with an exact total', () => {
  const cases = [
    [36, 10, 10000],
    [72, 6, 6000],
    [20, 2, 2000],
  ]
  for (const [n, d, total] of cases) {
    const durations = frameDurationsMs(n, d)
    assert.equal(durations.length, n)
    assert.equal(sum(durations), total, `${n} frames over ${d} s`)
    assert.ok(durations.every((ms) => Number.isInteger(ms)))
    assert.ok(Math.max(...durations) - Math.min(...durations) <= 1, `${n} frames over ${d} s: ${durations}`)
  }
  assert.deepEqual(new Set(frameDurationsMs(20, 2)), new Set([100]))
  assert.deepEqual(new Set(frameDurationsMs(36, 10)), new Set([277, 278]))
})

test('frameDurationsMs raises the duration so no frame is shorter than MIN_FRAME_MS', () => {
  assert.equal(MIN_FRAME_MS, 20)
  const durations = frameDurationsMs(120, 2)
  assert.equal(durations.length, 120)
  assert.ok(durations.every((ms) => ms === 20))
  assert.equal(sum(durations), 2400)
})

test('animationFrameCount rounds and clamps an explicit frame count, and ignores bad values', () => {
  assert.equal(animationFrameCount(36), 36)
  assert.equal(animationFrameCount(35.6), 36)
  assert.equal(animationFrameCount(5000), 600)
  assert.equal(animationFrameCount(0), null)
  assert.equal(animationFrameCount(-3), null)
  assert.equal(animationFrameCount(undefined), null)
  assert.equal(animationFrameCount('x'), null)
})

test('a turntable with frameCount renders that many frames and muxes the exact total duration', async () => {
  const { session, sent } = await renderAnimation({ frameCount: 36, fps: 4, durationSeconds: 10, motion: 'turntable' })
  assert.equal(session.frames.length, 36)
  assert.equal(new Set(session.frames.map((f) => f.camera.position.join(','))).size, 36)
  assert.equal(sent.ready.length, 1)
  const durations = anmfDurations(sent.ready[0].dataUrl)
  assert.equal(durations.length, 36)
  assert.deepEqual(durations, frameDurationsMs(36, 10))
  assert.equal(sum(durations), 10000)
})

test('a 72-frame, 6 s motor animation gives each frame its share of the 6 s', async () => {
  const frames = Array.from({ length: 72 }, () => [])
  const { session, sent } = await renderAnimation({ frameCount: 72, fps: 12, durationSeconds: 6, motion: 'motor', framesExtraTriangles: frames })
  assert.equal(session.frames.length, 72)
  const durations = anmfDurations(sent.ready[0].dataUrl)
  assert.equal(durations.length, 72)
  assert.equal(sum(durations), 6000)
})

test('a motor animation has one frame per entry even when frameCount disagrees', async () => {
  const { session, sent } = await renderAnimation({ frameCount: 10, fps: 2, durationSeconds: 3, motion: 'motor', framesExtraTriangles: [[], [], []] })
  assert.equal(session.frames.length, 3)
  assert.deepEqual(anmfDurations(sent.ready[0].dataUrl), [1000, 1000, 1000])
})

test('requests without frameCount keep the old frame count and uniform durations', async () => {
  const turntable = await renderAnimation({ fps: 10, durationSeconds: 0.5 })
  assert.equal(turntable.session.frames.length, 5)
  assert.deepEqual(anmfDurations(turntable.sent.ready[0].dataUrl), [100, 100, 100, 100, 100])
  // An old motor request: 3 frames at 10 fps stay 100 ms each, whatever the duration.
  const motor = await renderAnimation({ fps: 10, durationSeconds: 2, motion: 'motor', framesExtraTriangles: [[], [], []] })
  assert.deepEqual(anmfDurations(motor.sent.ready[0].dataUrl), [100, 100, 100])
  // 60 fps keeps its 17 ms frames (no 20 ms floor for old requests).
  const fast = await renderAnimation({ fps: 60, durationSeconds: 0.5 })
  assert.deepEqual(new Set(anmfDurations(fast.sent.ready[0].dataUrl)), new Set([17]))
})
