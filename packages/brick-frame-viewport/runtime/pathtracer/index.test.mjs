/**
 * End-to-end test of the orchestrator protocol (index.js) with the real
 * worker.js. Each "Web Worker" is a Node worker_threads thread running
 * worker.js under a minimal `self` shim, exposed with the Web Worker surface
 * the orchestrator uses (`onmessage`, `onerror`, `postMessage`, `terminate`).
 */
import assert from 'node:assert/strict'
import test from 'node:test'
import { Worker } from 'node:worker_threads'
import { createRenderSession, renderSnapshot, samplesPerPixelFor } from './index.js'

const WORKER_HREF = new URL('./worker.js', import.meta.url).href

/**
 * Runs in the thread: installs `self` before worker.js loads, then forwards
 * messages from the parent port into `self.onmessage`. Messages posted before
 * the listener is attached wait in the port's queue.
 */
const SHIM = `
const { parentPort } = require('node:worker_threads')
globalThis.self = {
  onmessage: null,
  postMessage: (msg, transfer) => parentPort.postMessage(msg, transfer),
}
import(${JSON.stringify(WORKER_HREF)}).then(
  () => parentPort.on('message', (data) => self.onmessage({ data })),
  (err) => parentPort.postMessage({ type: 'error', message: String(err && err.stack || err) }),
)
`

/** Web-Worker-like wrapper around a worker_threads thread running worker.js. */
class ThreadWorker {
  constructor() {
    this.thread = new Worker(SHIM, { eval: true })
    this.onmessage = null
    this.onerror = null
    this.terminated = false
    this.thread.on('message', (data) => this.onmessage?.({ data }))
    this.thread.on('error', (err) => this.onerror?.({ message: err.message }))
  }

  postMessage(msg, transfer) {
    this.thread.postMessage(msg, transfer)
  }

  terminate() {
    this.terminated = true
    return this.thread.terminate()
  }
}

// A white-ish square at height 0 and a red one above it, both in view.
const SQUARE = [
  -60, 0, -60, 60, 0, -60, 60, 0, 60, 0.9, 0.9, 0.9, 1, 0,
  -60, 0, -60, 60, 0, 60, -60, 0, 60, 0.9, 0.9, 0.9, 1, 0,
  -20, 30, -20, 20, 30, -20, 20, 30, 20, 0.8, 0.1, 0.1, 1, 0,
  -20, 30, -20, 20, 30, 20, -20, 30, 20, 0.8, 0.1, 0.1, 1, 0,
]
const CAMERA = { position: [0, 80, 300], target: [0, 30, 0], up: [0, 1, 0], fovYDegrees: 45 }
const WIDTH = 24
const HEIGHT = 16

function baseRequest(overrides = {}) {
  return {
    width: WIDTH,
    height: HEIGHT,
    transparent: false,
    camera: CAMERA,
    triangles: Float32Array.from(SQUARE),
    timeBudgetMs: 300,
    ...overrides,
  }
}

/** Test encoder: checks the RGBA buffer size and returns a marker URL. */
function fakeEncode(seen) {
  return async (rgba, width, height) => {
    seen.push({ length: rgba.length, width, height })
    return 'data:image/png;base64,FAKE'
  }
}

function recordingFactory() {
  const workers = []
  const createWorker = () => {
    const w = new ThreadWorker()
    workers.push(w)
    return w
  }
  return { workers, createWorker }
}

test('renderSnapshot resolves with a PNG, reports progress, and terminates its workers', async () => {
  const { workers, createWorker } = recordingFactory()
  const progress = []
  const encoded = []
  const { dataUrl } = await renderSnapshot(baseRequest(), {
    onProgress: (p) => progress.push(p),
    workerCount: 2,
    createWorker,
    encodePng: fakeEncode(encoded),
    minBudgetMs: 0,
  })
  assert.equal(dataUrl, 'data:image/png;base64,FAKE')
  assert.equal(workers.length, 2)
  assert.ok(progress.length >= 1, 'progress should be reported at least once')
  for (const p of progress) {
    assert.ok(p.fraction >= 0 && p.fraction <= 1, `fraction ${p.fraction}`)
    assert.ok(p.samples >= 0)
  }
  assert.deepEqual(encoded, [{ length: WIDTH * HEIGHT * 4, width: WIDTH, height: HEIGHT }])
  assert.ok(workers.every((w) => w.terminated), 'every worker is terminated after resolving')
})

test('aborting through AbortController rejects with AbortError and terminates the workers', async () => {
  const { workers, createWorker } = recordingFactory()
  const controller = new AbortController()
  const pending = renderSnapshot(baseRequest({ timeBudgetMs: 5000 }), {
    signal: controller.signal,
    workerCount: 2,
    createWorker,
    encodePng: fakeEncode([]),
    minBudgetMs: 0,
  })
  setTimeout(() => controller.abort(), 50)
  await assert.rejects(pending, (err) => err.name === 'AbortError')
  assert.equal(workers.length, 2)
  assert.ok(workers.every((w) => w.terminated), 'abort terminates every worker')
})

test('a wrong triangle stride rejects before any worker starts', async () => {
  const { workers, createWorker } = recordingFactory()
  await assert.rejects(
    renderSnapshot(baseRequest({ triangles: new Float32Array(15) }), { workerCount: 2, createWorker, minBudgetMs: 0 }),
    /14 numbers per triangle/,
  )
  assert.equal(workers.length, 0)
})

test('an already-aborted signal rejects with AbortError without starting workers', async () => {
  const { workers, createWorker } = recordingFactory()
  const controller = new AbortController()
  controller.abort()
  await assert.rejects(
    renderSnapshot(baseRequest(), { signal: controller.signal, workerCount: 2, createWorker }),
    (err) => err.name === 'AbortError',
  )
  assert.equal(workers.length, 0)
})

const SIDE_CAMERA = { position: [300, 80, 0], target: [0, 30, 0], up: [0, 1, 0], fovYDegrees: 45 }

/** Test encoder that keeps a copy of every RGBA buffer it is given. */
function capturingEncode(frames) {
  return async (rgba) => {
    frames.push(Uint8ClampedArray.from(rgba))
    return 'data:image/webp;base64,FAKE'
  }
}

test('a still render with a budget shorter than one pass still gives every row a sample', async () => {
  const { createWorker } = recordingFactory()
  const encoded = []
  const { dataUrl } = await renderSnapshot(baseRequest({ timeBudgetMs: 1 }), {
    workerCount: 2,
    createWorker,
    encodePng: fakeEncode(encoded),
    minBudgetMs: 0,
  })
  assert.equal(dataUrl, 'data:image/png;base64,FAKE')
  assert.equal(encoded.length, 1)
})

test('a session renders several frames on the same workers, each with its own camera', async () => {
  const { workers, createWorker } = recordingFactory()
  const frames = []
  const session = createRenderSession(baseRequest(), { workerCount: 3, createWorker, encodePng: capturingEncode(frames) })
  try {
    const first = await session.renderFrame({ camera: CAMERA, samplesPerPixel: 2 })
    const second = await session.renderFrame({ camera: SIDE_CAMERA, samplesPerPixel: 2 })
    assert.equal(first.samples, 2)
    assert.equal(second.samples, 2)
    assert.equal(workers.length, 3, 'no new workers for the second frame')
    assert.ok(workers.every((w) => !w.terminated), 'workers stay alive between frames')
    assert.equal(frames.length, 2)
    assert.notDeepEqual(Array.from(frames[0]), Array.from(frames[1]), 'frame 2 is not frame 1 again')
  } finally {
    session.close()
  }
  assert.ok(workers.every((w) => w.terminated), 'close terminates every worker')
})

test('a fixed-sample frame reports progress up to fraction 1 and N samples, never going back', async () => {
  const { createWorker } = recordingFactory()
  const session = createRenderSession(baseRequest(), { workerCount: 2, createWorker, encodePng: fakeEncode([]) })
  const progress = []
  try {
    const result = await session.renderFrame({ camera: CAMERA, samplesPerPixel: 5, onProgress: (p) => progress.push(p) })
    assert.equal(result.samples, 5)
  } finally {
    session.close()
  }
  const last = progress[progress.length - 1]
  assert.equal(last.fraction, 1)
  assert.equal(last.samples, 5)
  for (let i = 1; i < progress.length; i++) {
    assert.ok(progress[i].fraction >= progress[i - 1].fraction, 'fraction never decreases')
  }
})

test('a sample count that is not a positive integer rejects the frame instead of hanging', async () => {
  const { createWorker } = recordingFactory()
  const session = createRenderSession(baseRequest(), { workerCount: 2, createWorker, encodePng: fakeEncode([]) })
  try {
    for (const bad of [NaN, Infinity, 0, -3, 2.5, '4', () => 4]) {
      await assert.rejects(session.renderFrame({ camera: CAMERA, samplesPerPixel: bad }), /positive integer/, String(bad))
    }
    const result = await session.renderFrame({ camera: CAMERA, samplesPerPixel: 2 })
    assert.equal(result.samples, 2, 'the session still renders after a rejected frame')
  } finally {
    session.close()
  }
})

test('a fixed-sample frame is the same image for any worker count, denoised or not', async () => {
  for (const skipDenoise of [true, false]) {
    const images = []
    for (const workerCount of [2, 3]) {
      const { createWorker } = recordingFactory()
      const frames = []
      const session = createRenderSession(baseRequest(), { workerCount, createWorker, skipDenoise, encodePng: capturingEncode(frames) })
      try {
        await session.renderFrame({ camera: CAMERA, samplesPerPixel: 2 })
      } finally {
        session.close()
      }
      images.push(frames[0])
    }
    assert.deepEqual(Array.from(images[0]), Array.from(images[1]), `skipDenoise ${skipDenoise}`)
  }
})

test('aborting during a later frame rejects it, terminates every worker, and closes the session', async () => {
  const { workers, createWorker } = recordingFactory()
  const controller = new AbortController()
  const session = createRenderSession(baseRequest(), {
    workerCount: 2,
    createWorker,
    encodePng: fakeEncode([]),
    signal: controller.signal,
  })
  await session.renderFrame({ camera: CAMERA, samplesPerPixel: 1 })
  const pending = session.renderFrame({ camera: SIDE_CAMERA, samplesPerPixel: 100000 })
  setTimeout(() => controller.abort(), 30)
  await assert.rejects(pending, (err) => err.name === 'AbortError')
  assert.ok(workers.every((w) => w.terminated), 'abort terminates every worker')
  await assert.rejects(session.renderFrame({ camera: CAMERA, samplesPerPixel: 1 }), (err) => err.name === 'AbortError')
})

test('a frame after close rejects with AbortError', async () => {
  const { createWorker } = recordingFactory()
  const session = createRenderSession(baseRequest(), { workerCount: 1, createWorker, encodePng: fakeEncode([]) })
  session.close()
  await assert.rejects(session.renderFrame({ camera: CAMERA, samplesPerPixel: 1 }), (err) => err.name === 'AbortError')
})

test('more workers than rows: one worker per row', async () => {
  const { workers, createWorker } = recordingFactory()
  const session = createRenderSession(baseRequest(), { workerCount: 32, createWorker, encodePng: fakeEncode([]) })
  try {
    assert.equal(session.workerCount, HEIGHT)
    const result = await session.renderFrame({ camera: CAMERA, samplesPerPixel: 1 })
    assert.equal(result.samples, 1)
  } finally {
    session.close()
  }
  assert.equal(workers.length, HEIGHT)
})

test('a motor-style session with no static triangles renders frames that bring their own', async () => {
  const { createWorker } = recordingFactory()
  const session = createRenderSession(baseRequest({ triangles: [] }), { workerCount: 2, createWorker, encodePng: fakeEncode([]) })
  try {
    const result = await session.renderFrame({ camera: CAMERA, extraTriangles: SQUARE, samplesPerPixel: 1 })
    assert.equal(result.samples, 1)
    await assert.rejects(session.renderFrame({ camera: CAMERA, samplesPerPixel: 1 }), /No model triangles/)
  } finally {
    session.close()
  }
})

test('samplesPerPixelFor spends about 4M samples a frame, within [6, 32]', () => {
  assert.equal(samplesPerPixelFor(1280, 720), 6)
  assert.equal(samplesPerPixelFor(854, 480), 10)
  assert.equal(samplesPerPixelFor(640, 360), 17)
  assert.equal(samplesPerPixelFor(3840, 2160), 6)
  assert.equal(samplesPerPixelFor(64, 48), 32)
})

test('a frame reports its stages in order: preparing, sampling, then finishing last', async () => {
  const { createWorker } = recordingFactory()
  const session = createRenderSession(baseRequest(), { workerCount: 2, createWorker, encodePng: fakeEncode([]) })
  const progress = []
  try {
    await session.renderFrame({ camera: CAMERA, samplesPerPixel: 3, onProgress: (p) => progress.push(p) })
  } finally {
    session.close()
  }
  const stages = progress.map((p) => p.stage)
  assert.equal(stages[0], 'preparing', 'reported before any worker is ready')
  assert.deepEqual(progress[0], { fraction: 0, samples: 0, stage: 'preparing' })
  assert.ok(stages.includes('sampling'), `stages ${stages}`)
  assert.deepEqual(progress[progress.length - 1], { fraction: 1, samples: 3, stage: 'finishing' })
  const order = { preparing: 0, sampling: 1, finishing: 2 }
  for (let i = 1; i < stages.length; i++) {
    assert.ok(order[stages[i]] >= order[stages[i - 1]], `stages never go back: ${stages}`)
  }
})
