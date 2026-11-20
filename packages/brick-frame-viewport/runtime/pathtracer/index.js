/**
 * Offline progressive path tracer for high-quality snapshots.
 *
 * `createRenderSession(request, options)` starts a pool of module workers
 * (`worker.js`) that build the static geometry of `request.triangles` once
 * and then render frames: `session.renderFrame({ camera, extraTriangles,
 * samplesPerPixel, timeBudgetMs, minBudgetMs, onProgress })` renders one
 * camera view (plus optional per-frame triangles, e.g. turning gears), then
 * denoises it (`denoise.js`), draws outlines, tone maps (`output.js`) and
 * encodes it. The workers stay alive between frames until `session.close()`.
 *
 * A frame samples in one of two modes:
 * - `samplesPerPixel` a positive integer N: every pixel gets exactly N
 *   samples, whatever the time. Progress `fraction` is the share of the N
 *   passes done over all rows, and `samples` the fewest samples in any row.
 *   Animations use this, with N from `samplesPerPixelFor`, so every frame has
 *   the same quality. Any other value given for `samplesPerPixel` rejects.
 * - `samplesPerPixel` omitted (null or undefined): a time budget
 *   (`timeBudgetMs`, clamped by `clampBudget`, with `minBudgetMs` lowering
 *   the 5 s floor): passes until it runs out. `fraction` follows elapsed time
 *   against the budget.
 * The frame resolves to `{ dataUrl, width, height, samples }`, where
 * `samples` is the fewest samples in any row. Progress arrives at most 4
 * times a second in budget mode, and once per worker pass otherwise.
 *
 * Samples are seeded per pixel and sample number, so a frame rendered to N
 * samples is the same image for any worker count.
 *
 * `renderSnapshot(request, options)` is the still-image path: a one-frame
 * session in time-budget mode, closed when it settles.
 *
 * Cancelling through `signal` terminates every worker and rejects the frame
 * in flight with an AbortError. Nothing is posted for a cancelled render.
 */

import { denoise } from './denoise.js'
import { toRgba8 } from './output.js'
import { makeCamera } from './scene.js'
import { extractSharpEdges, rasterizeEdges } from './edges.js'

const DEFAULT_BUDGET_MS = 30000
const MIN_BUDGET_MS = 5000
const MAX_BUDGET_MS = 120000
const PROGRESS_INTERVAL_MS = 250
const TRIANGLE_STRIDE = 14

/** Time budget in ms, defaulting to 30 s and clamped to [min, 120 s] (min defaults to 5 s). */
export function clampBudget(ms, min = MIN_BUDGET_MS) {
  const value = Number.isFinite(ms) ? ms : DEFAULT_BUDGET_MS
  return Math.min(MAX_BUDGET_MS, Math.max(min, value))
}

/** One worker per spare core, never fewer than one. */
export function workerCountFor(hardwareConcurrency) {
  return Math.max(1, (hardwareConcurrency || 4) - 1)
}

function abortError() {
  const err = new Error('Snapshot render cancelled')
  err.name = 'AbortError'
  return err
}

async function encodePng(rgba, width, height) {
  if (typeof document !== 'undefined') {
    const canvas = document.createElement('canvas')
    canvas.width = width
    canvas.height = height
    const ctx = canvas.getContext('2d')
    if (!ctx) throw new Error('Canvas 2D context unavailable')
    ctx.putImageData(new ImageData(rgba, width, height), 0, 0)
    return canvas.toDataURL('image/png')
  }
  const canvas = new OffscreenCanvas(width, height)
  const ctx = canvas.getContext('2d')
  ctx.putImageData(new ImageData(rgba, width, height), 0, 0)
  const blob = await canvas.convertToBlob({ type: 'image/png' })
  return new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = () => resolve(reader.result)
    reader.onerror = () => reject(reader.error)
    reader.readAsDataURL(blob)
  })
}

/**
 * Merges the per-worker results into full-image buffers. Row `y` belongs to
 * worker `y % count`, at local row `floor(y / count)`.
 */
function assemble(results, width, height, count) {
  const pixels = width * height
  const sums = new Float32Array(pixels * 4)
  const albedo = new Float32Array(pixels * 3)
  const normal = new Float32Array(pixels * 3)
  const depthBuffer = new Float32Array(pixels)
  for (const result of results) {
    const { index, sums: rowSums, counts, guide } = result
    for (let j = 0; j < counts.length; j++) {
      const y = index + j * count
      const dst = y * width
      sums.set(rowSums.subarray(j * width * 4, (j + 1) * width * 4), dst * 4)
      albedo.set(guide.albedo.subarray(j * width * 3, (j + 1) * width * 3), dst * 3)
      normal.set(guide.normal.subarray(j * width * 3, (j + 1) * width * 3), dst * 3)
      if (guide.depth) {
        depthBuffer.set(guide.depth.subarray(j * width, (j + 1) * width), dst)
      }
    }
  }
  return { sums, albedo, normal, depthBuffer }
}

/**
 * Turns accumulated premultiplied sums into unpremultiplied colour and
 * coverage, averaging each row by its own sample count.
 */
function resolveSums(results, width, height, count, sums) {
  const pixels = width * height
  const color = new Float32Array(pixels * 3)
  const alpha = new Float32Array(pixels)
  for (const result of results) {
    for (let j = 0; j < result.counts.length; j++) {
      const n = result.counts[j]
      if (n === 0) continue
      const y = result.index + j * count
      for (let x = 0; x < width; x++) {
        const p = y * width + x
        const a = sums[p * 4 + 3] / n
        alpha[p] = a
        if (a > 0) {
          color[p * 3] = sums[p * 4] / n / a
          color[p * 3 + 1] = sums[p * 4 + 1] / n / a
          color[p * 3 + 2] = sums[p * 4 + 2] / n / a
        }
      }
    }
  }
  return { color, alpha }
}


/** Samples per pixel of an animation frame: the bounds and the per-frame sample budget. */
const MIN_FRAME_SAMPLES = 6
const MAX_FRAME_SAMPLES = 32
const FRAME_SAMPLE_BUDGET = 4e6

/**
 * Samples per pixel for an animation frame of `width` × `height` (the final
 * size, after any crop): about FRAME_SAMPLE_BUDGET samples per frame, so the
 * cost per frame is roughly the same at every resolution, and smaller frames
 * get more samples for the denoiser. Clamped to [6, 32]: 1280×720 gets 6,
 * 854×480 gets 10, 640×360 gets 17.
 */
export function samplesPerPixelFor(width, height) {
  const n = Math.round(FRAME_SAMPLE_BUDGET / (width * height))
  return Math.min(MAX_FRAME_SAMPLES, Math.max(MIN_FRAME_SAMPLES, Number.isFinite(n) ? n : MIN_FRAME_SAMPLES))
}

/** Default worker factory: the module worker next to this file. */
function defaultCreateWorker() {
  return new Worker(new URL('./worker.js', import.meta.url), { type: 'module' })
}

function checkStride(triangles) {
  if (triangles.length % TRIANGLE_STRIDE !== 0) throw new Error('Triangle data is not 14 numbers per triangle')
}

/**
 * Output size of a request: `width` × `height`, or for a crop the crop's
 * aspect scaled so its longer side matches the longer requested side (at
 * least 64 pixels each way). `baseAspect` is the uncropped aspect.
 */
function renderSize(request) {
  let width = Math.round(request.width)
  let height = Math.round(request.height)
  if (!(width > 0 && height > 0)) throw new Error('Render size must be positive')
  const baseAspect = width / height
  const maxWidth = width
  const maxHeight = height
  if (request.crop) {
    const cw = Math.max(0.01, Math.min(1, request.crop.width || 1))
    const ch = Math.max(0.01, Math.min(1, request.crop.height || 1))
    const rawW = width * cw
    const rawH = height * ch
    const maxDim = Math.max(maxWidth, maxHeight)
    const scale = maxDim / Math.max(rawW, rawH)
    width = Math.max(64, Math.round(rawW * scale))
    height = Math.max(64, Math.round(rawH * scale))
  }
  return { width, height, baseAspect }
}

/**
 * Starts a persistent render session (see the module header). `request` is
 * the contract payload without the per-frame parts: `width`, `height`,
 * `transparent`, `crop`, `outlineEdges`, `shader`, `lightDirection`,
 * `triangles` (the static geometry, an array or typed array of 14 numbers per
 * triangle; may be empty when every frame brings its own triangles) and
 * optionally `groundMinY`, the lowest model Y over all frames, which pins the
 * ground plane.
 *
 * Options: `signal`, `workerCount`, `skipDenoise`, plus two injection points
 * for tests: `createWorker(index)` returns a Web-Worker-like object
 * (`onmessage`, `onerror`, `postMessage`, `terminate`), and `encodePng(rgba,
 * width, height)` returns a data URL.
 *
 * Returns `{ width, height, workerCount, renderFrame(frameOptions), close() }`.
 * One frame renders at a time. A worker error or an abort rejects the frame
 * in flight and closes the session; later frames reject too.
 */
export function createRenderSession(request, options = {}) {
  const { signal, createWorker = defaultCreateWorker, encodePng: encode = encodePng } = options
  if (signal?.aborted) throw abortError()
  const source = request.triangles ?? []
  checkStride(source)
  const { width, height, baseAspect } = renderSize(request)
  const staticTriangles = Float32Array.from(source)
  const count = Math.min(height, options.workerCount ?? workerCountFor(globalThis.navigator?.hardwareConcurrency))
  const rowsOf = new Array(count)
  for (let i = 0; i < count; i++) rowsOf[i] = Math.ceil((height - i) / count)
  const shared = {
    width,
    height,
    baseAspect,
    transparent: Boolean(request.transparent),
    lightDirection: request.lightDirection,
    crop: request.crop,
    outlineEdges: Boolean(request.outlineEdges),
    shader: request.shader || 'glossy',
    groundMinY: Number.isFinite(request.groundMinY) ? request.groundMinY : undefined,
  }
  const outlines = request.outlineEdges !== false
  const workers = []
  let closed = false
  let failure = null
  let frameId = 0
  /** State of the frame in flight, or null. */
  let pending = null
  let staticEdges = null

  function clearFrameTimers(state) {
    clearTimeout(state.stopTimer)
    clearInterval(state.progressTimer)
    state.stopTimer = null
    state.progressTimer = null
  }

  function close() {
    if (closed) return
    closed = true
    signal?.removeEventListener('abort', onAbort)
    for (const w of workers) w.terminate()
    workers.length = 0
    const state = pending
    pending = null
    if (state) {
      clearFrameTimers(state)
      state.reject(failure ?? abortError())
    }
  }

  function fail(err) {
    if (closed) return
    failure = err
    close()
  }

  function onAbort() {
    fail(abortError())
  }

  function post(msg, i) {
    workers[i].postMessage(msg)
  }

  function emitProgress(state) {
    if (pending !== state || !state.onProgress) return
    const samples = Math.min(...state.latest)
    let fraction
    if (state.budget !== null) {
      fraction = Math.min(1, (Date.now() - state.startedAt) / state.budget)
    } else {
      let done = 0
      for (let i = 0; i < count; i++) done += Math.min(state.target, state.latest[i]) * rowsOf[i]
      fraction = Math.min(1, done / (height * state.target))
    }
    state.onProgress({ fraction, samples })
  }

  async function finish(state) {
    const done = state.results
    const { sums, albedo, normal, depthBuffer } = assemble(
      done.map((r, i) => ({ ...r, guide: state.guides[i] })),
      width,
      height,
      count,
    )
    const { color, alpha } = resolveSums(done, width, height, count, sums)
    const denoised = options.skipDenoise === true ? { color, alpha } : denoise(color, alpha, albedo, normal, width, height)
    if (outlines) {
      if (staticEdges === null) staticEdges = extractSharpEdges(staticTriangles)
      const edges = state.extra ? staticEdges.concat(extractSharpEdges(state.extra)) : staticEdges
      rasterizeEdges(edges, state.camera, depthBuffer, width, height, denoised.color, {
        crop: request.crop,
        lineWidth: 1.2,
        feather: 0.35,
        edgeColor: [0.03, 0.03, 0.03],
      })
    }
    const rgba = toRgba8(denoised.color, denoised.alpha, width, height)
    const dataUrl = await encode(rgba, width, height)
    if (pending !== state) return
    pending = null
    let samples = Infinity
    for (const r of done) for (const n of r.counts) samples = Math.min(samples, n)
    state.resolve({ dataUrl, width, height, samples: Number.isFinite(samples) ? samples : 0 })
  }

  function onMessage(msg) {
    const state = pending
    if (closed) return
    if (msg.type === 'error') {
      fail(new Error(msg.message))
      return
    }
    if (!state || msg.frame !== state.frame) return
    if (msg.type === 'guide') {
      state.guides[msg.index] = { albedo: msg.albedo, normal: msg.normal, depth: msg.depth }
    } else if (msg.type === 'progress') {
      state.latest[msg.index] = msg.samples
      emitProgress(state)
    } else if (msg.type === 'done') {
      state.results[msg.index] = { index: msg.index, sums: msg.sums, counts: msg.counts }
      state.remaining--
      if (state.remaining === 0) {
        clearFrameTimers(state)
        finish(state).catch(fail)
      }
    }
  }

  /**
   * Renders one frame. `camera` is the contract camera; `extraTriangles` adds
   * geometry for this frame only; `samplesPerPixel` (a positive integer)
   * picks fixed-sample mode, otherwise `timeBudgetMs` and
   * `minBudgetMs` set the time budget. `onProgress({ fraction, samples })`
   * reports this frame's progress.
   */
  function renderFrame(frameOptions = {}) {
    if (failure) return Promise.reject(failure)
    if (closed) return Promise.reject(abortError())
    if (pending) return Promise.reject(new Error('A frame is already rendering'))
    const { camera, extraTriangles, samplesPerPixel, timeBudgetMs, onProgress } = frameOptions
    let extra = null
    let viewCamera
    try {
      if (extraTriangles && extraTriangles.length > 0) {
        checkStride(extraTriangles)
        extra = Float32Array.from(extraTriangles)
      }
      if (staticTriangles.length === 0 && !extra) throw new Error('No model triangles to render')
      viewCamera = makeCamera(camera, width, height, baseAspect)
      if (samplesPerPixel != null && !(Number.isInteger(samplesPerPixel) && samplesPerPixel > 0)) {
        throw new Error(`samplesPerPixel must be a positive integer, got ${samplesPerPixel}`)
      }
    } catch (err) {
      return Promise.reject(err)
    }
    const frame = ++frameId
    const fixed = samplesPerPixel ?? null
    let budget = null
    if (fixed === null) {
      const minBudget = frameOptions.minBudgetMs ?? (timeBudgetMs > 0 && timeBudgetMs <= 1000 ? 30 : MIN_BUDGET_MS)
      budget = clampBudget(timeBudgetMs, minBudget)
    }
    return new Promise((resolve, reject) => {
      // Everything a frame accumulates lives here, never on the session.
      const state = {
        frame,
        resolve,
        reject,
        onProgress,
        camera: viewCamera,
        extra,
        budget,
        target: fixed,
        results: new Array(count),
        guides: new Array(count),
        latest: new Array(count).fill(0),
        remaining: count,
        startedAt: Date.now(),
        stopTimer: null,
        progressTimer: null,
      }
      pending = state
      for (let i = 0; i < count; i++) {
        const copy = extra ? new Float32Array(extra) : undefined
        workers[i].postMessage(
          { type: 'frame', frame, camera, extraTriangles: copy, samples: fixed },
          copy ? [copy.buffer] : [],
        )
      }
      if (budget !== null) {
        state.stopTimer = setTimeout(() => {
          if (pending !== state) return
          for (let i = 0; i < count; i++) post({ type: 'stop', frame }, i)
        }, budget)
        if (onProgress) state.progressTimer = setInterval(() => emitProgress(state), PROGRESS_INTERVAL_MS)
      }
    })
  }

  signal?.addEventListener('abort', onAbort, { once: true })
  for (let i = 0; i < count; i++) {
    const worker = createWorker(i)
    workers.push(worker)
    worker.onmessage = (event) => onMessage(event.data)
    worker.onerror = (event) => fail(new Error(event.message || 'Path tracer worker failed'))
    const copy = new Float32Array(staticTriangles)
    worker.postMessage({ type: 'init', request: shared, triangles: copy, index: i, count }, [copy.buffer])
  }

  return { width, height, workerCount: count, renderFrame, close }
}

/**
 * Renders the request with path tracing and resolves to `{ dataUrl, width,
 * height }` (PNG). `request` is the contract payload (`width`, `height`,
 * `transparent`, `camera`, `triangles`, `timeBudgetMs`); `triangles` may be an
 * array or a typed array of 14 numbers per triangle. The render runs until
 * the time budget is used up.
 *
 * Options: `onProgress`, `signal`, `workerCount`, `skipDenoise`, and the test
 * injection points `createWorker` and `encodePng` (see
 * `createRenderSession`). `minBudgetMs` lowers the usual 5 s floor on the
 * time budget.
 */
export async function renderSnapshot(request, options = {}) {
  if (options.signal?.aborted) throw abortError()
  const source = request.triangles
  if (!source || source.length === 0) throw new Error('No model triangles to render')
  checkStride(source)
  const session = createRenderSession(request, options)
  try {
    const { dataUrl, width, height } = await session.renderFrame({
      camera: request.camera,
      timeBudgetMs: request.timeBudgetMs,
      minBudgetMs: options.minBudgetMs,
      onProgress: options.onProgress,
    })
    return { dataUrl, width, height }
  } finally {
    session.close()
  }
}
