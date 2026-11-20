/**
 * Snapshot capture and save for the host apps' camera-globe snapshot button.
 *
 * A host calls `initSnapshots(app.ports)` and declares the ports below. Every
 * port is optional; missing ones are skipped.
 *
 * - `captureRender` (Elm → JS): `{ width, height, transparent, filename,
 *   timeBudgetMs, camera, triangles }`. When `triangles` is a non-empty flat
 *   array (14 numbers per triangle, render space, LDU), the frame is rendered
 *   by the offline progressive path tracer in `pathtracer/`. Any render still
 *   in flight is cancelled first. Without `triangles`, the largest visible
 *   WebGL canvas is copied onto an offscreen canvas that fits inside
 *   `width` × `height` with its aspect ratio kept (the fallback). Unless
 *   `transparent`, the page's `--color-surface-canvas` token fills the
 *   background first.
 * - `captureRender` with `animation: { fps, durationSeconds, motion?,
 *   framesExtraTriangles?, samplesPerPixel? }` renders an animated WebP
 *   instead (see "Animations" below).
 * - `cancelSnapshot` (Elm → JS): stops the render in flight and terminates
 *   its workers. Nothing is sent for a cancelled render.
 * - `snapshotProgress` (JS → Elm): `{ fraction, samples, stage, elapsedMs,
 *   etaMs }`, at most about four times a second while a still renders.
 *   `stage` is `"preparing"`, `"sampling"`, `"finishing"` (denoise and
 *   encode) or, for animations, `"assembling"` (building the WebP file).
 *   `elapsedMs` counts from the start of the render and `etaMs` estimates
 *   the time left, or is null while there is nothing to estimate from yet.
 *   Animations add `frame` (1-based), `totalFrames` and `targetSamples`, and
 *   report after every worker pass.
 * - `snapshotReady` (JS → Elm): `{ dataUrl, filename }`, sent as soon as the
 *   PNG (or animated WebP) is encoded.
 * - `snapshotFailed` (JS → Elm): a string error message, e.g. no triangles or
 *   a worker error.
 * - `saveSnapshot` (Elm → JS): `{ dataUrl, filename }`, downloaded as a file.
 *
 * Animations: one persistent render session (`createRenderSession` in
 * pathtracer/index.js) renders every frame, so the static model is sent to
 * the workers and its BVH built once. `animation.motion` picks the kind
 * (`animationMotion`):
 * - `"turntable"`: the camera makes one full turn around its target, over
 *   `fps × durationSeconds` frames. `framesExtraTriangles` is ignored.
 * - `"motor"`: the camera stays put and frame f adds `framesExtraTriangles[f]`
 *   (the turning gears at frame f) as a small per-frame layer, with the
 *   ground pinned at the lowest point over all frames. There is one frame per
 *   entry (or `fps × durationSeconds` frames when there are none), and an
 *   entry may be empty: that frame shows only the static model.
 * Without `motion` (older hosts), a non-empty `framesExtraTriangles` array
 * means motor and anything else turntable.
 *
 * Every frame gets the same number of samples per pixel N, so frames look
 * alike and do not flicker: `animation.samplesPerPixel` when given (rounded,
 * 1 to 256), otherwise `samplesPerPixelFor` of the final frame size (6 at
 * 720p, 10 at 480p, 17 at 360p). Render time therefore follows the frame
 * count and size; `timeBudgetMs` is not used for animations. Progress
 * `fraction` counts sampling and the main-thread denoise and encode of each
 * frame (their split is measured on the frames done so far); `samples` is
 * the current frame's samples per pixel, as for a still. Frames are encoded
 * as WebP at WEBP_QUALITY and muxed into one animated WebP.
 *
 * `initSnapshots(ports, deps)` takes optional test injection points:
 * `deps.createRenderSession` replaces the session factory for animations, and
 * `deps.sessionOptions` is passed to it (e.g. `createWorker`, `workerCount`).
 */
import { clampBudget, createRenderSession, renderSnapshot, samplesPerPixelFor, yieldToPaint } from './pathtracer/index.js'
import { triangleMinY } from './pathtracer/scene.js'
import { muxWebpAnimation } from './webp-anim.js'

if (typeof HTMLCanvasElement !== 'undefined' && HTMLCanvasElement.prototype?.getContext && !HTMLCanvasElement.prototype.__preservePatched) {
  const origGetContext = HTMLCanvasElement.prototype.getContext
  HTMLCanvasElement.prototype.getContext = function (type, attrs) {
    if (type === 'webgl' || type === 'webgl2' || type === 'experimental-webgl') {
      attrs = Object.assign({}, attrs, { preserveDrawingBuffer: true })
    }
    return origGetContext.call(this, type, attrs)
  }
  HTMLCanvasElement.prototype.__preservePatched = true
}

const DEFAULT_FILENAME = 'palikkakehys-render.png'
const REVOKE_DELAY_MS = 60000
/** Lossy WebP quality for animation frames (0..1). */
export const WEBP_QUALITY = 0.92
/** Share of a frame's progress that is sampling, until a frame has been timed. */
const DEFAULT_SAMPLING_SHARE = 0.75
/** Main-thread denoise and encode time per pixel, for a still's estimate. */
const FINISH_MS_PER_PIXEL = 0.0025
/** Share of the first frame that must be done before it is used to estimate. */
const MIN_ESTIMATE_FRACTION = 0.05
/** Most samples per pixel an explicit `animation.samplesPerPixel` may ask for. */
const MAX_OVERRIDE_SAMPLES = 256

/** The render currently in flight, or null. */
let activeRender = null

/** The visible canvas with the largest on-screen area, or null when none is shown. */
function largestVisibleCanvas(doc) {
  let best = null
  let bestArea = 0
  for (const canvas of doc.querySelectorAll('canvas')) {
    const area = canvas.clientWidth * canvas.clientHeight
    if (area > bestArea) {
      best = canvas
      bestArea = area
    }
  }
  return best
}

/**
 * Size of `source` scaled to fit inside `maxWidth` × `maxHeight`, keeping its
 * aspect ratio. A missing bound leaves that axis to the source size.
 */
export function fittedSize(sourceWidth, sourceHeight, maxWidth, maxHeight) {
  const scale = Math.min(
    maxWidth > 0 ? maxWidth / sourceWidth : Infinity,
    maxHeight > 0 ? maxHeight / sourceHeight : Infinity,
  )
  const finiteScale = Number.isFinite(scale) ? scale : 1
  return {
    width: Math.max(1, Math.round(sourceWidth * finiteScale)),
    height: Math.max(1, Math.round(sourceHeight * finiteScale)),
  }
}

function fillBackground(ctx, width, height) {
  let bg = ''
  try {
    if (typeof getComputedStyle === 'function' && typeof document !== 'undefined' && document.documentElement) {
      bg = getComputedStyle(document.documentElement)
        .getPropertyValue('--color-surface-canvas')
        .trim()
    }
  } catch {
    bg = ''
  }
  ctx.fillStyle = bg || '#05131d'
  ctx.fillRect(0, 0, width, height)
}

/**
 * Copies a WebGL or 2D canvas to a target 2D canvas context.
 *
 * Direct `ctx.drawImage(webglCanvas)` on Firefox Linux (WebRender / Mesa DMABUF)
 * reads the native BGRA surface without swizzling, swapping Red and Blue channels.
 * Reading via `gl.readPixels(..., gl.RGBA)` guarantees correct RGBA channel order
 * across all browsers, operating systems, and GPU drivers.
 */
export function copyWebglCanvasTo2d(canvas, targetCtx, targetWidth, targetHeight, transparent = false) {
  let gl = null
  try {
    gl =
      canvas.getContext?.('webgl2') ||
      canvas.getContext?.('webgl') ||
      canvas.getContext?.('experimental-webgl')
  } catch {
    gl = null
  }

  if (gl && typeof gl.readPixels === 'function' && gl.drawingBufferWidth > 0 && gl.drawingBufferHeight > 0) {
    const srcW = gl.drawingBufferWidth
    const srcH = gl.drawingBufferHeight
    const raw = new Uint8Array(srcW * srcH * 4)
    gl.readPixels(0, 0, srcW, srcH, gl.RGBA ?? 0x1908, gl.UNSIGNED_BYTE ?? 0x1401, raw)

    const flipped = new Uint8ClampedArray(srcW * srcH * 4)
    const rowBytes = srcW * 4
    for (let y = 0; y < srcH; y++) {
      const srcOffset = (srcH - 1 - y) * rowBytes
      const dstOffset = y * rowBytes
      flipped.set(raw.subarray(srcOffset, srcOffset + rowBytes), dstOffset)
    }

    if (typeof document !== 'undefined' && typeof document.createElement === 'function') {
      const srcCanvas = document.createElement('canvas')
      srcCanvas.width = srcW
      srcCanvas.height = srcH
      const srcCtx = srcCanvas.getContext?.('2d')
      if (srcCtx && typeof srcCtx.putImageData === 'function') {
        srcCtx.putImageData(new ImageData(flipped, srcW, srcH), 0, 0)
        if (!transparent) {
          fillBackground(targetCtx, targetWidth, targetHeight)
        }
        targetCtx.imageSmoothingEnabled = true
        targetCtx.imageSmoothingQuality = 'high'
        const srcAspect = srcW / srcH
        const drawnW = Math.round(targetHeight * srcAspect)
        const offsetX = Math.round((targetWidth - drawnW) / 2)
        targetCtx.drawImage(srcCanvas, offsetX, 0, drawnW, targetHeight)
        return
      }
    }
  }

  if (!transparent) {
    fillBackground(targetCtx, targetWidth, targetHeight)
  }
  targetCtx.imageSmoothingEnabled = true
  targetCtx.imageSmoothingQuality = 'high'
  const srcW = canvas.width || targetWidth
  const srcH = canvas.height || targetHeight
  const srcAspect = srcW / srcH
  const drawnW = Math.round(targetHeight * srcAspect)
  const offsetX = Math.round((targetWidth - drawnW) / 2)
  targetCtx.drawImage(canvas, offsetX, 0, drawnW, targetHeight)
}

/** Fallback: copies the on-screen WebGL canvas. */
function captureCanvasCopy(ports, { width, height, transparent = true, filename }) {
  cancelRender()
  const canvas = largestVisibleCanvas(document)
  if (!canvas) return

  const targetWidth = width > 0 ? Math.round(width) : (canvas.width || canvas.clientWidth)
  const targetHeight = height > 0 ? Math.round(height) : (canvas.height || canvas.clientHeight)
  if (!targetWidth || !targetHeight) return

  const offscreen = document.createElement('canvas')
  offscreen.width = targetWidth
  offscreen.height = targetHeight
  const ctx = offscreen.getContext('2d')
  if (!ctx) return

  copyWebglCanvasTo2d(canvas, ctx, targetWidth, targetHeight, transparent)

  ports.snapshotReady?.send({
    dataUrl: offscreen.toDataURL('image/png'),
    filename: filename || DEFAULT_FILENAME,
  })
}

function cancelRender() {
  if (!activeRender) return
  const render = activeRender
  activeRender = null
  render.abort()
}

/**
 * Wraps a still render's `send` so each progress report also carries
 * `elapsedMs` and `etaMs`. The time left is what remains of the time budget
 * (clamped like `renderSnapshot` does) plus an estimate of the main-thread
 * finish, and during `"finishing"` only what remains of that estimate.
 */
export function stillProgressReporter(send, request, now = Date.now) {
  const startedAt = now()
  const ms = request.timeBudgetMs
  const budget = clampBudget(ms, ms > 0 && ms <= 1000 ? 30 : undefined)
  const finishMs = Math.max(0, (request.width || 0) * (request.height || 0) * FINISH_MS_PER_PIXEL)
  let finishingAt = null
  return (progress) => {
    const t = now()
    const elapsed = t - startedAt
    let eta
    if (progress.stage === 'finishing') {
      if (finishingAt === null) finishingAt = t
      eta = Math.max(0, finishMs - (t - finishingAt))
    } else {
      eta = Math.max(0, budget - elapsed) + finishMs
    }
    send({ ...progress, elapsedMs: Math.round(elapsed), etaMs: Math.round(eta) })
  }
}

/**
 * Time left in an animation, in ms, or null before there is anything to go
 * on. `frameMs` holds the durations of the frames done so far; frame 0 also
 * builds the scene, so later frames are averaged without it once there are
 * any. Before the first frame is done, its own pace is used once it is at
 * least MIN_ESTIMATE_FRACTION done.
 */
export function animationEtaMs({ frameMs, totalFrames, frame, frameFraction, frameElapsedMs }) {
  const remaining = Math.max(0, totalFrames - frame - frameFraction)
  if (frameMs.length > 0) {
    const steady = frameMs.length > 1 ? frameMs.slice(1) : frameMs
    const average = steady.reduce((a, b) => a + b, 0) / steady.length
    return Math.round(average * remaining)
  }
  if (frameFraction >= MIN_ESTIMATE_FRACTION) {
    return Math.round((frameElapsedMs / frameFraction) * remaining)
  }
  return null
}

/** Path-traced render; reports progress, then the PNG or a failure message. */
function startPathTrace(ports, request) {
  cancelRender()
  const controller = new AbortController()
  activeRender = controller
  const filename = request.filename || DEFAULT_FILENAME
  renderSnapshot(request, {
    signal: controller.signal,
    onProgress: stillProgressReporter((progress) => ports.snapshotProgress?.send(progress), request),
  })
    .then(({ dataUrl }) => {
      if (activeRender !== controller) return
      activeRender = null
      ports.snapshotReady?.send({ dataUrl, filename })
    })
    .catch((err) => {
      if (controller.signal.aborted || activeRender !== controller) return
      activeRender = null
      ports.snapshotFailed?.send(String(err?.message ?? err))
    })
}

/** Encodes RGBA pixels as a WebP data URL at `quality` (0..1). */
export async function encodeWebp(rgba, width, height, quality = WEBP_QUALITY) {
  if (typeof document !== 'undefined') {
    const canvas = document.createElement('canvas')
    canvas.width = width
    canvas.height = height
    const ctx = canvas.getContext('2d')
    if (!ctx) throw new Error('Canvas 2D context unavailable')
    ctx.putImageData(new ImageData(rgba, width, height), 0, 0)
    return canvas.toDataURL('image/webp', quality)
  }
  const canvas = new OffscreenCanvas(width, height)
  const ctx = canvas.getContext('2d')
  ctx.putImageData(new ImageData(rgba, width, height), 0, 0)
  const blob = await canvas.convertToBlob({ type: 'image/webp', quality })
  return new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = () => resolve(reader.result)
    reader.onerror = () => reject(reader.error)
    reader.readAsDataURL(blob)
  })
}

/**
 * Samples per pixel from an explicit `animation.samplesPerPixel`, rounded and
 * clamped to [1, MAX_OVERRIDE_SAMPLES], or null when it is missing or not
 * positive.
 */
export function animationSamplesOverride(value) {
  const n = Number(value)
  if (!(Number.isFinite(n) && n > 0)) return null
  return Math.min(MAX_OVERRIDE_SAMPLES, Math.max(1, Math.round(n)))
}

/**
 * The kind of animation a request asks for, `'motor'` or `'turntable'`: the
 * explicit `animation.motion` when it is one of those, otherwise motor
 * exactly when `framesExtraTriangles` is a non-empty array.
 */
export function animationMotion(anim = {}) {
  if (anim.motion === 'motor' || anim.motion === 'turntable') return anim.motion
  return Array.isArray(anim.framesExtraTriangles) && anim.framesExtraTriangles.length > 0 ? 'motor' : 'turntable'
}

/** Lowest model Y over the base and every frame's extra triangles, or undefined when there are none. */
function animationMinY(baseTriangles, framesExtraTriangles) {
  let minY = triangleMinY(baseTriangles)
  for (const extra of framesExtraTriangles ?? []) minY = Math.min(minY, triangleMinY(extra))
  return Number.isFinite(minY) ? minY : undefined
}

/**
 * Multi-frame animation render (turntable or motor animation, see the module
 * header) encoded to an animated WebP file. `deps.createRenderSession` and
 * `deps.sessionOptions` are injection points for tests.
 */
async function startAnimationPathTrace(ports, request, deps = {}) {
  cancelRender()
  const controller = new AbortController()
  activeRender = controller
  const isCurrent = () => !controller.signal.aborted && activeRender === controller

  const anim = request.animation || {}
  const fps = Math.max(1, Math.min(60, Math.round(anim.fps || 15)))
  const durationSeconds = Math.max(0.5, Math.min(30, Number(anim.durationSeconds) || 4.0))
  const motor = animationMotion(anim) === 'motor'
  const framesExtraTriangles =
    motor && Array.isArray(anim.framesExtraTriangles) && anim.framesExtraTriangles.length > 0
      ? anim.framesExtraTriangles
      : null
  const totalFrames = framesExtraTriangles
    ? framesExtraTriangles.length
    : Math.max(1, Math.round(fps * durationSeconds))
  const frameDurationMs = Math.max(10, Math.round(1000 / fps))
  const override = animationSamplesOverride(anim.samplesPerPixel)

  const baseFilename = request.filename || 'palikkakehys-render.webp'
  const filename = baseFilename.replace(/\.png$/i, '.webp')

  const camPos = request.camera?.position || [0, 100, 300]
  const camTarget = request.camera?.target || [0, 0, 0]
  const dx = camPos[0] - camTarget[0]
  const dy = camPos[1] - camTarget[1]
  const dz = camPos[2] - camTarget[2]

  const baseTriangles = request.triangles || []
  const frames = []
  const createSession = deps.createRenderSession ?? createRenderSession
  let session = null

  try {
    session = createSession(
      {
        ...request,
        triangles: baseTriangles,
        groundMinY: framesExtraTriangles ? animationMinY(baseTriangles, framesExtraTriangles) : undefined,
      },
      { encodePng: encodeWebp, ...deps.sessionOptions, signal: controller.signal },
    )
    /** Samples per pixel, the same for every frame. */
    const spp = override ?? samplesPerPixelFor(session.width, session.height)
    /** Sampling and finishing (denoise, encode) time of the frames done so far. */
    let samplingMs = 0
    let finishingMs = 0
    const animationStartedAt = Date.now()
    /** Wall time of each finished frame. */
    const frameMs = []
    let frameStartedAt = animationStartedAt
    const sendProgress = (f, frameFraction, frameSamples, stage) => {
      const now = Date.now()
      ports.snapshotProgress?.send({
        fraction: Math.min(1, (f + frameFraction) / totalFrames),
        samples: frameSamples,
        stage,
        elapsedMs: now - animationStartedAt,
        etaMs:
          stage === 'assembling'
            ? null
            : animationEtaMs({ frameMs, totalFrames, frame: f, frameFraction, frameElapsedMs: now - frameStartedAt }),
        frame: f + 1,
        totalFrames,
        targetSamples: spp,
      })
    }
    sendProgress(0, 0, 0, 'preparing')

    for (let f = 0; f < totalFrames; f++) {
      if (!isCurrent()) return

      let frameCamera = request.camera
      if (!motor) {
        const angle = (2 * Math.PI * f) / totalFrames
        const rx = dx * Math.cos(angle) + dz * Math.sin(angle)
        const rz = -dx * Math.sin(angle) + dz * Math.cos(angle)
        frameCamera = {
          ...request.camera,
          position: [camTarget[0] + rx, camTarget[1] + dy, camTarget[2] + rz],
        }
      }

      // Fixed for the frame, so its progress never goes back.
      const samplingShare = samplingMs > 0 ? samplingMs / (samplingMs + finishingMs) : DEFAULT_SAMPLING_SHARE
      const startedAt = Date.now()
      frameStartedAt = startedAt
      let sampledAt = null
      const { dataUrl } = await session.renderFrame({
        camera: frameCamera,
        extraTriangles: framesExtraTriangles?.[f],
        samplesPerPixel: spp,
        onProgress: (p) => {
          if (p.fraction >= 1 && sampledAt === null) sampledAt = Date.now()
          if (!isCurrent()) return
          // Only the first frame builds the scene; later frames just re-capture
          // their guides, which is part of sampling as far as the user can tell.
          const stage = p.stage === 'preparing' && f > 0 ? 'sampling' : (p.stage ?? 'sampling')
          sendProgress(f, samplingShare * p.fraction, p.samples, stage)
        },
      })
      const endedAt = Date.now()
      samplingMs += (sampledAt ?? endedAt) - startedAt
      finishingMs += endedAt - (sampledAt ?? endedAt)
      frameMs.push(endedAt - startedAt)

      frames.push({ dataUrl, durationMs: frameDurationMs })

      if (!isCurrent()) return
      sendProgress(f, 1, spp, f + 1 < totalFrames ? 'sampling' : 'assembling')
    }

    if (activeRender !== controller) return
    session.close()
    await yieldToPaint()
    if (activeRender !== controller) return
    activeRender = null

    const { dataUrl: animatedWebpUrl } = muxWebpAnimation(frames, {
      width: session.width,
      height: session.height,
      fps,
      loopCount: 0,
    })

    ports.snapshotReady?.send({ dataUrl: animatedWebpUrl, filename })
  } catch (err) {
    if (controller.signal.aborted || activeRender !== controller) return
    activeRender = null
    ports.snapshotFailed?.send(String(err?.message ?? err))
  } finally {
    session?.close()
  }
}

function captureSnapshot(ports, request, deps) {
  if (request.triangles !== undefined && request.triangles !== null) {
    if (request.animation) {
      startAnimationPathTrace(ports, request, deps)
    } else {
      startPathTrace(ports, request)
    }
  } else {
    captureCanvasCopy(ports, request)
  }
}

/**
 * Converts a `data:` URL into a `Blob` synchronously so mobile browsers
 * (notably iOS Safari) can download or preview it via a `blob:` object URL.
 */
export function dataUrlToBlob(dataUrl) {
  if (typeof dataUrl !== 'string' || !dataUrl.startsWith('data:')) return null
  const commaIndex = dataUrl.indexOf(',')
  if (commaIndex < 0) return null
  const header = dataUrl.slice(5, commaIndex)
  const body = dataUrl.slice(commaIndex + 1)
  const parts = header.split(';')
  const mime = parts[0] || 'image/png'
  const isBase64 = parts.includes('base64')
  try {
    if (isBase64) {
      const binary = atob(body)
      const bytes = new Uint8Array(binary.length)
      for (let i = 0; i < binary.length; i++) {
        bytes[i] = binary.charCodeAt(i)
      }
      return new Blob([bytes], { type: mime })
    }
    return new Blob([decodeURIComponent(body)], { type: mime })
  } catch {
    return null
  }
}

function downloadSnapshot({ dataUrl, filename }) {
  if (!dataUrl || typeof document === 'undefined') return
  const blob = dataUrlToBlob(dataUrl)
  const objectUrl =
    blob && typeof URL !== 'undefined' && typeof URL.createObjectURL === 'function'
      ? URL.createObjectURL(blob)
      : null
  const link = document.createElement('a')
  link.href = objectUrl || dataUrl
  link.download = filename || DEFAULT_FILENAME
  if (link.style) link.style.display = 'none'
  document.body.appendChild(link)
  link.click()
  const timer = setTimeout(() => {
    link.remove()
    if (objectUrl && typeof URL.revokeObjectURL === 'function') {
      URL.revokeObjectURL(objectUrl)
    }
  }, REVOKE_DELAY_MS)
  timer?.unref?.()
}

/**
 * Wire the snapshot ports of a host app. Ports the host does not declare are
 * skipped. `deps` holds the test injection points (see the module header).
 */
export function initSnapshots(ports, deps = {}) {
  if (ports?.captureRender) {
    ports.captureRender.subscribe((request) => captureSnapshot(ports, request, deps))
  }
  if (ports?.cancelSnapshot) {
    ports.cancelSnapshot.subscribe(cancelRender)
  }
  if (ports?.saveSnapshot) {
    ports.saveSnapshot.subscribe(downloadSnapshot)
  }
}
