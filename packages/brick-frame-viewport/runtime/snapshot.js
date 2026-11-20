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
 * - `cancelSnapshot` (Elm → JS): stops the render in flight and terminates
 *   its workers. Nothing is sent for a cancelled render.
 * - `snapshotProgress` (JS → Elm): `{ fraction, samples }`, at most about
 *   four times a second while the path tracer runs.
 * - `snapshotReady` (JS → Elm): `{ dataUrl, filename }`, sent as soon as the
 *   PNG is encoded.
 * - `snapshotFailed` (JS → Elm): a string error message, e.g. no triangles or
 *   a worker error.
 * - `saveSnapshot` (Elm → JS): `{ dataUrl, filename }`, downloaded as a file.
 */
import { renderSnapshot } from './pathtracer/index.js'
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

/** Path-traced render; reports progress, then the PNG or a failure message. */
function startPathTrace(ports, request) {
  cancelRender()
  const controller = new AbortController()
  activeRender = controller
  const filename = request.filename || DEFAULT_FILENAME
  renderSnapshot(request, {
    signal: controller.signal,
    onProgress: (progress) => ports.snapshotProgress?.send(progress),
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

export async function encodeWebp(rgba, width, height) {
  if (typeof document !== 'undefined') {
    const canvas = document.createElement('canvas')
    canvas.width = width
    canvas.height = height
    const ctx = canvas.getContext('2d')
    if (!ctx) throw new Error('Canvas 2D context unavailable')
    ctx.putImageData(new ImageData(rgba, width, height), 0, 0)
    return canvas.toDataURL('image/webp')
  }
  const canvas = new OffscreenCanvas(width, height)
  const ctx = canvas.getContext('2d')
  ctx.putImageData(new ImageData(rgba, width, height), 0, 0)
  const blob = await canvas.convertToBlob({ type: 'image/webp' })
  return new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = () => resolve(reader.result)
    reader.onerror = () => reject(reader.error)
    reader.readAsDataURL(blob)
  })
}

/** Multi-frame turntable animation render encoded to an animated WebP file. */
async function startAnimationPathTrace(ports, request) {
  cancelRender()
  const controller = new AbortController()
  activeRender = controller

  const anim = request.animation || {}
  const fps = Math.max(1, Math.min(60, Math.round(anim.fps || 15)))
  const durationSeconds = Math.max(0.5, Math.min(30, Number(anim.durationSeconds) || 4.0))
  const framesExtraTriangles =
    Array.isArray(anim.framesExtraTriangles) && anim.framesExtraTriangles.length > 0
      ? anim.framesExtraTriangles
      : null
  const totalFrames = framesExtraTriangles
    ? framesExtraTriangles.length
    : Math.max(1, Math.round(fps * durationSeconds))
  const frameDurationMs = Math.max(10, Math.round(1000 / fps))

  const totalBudgetMs = request.timeBudgetMs || 20000
  const frameBudgetMs = Math.max(100, Math.floor(totalBudgetMs / totalFrames))

  const baseFilename = request.filename || 'palikkakehys-render.webp'
  const filename = baseFilename.replace(/\.png$/i, '.webp')

  const camPos = request.camera?.position || [0, 100, 300]
  const camTarget = request.camera?.target || [0, 0, 0]
  const dx = camPos[0] - camTarget[0]
  const dy = camPos[1] - camTarget[1]
  const dz = camPos[2] - camTarget[2]

  const baseTriangles = request.triangles || []
  const frames = []
  let renderWidth = request.width || 854
  let renderHeight = request.height || 480

  try {
    for (let f = 0; f < totalFrames; f++) {
      if (controller.signal.aborted || activeRender !== controller) return

      let frameCamera = request.camera
      if (!framesExtraTriangles) {
        const angle = (2 * Math.PI * f) / totalFrames
        const rx = dx * Math.cos(angle) + dz * Math.sin(angle)
        const rz = -dx * Math.sin(angle) + dz * Math.cos(angle)
        frameCamera = {
          ...request.camera,
          position: [camTarget[0] + rx, camTarget[1] + dy, camTarget[2] + rz],
        }
      }

      let frameTriangles = baseTriangles
      if (framesExtraTriangles && framesExtraTriangles[f]?.length > 0) {
        const extra = framesExtraTriangles[f]
        if (baseTriangles instanceof Float32Array) {
          const combined = new Float32Array(baseTriangles.length + extra.length)
          combined.set(baseTriangles, 0)
          combined.set(extra, baseTriangles.length)
          frameTriangles = combined
        } else {
          frameTriangles = baseTriangles.concat(extra)
        }
      }

      const frameReq = {
        ...request,
        camera: frameCamera,
        triangles: frameTriangles,
        timeBudgetMs: frameBudgetMs,
      }

      const { dataUrl, width, height } = await renderSnapshot(frameReq, {
        signal: controller.signal,
        encodePng: encodeWebp,
        minBudgetMs: 30,
        onProgress: (prog) => {
          if (controller.signal.aborted || activeRender !== controller) return
          const fraction = Math.min(1, (f + prog.fraction) / totalFrames)
          ports.snapshotProgress?.send({ fraction, samples: f + 1 })
        },
      })

      if (width && height) {
        renderWidth = width
        renderHeight = height
      }

      frames.push({ dataUrl, durationMs: frameDurationMs })

      if (controller.signal.aborted || activeRender !== controller) return
      ports.snapshotProgress?.send({ fraction: (f + 1) / totalFrames, samples: f + 1 })
    }

    if (activeRender !== controller) return
    activeRender = null

    const { dataUrl: animatedWebpUrl } = muxWebpAnimation(frames, {
      width: renderWidth,
      height: renderHeight,
      fps,
      loopCount: 0,
    })

    ports.snapshotReady?.send({ dataUrl: animatedWebpUrl, filename })
  } catch (err) {
    if (controller.signal.aborted || activeRender !== controller) return
    activeRender = null
    ports.snapshotFailed?.send(String(err?.message ?? err))
  }
}

function captureSnapshot(ports, request) {
  if (request.triangles !== undefined && request.triangles !== null) {
    if (request.animation) {
      startAnimationPathTrace(ports, request)
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

/** Wire the snapshot ports of a host app. Ports the host does not declare are skipped. */
export function initSnapshots(ports) {
  if (ports?.captureRender) {
    ports.captureRender.subscribe((request) => captureSnapshot(ports, request))
  }
  if (ports?.cancelSnapshot) {
    ports.cancelSnapshot.subscribe(cancelRender)
  }
  if (ports?.saveSnapshot) {
    ports.saveSnapshot.subscribe(downloadSnapshot)
  }
}
