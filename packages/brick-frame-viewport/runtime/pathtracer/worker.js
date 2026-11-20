/**
 * Module Web Worker for the path tracer. Each worker renders the rows
 * `y ≡ index (mod count)` of the image with one sample per pass. A worker is
 * persistent: it builds the static geometry once (`init`) and then renders
 * any number of frames, each with its own camera and optional per-frame
 * triangles (`frame`).
 *
 * Messages in:
 * - `{ type: 'init', request, triangles, index, count }`: `request` carries
 *   the view-independent fields (width, height, baseAspect, transparent,
 *   crop, outlineEdges, shader, lightDirection, groundMinY); `triangles` is a
 *   Float32Array transferred in, the static geometry (may be empty). Builds
 *   the static layer synchronously. No reply.
 * - `{ type: 'frame', frame, camera, extraTriangles, samples }`: renders frame
 *   number `frame`. `extraTriangles` (optional Float32Array) becomes a second
 *   geometry layer for this frame only. The worker writes the guide buffers,
 *   then samples in one of two modes:
 *   - `samples` a positive integer N: exactly N passes, so every row ends with
 *     N samples. `stop` is ignored.
 *   - `samples` null or undefined (time-budget mode, still renders): passes
 *     until `stop`. Rows that already have a sample are skipped after `stop`,
 *     so every row ends with at least one sample.
 *   Any other `samples` value is an error. Pass s of a row uses sample index
 *   s, so a frame's pixels are the same for any worker count.
 * - `{ type: 'stop', frame }`: stops the budget-mode frame `frame` (and any
 *   earlier one). Handled at once, never queued behind a frame, and a stop for
 *   an earlier frame does not cut a later frame short.
 *
 * Messages out (all but init errors carry `frame`):
 * - `{ type: 'guide', frame, index, albedo, normal, depth }` once per frame,
 *   after the scene is built.
 * - `{ type: 'progress', frame, index, samples }` after each pass (samples =
 *   fewest samples in any of this worker's rows).
 * - `{ type: 'done', frame, index, sums, counts }` with the premultiplied RGBA
 *   sums and per-row sample counts (local row order), fresh buffers per frame.
 * - `{ type: 'error', frame, message }` on failure.
 *
 * Yielding: rendering yields to the event loop about every YIELD_INTERVAL_MS
 * of work, and always at the end of a pass, so a queued `stop` is seen
 * promptly. Yields use a MessageChannel rather than setTimeout, because
 * browsers clamp nested timeouts to at least 4 ms.
 *
 * `createPathTracerWorker(post)` holds the protocol and is testable without a
 * browser. The module-level wiring at the bottom attaches it to `self`.
 */
import { prepareGeometry, prepareView } from './scene.js'
import { createRng } from './rng.js'
import { captureGuide, renderRowSample } from './trace.js'

/** Work time (ms) between yields to the event loop. */
export const YIELD_INTERVAL_MS = 30

/**
 * Returns a `yieldTurn()` that resolves in a later macrotask, via a
 * MessageChannel. Each call posts one message to the channel and resolves
 * when it comes back, so a `stop` message queued meanwhile is handled first.
 */
function createYielder() {
  const channel = new MessageChannel()
  const waiters = []
  channel.port1.onmessage = () => {
    const resolve = waiters.shift()
    if (resolve) resolve()
  }
  return () =>
    new Promise((resolve) => {
      waiters.push(resolve)
      channel.port2.postMessage(null)
    })
}

/**
 * Creates one worker's message handler. `post(message, transfer)` sends a
 * message back to the orchestrator. Returns `{ handle(msg) }` for incoming
 * messages.
 */
export function createPathTracerWorker(post, { yieldTurn = createYielder(), now = () => performance.now() } = {}) {
  let init = null
  let staticLayer = null
  let rowYs = []
  /** Highest frame number a `stop` has been received for. */
  let stoppedUpTo = -Infinity
  /** Frames run one after another, so a frame posted early waits its turn. */
  let queue = Promise.resolve()
  const rng = createRng(1)
  const pixel = new Float64Array(4)

  async function runFrame(msg) {
    const { frame } = msg
    if (!init) throw new Error('Path tracer worker used before init')
    const budgetMode = msg.samples == null
    if (!budgetMode && !(Number.isInteger(msg.samples) && msg.samples > 0)) {
      throw new Error(`Sample count must be a positive integer, got ${msg.samples}`)
    }
    const { index } = init
    const width = init.request.width
    const rows = rowYs.length
    if (rows === 0) {
      post({ type: 'done', frame, index, sums: new Float32Array(0), counts: new Uint32Array(0) }, [])
      return
    }
    const dynamicLayer = prepareGeometry(msg.extraTriangles)
    const scene = prepareView({ ...init.request, camera: msg.camera }, [staticLayer, dynamicLayer])

    const rowLen = width * 3
    const albedo = new Float32Array(rows * rowLen)
    const normal = new Float32Array(rows * rowLen)
    const depth = new Float32Array(rows * width)
    for (let j = 0; j < rows; j++) {
      captureGuide(
        scene,
        rowYs[j],
        albedo.subarray(j * rowLen, (j + 1) * rowLen),
        normal.subarray(j * rowLen, (j + 1) * rowLen),
        depth.subarray(j * width, (j + 1) * width),
      )
    }
    post({ type: 'guide', frame, index, albedo, normal, depth }, [albedo.buffer, normal.buffer, depth.buffer])

    const sums = new Float32Array(rows * width * 4)
    const counts = new Uint32Array(rows)
    const target = budgetMode ? Infinity : msg.samples
    const stopped = () => budgetMode && stoppedUpTo >= frame
    let sliceStart = now()
    for (let pass = 0; pass < target; pass++) {
      for (let j = 0; j < rows; j++) {
        if (stopped() && counts[j] > 0) continue
        renderRowSample(scene, rng, rowYs[j], sums.subarray(j * width * 4, (j + 1) * width * 4), pixel, counts[j])
        counts[j]++
        if (now() - sliceStart >= YIELD_INTERVAL_MS) {
          await yieldTurn()
          sliceStart = now()
        }
      }
      let fewest = Infinity
      for (let j = 0; j < rows; j++) fewest = Math.min(fewest, counts[j])
      post({ type: 'progress', frame, index, samples: fewest })
      // Always yield at a pass boundary: a fast pass would otherwise never let
      // a queued `stop` message in.
      await yieldTurn()
      sliceStart = now()
      if (stopped()) break
    }
    post({ type: 'done', frame, index, sums, counts }, [sums.buffer, counts.buffer])
  }

  function fail(err, frame) {
    post({ type: 'error', frame, message: String(err?.message ?? err) })
  }

  return {
    handle(msg) {
      if (msg.type === 'stop') {
        stoppedUpTo = Math.max(stoppedUpTo, msg.frame)
      } else if (msg.type === 'init') {
        try {
          init = msg
          staticLayer = prepareGeometry(msg.triangles)
          rowYs = []
          for (let y = msg.index; y < msg.request.height; y += msg.count) rowYs.push(y)
        } catch (err) {
          fail(err)
        }
      } else if (msg.type === 'frame') {
        queue = queue.then(() => runFrame(msg)).catch((err) => fail(err, msg.frame))
      }
    },
  }
}

if (typeof self !== 'undefined' && typeof self.postMessage === 'function') {
  const worker = createPathTracerWorker((msg, transfer = []) => self.postMessage(msg, transfer))
  self.onmessage = (event) => worker.handle(event.data)
}
