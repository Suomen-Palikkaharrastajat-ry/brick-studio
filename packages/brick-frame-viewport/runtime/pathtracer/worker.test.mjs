/**
 * Protocol tests of the persistent path tracer worker (worker.js), driven
 * through `createPathTracerWorker` directly, without threads. Yields go
 * through setImmediate, so a message the test hands in between passes is
 * seen at the next yield, as it would be in a real worker.
 */
import assert from 'node:assert/strict'
import test from 'node:test'
import { createPathTracerWorker } from './worker.js'

// A white-ish square at height 0 and a red one above it, both in view.
const SQUARE = [
  -60, 0, -60, 60, 0, -60, 60, 0, 60, 0.9, 0.9, 0.9, 1, 0,
  -60, 0, -60, 60, 0, 60, -60, 0, 60, 0.9, 0.9, 0.9, 1, 0,
  -20, 30, -20, 20, 30, -20, 20, 30, 20, 0.8, 0.1, 0.1, 1, 0,
  -20, 30, -20, 20, 30, 20, -20, 30, 20, 0.8, 0.1, 0.1, 1, 0,
]
// A blue quad between the camera and the target, covering the centre pixel.
const BLUE_QUAD = [
  -15, 50, 200, 15, 50, 200, 15, 76, 200, 0, 0, 1, 1, 0,
  -15, 50, 200, 15, 76, 200, -15, 76, 200, 0, 0, 1, 1, 0,
]
const CAMERA = { position: [0, 80, 300], target: [0, 30, 0], up: [0, 1, 0], fovYDegrees: 45 }
const SIDE_CAMERA = { position: [300, 80, 0], target: [0, 30, 0], up: [0, 1, 0], fovYDegrees: 45 }
const WIDTH = 12
const HEIGHT = 8
const REQUEST = { width: WIDTH, height: HEIGHT, baseAspect: WIDTH / HEIGHT, transparent: false, shader: 'glossy' }

const tick = () => new Promise((resolve) => setImmediate(resolve))

/** A worker plus the messages it posted. `next(pred)` waits for a matching message not yet consumed. */
function harness({ index = 0, count = 1, triangles = SQUARE } = {}) {
  const messages = []
  const worker = createPathTracerWorker((msg) => messages.push(msg), { yieldTurn: tick })
  worker.handle({ type: 'init', request: REQUEST, triangles: Float32Array.from(triangles), index, count })
  async function next(pred, limit = 20000) {
    for (let i = 0; i < limit; i++) {
      const found = messages.find(pred)
      if (found) return found
      await tick()
    }
    throw new Error('message never arrived')
  }
  return { worker, messages, next }
}

function frameMsg(frame, extra = {}) {
  return { type: 'frame', frame, camera: CAMERA, samples: null, ...extra }
}

test('a fixed-sample frame renders every row exactly N times and echoes the frame id', async () => {
  const { worker, messages, next } = harness()
  worker.handle(frameMsg(1, { samples: 3 }))
  const done = await next((m) => m.type === 'done')
  assert.equal(done.frame, 1)
  assert.equal(done.counts.length, HEIGHT)
  assert.ok(done.counts.every((n) => n === 3), `counts ${Array.from(done.counts)}`)
  assert.equal(done.sums.length, HEIGHT * WIDTH * 4)
  assert.equal(messages.filter((m) => m.type === 'guide').length, 1)
  const progress = messages.filter((m) => m.type === 'progress').map((m) => m.samples)
  assert.deepEqual(progress, [1, 2, 3])
  assert.ok(messages.every((m) => m.frame === 1))
})

test('a fixed-sample frame ignores stop', async () => {
  const { worker, next } = harness()
  worker.handle(frameMsg(1, { samples: 4 }))
  worker.handle({ type: 'stop', frame: 1 })
  const done = await next((m) => m.type === 'done')
  assert.ok(done.counts.every((n) => n === 4))
})

test('the same worker renders a second frame with a new camera without a new init', async () => {
  const { worker, next } = harness()
  worker.handle(frameMsg(1, { samples: 1 }))
  const guide1 = await next((m) => m.type === 'guide' && m.frame === 1)
  await next((m) => m.type === 'done' && m.frame === 1)
  worker.handle(frameMsg(2, { samples: 1, camera: SIDE_CAMERA }))
  const guide2 = await next((m) => m.type === 'guide' && m.frame === 2)
  const done2 = await next((m) => m.type === 'done' && m.frame === 2)
  assert.ok(done2.counts.every((n) => n === 1))
  assert.notDeepEqual(Array.from(guide1.normal), Array.from(guide2.normal))
})

test('extra triangles are a per-frame layer: they show in their frame and are gone in the next', async () => {
  const { worker, next } = harness()
  const centre = ((HEIGHT / 2) * WIDTH + WIDTH / 2) * 3
  worker.handle(frameMsg(1, { samples: 1, extraTriangles: Float32Array.from(BLUE_QUAD) }))
  const withExtra = await next((m) => m.type === 'guide' && m.frame === 1)
  assert.deepEqual(Array.from(withExtra.albedo.subarray(centre, centre + 3)), [0, 0, 1])
  await next((m) => m.type === 'done' && m.frame === 1)
  worker.handle(frameMsg(2, { samples: 1 }))
  const without = await next((m) => m.type === 'guide' && m.frame === 2)
  const a = Array.from(without.albedo.subarray(centre, centre + 3))
  assert.ok(a[0] > 0.5 && a[2] < 0.6, `the base scene, not the blue quad, at the centre: got ${a}`)
  const fresh = harness()
  fresh.worker.handle(frameMsg(1, { samples: 1 }))
  const reference = await fresh.next((m) => m.type === 'guide')
  assert.deepEqual(Array.from(without.albedo), Array.from(reference.albedo))
})

test('budget mode: a stop that arrives right after the frame, before any yield, still ends it', async () => {
  const { worker, next } = harness()
  worker.handle(frameMsg(1))
  worker.handle({ type: 'stop', frame: 1 })
  const done = await next((m) => m.type === 'done')
  assert.ok(done.counts.every((n) => n === 1), `counts ${Array.from(done.counts)}`)
})

test('budget mode: a stop after the first pass ends the frame with even row counts', async () => {
  const { worker, next } = harness()
  worker.handle(frameMsg(1))
  await next((m) => m.type === 'progress')
  worker.handle({ type: 'stop', frame: 1 })
  const done = await next((m) => m.type === 'done')
  const counts = Array.from(done.counts)
  assert.ok(Math.min(...counts) >= 1)
  assert.ok(Math.max(...counts) - Math.min(...counts) <= 1)
})

test('a stale stop for an earlier frame does not cut the next budget frame short', async () => {
  const { worker, messages, next } = harness()
  worker.handle(frameMsg(1, { samples: 1 }))
  await next((m) => m.type === 'done' && m.frame === 1)
  worker.handle(frameMsg(2))
  worker.handle({ type: 'stop', frame: 1 })
  await next((m) => m.type === 'progress' && m.frame === 2 && m.samples >= 3)
  assert.equal(messages.filter((m) => m.type === 'done' && m.frame === 2).length, 0)
  worker.handle({ type: 'stop', frame: 2 })
  const done = await next((m) => m.type === 'done' && m.frame === 2)
  assert.ok(Math.min(...done.counts) >= 3)
})

test('a sample count that is not a positive integer is an error, not an endless frame', async () => {
  for (const bad of [NaN, Infinity, 0, -1, 1.5, '3']) {
    const { worker, messages, next } = harness()
    worker.handle(frameMsg(1, { samples: bad }))
    const err = await next((m) => m.type === 'error')
    assert.match(err.message, /positive integer/, String(bad))
    assert.equal(messages.filter((m) => m.type === 'progress').length, 0)
  }
})

test('rows are identical whichever worker renders them (per-pixel seeding)', async () => {
  const single = harness({ index: 0, count: 1 })
  single.worker.handle(frameMsg(1, { samples: 2 }))
  const all = await single.next((m) => m.type === 'done')
  for (const index of [0, 1]) {
    const part = harness({ index, count: 2 })
    part.worker.handle(frameMsg(1, { samples: 2 }))
    const done = await part.next((m) => m.type === 'done')
    for (let j = 0; j < done.counts.length; j++) {
      const y = index + j * 2
      assert.deepEqual(
        Array.from(done.sums.subarray(j * WIDTH * 4, (j + 1) * WIDTH * 4)),
        Array.from(all.sums.subarray(y * WIDTH * 4, (y + 1) * WIDTH * 4)),
        `row ${y}`,
      )
    }
  }
})

test('a frame with no static and no extra triangles reports an error', async () => {
  const { worker, next } = harness({ triangles: [] })
  worker.handle(frameMsg(1, { samples: 1 }))
  const err = await next((m) => m.type === 'error')
  assert.match(err.message, /No model triangles/)
  assert.equal(err.frame, 1)
})

test('a motor-style frame with empty static triangles renders its extra triangles', async () => {
  const { worker, next } = harness({ triangles: [] })
  worker.handle(frameMsg(1, { samples: 1, extraTriangles: Float32Array.from(SQUARE) }))
  const done = await next((m) => m.type === 'done')
  assert.ok(done.counts.every((n) => n === 1))
})

test('extra triangles with a wrong stride report an error', async () => {
  const { worker, next } = harness()
  worker.handle(frameMsg(1, { samples: 1, extraTriangles: new Float32Array(15) }))
  const err = await next((m) => m.type === 'error')
  assert.match(err.message, /14 numbers per triangle/)
})
