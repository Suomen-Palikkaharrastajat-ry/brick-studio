/**
 * Seedable 32-bit PRNG (sfc32) for the path tracer's hot loop.
 *
 * `createRng(seed)` returns `{ next(), reset(seed) }` where `next()` yields a
 * float in [0, 1) and `reset(seed)` re-seeds the generator in place (no
 * allocation), as if it had just been created with that seed. Two generators
 * with the same seed produce the same sequence, so renders are reproducible.
 * No `Math.random` anywhere in the tracer.
 *
 * `pixelSeed(x, y, sampleIndex)` hashes a pixel and sample number into a
 * 32-bit seed. The tracer reseeds per pixel sample with it, so an image does
 * not depend on how rows are split between workers.
 */

/** murmur3 32-bit finaliser: a cheap, well-mixed integer hash. */
function fmix32(h) {
  h ^= h >>> 16
  h = Math.imul(h, 0x85ebca6b)
  h ^= h >>> 13
  h = Math.imul(h, 0xc2b2ae35)
  h ^= h >>> 16
  return h >>> 0
}

/** 32-bit seed for sample `sampleIndex` of pixel (x, y). */
export function pixelSeed(x, y, sampleIndex) {
  return fmix32(Math.imul(x, 73856093) ^ Math.imul(y, 19349663) ^ Math.imul(sampleIndex, 83492791))
}

/** Deterministic generator seeded from an integer (any value). */
export function createRng(seed = 1) {
  const state = { a: 0, b: 0, c: 0, d: 1 }
  const warm = () => {
    let t = (state.a + state.b) | 0
    state.a = state.b ^ (state.b >>> 9)
    state.b = (state.c + (state.c << 3)) | 0
    state.c = (state.c << 21) | (state.c >>> 11)
    state.d = (state.d + 1) | 0
    t = (t + state.d) | 0
    state.c = (state.c + t) | 0
    return t
  }
  const reset = (s) => {
    // Integer multiplies keep the low bits of large (hashed) seeds.
    state.a = 0x9e3779b9 ^ s
    state.b = 0x243f6a88 ^ Math.imul(s, 0x85ebca6b)
    state.c = 0xb7e15162 ^ Math.imul(s, 0xc2b2ae35)
    state.d = 1
    // Warm up so nearby seeds decorrelate.
    for (let i = 0; i < 16; i++) warm()
  }
  reset(seed)
  return {
    next() {
      return (warm() >>> 0) / 4294967296
    },
    reset,
  }
}
