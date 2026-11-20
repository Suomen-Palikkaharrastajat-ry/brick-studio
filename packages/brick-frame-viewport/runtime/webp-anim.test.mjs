import assert from 'node:assert/strict'
import test from 'node:test'
import { extractWebpFramePayload, muxWebpAnimation } from './webp-anim.js'

function createMockWebp(width, height, hasAlpha = false) {
  // Build a minimal single-frame RIFF WEBP file with VP8 or ALPH+VP8
  const vp8Data = new Uint8Array([0x9d, 0x01, 0x2a, width & 0xff, (width >> 8) & 0xff, height & 0xff, (height >> 8) & 0xff, 0x00])
  const chunks = []

  if (hasAlpha) {
    const alphData = new Uint8Array([0x00, 0x01, 0x02, 0x03])
    const alphChunk = new Uint8Array(8 + alphData.length)
    alphChunk.set([0x41, 0x4c, 0x50, 0x48], 0) // 'ALPH'
    alphChunk.set([alphData.length, 0, 0, 0], 4)
    alphChunk.set(alphData, 8)
    chunks.push(alphChunk)
  }

  const vp8Chunk = new Uint8Array(8 + vp8Data.length)
  vp8Chunk.set([0x56, 0x50, 0x38, 0x20], 0) // 'VP8 '
  vp8Chunk.set([vp8Data.length, 0, 0, 0], 4)
  vp8Chunk.set(vp8Data, 8)
  chunks.push(vp8Chunk)

  let payloadLen = 0
  for (const c of chunks) payloadLen += c.length

  const file = new Uint8Array(12 + payloadLen)
  file.set([0x52, 0x49, 0x46, 0x46], 0) // 'RIFF'
  const sz = file.length - 8
  file.set([sz & 0xff, (sz >> 8) & 0xff, (sz >> 16) & 0xff, (sz >> 24) & 0xff], 4)
  file.set([0x57, 0x45, 0x42, 0x50], 8) // 'WEBP'
  let off = 12
  for (const c of chunks) {
    file.set(c, off)
    off += c.length
  }
  return file
}

test('extractWebpFramePayload extracts VP8 and ALPH chunks correctly', () => {
  const mockWebp = createMockWebp(64, 48, true)
  const { payload, hasAlpha } = extractWebpFramePayload(mockWebp)
  assert.equal(hasAlpha, true)
  assert.ok(payload.length > 0)

  // Verify FourCC of first chunk is ALPH
  const tag1 = String.fromCharCode(...payload.subarray(0, 4))
  assert.equal(tag1, 'ALPH')
})

test('muxWebpAnimation builds valid RIFF WEBP animation file with VP8X, ANIM, and ANMF chunks', () => {
  const f1 = createMockWebp(64, 64, false)
  const f2 = createMockWebp(64, 64, true)

  const { bytes, dataUrl } = muxWebpAnimation([f1, f2], {
    width: 64,
    height: 64,
    fps: 10,
    loopCount: 0,
  })

  assert.ok(bytes instanceof Uint8Array)
  assert.ok(dataUrl.startsWith('data:image/webp;base64,'))

  // Check RIFF header
  assert.equal(String.fromCharCode(...bytes.subarray(0, 4)), 'RIFF')
  const totalMinus8 = bytes[4] | (bytes[5] << 8) | (bytes[6] << 16) | (bytes[7] << 24)
  assert.equal(totalMinus8, bytes.length - 8)
  assert.equal(String.fromCharCode(...bytes.subarray(8, 12)), 'WEBP')

  // Check VP8X chunk
  let off = 12
  assert.equal(String.fromCharCode(...bytes.subarray(off, off + 4)), 'VP8X')
  const vp8xSize = bytes[off + 4] | (bytes[off + 5] << 8) | (bytes[off + 6] << 16) | (bytes[off + 7] << 24)
  assert.equal(vp8xSize, 10)
  // Flags: animation (0x02) | alpha (0x10) = 0x12
  assert.equal(bytes[off + 8], 0x12)
  const canvasW = 1 + (bytes[off + 12] | (bytes[off + 13] << 8) | (bytes[off + 14] << 16))
  const canvasH = 1 + (bytes[off + 15] | (bytes[off + 16] << 8) | (bytes[off + 17] << 16))
  assert.equal(canvasW, 64)
  assert.equal(canvasH, 64)
  off += 8 + vp8xSize

  // Check ANIM chunk
  assert.equal(String.fromCharCode(...bytes.subarray(off, off + 4)), 'ANIM')
  const animSize = bytes[off + 4] | (bytes[off + 5] << 8) | (bytes[off + 6] << 16) | (bytes[off + 7] << 24)
  assert.equal(animSize, 6)
  const loopCount = bytes[off + 12] | (bytes[off + 13] << 8)
  assert.equal(loopCount, 0)
  off += 8 + animSize

  // Check ANMF chunk 1
  assert.equal(String.fromCharCode(...bytes.subarray(off, off + 4)), 'ANMF')
  const anmf1Size = bytes[off + 4] | (bytes[off + 5] << 8) | (bytes[off + 6] << 16) | (bytes[off + 7] << 24)
  const dur1 = bytes[off + 20] | (bytes[off + 21] << 8) | (bytes[off + 22] << 16)
  assert.equal(dur1, 100) // 10 fps -> 100ms
  off += 8 + anmf1Size + (anmf1Size % 2)

  // Check ANMF chunk 2
  assert.equal(String.fromCharCode(...bytes.subarray(off, off + 4)), 'ANMF')
  const anmf2Size = bytes[off + 4] | (bytes[off + 5] << 8) | (bytes[off + 6] << 16) | (bytes[off + 7] << 24)
  const dur2 = bytes[off + 20] | (bytes[off + 21] << 8) | (bytes[off + 22] << 16)
  assert.equal(dur2, 100)
  off += 8 + anmf2Size + (anmf2Size % 2)

  assert.equal(off, bytes.length)
})

test('muxWebpAnimation throws on empty frames or invalid dimensions', () => {
  assert.throws(() => muxWebpAnimation([], { width: 64, height: 64 }), /No frames/)
  assert.throws(() => muxWebpAnimation([createMockWebp(64, 64)], { width: 0, height: 64 }), /Invalid width/)
})
