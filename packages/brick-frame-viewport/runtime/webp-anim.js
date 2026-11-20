/**
 * WebP RIFF Animation Muxer.
 *
 * Muxes single-frame WebP bitstreams (VP8 / VP8L / ALPH) into a standard
 * animated WebP file (RFC 9649) with VP8X, ANIM, and sequential ANMF chunks.
 */

function uint24LE(val) {
  return [val & 0xff, (val >> 8) & 0xff, (val >> 16) & 0xff]
}

function uint32LE(val) {
  return [val & 0xff, (val >> 8) & 0xff, (val >> 16) & 0xff, (val >> 24) & 0xff]
}

function uint16LE(val) {
  return [val & 0xff, (val >> 8) & 0xff]
}

function str4(s) {
  return [s.charCodeAt(0), s.charCodeAt(1), s.charCodeAt(2), s.charCodeAt(3)]
}

/**
 * Extracts payload chunks (ALPH, VP8, VP8L) from a single-frame WebP container.
 * Accepts a data URL ('data:image/webp;base64,...'), a Uint8Array, or an ArrayBuffer.
 */
export function extractWebpFramePayload(webpData) {
  let bytes
  if (typeof webpData === 'string') {
    const commaIndex = webpData.indexOf(',')
    const b64 = commaIndex >= 0 ? webpData.slice(commaIndex + 1) : webpData
    if (typeof Buffer !== 'undefined') {
      bytes = new Uint8Array(Buffer.from(b64, 'base64'))
    } else {
      const bin = atob(b64)
      bytes = new Uint8Array(bin.length)
      for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i)
    }
  } else if (webpData instanceof Uint8Array) {
    bytes = webpData
  } else if (webpData instanceof ArrayBuffer) {
    bytes = new Uint8Array(webpData)
  } else {
    throw new Error('Unsupported webpData type')
  }

  if (bytes.length < 12) throw new Error('WebP buffer too small')
  const tagRiff = String.fromCharCode(...bytes.subarray(0, 4))
  const tagWebp = String.fromCharCode(...bytes.subarray(8, 12))
  if (tagRiff !== 'RIFF' || tagWebp !== 'WEBP') {
    throw new Error('Not a valid RIFF WEBP file')
  }

  let i = 12
  const chunks = []
  let hasAlpha = false
  while (i < bytes.length) {
    if (i + 8 > bytes.length) break
    const tag = String.fromCharCode(...bytes.subarray(i, i + 4))
    const size = bytes[i + 4] | (bytes[i + 5] << 8) | (bytes[i + 6] << 16) | (bytes[i + 7] << 24)
    const paddedSize = size + (size % 2)
    const chunkTotal = 8 + paddedSize
    if (i + chunkTotal > bytes.length) break

    if (tag === 'ALPH') {
      hasAlpha = true
      chunks.push(bytes.subarray(i, i + chunkTotal))
    } else if (tag === 'VP8 ' || tag === 'VP8L') {
      chunks.push(bytes.subarray(i, i + chunkTotal))
    }
    i += chunkTotal
  }

  if (chunks.length === 0) {
    throw new Error('No VP8/VP8L/ALPH chunk found in WebP data')
  }

  let totalLen = 0
  for (const ch of chunks) totalLen += ch.length
  const payload = new Uint8Array(totalLen)
  let offset = 0
  for (const ch of chunks) {
    payload.set(ch, offset)
    offset += ch.length
  }

  return { payload, hasAlpha }
}

/**
 * Muxes multiple WebP frames into an animated WebP file (RFC 9649).
 *
 * @param {Array<{ dataUrl?: string, bytes?: Uint8Array, durationMs?: number } | string | Uint8Array>} frames
 * @param {Object} options
 * @param {number} options.width - Canvas width in pixels
 * @param {number} options.height - Canvas height in pixels
 * @param {number} [options.fps=15] - Target frames per second for default duration
 * @param {number} [options.loopCount=0] - Number of times to loop (0 = infinite)
 * @returns {{ bytes: Uint8Array, dataUrl: string }}
 */
export function muxWebpAnimation(frames, options = {}) {
  if (!frames || frames.length === 0) {
    throw new Error('No frames provided for WebP animation')
  }

  const width = options.width
  const height = options.height
  if (!(width > 0 && height > 0)) {
    throw new Error('Invalid width and height for WebP animation')
  }

  const defaultDurationMs = Math.max(10, Math.round(1000 / (options.fps || 15)))
  const loopCount = options.loopCount !== undefined ? options.loopCount : 0

  let anyAlpha = false
  const parsedFrames = []

  for (const f of frames) {
    const rawData = f.dataUrl || f.bytes || f
    const durationMs = f.durationMs !== undefined ? f.durationMs : defaultDurationMs
    const { payload, hasAlpha } = extractWebpFramePayload(rawData)
    if (hasAlpha) anyAlpha = true
    parsedFrames.push({ payload, durationMs })
  }

  const anmfChunks = []
  for (const pf of parsedFrames) {
    const subLen = pf.payload.length
    const anmfPayloadSize = 16 + subLen
    const paddedSize = anmfPayloadSize + (anmfPayloadSize % 2)
    const chunkBytes = new Uint8Array(8 + paddedSize)

    // FourCC 'ANMF'
    chunkBytes.set(str4('ANMF'), 0)
    chunkBytes.set(uint32LE(anmfPayloadSize), 4)

    // 16-byte ANMF Header
    // Frame X (offset / 2 = 0)
    chunkBytes.set(uint24LE(0), 8)
    // Frame Y (offset / 2 = 0)
    chunkBytes.set(uint24LE(0), 11)
    // Frame Width Minus One
    chunkBytes.set(uint24LE(width - 1), 14)
    // Frame Height Minus One
    chunkBytes.set(uint24LE(height - 1), 17)
    // Frame Duration in ms
    chunkBytes.set(uint24LE(pf.durationMs), 20)
    // Flags: bit 1 = do not blend (0x02)
    chunkBytes[23] = 0x02

    // Frame payload
    chunkBytes.set(pf.payload, 24)
    anmfChunks.push(chunkBytes)
  }

  // VP8X extended chunk
  const vp8x = new Uint8Array(8 + 10)
  vp8x.set(str4('VP8X'), 0)
  vp8x.set(uint32LE(10), 4)
  // Flags: bit 1 = animation (0x02), bit 4 = alpha (0x10)
  vp8x[8] = 0x02 | (anyAlpha ? 0x10 : 0x00)
  vp8x[9] = 0
  vp8x[10] = 0
  vp8x[11] = 0
  vp8x.set(uint24LE(width - 1), 12)
  vp8x.set(uint24LE(height - 1), 15)

  // ANIM chunk
  const anim = new Uint8Array(8 + 6)
  anim.set(str4('ANIM'), 0)
  anim.set(uint32LE(6), 4)
  anim.set([0, 0, 0, 0], 8) // Background color
  anim.set(uint16LE(loopCount), 12) // Loop count (0 = infinite)

  let totalFileSize = 12 + vp8x.length + anim.length
  for (const ch of anmfChunks) totalFileSize += ch.length

  const fileBytes = new Uint8Array(totalFileSize)
  fileBytes.set(str4('RIFF'), 0)
  fileBytes.set(uint32LE(totalFileSize - 8), 4)
  fileBytes.set(str4('WEBP'), 8)

  let offset = 12
  fileBytes.set(vp8x, offset)
  offset += vp8x.length
  fileBytes.set(anim, offset)
  offset += anim.length

  for (const ch of anmfChunks) {
    fileBytes.set(ch, offset)
    offset += ch.length
  }

  let dataUrl = ''
  if (typeof Buffer !== 'undefined') {
    dataUrl = 'data:image/webp;base64,' + Buffer.from(fileBytes).toString('base64')
  } else {
    let binary = ''
    for (let i = 0; i < fileBytes.length; i++) binary += String.fromCharCode(fileBytes[i])
    dataUrl = 'data:image/webp;base64,' + btoa(binary)
  }

  return { bytes: fileBytes, dataUrl }
}
