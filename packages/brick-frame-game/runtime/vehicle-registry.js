/**
 * Vehicle registry and pull-based model transfer for multiplayer rooms.
 *
 * A player's vehicle model (a build link hash or LDraw text, called the
 * "payload") is large compared with position telemetry, so it never travels
 * inside room updates. Instead every payload is identified by a short content
 * id, and peers fetch a payload they do not have yet exactly once:
 *
 *   - `vehicleIdFor(payload)` is the first 16 hex characters of the SHA-256
 *     digest of the payload's UTF-8 bytes. It uses `crypto.subtle` when the
 *     page has it and an equivalent built-in SHA-256 otherwise (pages served
 *     over plain http have no `crypto.subtle`), so every peer computes the
 *     same id for the same payload.
 *   - Room updates and telemetry carry only `vehicleId` (plus a ready flag).
 *   - A peer that sees an id it does not have sends `{type:'need', ids}` to
 *     the peer that announced it (in the star topology of `share.js` that is
 *     always the host). The holder answers once per peer with
 *     `{type:'vehicle', id, payload}`; large messages are chunked by the
 *     transport.
 *   - Each id is requested at most once per session. A request that is not
 *     answered is retried after `retryMs` (5 s) at most `maxRetries` (3)
 *     times, and in-flight requests are deduplicated.
 *   - A relaying holder (the host) remembers who asked for an id it does not
 *     have yet and answers them when the payload arrives.
 *
 * Payloads are not limited by byte size: build links already carry whole
 * models in brick-play URLs. LDraw text sent by a peer is limited to
 * `MAX_VEHICLE_PARTS` part lines (the same 600-part limit the sails ingest
 * enforces), and refused with a clear message to the sender otherwise.
 *
 * Everything is kept in memory only.
 */

/** Maximum number of part lines (`1 ...`) in LDraw text a peer may send. */
export const MAX_VEHICLE_PARTS = 600

/** Length of a vehicle id in hex characters (64 bits of the digest). */
export const VEHICLE_ID_LENGTH = 16

export const VEHICLE_RETRY_MS = 5000
export const VEHICLE_MAX_RETRIES = 3

// ── Content id ──────────────────────────────────────────────────────────────

const SHA256_K = new Uint32Array([
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
])

function utf8Bytes(text) {
  if (typeof TextEncoder !== 'undefined') return new TextEncoder().encode(text)
  const bin = unescape(encodeURIComponent(text))
  const out = new Uint8Array(bin.length)
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i)
  return out
}

/** Plain SHA-256 of a byte array, used when `crypto.subtle` is missing. */
export function sha256(bytes) {
  const bitLen = bytes.length * 8
  const padded = new Uint8Array(((bytes.length + 9 + 63) >> 6) << 6)
  padded.set(bytes)
  padded[bytes.length] = 0x80
  const view = new DataView(padded.buffer)
  view.setUint32(padded.length - 8, Math.floor(bitLen / 0x100000000))
  view.setUint32(padded.length - 4, bitLen >>> 0)

  const h = new Uint32Array([
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
  ])
  const w = new Uint32Array(64)
  const rotr = (x, n) => (x >>> n) | (x << (32 - n))
  for (let off = 0; off < padded.length; off += 64) {
    for (let i = 0; i < 16; i++) w[i] = view.getUint32(off + i * 4)
    for (let i = 16; i < 64; i++) {
      const s0 = rotr(w[i - 15], 7) ^ rotr(w[i - 15], 18) ^ (w[i - 15] >>> 3)
      const s1 = rotr(w[i - 2], 17) ^ rotr(w[i - 2], 19) ^ (w[i - 2] >>> 10)
      w[i] = (w[i - 16] + s0 + w[i - 7] + s1) >>> 0
    }
    let [a, b, c, d, e, f, g, hh] = h
    for (let i = 0; i < 64; i++) {
      const S1 = rotr(e, 6) ^ rotr(e, 11) ^ rotr(e, 25)
      const ch = (e & f) ^ (~e & g)
      const t1 = (hh + S1 + ch + SHA256_K[i] + w[i]) >>> 0
      const S0 = rotr(a, 2) ^ rotr(a, 13) ^ rotr(a, 22)
      const maj = (a & b) ^ (a & c) ^ (b & c)
      const t2 = (S0 + maj) >>> 0
      hh = g
      g = f
      f = e
      e = (d + t1) >>> 0
      d = c
      c = b
      b = a
      a = (t1 + t2) >>> 0
    }
    h[0] = (h[0] + a) >>> 0
    h[1] = (h[1] + b) >>> 0
    h[2] = (h[2] + c) >>> 0
    h[3] = (h[3] + d) >>> 0
    h[4] = (h[4] + e) >>> 0
    h[5] = (h[5] + f) >>> 0
    h[6] = (h[6] + g) >>> 0
    h[7] = (h[7] + hh) >>> 0
  }
  const out = new Uint8Array(32)
  const outView = new DataView(out.buffer)
  for (let i = 0; i < 8; i++) outView.setUint32(i * 4, h[i])
  return out
}

function toHexId(digest) {
  let hex = ''
  for (let i = 0; i < VEHICLE_ID_LENGTH / 2; i++) hex += digest[i].toString(16).padStart(2, '0')
  return hex
}

/** Content id of a payload, computed without `crypto.subtle`. */
export function vehicleIdForSync(payload) {
  return toHexId(sha256(utf8Bytes(String(payload ?? ''))))
}

/**
 * Content id of a payload: 16 hex characters of its SHA-256 digest.
 * `options.subtle` overrides the WebCrypto implementation (`null` forces the
 * built-in digest).
 */
export async function vehicleIdFor(payload, options = {}) {
  const subtle =
    options.subtle !== undefined
      ? options.subtle
      : typeof crypto !== 'undefined' && crypto.subtle
        ? crypto.subtle
        : null
  const bytes = utf8Bytes(String(payload ?? ''))
  if (subtle && typeof subtle.digest === 'function') {
    try {
      const digest = await subtle.digest('SHA-256', bytes)
      return toHexId(new Uint8Array(digest))
    } catch {
      // fall through to the built-in digest
    }
  }
  return toHexId(sha256(bytes))
}

export function isVehicleId(id) {
  return typeof id === 'string' && /^[0-9a-f]{16}$/.test(id)
}

// ── Payload checks ──────────────────────────────────────────────────────────

/** Whether a payload is LDraw text rather than a build link or a name. */
export function isLDrawText(payload) {
  if (typeof payload !== 'string') return false
  return payload.split(/\r?\n/).some((line) => {
    const t = line.trim()
    return t.startsWith('1 ') || t.startsWith('0 FILE') || t.startsWith('0 Name:')
  })
}

/** Number of part reference lines (`1 ...`) in LDraw text. */
export function countLDrawParts(text) {
  let count = 0
  for (const line of String(text ?? '').split(/\r?\n/)) {
    if (line.trim().startsWith('1 ')) count++
  }
  return count
}

/**
 * Check a payload before it is stored or shared. Returns `null` when it is
 * acceptable, or a short sentence saying why not.
 */
export function vehiclePayloadProblem(payload, maxParts = MAX_VEHICLE_PARTS) {
  if (typeof payload !== 'string' || payload.length === 0) return 'The model is empty.'
  if (isLDrawText(payload)) {
    const parts = countLDrawParts(payload)
    if (parts > maxParts) {
      return `That model has ${parts} parts; at most ${maxParts} can be shared in a room.`
    }
  }
  return null
}

// ── Exchange ────────────────────────────────────────────────────────────────

/**
 * Transport-agnostic registry plus `need`/`vehicle` protocol.
 *
 * Options:
 *   send(peerId, msg)     deliver a message to one peer; returns false if it
 *                         could not be sent
 *   onPayload(id, payload, fromPeer)  a payload became available
 *   onError(message, peerId)          a peer refused our payload, or a
 *                                     request gave up
 *   sourceFor(id)         for a relaying holder: which peer to ask for an id
 *                         someone needs and we do not have yet
 *   maxParts, retryMs, maxRetries, setTimeout, clearTimeout
 */
export class VehicleExchange {
  constructor(options = {}) {
    this.sendFn = options.send || (() => false)
    this.onPayload = options.onPayload || (() => {})
    this.onError = options.onError || (() => {})
    this.sourceFor = options.sourceFor || (() => null)
    this.maxParts = options.maxParts ?? MAX_VEHICLE_PARTS
    this.retryMs = options.retryMs ?? VEHICLE_RETRY_MS
    this.maxRetries = options.maxRetries ?? VEHICLE_MAX_RETRIES
    this.setTimer = options.setTimeout || ((fn, ms) => setTimeout(fn, ms))
    this.clearTimer = options.clearTimeout || ((t) => clearTimeout(t))
    this.subtle = options.subtle

    /** id -> payload; kept for the whole page session. */
    this.payloads = new Map()
    /** peerId -> Set of ids already sent to that peer. */
    this.sent = new Map()
    /** id -> { peerId, attempts, timer, state: 'pending' | 'done' | 'failed' } */
    this.requests = new Map()
    /** id -> Set of peers waiting for an id we do not have yet. */
    this.waiting = new Map()
    this.stats = { payloadSends: new Map(), needSends: 0, retries: 0 }
  }

  has(id) {
    return this.payloads.has(id)
  }

  get(id) {
    return this.payloads.get(id)
  }

  /** Register our own (or an inline legacy) payload; resolves to its id. */
  async register(payload) {
    const problem = vehiclePayloadProblem(payload, this.maxParts)
    if (problem) {
      const err = new Error(problem)
      err.code = 'vehicle_rejected'
      throw err
    }
    const id = await vehicleIdFor(payload, { subtle: this.subtle })
    this.store(id, payload, null)
    return id
  }

  /** Store a payload under an id. Returns false if the payload is refused. */
  store(id, payload, fromPeer) {
    if (!id || vehiclePayloadProblem(payload, this.maxParts)) return false
    const isNew = !this.payloads.has(id)
    this.payloads.set(id, payload)
    const req = this.requests.get(id)
    if (req) {
      if (req.timer) this.clearTimer(req.timer)
      req.timer = null
      req.state = 'done'
    }
    if (isNew) {
      try {
        this.onPayload(id, payload, fromPeer)
      } catch {
        // ignore callback error
      }
    }
    const waiters = this.waiting.get(id)
    if (waiters) {
      this.waiting.delete(id)
      for (const peer of waiters) this.offer(peer, id)
    }
    return true
  }

  wasSent(peerId, id) {
    return this.sent.get(peerId)?.has(id) === true
  }

  markSent(peerId, id) {
    let set = this.sent.get(peerId)
    if (!set) {
      set = new Set()
      this.sent.set(peerId, set)
    }
    set.add(id)
  }

  /** Send a payload to a peer, at most once per peer and id. */
  offer(peerId, id) {
    if (!this.payloads.has(id) || this.wasSent(peerId, id)) return false
    const ok = this.sendFn(peerId, { type: 'vehicle', id, payload: this.payloads.get(id) })
    if (ok === false) return false
    this.markSent(peerId, id)
    this.stats.payloadSends.set(peerId, (this.stats.payloadSends.get(peerId) || 0) + 1)
    return true
  }

  /**
   * Ask `peerId` for an id we do not have. Each id is requested once per
   * session; an unanswered request is retried at most `maxRetries` times.
   */
  request(id, peerId) {
    if (!id || this.payloads.has(id) || this.requests.has(id)) return false
    const req = { peerId, attempts: 0, timer: null, state: 'pending' }
    this.requests.set(id, req)
    this.sendNeed(id, req)
    return true
  }

  sendNeed(id, req) {
    req.attempts++
    this.stats.needSends++
    this.sendFn(req.peerId, { type: 'need', ids: [id] })
    req.timer = this.setTimer(() => {
      req.timer = null
      if (req.state !== 'pending' || this.payloads.has(id)) return
      if (req.attempts > this.maxRetries) {
        req.state = 'failed'
        try {
          this.onError("Could not load another player's model.", req.peerId)
        } catch {
          // ignore callback error
        }
        return
      }
      this.stats.retries++
      this.sendNeed(id, req)
    }, this.retryMs)
  }

  /** Handle a protocol message from a peer. Returns true if it was one. */
  handleMessage(peerId, msg) {
    if (!msg || typeof msg !== 'object') return false
    switch (msg.type) {
      case 'need': {
        const ids = Array.isArray(msg.ids) ? msg.ids : []
        for (const id of ids) {
          if (typeof id !== 'string') continue
          if (this.payloads.has(id)) {
            this.offer(peerId, id)
          } else {
            let set = this.waiting.get(id)
            if (!set) {
              set = new Set()
              this.waiting.set(id, set)
            }
            set.add(peerId)
            const source = this.sourceFor(id)
            if (source && source !== peerId) this.request(id, source)
          }
        }
        return true
      }
      case 'vehicle': {
        if (typeof msg.id !== 'string') return true
        const problem = vehiclePayloadProblem(msg.payload, this.maxParts)
        if (problem) {
          this.sendFn(peerId, { type: 'vehicle_error', id: msg.id, message: problem })
          const req = this.requests.get(msg.id)
          if (req) {
            if (req.timer) this.clearTimer(req.timer)
            req.timer = null
            req.state = 'failed'
          }
          return true
        }
        this.store(msg.id, msg.payload, peerId)
        return true
      }
      case 'vehicle_error': {
        try {
          this.onError(String(msg.message || 'The model was refused.'), peerId)
        } catch {
          // ignore callback error
        }
        return true
      }
      default:
        return false
    }
  }

  /** A peer left: forget what was sent to it and what it was asked for. */
  forgetPeer(peerId) {
    this.sent.delete(peerId)
    for (const set of this.waiting.values()) set.delete(peerId)
    for (const [id, req] of this.requests) {
      if (req.peerId === peerId && req.state !== 'done') {
        if (req.timer) this.clearTimer(req.timer)
        this.requests.delete(id)
      }
    }
  }

  /** End of a room session: keep the cache, drop all per-peer state. */
  resetSession() {
    for (const req of this.requests.values()) {
      if (req.timer) this.clearTimer(req.timer)
    }
    this.requests.clear()
    this.sent.clear()
    this.waiting.clear()
  }
}
