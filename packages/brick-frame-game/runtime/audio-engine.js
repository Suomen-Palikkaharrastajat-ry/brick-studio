/**
 * Base procedural Web Audio synthesizer for Brick Game family.
 * Zero external sound assets: provides AudioContext lifecycle, pink noise buffer,
 * master gain ramping, unlock handlers, and tone synthesis primitives.
 */

export function createPinkNoiseBuffer(ctx, durationSec = 2) {
  const sampleRate = ctx.sampleRate
  const bufferLen = Math.floor(sampleRate * durationSec)
  const noiseBuffer = ctx.createBuffer(1, bufferLen, sampleRate)
  const data = noiseBuffer.getChannelData(0)
  let b0 = 0
  let b1 = 0
  let b2 = 0
  for (let i = 0; i < bufferLen; i++) {
    const white = Math.random() * 2 - 1
    b0 = 0.99765 * b0 + white * 0.099046
    b1 = 0.963 * b1 + white * 0.2965164
    b2 = 0.57 * b2 + white * 1.0526913
    data[i] = Math.max(-1, Math.min(1, (b0 + b1 + b2 + white * 0.1848) * 0.16))
  }
  return noiseBuffer
}

export class BaseAudioEngine {
  constructor(initialMuted = false) {
    this.ctx = null
    this.masterGain = null
    this.noiseBuffer = null
    this.muted = Boolean(initialMuted)
    this.unlocked = false
  }

  ensureContext() {
    if (typeof window === 'undefined') return null
    const AudioCtx = window.AudioContext || window.webkitAudioContext
    if (!AudioCtx) return null
    if (this.ctx) {
      if (this.ctx.state === 'suspended') {
        this.ctx.resume().catch(() => {})
      }
      return this.ctx
    }

    this.ctx = new AudioCtx()
    this.masterGain = this.ctx.createGain()
    this.masterGain.gain.value = this.muted ? 0 : 0.45
    this.masterGain.connect(this.ctx.destination)

    this.noiseBuffer = createPinkNoiseBuffer(this.ctx, 2)
    this.initContinuousLoops(this.ctx, this.noiseBuffer, this.masterGain)
    this.unlocked = true
    return this.ctx
  }

  /** Override in game-specific audio engines */
  initContinuousLoops(ctx, noiseBuffer, masterGain) {}

  unlock() {
    this.ensureContext()
  }

  setMuted(nextMuted) {
    this.muted = Boolean(nextMuted)
    if (this.ctx && this.masterGain) {
      const now = this.ctx.currentTime
      this.masterGain.gain.cancelScheduledValues(now)
      this.masterGain.gain.linearRampToValueAtTime(this.muted ? 0 : 0.45, now + 0.04)
    }
  }

  playTone(freq, duration, delay = 0, type = 'triangle', peak = 0.25) {
    const c = this.ensureContext()
    if (!c || this.muted) return
    const t = c.currentTime + delay
    const osc = c.createOscillator()
    const g = c.createGain()
    osc.type = type
    osc.frequency.setValueAtTime(freq, t)
    g.gain.setValueAtTime(0.0001, t)
    g.gain.exponentialRampToValueAtTime(peak, t + 0.015)
    g.gain.exponentialRampToValueAtTime(0.0001, t + duration)
    osc.connect(g)
    g.connect(this.masterGain)
    osc.start(t)
    osc.stop(t + duration + 0.02)
  }

  createNoiseBurst(duration, decayTime = 0.04) {
    const c = this.ensureContext()
    if (!c) return null
    const sampleRate = c.sampleRate
    const len = Math.floor(sampleRate * duration)
    const buf = c.createBuffer(1, len, sampleRate)
    const ch = buf.getChannelData(0)
    for (let i = 0; i < len; i++) {
      ch[i] = (Math.random() * 2 - 1) * Math.exp(-i / (sampleRate * decayTime))
    }
    const src = c.createBufferSource()
    src.buffer = buf
    return src
  }

  /** Override in subclasses */
  updateContinuous(state) {}

  /** Override in subclasses */
  playSfx(event) {}
}
