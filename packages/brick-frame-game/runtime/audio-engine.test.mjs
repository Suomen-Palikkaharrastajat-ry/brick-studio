// Unit tests of the base audio engine's loop helpers against a fake
// AudioContext that records how the nodes are wired.
import assert from 'node:assert/strict'
import test from 'node:test'
import { createNoiseLoop, createOscillatorLoop } from './audio-engine.js'

function param(value = 0) {
  return { value }
}

function node(kind, extra = {}) {
  return {
    kind,
    connections: [],
    started: false,
    connect(target) {
      this.connections.push(target)
    },
    start() {
      this.started = true
    },
    ...extra,
  }
}

function fakeContext() {
  return {
    createBufferSource: () => node('source', { buffer: null, loop: false }),
    createBiquadFilter: () => node('filter', { type: 'lowpass', frequency: param(350), Q: param(1) }),
    createGain: () => node('gain', { gain: param(1) }),
    createOscillator: () => node('osc', { type: 'sine', frequency: param(440) }),
  }
}

test('a noise loop runs the buffer through the filter into a silent gain', () => {
  const ctx = fakeContext()
  const out = node('destination')
  const buffer = { samples: 1 }
  const { source, filter, level } = createNoiseLoop(ctx, buffer, out, {
    type: 'bandpass',
    frequency: 320,
    Q: 0.8,
  })
  assert.equal(source.buffer, buffer)
  assert.equal(source.loop, true)
  assert.equal(source.started, true)
  assert.deepEqual(source.connections, [filter])
  assert.equal(filter.type, 'bandpass')
  assert.equal(filter.frequency.value, 320)
  assert.equal(filter.Q.value, 0.8)
  assert.deepEqual(filter.connections, [level])
  assert.equal(level.gain.value, 0)
  assert.deepEqual(level.connections, [out])
})

test('a noise loop without Q keeps the filter default', () => {
  const { filter } = createNoiseLoop(fakeContext(), {}, node('destination'), {
    type: 'lowpass',
    frequency: 120,
  })
  assert.equal(filter.Q.value, 1)
})

test('an oscillator loop feeds a silent gain', () => {
  const out = node('destination')
  const { osc, level } = createOscillatorLoop(fakeContext(), out, { type: 'triangle', frequency: 42 })
  assert.equal(osc.type, 'triangle')
  assert.equal(osc.frequency.value, 42)
  assert.equal(osc.started, true)
  assert.deepEqual(osc.connections, [level])
  assert.equal(level.gain.value, 0)
  assert.deepEqual(level.connections, [out])
})
