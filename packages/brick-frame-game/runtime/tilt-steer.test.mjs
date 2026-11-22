// Unit tests of the pure parts of the shared tilt steering: the steering
// curve and how a saved choice and the phone default combine.
import assert from 'node:assert/strict'
import test from 'node:test'
import { defaultTiltOn, resolveMode, steerFromAngle } from './tilt-steer.js'

test('angles inside the dead zone steer straight ahead', () => {
  assert.equal(steerFromAngle(10, 10), 0)
  assert.equal(steerFromAngle(12.9, 10), 0)
  assert.equal(steerFromAngle(7.1, 10), 0)
})

test('full lock is reached at 22 degrees and clamped beyond', () => {
  assert.equal(steerFromAngle(22, 0), 1)
  assert.equal(steerFromAngle(-22, 0), -1)
  assert.equal(steerFromAngle(60, 0), 1)
})

test('steering grows linearly between the dead zone and full lock', () => {
  const half = 3 + (22 - 3) / 2
  assert.ok(Math.abs(steerFromAngle(half, 0) - 0.5) < 1e-9)
  assert.ok(Math.abs(steerFromAngle(-half, 0) + 0.5) < 1e-9)
})

test('angles wrap around the half turn', () => {
  assert.equal(steerFromAngle(-175, 175), steerFromAngle(10, 0))
  assert.equal(steerFromAngle(175, -175), steerFromAngle(-10, 0))
  assert.ok(steerFromAngle(-175, 175) > 0)
})

test('a saved choice wins over the phone default', () => {
  assert.equal(resolveMode('tilt', false), true)
  assert.equal(resolveMode('touch', true), false)
  assert.equal(resolveMode(null, true), true)
  assert.equal(resolveMode(null, false), false)
  assert.equal(resolveMode('something else', true), true)
})

test('tilt is on by default only on coarse pointers', () => {
  const coarse = { matchMedia: (q) => ({ matches: q === '(pointer: coarse)' }) }
  const fine = { matchMedia: () => ({ matches: false }) }
  const broken = {
    matchMedia: () => {
      throw new Error('no media queries')
    },
  }
  assert.equal(defaultTiltOn(coarse), true)
  assert.equal(defaultTiltOn(fine), false)
  assert.equal(defaultTiltOn(broken), false)
  assert.equal(defaultTiltOn({}), false)
})
