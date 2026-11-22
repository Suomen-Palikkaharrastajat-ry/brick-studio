import assert from 'node:assert/strict'
import test from 'node:test'
import { createHash } from 'node:crypto'
import {
  MAX_VEHICLE_PARTS,
  VehicleExchange,
  countLDrawParts,
  sha256,
  vehicleIdFor,
  vehicleIdForSync,
  vehiclePayloadProblem,
} from './vehicle-registry.js'

function ldrawWithParts(n) {
  const lines = ['0 FILE test.ldr', '0 Name: test.ldr']
  for (let i = 0; i < n; i++) lines.push(`1 4 ${i * 20} 0 0 1 0 0 0 1 0 0 0 1 3001.dat`)
  return lines.join('\n')
}

function manualTimers() {
  const pending = new Set()
  return {
    setTimeout: (fn) => {
      const t = { fn }
      pending.add(t)
      return t
    },
    clearTimeout: (t) => pending.delete(t),
    fireAll: () => {
      const list = Array.from(pending)
      pending.clear()
      for (const t of list) t.fn()
    },
    get count() {
      return pending.size
    },
  }
}

test('built-in SHA-256 matches the platform digest', () => {
  for (const text of ['', 'abc', 'ö'.repeat(1000), 'x'.repeat(55), 'y'.repeat(64), ldrawWithParts(40)]) {
    const expected = createHash('sha256').update(text, 'utf8').digest('hex')
    const actual = Buffer.from(sha256(new TextEncoder().encode(text))).toString('hex')
    assert.equal(actual, expected)
  }
})

test('vehicle id is stable for equal payloads and differs for different payloads', async () => {
  const a = '#track=quarry&seed=4&b/AAAA'
  const id1 = await vehicleIdFor(a)
  const id2 = await vehicleIdFor(String(a))
  assert.equal(id1, id2)
  assert.match(id1, /^[0-9a-f]{16}$/)
  assert.notEqual(await vehicleIdFor(a + 'B'), id1)
  assert.notEqual(await vehicleIdFor('Sloop'), await vehicleIdFor('Cutter'))
  // Pages without crypto.subtle compute the same id.
  assert.equal(await vehicleIdFor(a, { subtle: null }), id1)
  assert.equal(vehicleIdForSync(a), id1)
})

test('LDraw text over the part limit is refused; size alone is not', () => {
  assert.equal(countLDrawParts(ldrawWithParts(12)), 12)
  assert.equal(vehiclePayloadProblem(ldrawWithParts(MAX_VEHICLE_PARTS)), null)
  assert.match(vehiclePayloadProblem(ldrawWithParts(MAX_VEHICLE_PARTS + 1)), /601 parts; at most 600/)
  // A large build link is fine: there is no byte limit.
  assert.equal(vehiclePayloadProblem('#b/' + 'A'.repeat(2 * 1024 * 1024)), null)
  assert.notEqual(vehiclePayloadProblem(''), null)
})

test('register rejects a model with too many parts', async () => {
  const ex = new VehicleExchange()
  await assert.rejects(ex.register(ldrawWithParts(MAX_VEHICLE_PARTS + 5)), /at most 600/)
  assert.equal(ex.payloads.size, 0)
})

test('a need for an id is answered once per peer', async () => {
  const sent = []
  const holder = new VehicleExchange({ send: (peer, msg) => sent.push({ peer, msg }) })
  const id = await holder.register('Cutter')
  holder.handleMessage('p2', { type: 'need', ids: [id] })
  holder.handleMessage('p2', { type: 'need', ids: [id] })
  holder.handleMessage('p3', { type: 'need', ids: [id] })
  const vehicles = sent.filter((s) => s.msg.type === 'vehicle')
  assert.deepEqual(vehicles.map((s) => s.peer), ['p2', 'p3'])
  assert.equal(vehicles[0].msg.payload, 'Cutter')
})

test('a need for an unknown id is relayed and answered once when it arrives', async () => {
  const sent = []
  const host = new VehicleExchange({
    send: (peer, msg) => sent.push({ peer, msg }),
    sourceFor: () => 'owner',
    setTimeout: () => 0,
    clearTimeout: () => {},
  })
  const id = await vehicleIdFor('Schooner')
  host.handleMessage('p2', { type: 'need', ids: [id] })
  host.handleMessage('p2', { type: 'need', ids: [id] })
  host.handleMessage('p3', { type: 'need', ids: [id] })
  // The host asks the owner once.
  assert.equal(sent.filter((s) => s.msg.type === 'need' && s.peer === 'owner').length, 1)
  host.handleMessage('owner', { type: 'vehicle', id, payload: 'Schooner' })
  const vehicles = sent.filter((s) => s.msg.type === 'vehicle')
  assert.deepEqual(vehicles.map((s) => s.peer).sort(), ['p2', 'p3'])
  // Later needs from the same peers get nothing more.
  host.handleMessage('p2', { type: 'need', ids: [id] })
  assert.equal(sent.filter((s) => s.msg.type === 'vehicle').length, 2)
})

test('requests are deduplicated and retried at most 3 times', async () => {
  const timers = manualTimers()
  const needs = []
  const errors = []
  const ex = new VehicleExchange({
    send: (peer, msg) => {
      if (msg.type === 'need') needs.push(msg)
      return true
    },
    onError: (m) => errors.push(m),
    setTimeout: timers.setTimeout,
    clearTimeout: timers.clearTimeout,
  })
  const id = 'aaaaaaaaaaaaaaaa'
  assert.equal(ex.request(id, 'host'), true)
  assert.equal(ex.request(id, 'host'), false)
  assert.equal(needs.length, 1)
  for (let i = 0; i < 10; i++) timers.fireAll()
  assert.equal(needs.length, 4) // first request + 3 retries
  assert.equal(ex.stats.retries, 3)
  assert.equal(errors.length, 1)
  assert.equal(timers.count, 0)
  // Never asked again in this session.
  assert.equal(ex.request(id, 'host'), false)
  assert.equal(needs.length, 4)
})

test('an answered request stops retrying', async () => {
  const timers = manualTimers()
  let needs = 0
  const ex = new VehicleExchange({
    send: (peer, msg) => {
      if (msg.type === 'need') needs++
    },
    setTimeout: timers.setTimeout,
    clearTimeout: timers.clearTimeout,
  })
  const id = await vehicleIdFor('Sloop')
  ex.request(id, 'host')
  ex.handleMessage('host', { type: 'vehicle', id, payload: 'Sloop' })
  timers.fireAll()
  assert.equal(needs, 1)
  assert.equal(ex.get(id), 'Sloop')
})

test('an incoming payload over the part limit is refused with a message to the sender', () => {
  const sent = []
  const ex = new VehicleExchange({ send: (peer, msg) => sent.push({ peer, msg }) })
  ex.handleMessage('p2', { type: 'vehicle', id: 'bbbbbbbbbbbbbbbb', payload: ldrawWithParts(700) })
  assert.equal(ex.has('bbbbbbbbbbbbbbbb'), false)
  assert.equal(sent.length, 1)
  assert.equal(sent[0].peer, 'p2')
  assert.equal(sent[0].msg.type, 'vehicle_error')
  assert.match(sent[0].msg.message, /700 parts; at most 600/)
})
