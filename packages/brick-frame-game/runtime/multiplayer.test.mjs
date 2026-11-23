// In-process loopback tests of the multiplayer vehicle transfer: a fake
// signalling hub stands in for the WebSocket service and DataChannels, and
// every message goes through JSON like it would on the wire.
import assert from 'node:assert/strict'
import test from 'node:test'
import { EVENT_TTL_MS, EventInbox, EventLog, MAX_EVENTS, clampCapacity, createMultiplayerManager } from './multiplayer.js'
import { vehicleIdFor } from './vehicle-registry.js'

const tick = () => new Promise((resolve) => setTimeout(resolve, 0))

async function settle(rounds = 20) {
  for (let i = 0; i < rounds; i++) await tick()
}

function createHub() {
  const hub = {
    broadcaster: null,
    viewers: new Map(),
    nextViewer: 1,
    /** Every message on the wire: { from, to, msg }. */
    wire: [],
    record(from, to, raw) {
      hub.wire.push({ from, to, raw, msg: JSON.parse(raw) })
    },
    vehicleSends(to, id) {
      return hub.wire.filter((w) => w.to === to && w.msg.type === 'vehicle' && (!id || w.msg.id === id)).length +
        hub.wire.filter((w) => w.to === to && w.msg.type === 'join' && typeof w.msg.vehicle === 'string' && (!id || w.msg.vehicleId === id)).length
    },
  }

  class FakeBroadcaster {
    constructor(_url, opts) {
      this.opts = opts
      this.latest = undefined
      hub.broadcaster = this
    }
    async start() {}
    connectViewer(viewerId, fakeViewer) {
      if (this.latest !== undefined) this.deliver(viewerId, this.latest)
      fakeViewer.open()
    }
    deliver(viewerId, msg) {
      const v = hub.viewers.get(viewerId)
      if (!v) return false
      const raw = JSON.stringify(msg)
      hub.record('host', viewerId, raw)
      queueMicrotask(() => v.opts.onSnapshot(JSON.parse(raw)))
      return true
    }
    push(msg) {
      this.latest = msg
      for (const id of hub.viewers.keys()) this.deliver(id, msg)
    }
    sendTo(viewerId, msg) {
      return this.deliver(viewerId, msg)
    }
    stop() {
      hub.broadcaster = null
    }
  }

  class FakeViewer {
    constructor(_url, _room, opts) {
      this.opts = opts
      this.viewerId = `v${hub.nextViewer++}`
      this.isOpen = false
    }
    async start() {
      hub.viewers.set(this.viewerId, this)
      queueMicrotask(() => hub.broadcaster?.connectViewer(this.viewerId, this))
    }
    open() {
      this.isOpen = true
      this.opts.onChannelOpen?.()
    }
    sendMessage(msg) {
      if (!this.isOpen || !hub.broadcaster) return false
      const raw = JSON.stringify(msg)
      hub.record(this.viewerId, 'host', raw)
      const b = hub.broadcaster
      queueMicrotask(() => b.opts.onParticipantMessage(this.viewerId, JSON.parse(raw)))
      return true
    }
    stop() {
      hub.viewers.delete(this.viewerId)
      hub.broadcaster?.opts.onParticipantLeave(this.viewerId)
    }
  }

  return { hub, FakeBroadcaster, FakeViewer }
}

function fakePorts() {
  const out = { loaded: [], states: [], errors: [] }
  out.ports = {
    multiplayerVehicleLoaded: { send: (v) => out.loaded.push(v) },
    multiplayerRoomStateChanged: { send: (s) => out.states.push(s) },
    multiplayerError: { send: (e) => out.errors.push(e) },
  }
  return out
}

function makePeer(env, extra = {}) {
  const p = fakePorts()
  const mgr = createMultiplayerManager(p.ports, 'http://signalling.invalid', {
    Broadcaster: env.FakeBroadcaster,
    Viewer: env.FakeViewer,
    retryMs: 50,
    ...extra,
  })
  return { ...p, mgr }
}

function bigLink(seed) {
  return `#track=quarry&seed=${seed}&b/` + 'Q'.repeat(300 * 1024) + seed
}

function telemetry(i) {
  return { pos: [i, 0, i * 2], rot: [0, i / 10, 0], speed: i, progress: i / 100, finished: false }
}

test('each payload crosses the wire once per peer, never inside room updates', async () => {
  const env = createHub()
  const host = makePeer(env)
  const a = makePeer(env)
  const b = makePeer(env)
  const hostCar = bigLink(1)
  const carA = bigLink(2)
  const carB = bigLink(3)
  host.mgr.startHost('ROOM1', 'course', 'Host', hostCar)
  await settle()
  a.mgr.joinRoom('ROOM1', 'A', carA)
  b.mgr.joinRoom('ROOM1', 'B', carB)
  await settle()
  for (let i = 0; i < 30; i++) {
    host.mgr.sendTelemetry(telemetry(i))
    a.mgr.sendTelemetry(telemetry(i + 1))
    b.mgr.sendTelemetry(telemetry(i + 2))
    await settle(2)
  }
  await settle()

  const [hostId, idA, idB] = await Promise.all([hostCar, carA, carB].map((c) => vehicleIdFor(c)))
  const [va, vb] = ['v1', 'v2']

  // Joiners upload their own model once, with the join.
  assert.equal(env.hub.vehicleSends('host', idA), 1)
  assert.equal(env.hub.vehicleSends('host', idB), 1)
  // Each joiner gets every other model exactly once, and never its own back.
  assert.equal(env.hub.vehicleSends(va, hostId), 1)
  assert.equal(env.hub.vehicleSends(va, idB), 1)
  assert.equal(env.hub.vehicleSends(va, idA), 0)
  assert.equal(env.hub.vehicleSends(vb, hostId), 1)
  assert.equal(env.hub.vehicleSends(vb, idA), 1)
  assert.equal(env.hub.vehicleSends(vb, idB), 0)

  // Room updates and telemetry never carry a payload.
  for (const w of env.hub.wire) {
    if (w.msg.type === 'room_state' || w.msg.type === 'telemetry') {
      assert.ok(w.raw.length < 4096, `${w.msg.type} is ${w.raw.length} bytes`)
      assert.equal(w.raw.includes('QQQQ'), false)
    }
  }
  const lastState = env.hub.wire.filter((w) => w.msg.type === 'room_state').at(-1).msg
  assert.deepEqual(lastState.players.map((p) => p.vehicleId).sort(), [hostId, idA, idB].sort())
  assert.ok(lastState.players.every((p) => p.vehicleReady === true))

  // Every peer's Elm side got each payload once.
  for (const peer of [host, a, b]) {
    const ids = peer.loaded.map((l) => l.id).sort()
    assert.deepEqual(ids, [hostId, idA, idB].sort())
  }
  assert.equal(a.loaded.find((l) => l.id === idB).payload, carB)
  const elmState = a.states.at(-1)
  assert.equal(elmState.mySlot, 2)
  assert.ok(elmState.players.every((p) => p.vehicleLoaded && p.vehicleId && !('vehicleHash' in p)))

  host.mgr.cleanup()
  a.mgr.cleanup()
  b.mgr.cleanup()
})

test('a late joiner gets each cached payload once from the host', async () => {
  const env = createHub()
  const host = makePeer(env)
  const a = makePeer(env)
  const late = makePeer(env)
  host.mgr.startHost('ROOM2', 'course', 'Host', 'Cutter')
  await settle()
  a.mgr.joinRoom('ROOM2', 'A', 'Schooner')
  for (let i = 0; i < 10; i++) {
    host.mgr.sendTelemetry(telemetry(i))
    a.mgr.sendTelemetry(telemetry(i))
    await settle(2)
  }
  await settle()
  const sendsToA = env.hub.wire.filter((w) => w.to === 'v1' && w.msg.type === 'vehicle').length

  late.mgr.joinRoom('ROOM2', 'Late', 'Sloop')
  for (let i = 0; i < 10; i++) {
    host.mgr.sendTelemetry(telemetry(i + 20))
    a.mgr.sendTelemetry(telemetry(i + 20))
    late.mgr.sendTelemetry(telemetry(i + 20))
    await settle(2)
  }
  await settle()

  const [cutter, schooner, sloop] = await Promise.all(['Cutter', 'Schooner', 'Sloop'].map((c) => vehicleIdFor(c)))
  assert.equal(env.hub.vehicleSends('v2', cutter), 1)
  assert.equal(env.hub.vehicleSends('v2', schooner), 1)
  assert.equal(env.hub.vehicleSends('v2', sloop), 0)
  // The earlier player only receives the newcomer's model, nothing again.
  assert.equal(env.hub.vehicleSends('v1', sloop), 1)
  assert.equal(env.hub.wire.filter((w) => w.to === 'v1' && w.msg.type === 'vehicle').length, sendsToA + 1)
  // The host relayed: the late joiner only ever talks to the host.
  assert.deepEqual(late.loaded.map((l) => l.payload).sort(), ['Cutter', 'Schooner', 'Sloop'])

  host.mgr.cleanup()
  a.mgr.cleanup()
  late.mgr.cleanup()
})

test('unchanged telemetry is not resent', async () => {
  const env = createHub()
  const host = makePeer(env)
  const a = makePeer(env)
  host.mgr.startHost('ROOM3', 'course', 'Host', 'Cutter')
  await settle()
  a.mgr.joinRoom('ROOM3', 'A', 'Sloop')
  await settle()
  for (let i = 0; i < 20; i++) {
    a.mgr.sendTelemetry(telemetry(1))
    await settle(1)
  }
  await settle()
  const sent = env.hub.wire.filter((w) => w.from === 'v1' && w.msg.type === 'telemetry')
  assert.equal(sent.length, 1)
  assert.equal('vehicleHash' in sent[0].msg, false)
  assert.equal('name' in sent[0].msg, false)
  host.mgr.cleanup()
  a.mgr.cleanup()
})

test('a model with too many parts is refused with a clear error to the sender', async () => {
  const env = createHub()
  const host = makePeer(env)
  const a = makePeer(env)
  const lines = ['0 FILE big.ldr']
  for (let i = 0; i < 650; i++) lines.push(`1 4 ${i} 0 0 1 0 0 0 1 0 0 0 1 3001.dat`)
  host.mgr.startHost('ROOM4', 'course', 'Host', 'Cutter')
  await settle()
  a.mgr.joinRoom('ROOM4', 'A', lines.join('\n'))
  await settle()
  assert.equal(a.errors.length, 1)
  assert.match(a.errors[0], /650 parts; at most 600/)
  // Nothing was uploaded and the player shows with no model.
  assert.equal(env.hub.vehicleSends('host'), 0)
  const me = a.states.at(-1).players.find((p) => p.slot === 2)
  assert.equal(me.vehicleId, '')

  // A peer that bypasses its own check is refused by the host.
  env.hub.viewers.get('v1').sendMessage({ type: 'join', name: 'A', vehicle: lines.join('\n') })
  await settle()
  assert.equal(a.errors.length, 2)
  assert.match(a.errors[1], /too many parts/)
  host.mgr.cleanup()
  a.mgr.cleanup()
})

test('older peers sending an inline vehicleHash keep working', async () => {
  const env = createHub()
  const host = makePeer(env)
  const a = makePeer(env)
  host.mgr.startHost('ROOM5', 'course', 'Host', 'Cutter')
  await settle()
  a.mgr.joinRoom('ROOM5', 'A', '')
  await settle()
  const legacyCar = '#b/legacy-car'
  const v = env.hub.viewers.get('v1')
  for (let i = 0; i < 5; i++) {
    // The old runtime relabelled telemetry as participant_snapshot and
    // repeated the full vehicleHash every time.
    v.sendMessage({ type: 'participant_snapshot', bytes: [], name: 'Old', vehicleHash: legacyCar, ...telemetry(i) })
    await settle(2)
  }
  await settle()
  const id = await vehicleIdFor(legacyCar)
  assert.ok(host.loaded.some((l) => l.id === id && l.payload === legacyCar))
  const last = env.hub.wire.filter((w) => w.msg.type === 'room_state').at(-1).msg
  assert.equal(last.players.find((p) => p.slot === 2).vehicleId, id)
  host.mgr.cleanup()
  a.mgr.cleanup()
})

test('a player asks again at most 3 times when the host never answers', async () => {
  const env = createHub()
  const a = makePeer(env, { retryMs: 5 })
  // A host that announces a model but never answers `need`.
  const broadcaster = new env.FakeBroadcaster('', { onParticipantMessage: () => {}, onParticipantLeave: () => {} })
  a.mgr.joinRoom('ROOM6', 'A', 'Sloop')
  await settle()
  broadcaster.sendTo('v1', {
    type: 'room_state',
    roomId: 'ROOM6',
    status: 'lobby',
    countdown: 3,
    players: [{ slot: 1, id: 'host', name: 'Host', vehicleId: 'cccccccccccccccc', vehicleReady: true, pos: [0, 0, 0], rot: [0, 0, 0] }],
  })
  await new Promise((r) => setTimeout(r, 100))
  const needs = env.hub.wire.filter((w) => w.msg.type === 'need')
  assert.equal(needs.length, 4)
  assert.equal(a.mgr.stats().retries, 3)
  assert.equal(a.errors.length, 1)
  a.mgr.cleanup()
})

test('clampCapacity keeps rooms between two and eight players, four by default', () => {
  assert.equal(clampCapacity(undefined), 4)
  assert.equal(clampCapacity(0), 2)
  assert.equal(clampCapacity(2), 2)
  assert.equal(clampCapacity('8'), 8)
  assert.equal(clampCapacity(12), 8)
  assert.equal(clampCapacity(NaN), 4)
})

test('a room holds the players its host asked for, and turns the rest away with an error', async () => {
  for (const [capacity, expectedMax] of [[undefined, 4], [2, 2], [8, 8]]) {
    const env = createHub()
    const host = makePeer(env)
    host.mgr.startHost('CAP' + expectedMax, 'course', 'Host', 'Cutter', capacity)
    await settle()
    const joiners = []
    for (let i = 0; i < expectedMax - 1; i++) {
      const j = makePeer(env)
      joiners.push(j)
      j.mgr.joinRoom('CAP' + expectedMax, `J${i + 1}`, 'Schooner')
      await settle()
    }
    const last = joiners.at(-1)
    assert.equal(last.errors.length, 0, 'the last seat is taken, not refused')
    const extra = makePeer(env)
    extra.mgr.joinRoom('CAP' + expectedMax, 'Late', 'Schooner')
    await settle()
    assert.equal(extra.errors.length, 1, `capacity ${expectedMax}: a joiner past the last seat is refused`)
    assert.match(extra.errors[0], /full/)

    const state = env.hub.wire.filter((w) => w.msg.type === 'room_state').at(-1).msg
    assert.equal(state.maxPlayers, expectedMax)
    assert.equal(state.players.length, expectedMax)
    assert.deepEqual(state.players.map((p) => p.slot).sort((a, b) => a - b), Array.from({ length: expectedMax }, (_, i) => i + 1))
    assert.equal(host.states.at(-1).maxPlayers, expectedMax)
    host.mgr.cleanup()
    for (const j of [...joiners, extra]) j.mgr.cleanup()
  }
})

test('an event log keeps events for 2 s, at most 8, and each id once', () => {
  let t = 1000
  const log = new EventLog({ now: () => t })
  assert.equal(log.add({ kind: 'smash', id: 'a1', at: 1.5, pos: [0, 0, 0], obstacle: 4 }), true)
  assert.equal(log.add({ kind: 'smash', id: 'a1', at: 1.5, pos: [0, 0, 0], obstacle: 4 }), false, 'same id twice')
  assert.equal(log.add({ kind: 'bump', at: 1 }), false, 'no id')
  assert.equal(log.add({ id: 'x' }), false, 'no kind')
  assert.deepEqual(log.list(), [{ kind: 'smash', id: 'a1', at: 1.5, pos: [0, 0, 0], obstacle: 4 }])
  t += EVENT_TTL_MS - 1
  assert.equal(log.list().length, 1)
  t += 1
  assert.equal(log.list().length, 0, 'expired after 2 s')
  assert.equal(log.add({ kind: 'smash', id: 'a1' }), false, 'an expired id does not come back')
  for (let i = 0; i < 12; i++) log.add({ kind: 'bump', id: `b${i}` })
  assert.equal(log.list().length, MAX_EVENTS)
  assert.equal(log.list().at(-1).id, 'b11', 'the newest are kept')
})

test('an inbox hands each (slot, id) on once and forgets it later', () => {
  let t = 0
  const inbox = new EventInbox({ now: () => t, seenMs: 10000 })
  const ev = { kind: 'smash', id: 'k1', pos: [1, 2, 3] }
  assert.deepEqual(inbox.take(2, [ev]), [{ ...ev, slot: 2 }])
  assert.deepEqual(inbox.take(2, [ev]), [], 'seen already')
  assert.equal(inbox.take(3, [ev]).length, 1, 'the same id from another slot is another event')
  assert.deepEqual(inbox.take(2, [{ kind: 'smash' }, null, 'x']), [], 'invalid events are skipped')
  t = 10000
  assert.equal(inbox.take(2, [ev]).length, 1)
})

test('events reach every other page once, through the host, and expire', async () => {
  let t = 0
  const now = () => t
  const env = createHub()
  const host = makePeer(env, { now })
  const a = makePeer(env, { now })
  const b = makePeer(env, { now })
  host.mgr.startHost('EVT1', 'course', 'Host', 'Cutter')
  await settle()
  a.mgr.joinRoom('EVT1', 'A', 'Sloop')
  b.mgr.joinRoom('EVT1', 'B', 'Schooner')
  await settle()
  const smash = { kind: 'smash', at: 12.5, pos: [10, 0, 20], obstacleId: 7, impactSpeed: 9, direction: [0, 0, 1] }
  const bump = { kind: 'bump', at: 3, pos: [0, 0, 0], target: 3, impulse: [1, 0, 0], damage: 12 }
  for (let i = 0; i < 10; i++) {
    t += 50
    host.mgr.sendTelemetry(telemetry(i))
    a.mgr.sendTelemetry({ ...telemetry(i), events: i === 2 ? [smash] : undefined })
    b.mgr.sendTelemetry(telemetry(i))
    if (i === 4) host.mgr.sendEvents([bump])
    await settle(2)
  }
  await settle()

  const all = (peer) => peer.states.flatMap((s) => s.events)
  // A's smash: once on the host and once on B, never back on A.
  for (const peer of [host, b]) {
    const got = all(peer).filter((e) => e.kind === 'smash')
    assert.equal(got.length, 1)
    assert.equal(got[0].slot, 2)
    assert.equal(got[0].obstacleId, 7)
    assert.deepEqual(got[0].pos, [10, 0, 20])
    assert.equal(typeof got[0].id, 'string')
  }
  assert.equal(all(a).filter((e) => e.kind === 'smash').length, 0)
  // The host's bump: once on A and once on B, never on the host.
  for (const peer of [a, b]) {
    const got = all(peer).filter((e) => e.kind === 'bump')
    assert.equal(got.length, 1)
    assert.equal(got[0].slot, 1)
    assert.deepEqual(got[0].impulse, [1, 0, 0])
  }
  assert.equal(all(host).filter((e) => e.kind === 'bump').length, 0)
  // A repeated its smash with each telemetry while it was current.
  const fromA = env.hub.wire.filter((w) => w.from === 'v1' && w.msg.type === 'telemetry' && w.msg.events)
  assert.ok(fromA.length > 1)
  assert.ok(fromA.every((w) => w.msg.events.length === 1 && w.msg.events[0].obstacleId === 7))

  // After 2 s nobody sends or relays them any more.
  t += EVENT_TTL_MS
  for (let i = 0; i < 3; i++) {
    t += 50
    host.mgr.sendTelemetry(telemetry(100 + i))
    a.mgr.sendTelemetry(telemetry(100 + i))
    await settle(2)
  }
  await settle()
  const lastState = env.hub.wire.filter((w) => w.msg.type === 'room_state').at(-1).msg
  assert.ok(lastState.players.every((p) => p.events.length === 0))
  const lastFromA = env.hub.wire.filter((w) => w.from === 'v1' && w.msg.type === 'telemetry').at(-1).msg
  assert.equal('events' in lastFromA, false)
  assert.equal(all(b).filter((e) => e.kind === 'smash').length, 1)

  host.mgr.cleanup()
  a.mgr.cleanup()
  b.mgr.cleanup()
})

test('a late repeat of an expired event is not applied again', async () => {
  let t = 0
  const env = createHub()
  const host = makePeer(env, { now: () => t })
  const a = makePeer(env, { now: () => t })
  host.mgr.startHost('EVT2', 'course', 'Host', 'Cutter')
  await settle()
  a.mgr.joinRoom('EVT2', 'A', 'Sloop')
  await settle()
  const v = env.hub.viewers.get('v1')
  const ev = { kind: 'smash', id: 'zz1', at: 1, pos: [0, 0, 0], obstacleId: 3 }
  v.sendMessage({ type: 'telemetry', ...telemetry(1), events: [ev] })
  await settle()
  t += EVENT_TTL_MS + 100
  // The sender's clock runs behind: it still repeats the event.
  v.sendMessage({ type: 'telemetry', ...telemetry(2), events: [ev] })
  await settle()
  assert.equal(host.states.flatMap((s) => s.events).length, 1)
  const last = env.hub.wire.filter((w) => w.msg.type === 'room_state').at(-1).msg
  assert.deepEqual(last.players.find((p) => p.slot === 2).events, [])
  host.mgr.cleanup()
  a.mgr.cleanup()
})
