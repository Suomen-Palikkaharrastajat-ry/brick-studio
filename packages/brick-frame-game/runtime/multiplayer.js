/**
 * 2-4 Player Online Multiplayer Manager for Brick Family Games.
 * Powered by WebRTC DataChannels and a WebSocket signalling service.
 *
 * Topology: the host runs a `Broadcaster` with one DataChannel per joined
 * player; players run a `Viewer` connected to the host only.
 *
 * Vehicle models are sent once per peer, never inside room updates (see
 * `vehicle-registry.js`):
 *
 *   - `room_state` and `telemetry` carry each player's `vehicleId` and a
 *     `vehicleReady` flag (the host holds that payload), never the payload.
 *   - A joiner's first `join` carries its `vehicleId` and, once, its payload.
 *   - A peer that sees an id it does not have asks the host with
 *     `{type:'need', ids}`. The host answers once per peer from its cache, or
 *     asks the owning player first and answers when the payload arrives.
 *   - When a payload becomes available it is handed to Elm once per room
 *     session through `multiplayerVehicleLoaded` as `{ id, payload }`.
 *   - Peers that still send an inline `vehicleHash` keep working: the inline
 *     value is registered as a payload by its id.
 *
 * Elm ports used (all optional):
 *   createMultiplayerRoom { roomId, courseId, name, vehicleHash }
 *   joinMultiplayerRoom { roomId, name, vehicleHash }
 *   setMultiplayerVehicle payload        change the local vehicle in a room
 *   sendMultiplayerTelemetry { pos, rot, speed, progress, finished, finishTime?, events? }
 *   sendMultiplayerEvents [ { kind, at, pos, ... } ]   this player's new events
 *   startMultiplayerCountdown ()
 *   leaveMultiplayerRoom ()
 *   multiplayerRoomStateChanged <- room state with players[].vehicleId
 *   multiplayerVehicleLoaded <- { id, payload }
 *   multiplayerError <- message
 *
 * Events (a smash, a bump) are told, not inferred: each player adds its own
 * new events to its telemetry as `events`, a list the host passes on as is
 * inside that player's `room_state` entry. An event is a small object
 * `{ kind, id, at, pos, ... }` whose `id` this runtime gives it (unique per
 * sender and session); the rest is the game's own. A sender repeats each event
 * for `EVENT_TTL_MS` (at most `MAX_EVENTS` at once), the host keeps each one
 * for as long after it first saw it, and every page hands each event of the
 * other players to Elm once, in `multiplayerRoomStateChanged` as
 * `events: [{ slot, ...event }]`, deduplicated by (slot, id). Events travel
 * over the same DataChannels as telemetry, never through the signalling.
 *
 * All multiplayer state is kept in memory.
 */
import { Broadcaster, Viewer, randomRoomId, DEFAULT_SIGNALING_URL } from './share.js'
import { VehicleExchange, MAX_VEHICLE_PARTS, vehicleIdFor } from './vehicle-registry.js'

const HOST_PEER = 'host'
const DEFAULT_CAPACITY = 4 // rooms from hosts that did not say how many
const MAX_CAPACITY = 8

/** How long a sender repeats an event, and the host keeps it (ms). */
export const EVENT_TTL_MS = 2000
/** Most events one player's telemetry or room entry carries at once. */
export const MAX_EVENTS = 8
/** How long a page remembers an event id, to apply each event once (ms). */
export const EVENT_SEEN_MS = 10000
const MAX_EVENT_BYTES = 512

/**
 * A copy of one event as it may travel, or null: an object with a short
 * string `kind` and an `id`, at most `MAX_EVENT_BYTES` of JSON. Everything
 * else in it is passed on unchanged.
 */
export function cleanEvent(ev) {
  if (!ev || typeof ev !== 'object' || Array.isArray(ev)) return null
  if (typeof ev.kind !== 'string' || !ev.kind || ev.kind.length > 16) return null
  const id = typeof ev.id === 'number' && Number.isFinite(ev.id) ? String(ev.id) : ev.id
  if (typeof id !== 'string' || !id || id.length > 40) return null
  let raw
  try {
    raw = JSON.stringify({ ...ev, id })
  } catch {
    return null
  }
  if (raw.length > MAX_EVENT_BYTES) return null
  return JSON.parse(raw)
}

/**
 * One player's recent events, newest last: each is kept `ttlMs` after it was
 * added, at most `max` at once, and an id is added only once (it is
 * remembered for `seenMs`, so a late repeat of an expired event stays out).
 */
export class EventLog {
  constructor({ ttlMs = EVENT_TTL_MS, max = MAX_EVENTS, seenMs = EVENT_SEEN_MS, now = () => Date.now() } = {}) {
    this.ttlMs = ttlMs
    this.max = max
    this.seenMs = seenMs
    this.now = now
    this.entries = []
    this.seen = new Map()
  }

  prune() {
    const t = this.now()
    this.entries = this.entries.filter((e) => t - e.t < this.ttlMs)
    if (this.entries.length > this.max) this.entries = this.entries.slice(-this.max)
    for (const [id, at] of this.seen) if (t - at >= this.seenMs) this.seen.delete(id)
  }

  /** Add an event (cleaned first); false when it is invalid or already seen. */
  add(ev) {
    const clean = cleanEvent(ev)
    if (!clean) return false
    this.prune()
    if (this.seen.has(clean.id)) return false
    const t = this.now()
    this.seen.set(clean.id, t)
    this.entries.push({ ev: clean, t })
    this.prune()
    return true
  }

  /** The events still current, oldest first. */
  list() {
    this.prune()
    return this.entries.map((e) => e.ev)
  }

  clear() {
    this.entries = []
    this.seen.clear()
  }
}

/**
 * What a page has applied: `take(slot, events)` returns the events of that
 * player it has not seen before, each with its `slot`, and remembers their
 * (slot, id) for `seenMs`, so each event is handed on once per page.
 */
export class EventInbox {
  constructor({ seenMs = EVENT_SEEN_MS, now = () => Date.now() } = {}) {
    this.seenMs = seenMs
    this.now = now
    this.seen = new Map()
  }

  take(slot, events) {
    if (!Array.isArray(events) || !events.length) return []
    const t = this.now()
    for (const [key, at] of this.seen) if (t - at >= this.seenMs) this.seen.delete(key)
    const fresh = []
    for (const ev of events.slice(-MAX_EVENTS)) {
      const clean = cleanEvent(ev)
      if (!clean) continue
      const key = `${slot}:${clean.id}`
      if (this.seen.has(key)) continue
      this.seen.set(key, t)
      fresh.push({ ...clean, slot })
    }
    return fresh
  }

  clear() {
    this.seen.clear()
  }
}

/** Players a room holds: 2 to 8, the default 4 when the host did not say. */
export function clampCapacity(value) {
  const n = Math.floor(Number(value))
  if (!Number.isFinite(n)) return DEFAULT_CAPACITY
  return Math.min(MAX_CAPACITY, Math.max(2, n))
}

function readPendingRoomId() {
  try {
    const hash = (typeof window !== 'undefined' && window.location && window.location.hash) || ''
    const match = hash.match(/[#&](?:room|join)=([A-Za-z0-9]{4,12})/i)
    return match ? match[1].toUpperCase() : ''
  } catch {
    return ''
  }
}

export function createMultiplayerManager(ports, signalingUrl = DEFAULT_SIGNALING_URL, options = {}) {
  const BroadcasterClass = options.Broadcaster || Broadcaster
  const ViewerClass = options.Viewer || Viewer
  const debug = Boolean(options.debug ?? (typeof globalThis !== 'undefined' && globalThis.__BRICK_MP_DEBUG))

  let broadcaster = null
  let viewer = null
  let role = null // 'host' | 'player'
  let roomId = null
  let lastRoomId = ''
  let mySlot = 1
  let roomCapacity = DEFAULT_CAPACITY
  let myName = 'Player'
  let myVehicleId = ''
  let myVehicleReady = Promise.resolve('')
  let roomStatus = 'lobby' // 'lobby' | 'countdown' | 'in_race' | 'finished'
  let countdownSeconds = 3
  let countdownTimer = null
  let session = 0
  let joinSent = false
  let lastTelemetryKey = ''
  let lastTelemetry = null

  const now = options.now || (() => Date.now())
  const eventOptions = { now }
  // This player's own recent events (sent with every telemetry while current).
  const myEvents = new EventLog(eventOptions)
  // Host only: slot -> EventLog of what that player told the host.
  const eventLogs = new Map()
  // The other players' events this page has already handed to Elm.
  const inbox = new EventInbox(eventOptions)
  let eventPrefix = ''
  let eventCounter = 0

  // Slot (1..4) -> player record. On the host this is authoritative; on a
  // player it mirrors the latest room_state.
  const players = new Map()
  const viewerSlotMap = new Map() // viewerId -> slot (2..4)
  const legacyInline = new Map() // peer -> last inline vehicleHash seen
  const deliveredToElm = new Set()

  function log(...args) {
    if (debug) console.log('[multiplayer]', ...args)
  }

  function reportError(message) {
    log('error', message)
    ports?.multiplayerError?.send(String(message))
  }

  function send(peerId, msg) {
    if (role === 'host' && broadcaster) {
      const ok = broadcaster.sendTo(peerId, msg)
      if (ok && msg.type === 'vehicle') log('sent vehicle', msg.id, 'to', peerId)
      return ok
    }
    if (role === 'player' && viewer && peerId === HOST_PEER) {
      const ok = viewer.sendMessage(msg)
      if (ok && msg.type === 'vehicle') log('sent vehicle', msg.id, 'to host')
      return ok
    }
    return false
  }

  const exchange = new VehicleExchange({
    send,
    maxParts: options.maxParts ?? MAX_VEHICLE_PARTS,
    retryMs: options.retryMs,
    maxRetries: options.maxRetries,
    setTimeout: options.setTimeout,
    clearTimeout: options.clearTimeout,
    subtle: options.subtle,
    onPayload: (id) => {
      log('payload available', id)
      if (role === 'host') {
        let changed = false
        for (const p of players.values()) {
          if (p.vehicleId === id && !p.vehicleReady) {
            p.vehicleReady = true
            changed = true
          }
        }
        if (changed) broadcastRoomState()
      }
      deliverVehicles()
    },
    onError: (message) => reportError(message),
    // The host relays: ask the player that announced an id someone needs.
    sourceFor: (id) => {
      if (role !== 'host') return null
      for (const [viewerId, slot] of viewerSlotMap) {
        if (players.get(slot)?.vehicleId === id) return viewerId
      }
      return null
    },
  })

  /** Register the local payload; resolves to its id ('' when there is none). */
  function setLocalVehicle(payload) {
    const text = typeof payload === 'string' ? payload : ''
    if (!text) {
      myVehicleId = ''
      myVehicleReady = Promise.resolve('')
      return myVehicleReady
    }
    myVehicleReady = exchange.register(text).then(
      (id) => {
        myVehicleId = id
        return id
      },
      (err) => {
        reportError(err?.message || err)
        myVehicleId = ''
        return ''
      },
    )
    return myVehicleReady
  }

  /** Accept an inline payload from an older peer (or an older host). */
  async function registerInline(peer, payload) {
    if (typeof payload !== 'string' || !payload) return ''
    if (legacyInline.get(peer)?.payload === payload) return legacyInline.get(peer).id
    const id = await vehicleIdFor(payload, { subtle: options.subtle })
    legacyInline.set(peer, { payload, id })
    if (!exchange.has(id)) exchange.store(id, payload, peer)
    return exchange.has(id) ? id : ''
  }

  function publicPlayer(p) {
    return {
      slot: p.slot,
      id: p.id || '',
      name: p.name || `Player ${p.slot}`,
      vehicleId: p.vehicleId || '',
      vehicleReady: Boolean(p.vehicleReady),
      pos: p.pos || [0, 0, 0],
      rot: p.rot || [0, 0, 0],
      speed: Number(p.speed) || 0,
      progress: Number(p.progress) || 0,
      finished: Boolean(p.finished),
      finishTime: p.finishTime !== undefined && p.finishTime !== null ? Number(p.finishTime) : undefined,
      events: eventsOf(p),
    }
  }

  /** A player's current events: from the host's own logs, or as received. */
  function eventsOf(p) {
    if (role === 'host') {
      const log = p.slot === mySlot ? myEvents : eventLogs.get(p.slot)
      return log ? log.list() : []
    }
    return Array.isArray(p.events) ? p.events : []
  }

  /** The other players' events this page has not handed to Elm yet. */
  function freshEvents() {
    const fresh = []
    for (const p of players.values()) {
      if (p.slot === mySlot) continue
      fresh.push(...inbox.take(p.slot, eventsOf(p)))
    }
    return fresh
  }

  /** Host: keep what a player told about its own events. */
  function storeEvents(slot, events) {
    if (!Array.isArray(events) || !events.length) return
    let log = eventLogs.get(slot)
    if (!log) {
      log = new EventLog(eventOptions)
      eventLogs.set(slot, log)
    }
    for (const ev of events.slice(-MAX_EVENTS)) log.add(ev)
  }

  /** Give this player's new events their ids and keep them to send. */
  function addLocalEvents(events) {
    if (!Array.isArray(events)) return false
    if (!eventPrefix) eventPrefix = Math.random().toString(36).slice(2, 8) || 'e'
    let added = false
    for (const ev of events) {
      if (!ev || typeof ev !== 'object') continue
      eventCounter++
      if (myEvents.add({ ...ev, id: `${eventPrefix}${eventCounter}` })) added = true
    }
    return added
  }

  function roomStateMessage(extra = {}) {
    return {
      type: 'room_state',
      roomId,
      status: roomStatus,
      countdown: countdownSeconds,
      maxPlayers: roomCapacity,
      players: Array.from(players.values()).map(publicPlayer),
      ...extra,
    }
  }

  /** Hand every payload that a player in the room uses to Elm, once. */
  function deliverVehicles() {
    if (!ports?.multiplayerVehicleLoaded) return
    for (const p of players.values()) {
      const id = p.vehicleId
      if (!id || deliveredToElm.has(id) || !exchange.has(id)) continue
      deliveredToElm.add(id)
      ports.multiplayerVehicleLoaded.send({ id, payload: exchange.get(id) })
    }
  }

  function emitRoomState() {
    deliverVehicles()
    if (!ports?.multiplayerRoomStateChanged) return
    const playerList = Array.from(players.values()).map((p) => {
      const pub = publicPlayer(p)
      return {
        slot: pub.slot,
        id: pub.id,
        name: pub.name,
        vehicleId: pub.vehicleId,
        vehicleReady: pub.vehicleReady,
        vehicleLoaded: pub.vehicleId ? exchange.has(pub.vehicleId) : false,
        position: pub.pos,
        rotation: pub.rot,
        speed: pub.speed,
        progress: pub.progress,
        finished: pub.finished,
        finishTime: pub.finishTime !== undefined ? pub.finishTime : null,
      }
    })
    const events = freshEvents()

    ports.multiplayerRoomStateChanged.send({
      roomId: roomId || '',
      role: role || '',
      mySlot: mySlot,
      status: roomStatus,
      countdown: countdownSeconds,
      maxPlayers: roomCapacity,
      players: playerList,
      events,
    })
  }

  function broadcastRoomState() {
    if (broadcaster) {
      try {
        broadcaster.push(roomStateMessage())
      } catch {
        // Ignore send errors
      }
    }
    emitRoomState()
  }

  // ── Host ──────────────────────────────────────────────────────────────────

  function startHost(requestedRoomId, courseId, hostName, hostVehicle, capacity) {
    cleanup()
    const mySession = session
    role = 'host'
    roomId = requestedRoomId || randomRoomId(6)
    lastRoomId = roomId
    roomCapacity = clampCapacity(capacity ?? DEFAULT_CAPACITY)
    mySlot = 1
    myName = hostName || 'Host'
    roomStatus = 'lobby'

    // Slot 1 is always the Host
    players.set(1, {
      slot: 1,
      id: 'host',
      name: myName,
      vehicleId: '',
      vehicleReady: false,
      pos: [0, 0, 0],
      rot: [0, 0, 0],
      speed: 0,
      progress: 0,
      finished: false,
    })
    setLocalVehicle(hostVehicle).then((id) => {
      if (session !== mySession) return
      const me = players.get(1)
      if (me) {
        me.vehicleId = id
        me.vehicleReady = Boolean(id)
      }
      broadcastRoomState()
    })

    broadcaster = new BroadcasterClass(signalingUrl, {
      roomId,
      onViewers: () => {},
      onParticipantMessage: (viewerId, msg) => {
        handleViewerMessage(viewerId, msg)
      },
      onParticipantLeave: (viewerId) => {
        handleViewerLeave(viewerId)
      },
      onError: (err) => {
        reportError(err?.message || err)
      },
    })

    broadcaster.start().then(() => {
      emitRoomState()
    }).catch((err) => {
      reportError(err?.message || 'Could not start host session')
    })
  }

  function allocateSlot(viewerId) {
    let assignedSlot = viewerSlotMap.get(viewerId)
    if (!assignedSlot) {
      for (let s = 2; s <= roomCapacity; s++) {
        if (!players.has(s)) {
          assignedSlot = s
          break
        }
      }
    }
    return assignedSlot
  }

  /** Point a player record at a vehicle id and fetch the payload if needed. */
  function assignVehicle(viewerId, record, id) {
    record.vehicleId = id || ''
    record.vehicleReady = Boolean(id) && exchange.has(id)
    if (id && !exchange.has(id)) exchange.request(id, viewerId)
  }

  async function handleJoin(viewerId, msg) {
    const mySession = session
    const assignedSlot = allocateSlot(viewerId)
    if (!assignedSlot) {
      // Room full: tell this viewer, so it does not wait in the lobby forever.
      send(viewerId, { type: 'error', message: `This room is full (${roomCapacity} players).` })
      return
    }
    const isNew = !viewerSlotMap.has(viewerId)
    viewerSlotMap.set(viewerId, assignedSlot)
    const record = players.get(assignedSlot) || {
      slot: assignedSlot,
      id: viewerId,
      pos: [0, 0, 0],
      rot: [0, 0, 0],
      speed: 0,
      progress: 0,
      finished: false,
    }
    record.name = msg.name || record.name || `Player ${assignedSlot}`
    players.set(assignedSlot, record)

    let id = typeof msg.vehicleId === 'string' ? msg.vehicleId : ''
    const inline = typeof msg.vehicle === 'string' ? msg.vehicle : (typeof msg.vehicleHash === 'string' ? msg.vehicleHash : '')
    if (inline) {
      // The joiner's payload rides along once with its first join; an older
      // peer sends it as `vehicleHash`. Either way it is stored by its id.
      const computed = await vehicleIdFor(inline, { subtle: options.subtle })
      if (session !== mySession) return
      if (!exchange.has(computed) && !exchange.store(computed, inline, viewerId)) {
        send(viewerId, {
          type: 'vehicle_error',
          id: computed,
          message: `That model has too many parts to share; at most ${exchange.maxParts} are allowed.`,
        })
        id = ''
      } else {
        id = computed
        // The joiner already has its own payload: never send it back.
        exchange.markSent(viewerId, id)
      }
    } else if (id) {
      exchange.markSent(viewerId, id)
    }
    assignVehicle(viewerId, record, id)

    if (isNew) {
      // Welcome only this viewer with its slot.
      send(viewerId, roomStateMessage({ targetViewerId: viewerId, assignedSlot }))
    }
    broadcastRoomState()
  }

  function applyTelemetry(record, msg) {
    record.pos = msg.pos || record.pos
    record.rot = msg.rot || record.rot
    record.speed = msg.speed !== undefined ? msg.speed : record.speed
    record.progress = msg.progress !== undefined ? msg.progress : record.progress
    record.finished = Boolean(msg.finished)
    if (msg.finishTime !== undefined && msg.finishTime !== null) record.finishTime = msg.finishTime
  }

  async function handleTelemetry(viewerId, msg) {
    const slot = viewerSlotMap.get(viewerId)
    if (!slot) return
    const current = players.get(slot) || { slot, id: viewerId }
    applyTelemetry(current, msg)
    storeEvents(slot, msg.events)
    if (msg.name) current.name = msg.name
    players.set(slot, current)
    if (typeof msg.vehicleId === 'string' && msg.vehicleId && msg.vehicleId !== current.vehicleId) {
      exchange.markSent(viewerId, msg.vehicleId)
      assignVehicle(viewerId, current, msg.vehicleId)
    } else if (typeof msg.vehicleHash === 'string' && msg.vehicleHash) {
      // Older peer: inline payload in every telemetry. Hash it once.
      const id = await registerInline(viewerId, msg.vehicleHash)
      if (id && id !== current.vehicleId) {
        exchange.markSent(viewerId, id)
        assignVehicle(viewerId, current, id)
      }
    }

    // Relay updated room state to all peers
    broadcastRoomState()
  }

  function handleViewerMessage(viewerId, msg) {
    if (!msg || typeof msg !== 'object') return
    if (exchange.handleMessage(viewerId, msg)) return

    if (msg.type === 'join') {
      handleJoin(viewerId, msg)
      return
    }

    if (msg.type === 'telemetry') {
      handleTelemetry(viewerId, msg)
      return
    }

    // Older players sent their join and telemetry through
    // `sendParticipantSnapshot`, which relabels them `participant_snapshot`.
    if (msg.type === 'participant_snapshot' && (Array.isArray(msg.pos) || typeof msg.vehicleHash === 'string')) {
      if (!viewerSlotMap.has(viewerId)) {
        handleJoin(viewerId, msg).then(() => {
          if (Array.isArray(msg.pos)) handleTelemetry(viewerId, msg)
        })
      } else {
        handleTelemetry(viewerId, msg)
      }
    }
  }

  function handleViewerLeave(viewerId) {
    exchange.forgetPeer(viewerId)
    legacyInline.delete(viewerId)
    const slot = viewerSlotMap.get(viewerId)
    if (slot) {
      viewerSlotMap.delete(viewerId)
      players.delete(slot)
      eventLogs.delete(slot)
      broadcastRoomState()
    }
  }

  // ── Player ────────────────────────────────────────────────────────────────

  function sendJoin() {
    const mySession = session
    myVehicleReady.then((id) => {
      if (session !== mySession || !viewer) return
      const msg = { type: 'join', name: myName, vehicleId: id }
      // The payload rides along once per session; afterwards (a reconnect or
      // a new host) the host asks for it with `need` if it lacks it.
      if (id && !exchange.wasSent(HOST_PEER, id)) msg.vehicle = exchange.get(id)
      if (send(HOST_PEER, msg)) {
        joinSent = true
        if (msg.vehicle) {
          exchange.markSent(HOST_PEER, id)
          exchange.stats.payloadSends.set(HOST_PEER, (exchange.stats.payloadSends.get(HOST_PEER) || 0) + 1)
          log('sent vehicle', id, 'to host with join')
        }
      }
    })
  }

  async function handleHostMessage(msg) {
    if (!msg || typeof msg !== 'object') return
    if (exchange.handleMessage(HOST_PEER, msg)) return

    if (msg.type === 'error' && msg.message) {
      reportError(msg.message)
      return
    }

    if (msg.type !== 'room_state') return
    const mySession = session
    if (msg.assignedSlot && msg.targetViewerId) {
      mySlot = msg.assignedSlot
    }
    roomStatus = msg.status || roomStatus
    countdownSeconds = msg.countdown !== undefined ? msg.countdown : countdownSeconds
    if (Array.isArray(msg.players)) {
      const next = []
      for (const p of msg.players) {
        if (!p || typeof p !== 'object') continue
        const record = { ...p }
        record.events = Array.isArray(p.events) ? p.events.slice(-MAX_EVENTS).map(cleanEvent).filter(Boolean) : []
        if (!record.vehicleId && typeof p.vehicleHash === 'string' && p.vehicleHash) {
          // Older host: inline payloads in room_state.
          record.vehicleId = await registerInline(`slot${p.slot}`, p.vehicleHash)
          record.vehicleReady = Boolean(record.vehicleId)
          if (session !== mySession) return
        }
        delete record.vehicleHash
        next.push(record)
      }
      players.clear()
      for (const p of next) players.set(p.slot, p)
      for (const p of next) {
        const id = p.vehicleId
        if (!id || exchange.has(id)) continue
        // Only ask once the host holds it (an older host sends no flag).
        if (p.vehicleReady === false) continue
        exchange.request(id, HOST_PEER)
      }
    }
    emitRoomState()
  }

  function joinRoom(targetRoomId, playerName, vehicle) {
    cleanup()
    role = 'player'
    roomId = (targetRoomId || '').toUpperCase()
    lastRoomId = roomId
    mySlot = 0 // unknown until the host's welcome
    myName = playerName || 'Player'
    roomStatus = 'lobby'
    setLocalVehicle(vehicle)

    viewer = new ViewerClass(signalingUrl, roomId, {
      onSnapshot: (msg) => {
        handleHostMessage(msg)
      },
      onChannelOpen: () => {
        // A new connection: the host may be a new one, so it may ask again.
        exchange.forgetPeer(HOST_PEER)
        if (joinSent) {
          // Our payload went out with the first join of this session.
          if (myVehicleId) exchange.markSent(HOST_PEER, myVehicleId)
        }
        sendJoin()
      },
      onError: (err) => {
        reportError(err?.message || err)
      },
    })

    viewer.start().catch((err) => {
      reportError(err?.message || 'Could not connect to room')
    })
  }

  /** Change the local vehicle while in a room (or before joining). */
  function setVehicle(payload) {
    const mySession = session
    const text = typeof payload === 'string' ? payload : ''
    setLocalVehicle(text).then((id) => {
      if (session !== mySession) return
      if (role === 'host') {
        const me = players.get(1)
        if (me && me.vehicleId !== id) {
          me.vehicleId = id
          me.vehicleReady = Boolean(id)
          broadcastRoomState()
        }
      } else if (role === 'player' && viewer && joinSent) {
        sendJoin()
      }
    })
  }

  function sendTelemetry(telemetry) {
    if (!telemetry || typeof telemetry !== 'object') return
    if (Array.isArray(telemetry.events)) addLocalEvents(telemetry.events)
    lastTelemetry = { ...telemetry, events: undefined }

    const lean = {
      pos: telemetry.pos,
      rot: telemetry.rot,
      speed: telemetry.speed,
      progress: telemetry.progress,
      finished: Boolean(telemetry.finished),
      finishTime: telemetry.finishTime,
    }
    // This player's recent events, passed on as they are (see the header).
    const events = myEvents.list()
    if (events.length) lean.events = events
    // Unchanged telemetry (e.g. waiting on the start line) is not resent.
    const key = JSON.stringify(lean)
    if (key === lastTelemetryKey) return

    if (role === 'host') {
      lastTelemetryKey = key
      const current = players.get(1) || { slot: 1, id: 'host' }
      applyTelemetry(current, lean)
      players.set(1, current)
      broadcastRoomState()
    } else if (role === 'player' && viewer && joinSent) {
      if (viewer.sendMessage({ type: 'telemetry', ...lean })) lastTelemetryKey = key
    }
  }

  /** This player's new events: sent at once with the latest telemetry. */
  function sendEvents(events) {
    if (!addLocalEvents(events)) return
    if (lastTelemetry) sendTelemetry(lastTelemetry)
    else if (role === 'host') broadcastRoomState()
  }

  function startCountdown() {
    if (role !== 'host') return
    if (countdownTimer) clearInterval(countdownTimer)

    roomStatus = 'countdown'
    countdownSeconds = 3
    broadcastRoomState()

    countdownTimer = setInterval(() => {
      countdownSeconds--
      if (countdownSeconds <= 0) {
        clearInterval(countdownTimer)
        countdownTimer = null
        roomStatus = 'in_race'
        countdownSeconds = 0
      }
      broadcastRoomState()
    }, 1000)
  }

  function cleanup() {
    session++
    if (countdownTimer) {
      clearInterval(countdownTimer)
      countdownTimer = null
    }
    if (broadcaster) {
      try {
        broadcaster.stop()
      } catch {}
      broadcaster = null
    }
    if (viewer) {
      try {
        viewer.stop()
      } catch {}
      viewer = null
    }
    players.clear()
    viewerSlotMap.clear()
    roomCapacity = DEFAULT_CAPACITY
    legacyInline.clear()
    deliveredToElm.clear()
    exchange.resetSession()
    joinSent = false
    lastTelemetryKey = ''
    lastTelemetry = null
    myEvents.clear()
    eventLogs.clear()
    inbox.clear()
    eventPrefix = ''
    role = null
    roomId = null
    roomStatus = 'lobby'
  }

  // Hook Elm ports
  ports?.createMultiplayerRoom?.subscribe(({ roomId: rId, courseId, name, vehicle, vehicleHash, maxPlayers }) => {
    startHost(rId, courseId, name, vehicle ?? vehicleHash, maxPlayers)
  })

  ports?.joinMultiplayerRoom?.subscribe(({ roomId: rId, name, vehicle, vehicleHash }) => {
    const payload = vehicle ?? vehicleHash
    const target = rId || (role === 'player' ? roomId : '') || lastRoomId || readPendingRoomId()
    if (role === 'player' && viewer && target === roomId) {
      // Already in that room: only the vehicle changes.
      setVehicle(payload)
    } else if (target) {
      joinRoom(target, name, payload)
    } else {
      reportError('No room code to join. Open the room link from the host.')
    }
  })

  ports?.setMultiplayerVehicle?.subscribe((payload) => {
    setVehicle(payload)
  })

  ports?.sendMultiplayerTelemetry?.subscribe((telemetry) => {
    sendTelemetry(telemetry)
  })

  ports?.sendMultiplayerEvents?.subscribe((events) => {
    sendEvents(events)
  })

  ports?.startMultiplayerCountdown?.subscribe(() => {
    startCountdown()
  })

  ports?.leaveMultiplayerRoom?.subscribe(() => {
    cleanup()
    emitRoomState()
  })

  return {
    startHost,
    joinRoom,
    setVehicle,
    sendTelemetry,
    sendEvents,
    startCountdown,
    cleanup,
    /** Debug counters: payloads sent per peer, `need` messages, retries. */
    stats: () => ({
      payloadSends: Object.fromEntries(exchange.stats.payloadSends),
      needSends: exchange.stats.needSends,
      retries: exchange.stats.retries,
      cached: exchange.payloads.size,
    }),
    get vehicleId() {
      return myVehicleId
    },
    exchange,
  }
}
