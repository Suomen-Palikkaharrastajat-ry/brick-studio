/**
 * 2-4 Player Online Multiplayer Manager for Brick Family Games.
 * Powered by WebRTC DataChannels and Cloudflare Worker WebSocket Signaling.
 */
import { Broadcaster, Viewer, randomRoomId, DEFAULT_SIGNALING_URL } from './share.js'

export function createMultiplayerManager(ports, signalingUrl = DEFAULT_SIGNALING_URL) {
  let broadcaster = null
  let viewer = null
  let role = null // 'host' | 'player'
  let roomId = null
  let mySlot = 1
  let myName = 'Player'
  let myVehicleHash = ''
  let roomStatus = 'lobby' // 'lobby' | 'countdown' | 'in_race' | 'finished'
  let countdownSeconds = 3
  let countdownTimer = null

  // Host state: Map of slot (1..4) -> PlayerRecord
  const players = new Map()
  const viewerSlotMap = new Map() // viewerId -> slot (2..4)

  function emitRoomState() {
    if (!ports?.multiplayerRoomStateChanged) return
    const playerList = Array.from(players.values()).map((p) => ({
      slot: p.slot,
      id: p.id || '',
      name: p.name || `Player ${p.slot}`,
      vehicleHash: p.vehicleHash || '',
      position: p.pos || [0, 0, 0],
      rotation: p.rot || [0, 0, 0],
      speed: Number(p.speed) || 0,
      progress: Number(p.progress) || 0,
      finished: Boolean(p.finished),
      finishTime: p.finishTime !== undefined ? Number(p.finishTime) : null,
    }))

    ports.multiplayerRoomStateChanged.send({
      roomId: roomId || '',
      role: role || '',
      mySlot: mySlot,
      status: roomStatus,
      countdown: countdownSeconds,
      players: playerList,
    })
  }

  function broadcastToPlayers(msg) {
    if (broadcaster) {
      try {
        broadcaster.push(msg)
      } catch {
        // Ignore send errors
      }
    }
  }

  function startHost(requestedRoomId, courseId, hostName, hostVehicleHash) {
    cleanup()
    role = 'host'
    roomId = requestedRoomId || randomRoomId(6)
    mySlot = 1
    myName = hostName || 'Host'
    myVehicleHash = hostVehicleHash || ''
    roomStatus = 'lobby'

    // Slot 1 is always the Host
    players.set(1, {
      slot: 1,
      id: 'host',
      name: myName,
      vehicleHash: myVehicleHash,
      pos: [0, 0, 0],
      rot: [0, 0, 0],
      speed: 0,
      progress: 0,
      finished: false,
    })

    broadcaster = new Broadcaster(signalingUrl, {
      roomId,
      onViewers: () => {},
      onParticipantMessage: (viewerId, msg) => {
        handleViewerMessage(viewerId, msg)
      },
      onParticipantLeave: (viewerId) => {
        handleViewerLeave(viewerId)
      },
      onError: (err) => {
        ports?.multiplayerError?.send(String(err?.message || err))
      },
    })

    broadcaster.start().then(() => {
      emitRoomState()
    }).catch((err) => {
      ports?.multiplayerError?.send(String(err?.message || 'Could not start host session'))
    })
  }

  function handleViewerMessage(viewerId, msg) {
    if (!msg || typeof msg !== 'object') return

    if (msg.type === 'join') {
      // Allocate an available slot between 2 and 4
      let assignedSlot = viewerSlotMap.get(viewerId)
      if (!assignedSlot) {
        for (let s = 2; s <= 4; s++) {
          if (!players.has(s)) {
            assignedSlot = s
            break
          }
        }
      }
      if (!assignedSlot) {
        // Room full
        return
      }
      viewerSlotMap.set(viewerId, assignedSlot)
      players.set(assignedSlot, {
        slot: assignedSlot,
        id: viewerId,
        name: msg.name || `Player ${assignedSlot}`,
        vehicleHash: msg.vehicleHash || '',
        pos: [0, 0, 0],
        rot: [0, 0, 0],
        speed: 0,
        progress: 0,
        finished: false,
      })

      // Send welcome back to this viewer
      broadcastToPlayers({
        type: 'room_state',
        roomId,
        targetViewerId: viewerId,
        assignedSlot,
        status: roomStatus,
        countdown: countdownSeconds,
        players: Array.from(players.values()),
      })
      emitRoomState()
      return
    }

    if (msg.type === 'telemetry') {
      const slot = viewerSlotMap.get(viewerId)
      if (!slot) return
      const current = players.get(slot) || { slot, id: viewerId }
      current.pos = msg.pos || current.pos
      current.rot = msg.rot || current.rot
      current.speed = msg.speed !== undefined ? msg.speed : current.speed
      current.progress = msg.progress !== undefined ? msg.progress : current.progress
      current.finished = Boolean(msg.finished)
      if (msg.finishTime !== undefined) current.finishTime = msg.finishTime
      if (msg.vehicleHash) current.vehicleHash = msg.vehicleHash
      if (msg.name) current.name = msg.name

      players.set(slot, current)

      // Relay updated room state to all peers
      broadcastToPlayers({
        type: 'room_state',
        roomId,
        status: roomStatus,
        countdown: countdownSeconds,
        players: Array.from(players.values()),
      })
      emitRoomState()
    }
  }

  function handleViewerLeave(viewerId) {
    const slot = viewerSlotMap.get(viewerId)
    if (slot) {
      viewerSlotMap.delete(viewerId)
      players.delete(slot)
      broadcastToPlayers({
        type: 'room_state',
        roomId,
        status: roomStatus,
        countdown: countdownSeconds,
        players: Array.from(players.values()),
      })
      emitRoomState()
    }
  }

  function joinRoom(targetRoomId, playerName, vehicleHash) {
    cleanup()
    role = 'player'
    roomId = (targetRoomId || '').toUpperCase()
    myName = playerName || 'Player'
    myVehicleHash = vehicleHash || ''
    roomStatus = 'lobby'

    viewer = new Viewer(signalingUrl, roomId, {
      onSnapshot: (msg) => {
        if (!msg || typeof msg !== 'object') return
        if (msg.type === 'room_state') {
          if (msg.assignedSlot && msg.targetViewerId) {
            mySlot = msg.assignedSlot
          }
          roomStatus = msg.status || roomStatus
          countdownSeconds = msg.countdown !== undefined ? msg.countdown : countdownSeconds
          if (Array.isArray(msg.players)) {
            players.clear()
            for (const p of msg.players) {
              players.set(p.slot, p)
            }
          }
          emitRoomState()
        }
      },
      onStatus: (status) => {
        if (status === 'connected') {
          viewer.sendParticipantSnapshot({
            type: 'join',
            name: myName,
            vehicleHash: myVehicleHash,
          })
        }
      },
      onError: (err) => {
        ports?.multiplayerError?.send(String(err?.message || err))
      },
    })

    viewer.start().catch((err) => {
      ports?.multiplayerError?.send(String(err?.message || 'Could not connect to room'))
    })
  }

  function sendTelemetry(telemetry) {
    if (!telemetry || typeof telemetry !== 'object') return

    if (role === 'host') {
      const current = players.get(1) || { slot: 1, id: 'host' }
      current.pos = telemetry.pos || current.pos
      current.rot = telemetry.rot || current.rot
      current.speed = telemetry.speed !== undefined ? telemetry.speed : current.speed
      current.progress = telemetry.progress !== undefined ? telemetry.progress : current.progress
      current.finished = Boolean(telemetry.finished)
      if (telemetry.finishTime !== undefined) current.finishTime = telemetry.finishTime
      players.set(1, current)

      broadcastToPlayers({
        type: 'room_state',
        roomId,
        status: roomStatus,
        countdown: countdownSeconds,
        players: Array.from(players.values()),
      })
      emitRoomState()
    } else if (role === 'player' && viewer) {
      viewer.sendParticipantSnapshot({
        type: 'telemetry',
        slot: mySlot,
        name: myName,
        vehicleHash: myVehicleHash,
        pos: telemetry.pos,
        rot: telemetry.rot,
        speed: telemetry.speed,
        progress: telemetry.progress,
        finished: telemetry.finished,
        finishTime: telemetry.finishTime,
      })
    }
  }

  function startCountdown() {
    if (role !== 'host') return
    if (countdownTimer) clearInterval(countdownTimer)

    roomStatus = 'countdown'
    countdownSeconds = 3
    emitRoomState()
    broadcastToPlayers({
      type: 'room_state',
      roomId,
      status: roomStatus,
      countdown: countdownSeconds,
      players: Array.from(players.values()),
    })

    countdownTimer = setInterval(() => {
      countdownSeconds--
      if (countdownSeconds <= 0) {
        clearInterval(countdownTimer)
        countdownTimer = null
        roomStatus = 'in_race'
        countdownSeconds = 0
      }
      emitRoomState()
      broadcastToPlayers({
        type: 'room_state',
        roomId,
        status: roomStatus,
        countdown: countdownSeconds,
        players: Array.from(players.values()),
      })
    }, 1000)
  }

  function cleanup() {
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
    role = null
    roomId = null
    roomStatus = 'lobby'
  }

  // Hook Elm ports
  ports?.createMultiplayerRoom?.subscribe(({ roomId: rId, courseId, name, vehicleHash }) => {
    startHost(rId, courseId, name, vehicleHash)
  })

  ports?.joinMultiplayerRoom?.subscribe(({ roomId: rId, name, vehicleHash }) => {
    joinRoom(rId, name, vehicleHash)
  })

  ports?.sendMultiplayerTelemetry?.subscribe((telemetry) => {
    sendTelemetry(telemetry)
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
    sendTelemetry,
    startCountdown,
    cleanup,
  }
}
