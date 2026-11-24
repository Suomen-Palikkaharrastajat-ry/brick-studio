// Tilt steering shared by the Brick Frame games.
//
// Reads DeviceOrientationEvent, turns the phone's roll (relative to the
// screen, so it works in portrait and landscape) into a steering value
// -1 .. 1 and sends it to Elm through the `tiltSteer` port. Elm stays pure:
// it only receives numbers and a status record.
//
// - Neutral point: the angle at race start (`calibrateTilt` port) is
//   "straight ahead". Rotating the screen re-calibrates.
// - Dead zone of a few degrees, full helm at about 22 degrees.
// - On by default on phones (`(pointer: coarse)`), off elsewhere.
// - `forceOnPhones`: phones always steer by tilt; no toggle is needed.
// - On/off toggle: any element with `data-tilt-toggle`. The choice is saved
//   in localStorage under the game's `storageKey` and overrides the default.
//   On iOS the permission prompt must run inside a tap, so the toggle is
//   handled here, in the click itself, not through an Elm command. While
//   tilt is wanted but not yet listening, the next tap anywhere (the race
//   start button included) asks for the permission.
// - If permission is denied or no sensor exists, tilt quietly switches off
//   and the on-screen buttons keep working.
//
// The Elm side of this contract is `Game.Ports.calibrateTilt`,
// `Game.Ports.tiltSteer` and `Game.Ports.tiltStatus`.

const DEAD_ZONE_DEG = 3
const FULL_LOCK_DEG = 22

// Phones (touch-primary screens) start with tilt on; other devices start with
// buttons. A choice saved with the toggle always wins over this default.
export function defaultTiltOn(win = typeof window !== 'undefined' ? window : undefined) {
  try {
    return typeof win?.matchMedia === 'function' && win.matchMedia('(pointer: coarse)').matches
  } catch {
    return false
  }
}

export function resolveMode(saved, phoneDefault) {
  if (saved === 'tilt') return true
  if (saved === 'touch') return false
  return phoneDefault
}

function readMode(storageKey) {
  let saved = null
  try {
    saved = window.localStorage.getItem(storageKey)
  } catch {}
  return resolveMode(saved, defaultTiltOn())
}

function saveMode(storageKey, tilt) {
  try {
    window.localStorage.setItem(storageKey, tilt ? 'tilt' : 'touch')
  } catch {}
}

export function steerFromAngle(angleDeg, neutralDeg) {
  let d = angleDeg - neutralDeg
  if (d > 180) d -= 360
  if (d < -180) d += 360
  const mag = Math.abs(d)
  if (mag <= DEAD_ZONE_DEG) return 0
  const v = Math.min(1, (mag - DEAD_ZONE_DEG) / (FULL_LOCK_DEG - DEAD_ZONE_DEG))
  return Math.sign(d) * v
}

/**
 * Wire tilt steering to the game's ports.
 *
 * `storageKey` is the localStorage key that remembers the player's choice
 * (`'tilt'` or `'touch'`), e.g. `'sails.steerMode'`.
 */
export function setupTiltSteer(ports, { storageKey, forceOnPhones = false } = {}) {
  const hasApi = typeof window !== 'undefined' && 'DeviceOrientationEvent' in window
  const hasTouch =
    (typeof navigator !== 'undefined' && navigator.maxTouchPoints > 0) ||
    (typeof window.matchMedia === 'function' && window.matchMedia('(pointer: coarse)').matches)
  const available = hasApi && hasTouch
  const needsPermission =
    hasApi && typeof window.DeviceOrientationEvent.requestPermission === 'function'

  // `forceOnPhones`: on phones tilt is always on (the saved choice is ignored)
  // and there is no toggle. Only when permission is denied or no sensor
  // reports does tilt switch off, and then the on-screen buttons take over.
  const forced = forceOnPhones && defaultTiltOn()
  let enabled = available && (forced || readMode(storageKey))
  let permitted = !needsPermission
  let requesting = false
  let listening = false
  let lastAngle = null
  let neutral = null
  let lastSent = null

  function sendStatus() {
    ports?.tiltStatus?.send({ available, enabled })
  }

  function send(v) {
    // Skip repeats so Elm is not woken 60 times a second for nothing.
    if (lastSent !== null && Math.abs(v - lastSent) < 0.01) return
    lastSent = v
    ports?.tiltSteer?.send(v)
  }

  function onOrientation(event) {
    if (typeof event.gamma !== 'number' && typeof event.beta !== 'number') return
    const angle = getScreenTiltDegrees(event.beta ?? 0, event.gamma ?? 0)
    lastAngle = angle
    if (neutral === null) neutral = angle
    if (enabled) send(steerFromAngle(angle, neutral))
  }

  function listen() {
    if (listening) return
    listening = true
    window.addEventListener('deviceorientation', onOrientation, true)
  }

  function turnOff() {
    enabled = false
    saveMode(storageKey, false)
    send(0)
    sendStatus()
  }

  // Must be called synchronously from a tap.
  function enable() {
    if (!available) return
    if (permitted) {
      enabled = true
      saveMode(storageKey, true)
      neutral = null
      listen()
      sendStatus()
      return
    }
    if (requesting) return
    requesting = true
    let request
    try {
      request = window.DeviceOrientationEvent.requestPermission()
    } catch {
      requesting = false
      turnOff()
      return
    }
    Promise.resolve(request)
      .then((result) => {
        requesting = false
        if (result === 'granted') {
          permitted = true
          enabled = true
          saveMode(storageKey, true)
          neutral = null
          listen()
          sendStatus()
        } else {
          turnOff()
        }
      })
      .catch(() => {
        requesting = false
        turnOff()
      })
  }

  document.addEventListener(
    'click',
    (event) => {
      const target = event.target instanceof Element ? event.target : null
      if (!target) return
      if (target.closest('[data-tilt-toggle]')) {
        if (enabled) {
          turnOff()
        } else {
          enable()
        }
      } else if (enabled && !listening) {
        // Tilt is wanted but the sensor is not listening yet (iOS: the
        // permission has not been granted). Any tap, the race start button
        // included, asks for it, so the player never has to toggle tilt off
        // and on again. `enable` runs synchronously inside this tap.
        enable()
      }
    },
    true,
  )

  ports?.calibrateTilt?.subscribe(() => {
    neutral = lastAngle
    lastSent = null
    if (enabled) send(0)
  })

  const recalibrate = () => {
    neutral = null
  }
  window.addEventListener('orientationchange', recalibrate)
  window.screen?.orientation?.addEventListener?.('change', recalibrate)

  // Without an iOS-style prompt, tilt (saved or default) can start listening
  // now. With one, the first tap on the start button asks for permission.
  if (enabled && permitted) listen()
  sendStatus()
  setTimeout(sendStatus, 0)
}

export function getScreenTiltDegrees(betaDeg, gammaDeg) {
  const degToRad = Math.PI / 180
  const radToDeg = 180 / Math.PI
  const b = betaDeg * degToRad
  const g = gammaDeg * degToRad

  // Earth "Up" unit vector in physical device coordinates (W3C Z-X'-Y'' rotation matrix row 3):
  // +x = portrait right edge, +y = portrait top edge, +z = out of front screen
  const ux = -Math.cos(b) * Math.sin(g)
  const uy = Math.sin(b)
  const uz = Math.cos(b) * Math.cos(g)

  const legacyAngle = typeof window.orientation === 'number' ? window.orientation : null
  const modernAngle =
    typeof window.screen !== 'undefined' &&
    window.screen.orientation &&
    typeof window.screen.orientation.angle === 'number'
      ? window.screen.orientation.angle
      : null
  const mediaLandscape =
    typeof window.matchMedia === 'function' && window.matchMedia('(orientation: landscape)').matches

  const isLandscape =
    mediaLandscape ||
    window.innerWidth > window.innerHeight ||
    legacyAngle === 90 ||
    legacyAngle === -90 ||
    modernAngle === 90 ||
    modernAngle === 270

  let negURight
  let uTop
  if (isLandscape) {
    let topIsPosX
    if (Math.abs(ux) > 0.14) {
      topIsPosX = ux > 0
    } else if (legacyAngle === 90 || legacyAngle === -90) {
      topIsPosX = legacyAngle === 90
    } else {
      topIsPosX = modernAngle !== 270
    }

    if (topIsPosX) {
      negURight = uy
      uTop = Math.abs(ux)
    } else {
      negURight = -uy
      uTop = Math.abs(ux)
    }
  } else if (legacyAngle === 180 || modernAngle === 180) {
    negURight = ux
    uTop = Math.abs(uy)
  } else {
    negURight = -ux
    uTop = Math.abs(uy)
  }

  const denom = Math.max(0.35, Math.hypot(uTop, uz * 0.5))
  return Math.atan2(negURight, denom) * radToDeg
}
