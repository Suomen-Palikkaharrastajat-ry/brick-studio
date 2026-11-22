import { decodeHash, isBinaryHash } from '../../brick-frame-viewport/runtime/url-hash-codec.js'
import { getScreenTiltDegrees } from './tilt-steer.js'

export { getScreenTiltDegrees }

export const LANG_KEY = 'palikkakehys.language'

export function readLanguage() {
  try {
    const hashParams = new URLSearchParams(window.location.hash.replace(/^#/, ''))
    const searchParams = new URLSearchParams(window.location.search)
    const lang = searchParams.get('lang') || hashParams.get('lang')
    if (lang) {
      return lang.toLowerCase().startsWith('fi') ? 'fi' : 'en'
    }
    const stored = window.localStorage.getItem(LANG_KEY)
    if (stored) {
      return stored.toLowerCase().startsWith('fi') ? 'fi' : 'en'
    }
    return 'en'
  } catch {
    return 'en'
  }
}

export function readAudioMuted(storagePrefix) {
  try {
    return window.localStorage.getItem(`palikkakehys.${storagePrefix}.audioMuted`) === '1'
  } catch {
    return false
  }
}

export function readLeaderboard(storagePrefix) {
  try {
    return window.localStorage.getItem(`palikkakehys.${storagePrefix}.leaderboard`) || '[]'
  } catch {
    return '[]'
  }
}

export function extractBinaryHashFragment(rawInput) {
  const str = String(rawInput ?? '').trim()
  const hashIdx = str.indexOf('#')
  const candidate = hashIdx >= 0 ? str.slice(hashIdx) : str

  const parts = candidate.replace(/^#/, '').split('&')
  for (const part of parts) {
    if (part.startsWith('b/')) {
      return `#${part}`
    }
  }
  if (isBinaryHash(candidate)) {
    return candidate.startsWith('#') ? candidate : `#${candidate}`
  }
  return null
}

export function decodeHashToByteArray(rawHash) {
  const binaryFrag = extractBinaryHashFragment(rawHash)
  if (!binaryFrag) return null
  try {
    const bytes = decodeHash(binaryFrag)
    return bytes ? Array.from(bytes) : null
  } catch {
    return null
  }
}

/**
 * Standard game port wiring, audio unlocking, multiplayer room auto-join,
 * QR scanner wiring, and tilt orientation lifecycle.
 */
export function setupGameRuntime(ports, options = {}) {
  const {
    storagePrefix = 'game',
    audioEngine,
    multiplayer,
    qrScanner,
    playerPrefix = 'Player ',
    tiltFormat = 'object', // 'object' for { available, gamma, beta }, 'normalized' for Float (-1..1)
    onModelDecoded,
  } = options

  const audioKey = `palikkakehys.${storagePrefix}.audioMuted`
  const boardKey = `palikkakehys.${storagePrefix}.leaderboard`

  // 1. Audio unlock listeners
  if (audioEngine) {
    const unlockAudioOnce = () => {
      audioEngine.unlock()
    }
    window.addEventListener('pointerdown', unlockAudioOnce, { passive: true })
    window.addEventListener('keydown', unlockAudioOnce, { passive: true })
    window.addEventListener('touchstart', unlockAudioOnce, { passive: true })

    ports?.updateAudioState?.subscribe((state) => {
      audioEngine.updateContinuous(state)
    })

    ports?.playSfx?.subscribe((event) => {
      audioEngine.playSfx(event)
    })

    ports?.setAudioMuted?.subscribe((nextMuted) => {
      audioEngine.setMuted(nextMuted)
      try {
        window.localStorage.setItem(audioKey, nextMuted ? '1' : '0')
      } catch {}
    })
  }

  // 2. Storage & Language
  ports?.storeLanguage?.subscribe((lang) => {
    try {
      window.localStorage.setItem(LANG_KEY, lang)
    } catch {}
  })

  ports?.saveLeaderboard?.subscribe((jsonStr) => {
    try {
      window.localStorage.setItem(boardKey, String(jsonStr ?? '[]'))
    } catch {}
  })

  // 3. Binary hash decoding & sharing
  ports?.decodeBinaryHash?.subscribe((rawInput) => {
    const bytes = decodeHashToByteArray(rawInput)
    ports?.binaryHashDecoded?.send(bytes)
    if (bytes && typeof onModelDecoded === 'function') {
      onModelDecoded(rawInput, bytes)
    }
  })

  ports?.copyShareUrl?.subscribe((hashOrUrl) => {
    const full = String(hashOrUrl ?? '').startsWith('#')
      ? new URL(hashOrUrl, window.location.href).href
      : String(hashOrUrl ?? '')
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(full).catch(() => {})
    }
  })

  // 4. Device orientation & tilt steering
  let tiltAttached = false
  function sendTilt(event) {
    const hasAngles = typeof event.gamma === 'number' || typeof event.beta === 'number'
    const rawGamma = typeof event.gamma === 'number' ? event.gamma : 0
    const rawBeta = typeof event.beta === 'number' ? event.beta : 0
    const steerGamma = hasAngles ? getScreenTiltDegrees(rawBeta, rawGamma) : 0

    if (tiltFormat === 'normalized') {
      const clamped = Math.max(-45, Math.min(45, steerGamma))
      ports?.tiltOrientationReceived?.send(clamped / 45.0)
    } else {
      ports?.tiltOrientationReceived?.send({
        available: hasAngles,
        gamma: steerGamma,
        beta: rawBeta,
      })
    }
  }

  function attachTiltListener() {
    if (tiltAttached) return
    tiltAttached = true
    window.addEventListener('deviceorientation', sendTilt, true)
  }

  ports?.requestTiltPermission?.subscribe(async () => {
    try {
      if (
        typeof DeviceOrientationEvent !== 'undefined' &&
        typeof DeviceOrientationEvent.requestPermission === 'function'
      ) {
        const result = await DeviceOrientationEvent.requestPermission()
        if (result === 'granted') {
          attachTiltListener()
        } else if (tiltFormat === 'normalized') {
          ports?.tiltOrientationReceived?.send(0)
        } else {
          ports?.tiltOrientationReceived?.send({ available: false, gamma: 0, beta: 0 })
        }
      } else if (typeof window !== 'undefined' && 'ondeviceorientation' in window) {
        attachTiltListener()
      } else if (tiltFormat === 'normalized') {
        ports?.tiltOrientationReceived?.send(0)
      } else {
        ports?.tiltOrientationReceived?.send({ available: false, gamma: 0, beta: 0 })
      }
    } catch {
      if (tiltFormat === 'normalized') {
        ports?.tiltOrientationReceived?.send(0)
      } else {
        ports?.tiltOrientationReceived?.send({ available: false, gamma: 0, beta: 0 })
      }
    }
  })

  // 5. Hash change listener
  window.addEventListener('hashchange', () => {
    ports?.hashChanged?.send(window.location.hash ?? '')
  })

  // 6. Multiplayer room auto-join from URL hash (#room=CODE or #join=CODE)
  if (multiplayer) {
    try {
      const hashStr = window.location.hash || ''
      const roomMatch = hashStr.match(/[#&](?:room|join)=([A-Za-z0-9]{4,12})/i)
      if (roomMatch && roomMatch[1]) {
        setTimeout(() => {
          multiplayer.joinRoom(
            roomMatch[1].toUpperCase(),
            playerPrefix + Math.floor(Math.random() * 900 + 100),
            ''
          )
        }, 100)
      }
    } catch {}
  }
}
