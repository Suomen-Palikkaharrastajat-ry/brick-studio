/**
 * Shared boot of the Brick Frame games (brick-racer, brick-sails).
 *
 * Starts the Elm app through `initBricksRuntime` with the flags every game
 * reads, then wires the shared runtime around it: audio, storage, sharing,
 * multiplayer, the QR scanner (`setupGameRuntime`) and, when asked for, tilt
 * steering (`setupTiltSteer`). A game's `main.js` only names what is its own:
 * its storage prefix, its synth and its player name prefix.
 *
 * The Elm side of this contract is `Game.Ports` plus the game's own `Ports`
 * module (`updateAudioState`, `playSfx`).
 */

import { initBricksRuntime } from '../../brick-frame-viewport/runtime/initBricksRuntime.js'
import { createMultiplayerManager } from './multiplayer.js'
import { createQrCameraScanner } from './qr-camera-scanner.js'
import {
  readLanguage,
  readAudioMuted,
  readLeaderboard,
  decodeHashToByteArray,
  setupGameRuntime,
} from './game-runtime.js'
import { setupTiltSteer } from './tilt-steer.js'

const DEFAULT_LDRAW_BASE = 'https://catalog.palikkaharrastajat.fi/raw'
const DEFAULT_INDICES_BASE = 'https://catalog.palikkaharrastajat.fi/indices'

/**
 * Boot a game.
 *
 * - `elm`: the compiled `Elm.Main`.
 * - `env`: the app's `import.meta.env` (`VITE_LDRAW_BASE`,
 *   `VITE_LDRAW_INDICES_BASE`, `VITE_LDRAW_FALLBACK_BASE`).
 * - `storagePrefix`: localStorage namespace, e.g. `'racer'`
 *   (`palikkakehys.<prefix>.audioMuted`, `palikkakehys.<prefix>.leaderboard`).
 * - `createAudioEngine(initialMuted)`: the game's synth, a `BaseAudioEngine`.
 * - `playerPrefix`: name prefix for players auto-joining a room link.
 * - `tiltFormat`: what the `tiltOrientationReceived` port carries, see
 *   `setupGameRuntime`.
 * - `tiltSteer`: `{ storageKey }` to turn on the calibrated tilt steering of
 *   `tilt-steer.js` (`calibrateTilt`, `tiltSteer`, `tiltStatus` ports).
 * - `node`: the element Elm takes over (default `#app`).
 */
export function bootGame({
  elm,
  env = {},
  storagePrefix,
  createAudioEngine,
  playerPrefix,
  tiltFormat,
  tiltSteer,
  node = document.getElementById('app'),
}) {
  const ldrawBase = env.VITE_LDRAW_BASE || DEFAULT_LDRAW_BASE
  const indicesBase = env.VITE_LDRAW_INDICES_BASE || DEFAULT_INDICES_BASE

  const coarsePointer =
    typeof window.matchMedia === 'function' && window.matchMedia('(pointer: coarse)').matches

  // A build link in the page URL (`#b/...` or `#model=...`) is handed to Elm
  // already inflated, so the first frame can show the player's own build.
  const initialHash = window.location.hash ?? ''
  const savedAudioMuted = readAudioMuted(storagePrefix)
  const audioEngine = createAudioEngine ? createAudioEngine(savedAudioMuted) : undefined

  const runtime = initBricksRuntime({
    elm,
    node,
    mode: 'viewer',
    ldrawBase,
    indicesBase,
    ldrawFallbackBase: env.VITE_LDRAW_FALLBACK_BASE || '/raw',
    defaultModel: '',
    loadModelInJs: false,
    extraFlags: {
      coarsePointer,
      language: readLanguage(),
      savedAudioMuted,
      savedLeaderboardJson: readLeaderboard(storagePrefix),
      baseUrl: window.location.origin + window.location.pathname,
      predecodedInitialBytes: decodeHashToByteArray(initialHash),
    },
    initialHash,
    syncUrlHash: true,
    useWindowResize: true,
    dragDropTarget: window,
    suppressGestureTarget: window,
  })

  const ports = runtime.app?.ports
  const multiplayer = createMultiplayerManager(ports)
  const qrScanner = createQrCameraScanner(ports, {
    onScanned: (hash) => {
      const bytes = decodeHashToByteArray(hash)
      if (bytes) {
        ports?.binaryHashDecoded?.send(bytes)
        window.location.hash = hash
      }
    },
  })

  setupGameRuntime(ports, {
    storagePrefix,
    audioEngine,
    multiplayer,
    qrScanner,
    playerPrefix,
    ...(tiltFormat ? { tiltFormat } : {}),
  })

  if (tiltSteer) {
    setupTiltSteer(ports, tiltSteer)
  }

  return { runtime, ports, audioEngine, multiplayer, qrScanner }
}
