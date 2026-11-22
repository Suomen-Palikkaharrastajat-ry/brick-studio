/**
 * Cross-platform Camera QR Code Scanner for Brick Family Games.
 * Uses native BarcodeDetector API with seamless jsQR fallback.
 * Works on mobile (rear/front camera selection, touch-friendly UI) and desktop webcams.
 */
import jsQR from './jsQR.js'

export function createQrCameraScanner(ports, options = {}) {
  let stream = null
  let videoEl = null
  let canvasEl = null
  let ctx = null
  let modalEl = null
  let animFrameId = null
  let currentFacingMode = 'environment' // 'environment' (rear) or 'user' (front)
  let scanning = false
  let barcodeDetector = null

  if (typeof window !== 'undefined' && 'BarcodeDetector' in window) {
    try {
      barcodeDetector = new window.BarcodeDetector({ formats: ['qr_code'] })
    } catch {
      barcodeDetector = null
    }
  }

  function extractHash(raw) {
    if (!raw) return null
    const str = String(raw).trim()
    const match = str.match(/#b\/[A-Za-z0-9_-]+/)
    if (match) return match[0]
    if (str.startsWith('#b/') || str.startsWith('b/')) {
      return str.startsWith('#') ? str : `#${str}`
    }
    return str
  }

  function playSuccessBeep() {
    try {
      const AudioCtx = window.AudioContext || window.webkitAudioContext
      if (!AudioCtx) return
      const ctx = new AudioCtx()
      const now = ctx.currentTime
      const osc = ctx.createOscillator()
      const gain = ctx.createGain()
      osc.type = 'sine'
      osc.frequency.setValueAtTime(587.33, now) // D5
      osc.frequency.setValueAtTime(880.0, now + 0.08) // A5
      gain.gain.setValueAtTime(0.3, now)
      gain.gain.exponentialRampToValueAtTime(0.001, now + 0.35)
      osc.connect(gain)
      gain.connect(ctx.destination)
      osc.start(now)
      osc.stop(now + 0.35)
    } catch {
      // Ignore audio errors
    }
  }

  function createModalDom() {
    if (modalEl) return modalEl

    modalEl = document.createElement('div')
    modalEl.id = 'brick-qr-scanner-modal'
    modalEl.style.cssText = `
      position: fixed;
      inset: 0;
      z-index: 99999;
      display: none;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      background: rgba(2, 6, 23, 0.88);
      backdrop-filter: blur(8px);
      -webkit-backdrop-filter: blur(8px);
      padding: 16px;
      font-family: ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      color: #f8fafc;
      box-sizing: border-box;
    `

    const card = document.createElement('div')
    card.style.cssText = `
      position: relative;
      width: 100%;
      max-width: 440px;
      background: #0f172a;
      border: 1px solid rgba(255, 255, 255, 0.12);
      border-radius: 24px;
      overflow: hidden;
      box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.7);
      display: flex;
      flex-direction: column;
    `

    // Header
    const header = document.createElement('div')
    header.style.cssText = `
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 16px 20px;
      border-bottom: 1px solid rgba(255, 255, 255, 0.08);
    `
    header.innerHTML = `
      <div>
        <h3 style="margin: 0; font-size: 16px; font-weight: 800; letter-spacing: 0.02em; color: #fff;">Scan Model QR Code</h3>
        <p style="margin: 2px 0 0 0; font-size: 12px; color: #94a3b8;">Aim camera at Brick Play Share QR code</p>
      </div>
      <button id="qr-close-btn" style="
        background: rgba(255, 255, 255, 0.08);
        border: none;
        color: #cbd5e1;
        width: 32px;
        height: 32px;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 18px;
        font-weight: bold;
        cursor: pointer;
        transition: background 0.15s;
      ">&times;</button>
    `

    // Viewfinder Container
    const viewContainer = document.createElement('div')
    viewContainer.style.cssText = `
      position: relative;
      width: 100%;
      height: 320px;
      background: #020617;
      overflow: hidden;
      display: flex;
      align-items: center;
      justify-content: center;
    `

    videoEl = document.createElement('video')
    videoEl.setAttribute('playsinline', 'true')
    videoEl.setAttribute('autoplay', 'true')
    videoEl.muted = true
    videoEl.style.cssText = `
      width: 100%;
      height: 100%;
      object-fit: cover;
    `

    // Reticle Target overlay
    const reticle = document.createElement('div')
    reticle.id = 'qr-reticle'
    reticle.style.cssText = `
      position: absolute;
      width: 200px;
      height: 200px;
      border: 2px dashed rgba(56, 189, 248, 0.7);
      border-radius: 20px;
      box-shadow: 0 0 0 9999px rgba(2, 6, 23, 0.45);
      pointer-events: none;
      display: flex;
      align-items: center;
      justify-content: center;
      transition: border-color 0.2s, box-shadow 0.2s;
    `

    // Animated laser beam
    const laser = document.createElement('div')
    laser.style.cssText = `
      position: absolute;
      left: 10px;
      right: 10px;
      height: 2px;
      background: linear-gradient(90deg, transparent, #38bdf8, transparent);
      box-shadow: 0 0 8px #38bdf8;
      animation: qr-laser 2s ease-in-out infinite alternate;
    `
    reticle.appendChild(laser)

    // Hidden canvas for video sampling
    canvasEl = document.createElement('canvas')
    canvasEl.style.display = 'none'
    ctx = canvasEl.getContext('2d', { willReadFrequently: true })

    viewContainer.appendChild(videoEl)
    viewContainer.appendChild(reticle)
    viewContainer.appendChild(canvasEl)

    // Footer actions
    const footer = document.createElement('div')
    footer.style.cssText = `
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 14px 20px;
      border-top: 1px solid rgba(255, 255, 255, 0.08);
      gap: 12px;
    `

    const switchCamBtn = document.createElement('button')
    switchCamBtn.id = 'qr-switch-cam-btn'
    switchCamBtn.innerHTML = '&#8646; Flip Camera'
    switchCamBtn.style.cssText = `
      padding: 8px 14px;
      border-radius: 12px;
      background: rgba(30, 41, 59, 0.9);
      border: 1px solid rgba(255, 255, 255, 0.15);
      color: #e2e8f0;
      font-size: 12px;
      font-weight: 700;
      cursor: pointer;
    `
    switchCamBtn.onclick = () => {
      currentFacingMode = currentFacingMode === 'environment' ? 'user' : 'environment'
      restartCamera()
    }

    const fileLabel = document.createElement('label')
    fileLabel.innerHTML = '&#128193; Image File'
    fileLabel.style.cssText = `
      padding: 8px 14px;
      border-radius: 12px;
      background: rgba(30, 41, 59, 0.9);
      border: 1px solid rgba(255, 255, 255, 0.15);
      color: #e2e8f0;
      font-size: 12px;
      font-weight: 700;
      cursor: pointer;
      display: inline-block;
    `

    const fileInput = document.createElement('input')
    fileInput.type = 'file'
    fileInput.accept = 'image/*'
    fileInput.style.display = 'none'
    fileInput.onchange = (e) => {
      const file = e.target.files?.[0]
      if (file) handleImageFile(file)
    }
    fileLabel.appendChild(fileInput)

    footer.appendChild(switchCamBtn)
    footer.appendChild(fileLabel)

    card.appendChild(header)
    card.appendChild(viewContainer)
    card.appendChild(footer)
    modalEl.appendChild(card)

    // Add laser animation keyframes if not present
    if (!document.getElementById('qr-laser-keyframes')) {
      const style = document.createElement('style')
      style.id = 'qr-laser-keyframes'
      style.textContent = `
        @keyframes qr-laser {
          0% { top: 15px; opacity: 0.8; }
          50% { opacity: 1; }
          100% { top: 185px; opacity: 0.8; }
        }
      `
      document.head.appendChild(style)
    }

    document.body.appendChild(modalEl)

    header.querySelector('#qr-close-btn').onclick = () => stopScanner()

    return modalEl
  }

  function handleSuccess(scannedRaw) {
    if (!scanning) return
    const hash = extractHash(scannedRaw)
    if (!hash) {
      console.warn('QR scanned but no model hash detected:', scannedRaw)
      return
    }

    scanning = false
    playSuccessBeep()

    const reticle = document.getElementById('qr-reticle')
    if (reticle) {
      reticle.style.borderColor = '#22c55e'
      reticle.style.boxShadow = '0 0 20px #22c55e, 0 0 0 9999px rgba(2, 6, 23, 0.6)'
    }

    setTimeout(() => {
      stopScanner()
      if (ports?.qrCodeScanned) {
        ports.qrCodeScanned.send(hash)
      }
      if (typeof options.onScanned === 'function') {
        options.onScanned(hash)
      }
    }, 300)
  }

  async function handleImageFile(file) {
    try {
      const img = new Image()
      const url = URL.createObjectURL(file)
      img.onload = () => {
        URL.revokeObjectURL(url)
        if (!canvasEl) return
        canvasEl.width = img.width
        canvasEl.height = img.height
        ctx.drawImage(img, 0, 0)
        const imgData = ctx.getImageData(0, 0, img.width, img.height)
        const code = jsQR(imgData.data, imgData.width, imgData.height, {
          inversionAttempts: 'dontInvert',
        })
        if (code && code.data) {
          handleSuccess(code.data)
        } else {
          alert('No Brick Play QR code found in this image. Try another photo.')
        }
      }
      img.src = url
    } catch (err) {
      console.error('File scan error:', err)
    }
  }

  async function scanFrame() {
    if (!scanning || !videoEl) return

    if (videoEl.readyState >= videoEl.HAVE_CURRENT_DATA) {
      // 1. Try native BarcodeDetector if available
      if (barcodeDetector) {
        try {
          const barcodes = await barcodeDetector.detect(videoEl)
          if (barcodes && barcodes.length > 0) {
            handleSuccess(barcodes[0].rawValue)
            return
          }
        } catch {
          // Fall through to jsQR
        }
      }

      // 2. jsQR software fallback
      try {
        const w = Math.min(640, videoEl.videoWidth || 480)
        const h = Math.min(480, videoEl.videoHeight || 360)
        if (canvasEl.width !== w || canvasEl.height !== h) {
          canvasEl.width = w
          canvasEl.height = h
        }
        ctx.drawImage(videoEl, 0, 0, w, h)
        const imgData = ctx.getImageData(0, 0, w, h)
        const code = jsQR(imgData.data, imgData.width, imgData.height, {
          inversionAttempts: 'dontInvert',
        })
        if (code && code.data) {
          handleSuccess(code.data)
          return
        }
      } catch (err) {
        // Continue scanning
      }
    }

    if (scanning) {
      animFrameId = requestAnimationFrame(scanFrame)
    }
  }

  async function startCamera() {
    try {
      if (stream) {
        stream.getTracks().forEach((t) => t.stop())
        stream = null
      }

      const constraints = {
        video: {
          facingMode: { ideal: currentFacingMode },
          width: { ideal: 1280 },
          height: { ideal: 720 },
        },
        audio: false,
      }

      stream = await navigator.mediaDevices.getUserMedia(constraints)
      videoEl.srcObject = stream
      await videoEl.play()

      scanning = true
      animFrameId = requestAnimationFrame(scanFrame)
    } catch (err) {
      console.error('Camera access error:', err)
      const errMsg = err?.name === 'NotAllowedError'
        ? 'Camera permission denied. Please allow camera access in your browser settings.'
        : `Could not start camera: ${err?.message || err}`
      if (ports?.qrScannerError) {
        ports.qrScannerError.send(errMsg)
      }
      alert(errMsg)
      stopScanner()
    }
  }

  async function restartCamera() {
    if (scanning) {
      await startCamera()
    }
  }

  function startScanner() {
    createModalDom()
    modalEl.style.display = 'flex'
    const reticle = document.getElementById('qr-reticle')
    if (reticle) {
      reticle.style.borderColor = 'rgba(56, 189, 248, 0.7)'
      reticle.style.boxShadow = '0 0 0 9999px rgba(2, 6, 23, 0.45)'
    }
    startCamera()
  }

  function stopScanner() {
    scanning = false
    if (animFrameId) {
      cancelAnimationFrame(animFrameId)
      animFrameId = null
    }
    if (stream) {
      stream.getTracks().forEach((t) => t.stop())
      stream = null
    }
    if (videoEl) {
      videoEl.srcObject = null
    }
    if (modalEl) {
      modalEl.style.display = 'none'
    }
  }

  // Wire Elm ports if available
  if (ports?.openQrScanner) {
    ports.openQrScanner.subscribe(() => {
      startScanner()
    })
  }

  if (ports?.closeQrScanner) {
    ports.closeQrScanner.subscribe(() => {
      stopScanner()
    })
  }

  return {
    open: startScanner,
    close: stopScanner,
  }
}
