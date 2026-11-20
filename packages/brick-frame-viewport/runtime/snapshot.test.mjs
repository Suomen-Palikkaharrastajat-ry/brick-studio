import assert from 'node:assert/strict'
import test from 'node:test'
import { copyWebglCanvasTo2d, dataUrlToBlob, fittedSize, initSnapshots } from './snapshot.js'

test('fittedSize preserves aspect ratio within maxWidth and maxHeight', () => {
  assert.deepEqual(fittedSize(1920, 1080, 960, 960), { width: 960, height: 540 })
  assert.deepEqual(fittedSize(1000, 2000, 500, 500), { width: 250, height: 500 })
  assert.deepEqual(fittedSize(800, 600, 0, 0), { width: 800, height: 600 })
})

test('dataUrlToBlob decodes base64 PNG data URLs into a typed Blob', async () => {
  const bytes = new Uint8Array([137, 80, 78, 71, 13, 10, 26, 10])
  const b64 = Buffer.from(bytes).toString('base64')
  const blob = dataUrlToBlob(`data:image/png;base64,${b64}`)
  assert.ok(blob instanceof Blob)
  assert.equal(blob.type, 'image/png')
  const buf = new Uint8Array(await blob.arrayBuffer())
  assert.deepEqual(Array.from(buf), Array.from(bytes))
})

test('dataUrlToBlob decodes URL-encoded data URLs and returns null for invalid input', async () => {
  const blob = dataUrlToBlob('data:text/plain,hello%20world')
  assert.ok(blob instanceof Blob)
  assert.equal(blob.type, 'text/plain')
  assert.equal(await blob.text(), 'hello world')
  assert.equal(dataUrlToBlob('https://example.com/image.png'), null)
  assert.equal(dataUrlToBlob('data:image/png'), null)
  assert.equal(dataUrlToBlob(null), null)
})

test('saveSnapshot downloads via a blob: object URL and defers revocation for iOS Safari', () => {
  const prevDoc = globalThis.document
  const createdLinks = []
  const appended = []
  let removedCount = 0

  globalThis.document = {
    createElement(tag) {
      assert.equal(tag, 'a')
      const link = {
        href: '',
        download: '',
        style: {},
        clicked: false,
        click() {
          this.clicked = true
        },
        remove() {
          removedCount++
        },
      }
      createdLinks.push(link)
      return link
    },
    body: {
      appendChild(el) {
        appended.push(el)
      },
    },
  }

  try {
    let savedHandler = null
    initSnapshots({
      saveSnapshot: {
        subscribe(fn) {
          savedHandler = fn
        },
      },
    })
    assert.equal(typeof savedHandler, 'function')

    const b64 = Buffer.from([137, 80, 78, 71]).toString('base64')
    savedHandler({
      dataUrl: `data:image/png;base64,${b64}`,
      filename: 'model.png',
    })

    assert.equal(createdLinks.length, 1)
    const link = createdLinks[0]
    assert.ok(link.href.startsWith('blob:'), `expected blob: URL for iOS Safari compatibility, got ${link.href}`)
    assert.equal(link.download, 'model.png')
    assert.equal(link.style.display, 'none')
    assert.equal(link.clicked, true)
    assert.equal(appended.length, 1)
    assert.equal(
      removedCount,
      0,
      'link removal and URL revocation must be deferred so iOS Safari can complete its async download prompt',
    )
    if (typeof URL.revokeObjectURL === 'function') {
      URL.revokeObjectURL(link.href)
    }
  } finally {
    globalThis.document = prevDoc
  }
})

test('copyWebglCanvasTo2d reads pixels via gl.readPixels and flips rows vertically', () => {
  const prevDoc = globalThis.document
  const prevImageData = globalThis.ImageData
  let readPixelsCalled = false
  let putImageDataCalled = false
  let drawImageCalled = false

  globalThis.ImageData = class MockImageData {
    constructor(data, width, height) {
      this.data = data
      this.width = width
      this.height = height
    }
  }

  const mockSrcCanvas = {
    width: 0,
    height: 0,
    getContext(type) {
      if (type === '2d') {
        return {
          putImageData(imgData, x, y) {
            putImageDataCalled = true
            assert.equal(x, 0)
            assert.equal(y, 0)
            // Bottom row in WebGL (y=0) should be at top row in 2D (y=0)
            // Let's verify row flipping:
            // WebGL had [10, 20, 30, 255] at bottom row (index 0..3)
            // and [40, 50, 60, 255] at top row (index 4..7)
            // After flip: top row (index 0..3) is [40, 50, 60, 255]
            assert.equal(imgData.data[0], 40)
            assert.equal(imgData.data[1], 50)
            assert.equal(imgData.data[4], 10)
            assert.equal(imgData.data[5], 20)
          },
        }
      }
      return null
    },
  }

  globalThis.document = {
    createElement(tag) {
      if (tag === 'canvas') return mockSrcCanvas
      return {}
    },
    documentElement: {},
  }

  try {
    const mockWebgl = {
      drawingBufferWidth: 1,
      drawingBufferHeight: 2,
      readPixels(x, y, w, h, format, type, dst) {
        readPixelsCalled = true
        assert.equal(format, 0x1908)
        assert.equal(type, 0x1401)
        // Row 0 (bottom row):
        dst[0] = 10
        dst[1] = 20
        dst[2] = 30
        dst[3] = 255
        // Row 1 (top row):
        dst[4] = 40
        dst[5] = 50
        dst[6] = 60
        dst[7] = 255
      },
      RGBA: 0x1908,
      UNSIGNED_BYTE: 0x1401,
    }

    const mockCanvas = {
      getContext(type) {
        if (type === 'webgl2') return mockWebgl
        return null
      },
    }

    const targetCtx = {
      fillRect() {},
      drawImage(source, x, y, w, h) {
        drawImageCalled = true
        assert.equal(source, mockSrcCanvas)
        assert.equal(w, 100)
        assert.equal(h, 200)
      },
    }

    copyWebglCanvasTo2d(mockCanvas, targetCtx, 100, 200, false)
    assert.ok(readPixelsCalled, 'readPixels should be called')
    assert.ok(putImageDataCalled, 'putImageData should be called')
    assert.ok(drawImageCalled, 'drawImage should be called')
  } finally {
    globalThis.document = prevDoc
    globalThis.ImageData = prevImageData
  }
})

test('copyWebglCanvasTo2d falls back to drawImage when canvas lacks WebGL context', () => {
  let fallbackDrawn = false
  const mockCanvas = {
    getContext() {
      return null
    },
  }
  const targetCtx = {
    fillRect() {},
    drawImage(source, x, y, w, h) {
      fallbackDrawn = true
      assert.equal(source, mockCanvas)
      assert.equal(w, 80)
      assert.equal(h, 60)
    },
  }

  copyWebglCanvasTo2d(mockCanvas, targetCtx, 80, 60, true)
  assert.ok(fallbackDrawn, 'fallback drawImage should be called')
})
