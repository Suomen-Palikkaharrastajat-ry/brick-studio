import assert from 'node:assert/strict'
import test from 'node:test'
import { dataUrlToBlob, fittedSize, initSnapshots } from './snapshot.js'

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
