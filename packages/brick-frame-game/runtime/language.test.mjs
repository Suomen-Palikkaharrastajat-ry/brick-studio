// Language defaults to Finnish for the games; a link or a saved choice overrides it.
import { test } from 'node:test'
import assert from 'node:assert/strict'
import { readLanguage, LANG_KEY } from './language.js'

function withWindow({ search = '', hash = '', stored = null }, fn) {
  const previous = globalThis.window
  globalThis.window = {
    location: { search, hash },
    localStorage: {
      getItem: (key) => (key === LANG_KEY ? stored : null),
    },
  }
  try {
    return fn()
  } finally {
    globalThis.window = previous
  }
}

test('defaults to Finnish with no link and no saved choice', () => {
  assert.equal(withWindow({}, readLanguage), 'fi')
})

test('?lang=en selects English', () => {
  assert.equal(withWindow({ search: '?lang=en' }, readLanguage), 'en')
})

test('a saved English choice is kept', () => {
  assert.equal(withWindow({ stored: 'en' }, readLanguage), 'en')
})

test('a link overrides a saved choice', () => {
  assert.equal(withWindow({ search: '?lang=fi', stored: 'en' }, readLanguage), 'fi')
})
