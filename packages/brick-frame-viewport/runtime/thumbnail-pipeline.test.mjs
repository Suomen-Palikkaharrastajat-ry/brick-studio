import assert from 'node:assert/strict'
import test from 'node:test'
import {
  cleanPartTitle,
  partTitle,
  resolvePartTitle,
  MAX_TITLE_HOPS,
  isLikelyPrimitive,
  candidateRelativePaths,
  flattenModel,
  modelCacheKey,
} from './thumbnail-pipeline.js'

// A stub library: file name -> .dat text. Lookups are recorded to check hops.
function stubLoader(files) {
  const requested = []
  const load = async (name) => {
    requested.push(name)
    return Object.prototype.hasOwnProperty.call(files, name) ? files[name] : null
  }
  return { load, requested }
}

test('cleanPartTitle strips the LDraw markers and collapses spaces', () => {
  assert.equal(cleanPartTitle('=Technic Bush with Two Flanges'), 'Technic Bush with Two Flanges')
  assert.equal(cleanPartTitle('~Brick 2 x 4'), 'Brick 2 x 4')
  assert.equal(cleanPartTitle('_Internal  helper'), 'Internal helper')
  assert.equal(cleanPartTitle('| BuWizz 2.0 Hub'), 'BuWizz 2.0 Hub')
  assert.equal(cleanPartTitle('=~|Brick'), 'Brick')
  assert.equal(cleanPartTitle('Plate 1 x 2'), 'Plate 1 x 2')
})

test('a plain title is returned with runs of spaces collapsed', async () => {
  const { load } = stubLoader({ '3001.dat': '0 Brick  2 x  4\n0 Name: 3001.dat\n' })
  assert.equal(await partTitle('3001.dat', '', { load }), 'Brick 2 x 4')
})

test('an alias marker is stripped from the title', async () => {
  const { load } = stubLoader({ 'x.dat': '0 =Technic Bush with Two Flanges\n' })
  assert.equal(await partTitle('x.dat', '', { load }), 'Technic Bush with Two Flanges')
})

test('a moved part takes the title of the file it moved to', async () => {
  const { load, requested } = stubLoader({
    '3648.dat': '0 ~Moved to 3648b\n0 Name: 3648.dat\n',
    '3648b.dat': '0 Gear 24 Tooth Double Bevel\n',
  })
  assert.equal(await partTitle('3648.dat', '', { load }), 'Gear 24 Tooth Double Bevel')
  assert.deepEqual(requested, ['3648.dat', '3648b.dat'])
})

test('a redirect chain is followed for at most three hops', async () => {
  const files = {
    'a.dat': '0 ~Moved to b\n',
    'b.dat': '0 ~Moved to c\n',
    'c.dat': '0 ~Moved to d\n',
    'd.dat': '0 Final Name\n',
  }
  const { load, requested } = stubLoader(files)
  assert.equal(MAX_TITLE_HOPS, 3)
  assert.equal(await partTitle('a.dat', '', { load }), 'Final Name')
  assert.deepEqual(requested, ['a.dat', 'b.dat', 'c.dat', 'd.dat'])
})

test('a chain longer than three hops gives no title rather than a marker', async () => {
  const files = {
    'a.dat': '0 ~Moved to b\n',
    'b.dat': '0 ~Moved to c\n',
    'c.dat': '0 ~Moved to d\n',
    'd.dat': '0 ~Moved to e\n',
    'e.dat': '0 Too Far\n',
  }
  const { load, requested } = stubLoader(files)
  assert.equal(await partTitle('a.dat', '', { load }), null)
  assert.equal(requested.length, 4)
})

test('a redirect to a missing file, a cycle or an absent part gives null', async () => {
  const missing = stubLoader({ 'm.dat': '0 ~Moved to nowhere\n' })
  assert.equal(await partTitle('m.dat', '', { load: missing.load }), null)

  const cycle = stubLoader({ 'p.dat': '0 ~Moved to q\n', 'q.dat': '0 ~Moved to p\n' })
  assert.equal(await partTitle('p.dat', '', { load: cycle.load }), null)

  const absent = stubLoader({})
  assert.equal(await partTitle('none.dat', '', { load: absent.load }), null)
})

test('a file that does not start with a meta line has no title', async () => {
  const { load } = stubLoader({ 'z.dat': '1 16 0 0 0 1 0 0 0 1 0 0 0 1 stud.dat\n' })
  assert.equal(await partTitle('z.dat', '', { load }), null)
})

test('resolvePartTitle works with a bare loader function', async () => {
  const load = async (name) => (name === 'only.dat' ? '0 Only One\n' : null)
  assert.equal(await resolvePartTitle('only.dat', load), 'Only One')
})

test('isLikelyPrimitive identifies primitives and subfiles correctly', () => {
  const primitives = [
    'st4jfil4.dat',
    'wpin2.dat',
    'wpin2a.dat',
    'wpin.dat',
    'empty.dat',
    'wpinhol2.dat',
    't01o0556.dat',
    'axlehole.dat',
    'axl5hol9.dat',
    'axl5ho10.dat',
    'axl5hol8.dat',
    'axlehol2.dat',
    'axl3hole.dat',
    'stud.dat',
    '1-4cyli.dat',
    '8/1-4cyli.dat',
    '48/stud.dat',
  ]
  for (const name of primitives) {
    assert.equal(isLikelyPrimitive(name), true, `Expected ${name} to be classified as primitive`)
  }

  const parts = ['3001.dat', '3647.dat', '4185.dat', '32062.dat', '11214.dat']
  for (const name of parts) {
    assert.equal(isLikelyPrimitive(name), false, `Expected ${name} to NOT be classified as primitive`)
  }
})

test('candidateRelativePaths prioritizes correct directories and eliminates spurious 404s', () => {
  // 8/ and 48/ primitives map directly to p/
  assert.deepEqual(candidateRelativePaths('8/1-4cyli.dat'), ['p/8/1-4cyli.dat'])
  assert.deepEqual(candidateRelativePaths('48/stud.dat'), ['p/48/stud.dat'])

  // Explicit p/ or parts/ keep their path
  assert.deepEqual(candidateRelativePaths('p/stud.dat'), ['p/stud.dat'])
  assert.deepEqual(candidateRelativePaths('parts/3001.dat'), ['parts/3001.dat'])

  // Subparts s/ map directly to parts/s/
  assert.deepEqual(candidateRelativePaths('s/3001s01.dat'), ['parts/s/3001s01.dat'])

  // Geometric primitives prioritize p/ before parts/
  assert.deepEqual(candidateRelativePaths('st4jfil4.dat'), [
    'p/st4jfil4.dat',
    'parts/st4jfil4.dat',
    'parts/s/st4jfil4.dat',
  ])
  assert.deepEqual(candidateRelativePaths('wpin2.dat'), [
    'p/wpin2.dat',
    'parts/wpin2.dat',
    'parts/s/wpin2.dat',
  ])
  assert.deepEqual(candidateRelativePaths('axlehole.dat'), [
    'p/axlehole.dat',
    'parts/axlehole.dat',
    'parts/s/axlehole.dat',
  ])

  // Normal parts prioritize parts/ before p/
  assert.deepEqual(candidateRelativePaths('3001.dat'), [
    'parts/3001.dat',
    'parts/s/3001.dat',
    'p/3001.dat',
  ])
})

// A one-triangle part, for the whole-model tests.
const TRIANGLE_PART = '0 Triangle\n3 16 0 0 0 10 0 0 0 0 10\n2 24 0 0 0 10 0 0\n'

test('flattenModel places each line of a model with its own colour and matrix', async () => {
  const { load, requested } = stubLoader({ 'tri.dat': TRIANGLE_PART })
  const text = ['1 70 0 0 0 1 0 0 0 1 0 0 0 1 tri.dat', '1 15 100 0 0 1 0 0 0 1 0 0 0 1 tri.dat'].join('\n')
  const out = await flattenModel(text, load)
  assert.equal(out.triangles.length, 2)
  assert.deepEqual(
    out.triangles.map((t) => t.color),
    [70, 15],
  )
  assert.deepEqual(out.triangles[1].v[0], [100, 0, 0])
  assert.equal(out.lines.length, 4)
  // The model text itself is never fetched from the library.
  assert.deepEqual([...new Set(requested)], ['tri.dat'])
})

test('flattenModel skips parts the library does not have', async () => {
  const { load } = stubLoader({ 'tri.dat': TRIANGLE_PART })
  const out = await flattenModel('1 4 0 0 0 1 0 0 0 1 0 0 0 1 missing.dat\n1 4 0 0 0 1 0 0 0 1 0 0 0 1 tri.dat', load)
  assert.equal(out.triangles.length, 1)
})

test('modelCacheKey follows the model text and its colours', () => {
  const text = '1 70 0 0 0 1 0 0 0 1 0 0 0 1 3001.dat'
  const colours = [{ code: 70, rgb: [0.4, 0.2, 0.1] }]
  assert.equal(modelCacheKey(text, colours), modelCacheKey(text, colours))
  assert.notEqual(modelCacheKey(text, colours), modelCacheKey(text + '\n', colours))
  assert.notEqual(modelCacheKey(text, colours), modelCacheKey(text, [{ code: 70, rgb: [1, 1, 1] }]))
  assert.match(modelCacheKey(text, colours), /^model:/)
})
