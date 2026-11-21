#!/usr/bin/env node
// Writes the Elm module tests/LDraw/CuratedParts.elm of brick-frame-simulator:
// the visible (not hidden) part files of brick-catalog's curated list, which is
// what Brick Play's search can reach. CuratedCoverageTest checks that each has a
// Connectivity.knownCatalogPart entry.
//
//   node scripts/generate-curated-parts.mjs <source> > CuratedParts.elm
//
// <source> is brick-catalog's data/curated-parts.json (entries with "hidden"
// are skipped), or the live index
// https://catalog.palikkaharrastajat.fi/indices/parts-curated.json (or a saved
// copy), which lists only visible parts. Run it through `make curated-parts`,
// which also elm-formats the output.

import { execFileSync } from 'node:child_process'
import fs from 'node:fs'
import path from 'node:path'

async function readSource(source) {
  if (/^https?:\/\//.test(source)) {
    const response = await fetch(source)
    if (!response.ok) throw new Error(`${source}: HTTP ${response.status}`)
    return { data: await response.json(), label: source }
  }
  const data = JSON.parse(fs.readFileSync(source, 'utf8'))
  return { data, label: describeFile(source) }
}

// "brick-catalog data/curated-parts.json at <commit> (<date>)", noting local changes.
function describeFile(file) {
  const dir = path.dirname(path.resolve(file))
  try {
    const git = (...args) => execFileSync('git', ['-C', dir, ...args], { encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore'] }).trim()
    const commit = git('log', '-1', '--format=%h (%cs)', '--', path.resolve(file))
    let dirty = false
    try {
      git('diff', '--quiet', 'HEAD', '--', path.resolve(file))
    } catch {
      dirty = true
    }
    return `${path.basename(file)} at brick-catalog ${commit}${dirty ? ' with local changes' : ''}`
  } catch {
    return path.basename(file)
  }
}

export function visibleFiles(data) {
  if (Array.isArray(data)) return data.filter((e) => !e.hidden).map((e) => e.file)
  if (data && Array.isArray(data.parts)) return data.parts.map((p) => (Array.isArray(p) ? p[0] : p.file))
  throw new Error('unknown curated list format')
}

export function elmModule(files, label) {
  const items = files.map((f) => JSON.stringify(f))
  return `module LDraw.CuratedParts exposing (visibleFiles)

{-| The visible (not hidden) part files of brick-catalog's curated list: what
Brick Play's search can reach. CuratedCoverageTest checks each one against
Connectivity.knownCatalogPart.

Generated from ${label}.
Do not edit by hand: run \`make curated-parts\`.

-}


{-| ${files.length} visible curated part files, in catalog order.
-}
visibleFiles : List String
visibleFiles =
    [ ${items.join('\n    , ')}
    ]
`
}

const source = process.argv[2]
if (!source) {
  console.error('usage: generate-curated-parts.mjs <curated-parts.json | parts-curated.json URL>')
  process.exit(2)
}
const { data, label } = await readSource(source)
process.stdout.write(elmModule(visibleFiles(data), label))
