// Language choice of the games: a ?lang= link or a saved choice, Finnish by
// default. Kept free of imports so it can be tested without the viewport code.

export const LANG_KEY = 'palikkakehys.language'

// Finnish is the default for the games; an explicit ?lang= link or a saved
// choice (LANG_KEY) overrides it. Unknown codes fall back to Finnish too.
export const DEFAULT_LANGUAGE = 'fi'

function languageCode(value) {
  return value.toLowerCase().startsWith('en') ? 'en' : 'fi'
}

export function readLanguage() {
  try {
    const hashParams = new URLSearchParams(window.location.hash.replace(/^#/, ''))
    const searchParams = new URLSearchParams(window.location.search)
    const lang = searchParams.get('lang') || hashParams.get('lang')
    if (lang) {
      return languageCode(lang)
    }
    const stored = window.localStorage.getItem(LANG_KEY)
    if (stored) {
      return languageCode(stored)
    }
    return DEFAULT_LANGUAGE
  } catch {
    return DEFAULT_LANGUAGE
  }
}
