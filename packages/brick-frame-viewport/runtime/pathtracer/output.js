/**
 * Display mapping for the finished render: exposure, ACES filmic tone curve
 * (Narkowicz fit), then linear → sRGB, packed into RGBA8 with alpha kept as
 * coverage. The output is ready for ImageData.
 */

/** Scene-linear exposure multiplier applied before the tone curve. */
export const DEFAULT_EXPOSURE = 0.7

/** Share of luminance-preserving ACES vs per-channel ACES (prevents hue/saturation bleaching on LEGO colours). */
const LUM_PRESERVE = 0.75

/** ACES filmic approximation, input linear, output display-linear in 0..1. */
export function acesFilmic(x) {
  const v = x < 0 ? 0 : x
  const y = (v * (2.51 * v + 0.03)) / (v * (2.43 * v + 0.59) + 0.14)
  return y < 0 ? 0 : y > 1 ? 1 : y
}

/** Display-linear (0..1) to sRGB-encoded (0..1). */
export function linearToSrgb(x) {
  return x <= 0.0031308 ? 12.92 * x : 1.055 * x ** (1 / 2.4) - 0.055
}

/**
 * Tone-maps `color` (3 floats per pixel, linear) and `alpha` (1 per pixel)
 * into an RGBA8 `Uint8ClampedArray` of `width * height * 4` bytes.
 */
export function toRgba8(color, alpha, width, height, exposure = DEFAULT_EXPOSURE) {
  const out = new Uint8ClampedArray(width * height * 4)
  for (let p = 0; p < width * height; p++) {
    const c = p * 3
    const o = p * 4
    const r = Math.max(0, color[c] * exposure)
    const g = Math.max(0, color[c + 1] * exposure)
    const b = Math.max(0, color[c + 2] * exposure)
    const lum = 0.2126 * r + 0.7152 * g + 0.0722 * b
    const lumScale = lum > 1e-9 ? acesFilmic(lum) / lum : 0
    const rLum = Math.min(1, r * lumScale)
    const gLum = Math.min(1, g * lumScale)
    const bLum = Math.min(1, b * lumScale)
    const rMap = LUM_PRESERVE * rLum + (1 - LUM_PRESERVE) * acesFilmic(r)
    const gMap = LUM_PRESERVE * gLum + (1 - LUM_PRESERVE) * acesFilmic(g)
    const bMap = LUM_PRESERVE * bLum + (1 - LUM_PRESERVE) * acesFilmic(b)
    out[o] = Math.round(255 * linearToSrgb(rMap))
    out[o + 1] = Math.round(255 * linearToSrgb(gMap))
    out[o + 2] = Math.round(255 * linearToSrgb(bMap))
    const a = alpha[p]
    out[o + 3] = Math.round(255 * (a < 0 ? 0 : a > 1 ? 1 : a))
  }
  return out
}
