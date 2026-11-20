/**
 * Edge-aware à-trous denoiser for the accumulated path-traced image.
 *
 * The colour is demodulated by the first-hit albedo before filtering:
 * `illumination = colour / (albedo + DEMOD_EPSILON)`, and multiplied back
 * after the passes. Texture and colour edges then live in the albedo guide,
 * so the filter only has to smooth the illumination, which is far smoother
 * than the raw colour.
 *
 * Five passes of a 5×5 B3-spline kernel, dilated by 1, 2, 4, 8 and 16 pixels
 * per pass. Each tap is weighted by three guides, so filtering stops at brick
 * edges, stud outlines and creases:
 * - colour similarity on the demodulated illumination. The squared RGB
 *   distance is divided by a luminance-relative sigma (the darker of the two
 *   taps' luminance, squared, times SIGMA_RELATIVE, plus a small floor). Noise
 *   scales with brightness, so black and bright areas are filtered alike, and
 *   a dark pixel never borrows light from a bright neighbour.
 * - normal similarity from the first-hit normal buffer (dot^8),
 * - albedo similarity from the first-hit albedo buffer (rational falloff).
 * Weights are rational rather than exponential to keep the inner loop cheap.
 *
 * Operates on unpremultiplied linear colour and on alpha (coverage). Input
 * buffers are not modified.
 */

const KERNEL = Float32Array.from([1 / 16, 4 / 16, 6 / 16, 4 / 16, 1 / 16])
const PASSES = 4
/** Added to the albedo before demodulating, so near-black materials stay finite. */
const DEMOD_EPSILON = 0.02
/** Colour sigma as a fraction of the squared local luminance. */
const SIGMA_RELATIVE = 0.6
/** Absolute floor on the colour sigma, so pure black still filters. */
const SIGMA_FLOOR = 1e-4
const SIGMA_ALBEDO = 0.02
const NORMAL_POWER = 8

/**
 * Computes an interior surface mask in [0, 1] from local alpha coverage.
 *
 * Outer silhouette boundary pixels where the model meets transparent sky/ground
 * have fractional coverage (alpha < 0.999) from multi-sample sub-pixel antialiasing.
 * Preserving the raw multi-sample color and alpha along these outer silhouette
 * boundaries avoids stair-stepped aliasing artifacts from demodulation, while
 * allowing full denoising across all solid brick surfaces, crevices, and internal grooves.
 */
function computeInteriorMask(alpha, width, height) {
  const n = width * height
  const mask = new Float32Array(n)
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const p = y * width + x
      if (alpha[p] < 0.999) {
        mask[p] = 0.0
        continue
      }
      let isInterior = 1.0
      for (let dy = -1; dy <= 1; dy++) {
        const qy = Math.max(0, Math.min(height - 1, y + dy))
        for (let dx = -1; dx <= 1; dx++) {
          const qx = Math.max(0, Math.min(width - 1, x + dx))
          if (alpha[qy * width + qx] < 0.999) {
            isInterior = 0.0
            break
          }
        }
        if (isInterior === 0.0) break
      }
      mask[p] = isInterior
    }
  }
  return mask
}

/**
 * Denoises `color` (3 floats per pixel), `alpha` (1 float per pixel), guided
 * by `albedo` and `normal` (3 floats per pixel each). Returns new buffers
 * `{ color, alpha }`.
 */
export function denoise(color, alpha, albedo, normal, width, height, passes = PASSES) {
  const n = width * height
  let srcC = new Float32Array(n * 3)
  let srcA = Float32Array.from(alpha)
  for (let p = 0; p < n; p++) {
    for (let k = 0; k < 3; k++) {
      srcC[p * 3 + k] = color[p * 3 + k] / (albedo[p * 3 + k] + DEMOD_EPSILON)
    }
  }
  let dstC = new Float32Array(srcC.length)
  let dstA = new Float32Array(srcA.length)
  for (let pass = 0; pass < passes; pass++) {
    atrousPass(srcC, srcA, albedo, normal, dstC, dstA, width, height, 1 << pass)
    ;[srcC, dstC] = [dstC, srcC]
    ;[srcA, dstA] = [dstA, srcA]
  }
  const interior = computeInteriorMask(alpha, width, height)
  for (let p = 0; p < n; p++) {
    const m = interior[p]
    for (let k = 0; k < 3; k++) {
      const denoisedVal = srcC[p * 3 + k] * (albedo[p * 3 + k] + DEMOD_EPSILON)
      srcC[p * 3 + k] = denoisedVal * m + color[p * 3 + k] * (1 - m)
    }
    if (alpha[p] >= 0.999) {
      srcA[p] = 1.0
    } else {
      srcA[p] = srcA[p] * m + alpha[p] * (1 - m)
    }
  }
  return { color: srcC, alpha: srcA }
}

function atrousPass(srcC, srcA, albedo, normal, dstC, dstA, width, height, step) {
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const p = y * width + x
      const pc = p * 3
      const pr = srcC[pc]
      const pg = srcC[pc + 1]
      const pb = srcC[pc + 2]
      const pa = srcA[p]
      const pLum = 0.2126 * pr + 0.7152 * pg + 0.0722 * pb
      const nx = normal[pc]
      const ny = normal[pc + 1]
      const nz = normal[pc + 2]
      const ar = albedo[pc]
      const ag = albedo[pc + 1]
      const ab = albedo[pc + 2]
      let sr = 0
      let sg = 0
      let sb = 0
      let sa = 0
      let wsum = 0
      for (let dy = -2; dy <= 2; dy++) {
        const qy = y + dy * step
        if (qy < 0 || qy >= height) continue
        const hy = KERNEL[dy + 2]
        for (let dx = -2; dx <= 2; dx++) {
          const qx = x + dx * step
          if (qx < 0 || qx >= width) continue
          const q = qy * width + qx
          const qc = q * 3
          const qr = srcC[qc]
          const qg = srcC[qc + 1]
          const qb = srcC[qc + 2]
          const qa = srcA[q]
          const cr = qr - pr
          const cg = qg - pg
          const cb = qb - pb
          const qLum = 0.2126 * qr + 0.7152 * qg + 0.0722 * qb
          // The darker of the two taps sets the scale, so a dark pixel never
          // accepts a bright neighbour (and the reverse).
          const minLum = pLum < qLum ? pLum : qLum
          const sigmaColor = SIGMA_RELATIVE * minLum * minLum + SIGMA_FLOOR
          const colorDiff = (cr * cr + cg * cg + cb * cb) / sigmaColor
          const wColor = 1 / (1 + colorDiff)
          const dn = nx * normal[qc] + ny * normal[qc + 1] + nz * normal[qc + 2]
          let d2 = dn < 0 ? 0 : dn
          for (let k = 1; k < NORMAL_POWER; k *= 2) d2 *= d2
          const wNormal = d2
          const ra = albedo[qc] - ar
          const ga = albedo[qc + 1] - ag
          const ba = albedo[qc + 2] - ab
          const wAlbedo = 1 / (1 + (ra * ra + ga * ga + ba * ba) / SIGMA_ALBEDO)
          const diffA = qa - pa
          const wAlpha = 1 / (1 + (diffA * diffA) / 0.002)
          const w = KERNEL[dx + 2] * hy * wColor * wNormal * wAlbedo * wAlpha
          sr += w * qr
          sg += w * qg
          sb += w * qb
          sa += w * qa
          wsum += w
        }
      }
      if (wsum > 0) {
        dstC[pc] = sr / wsum
        dstC[pc + 1] = sg / wsum
        dstC[pc + 2] = sb / wsum
        dstA[p] = pa >= 0.999 ? 1.0 : sa / wsum
      } else {
        dstC[pc] = pr
        dstC[pc + 1] = pg
        dstC[pc + 2] = pb
        dstA[p] = pa >= 0.999 ? 1.0 : pa
      }
    }
  }
}
