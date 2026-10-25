import * as THREE from "three";

export const GLASS_COLOR = "#8db9cd";
export const GLASS_OPACITY = 0.28;
const EDGE_COLORS = ["#242f3a", "#748292", "#e6edf5"].map(value => new THREE.Color(value));

// Identify actual glazed model objects without inferring optical properties from their result values.
export function isGlazing(object) {
  return ["window", "glassdoor"].includes(String(object.surface_type ?? "").toLowerCase());
}

// Use linear-light luminance to compare candidate strokes against the displayed fill.
function luminance(color) {
  return color.r * 0.2126 + color.g * 0.7152 + color.b * 0.0722;
}

// Approximate the visible tint against the current background when glass or X-ray is enabled.
function composite(fill, background, opacity) {
  if (opacity >= 1) return fill;
  return fill.clone().convertLinearToSRGB().multiplyScalar(opacity)
    .add(background.clone().convertLinearToSRGB().multiplyScalar(1 - opacity)).convertSRGBToLinear();
}

// Choose the neutral stroke with the strongest minimum contrast to a face and its adjacent wall.
export function boundaryColor(fill, background, opacity = 1, adjacent = null) {
  const levels = [luminance(composite(fill, background, opacity))];
  if (adjacent) levels.push(luminance(adjacent));
  let best = EDGE_COLORS[0], bestScore = -1;
  for (const candidate of EDGE_COLORS) {
    const level = luminance(candidate);
    const score = Math.min(...levels.map(value => (Math.max(value, level) + 0.05) / (Math.min(value, level) + 0.05)));
    if (score > bestScore) { best = candidate; bestScore = score; }
  }
  return best;
}
