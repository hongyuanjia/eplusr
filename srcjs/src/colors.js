import * as THREE from "three";

const SURFACE_COLORS = {
  Wall: "#c9a87d",
  Roof: "#b85f5f",
  Ceiling: "#d8b4a0",
  Floor: "#8a9a83",
  Window: "#55a9d6",
  GlassDoor: "#60b6dc",
  Door: "#8c6239",
  Shading: "#8f9298",
  Daylighting: "#e63946",
  Undefined: "#d5d7da"
};

const BOUNDARY_COLORS = {
  Outdoors: "#4f87c5",
  Ground: "#8b6f47",
  Surface: "#e1a340",
  Adiabatic: "#b36bba",
  OtherSideCoefficients: "#65a96e",
  OtherSideConditionsModel: "#5e9a91"
};

const VIRIDIS = ["#440154", "#3b528b", "#21918c", "#5ec962", "#fde725"];
const DIVERGING = ["#3b4cc0", "#8db0fe", "#f7f7f7", "#f4987a", "#b40426"];
const GRAYSCALE = ["#111827", "#6b7280", "#f9fafb"];
const SEQUENTIAL = ["#fff7bc", "#fec44f", "#d95f0e", "#7f2704"];
const RAINBOW = ["#3b4cc0", "#00a6ca", "#7ad151", "#fde725", "#f8961e", "#d00000"];

// Keep palette lookup centralized so rendered surfaces and legends cannot drift apart.
function paletteStops(palette) {
  if (palette === "diverging") return DIVERGING;
  if (palette === "grayscale") return GRAYSCALE;
  if (palette === "sequential") return SEQUENTIAL;
  if (palette === "rainbow") return RAINBOW;
  return VIRIDIS;
}

// Produce deterministic categorical colors without maintaining a global palette registry.
function categoricalColor(value) {
  const text = String(value ?? "Undefined");
  let hash = 2166136261;
  for (let index = 0; index < text.length; index += 1) {
    hash ^= text.charCodeAt(index);
    hash = Math.imul(hash, 16777619);
  }
  const hue = Math.abs(hash) % 360;
  return new THREE.Color(`hsl(${hue}, 52%, 58%)`);
}

function interpolateStops(stops, amount) {
  const bounded = Math.max(0, Math.min(1, amount));
  const scaled = bounded * (stops.length - 1);
  const lower = Math.floor(scaled);
  const upper = Math.min(stops.length - 1, lower + 1);
  return new THREE.Color(stops[lower]).lerp(new THREE.Color(stops[upper]), scaled - lower);
}

export function resultColor(value, range, palette, missingColor = "#b8bdc5") {
  if (!Number.isFinite(value)) return new THREE.Color(missingColor);
  const [minimum, maximum] = range;
  const amount = maximum === minimum ? 0.5 : (value - minimum) / (maximum - minimum);
  return interpolateStops(paletteStops(palette), amount);
}

export function objectColor(object, renderBy, resultValue, resultRange, palette, missingColor) {
  if (renderBy === "result") return resultColor(resultValue, resultRange, palette, missingColor);
  if (renderBy === "surface_type") {
    return new THREE.Color(SURFACE_COLORS[object.surface_type] ?? SURFACE_COLORS.Undefined);
  }
  if (renderBy === "boundary") {
    return new THREE.Color(BOUNDARY_COLORS[object.outside_boundary_condition] ?? "#d5d7da");
  }
  if (renderBy === "construction") return categoricalColor(object.construction_name);
  if (renderBy === "zone") return categoricalColor(object.zone_name);
  if (renderBy === "space") return categoricalColor(object.space_name);
  if (renderBy === "normal") return new THREE.Color("#f4f5f7");
  return new THREE.Color("#d5d7da");
}

export function resultGradient(palette) {
  return paletteStops(palette).join(", ");
}
