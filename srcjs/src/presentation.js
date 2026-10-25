// Normalize common EnergyPlus unit spellings for display while preserving the original R metadata.
export function displayUnit(unit) {
  const value = String(unit ?? "");
  if (["C", "degC", "°C"].includes(value)) return "°C";
  if (["F", "degF", "°F"].includes(value)) return "°F";
  return value.replace(/m\^?2\b/g, "m²").replace(/m\^?3\b/g, "m³");
}

// Static tables do not imply a clock time; real time labels retain their source precision.
export function timeCaption(result, frame) {
  const label = String(result?.times?.[frame] ?? "");
  return label === "Value" ? "Static values" : `Time: ${label}`;
}
