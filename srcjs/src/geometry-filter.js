// Classify EnergyPlus objects consistently for rendering, filter controls and hidden-state explanations.
export function componentFor(object) {
  if (object.category === "shading") return "shading";
  if (object.category === "daylighting") return "daylighting";
  const type = String(object.surface_type ?? "").toLowerCase();
  if (type === "floor") return "floor";
  if (type === "wall") return "wall";
  if (type === "roof" || type === "ceiling") return "roof";
  if (type === "window" || type === "glassdoor") return "window";
  if (type === "door") return "door";
  return "wall";
}

// Name filtering uses literal, case-insensitive substrings; it never searches unrelated metadata.
export function matchesNameFilter(object, queries) {
  if (queries == null) return true;
  const values = Array.isArray(queries) ? queries : [queries];
  const name = String(object?.name ?? "").toLowerCase();
  return values.some(value => name.includes(String(value).trim().toLowerCase()));
}

// Explain the same intersection of criteria that controls drawing and picking.
export function objectVisibilityReasons(object, index, state) {
  if (!object) return ["Missing object"];
  const filters = state.filters ?? {}, reasons = [];
  const fields = { zone: "zone_name", space: "space_name", surface: "name" };
  for (const [key, field] of Object.entries(fields)) {
    if (filters[key] && !filters[key].includes(String(object[field] ?? "").toLowerCase())) {
      reasons.push(key[0].toUpperCase() + key.slice(1) + " filter");
    }
  }
  if (!matchesNameFilter(object, filters.name)) reasons.push("Name filter");
  const component = componentFor(object);
  if ((filters.type && !filters.type.includes(component)) || state.componentVisibility?.[component] === false) {
    reasons.push("Type filter (" + (component === "roof" ? "roof / ceiling" : component) + ")");
  }
  if (state.resultValueAllowed === false) reasons.push("Result value filter");
  if (state.hidden?.has(index)) reasons.push("Hidden manually");
  if (state.isolated && !state.isolated.has(index)) reasons.push("Isolation");
  if (state.diagnosticOnly && !state.diagnosticObjectSet?.has(index)) reasons.push("Diagnostics filter");
  return reasons;
}
