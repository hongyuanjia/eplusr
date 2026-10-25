// Keep parent filters authoritative and retain each child choice that still has recorded rows.
export function reconcileResultFilters(catalog, selections) {
  const fields = ["name", "environment_name", "reporting_frequency", "key_value", "day_type"];
  const lists = ["variables", "environments", "frequencies", "keys", "day_types"];
  const rows = catalog.combinations;
  const selected = {}, options = {}, removed = [];
  let available = Array.isArray(rows) ? rows : null;
  fields.forEach((field, index) => {
    const values = available ? [...new Set(available.map(row => row[field]).filter(value => value != null && value !== ""))].sort()
      : [catalog[lists[index]] ?? []].flat();
    const wanted = new Set((selections[field] ?? []).map(value => String(value).toLowerCase()));
    selected[field] = values.filter(value => wanted.has(String(value).toLowerCase()));
    options[field] = values;
    const retained = new Set(selected[field].map(value => String(value).toLowerCase()));
    const invalid = (selections[field] ?? []).filter(value => !retained.has(String(value).toLowerCase()));
    if (invalid.length) removed.push({ field, values: invalid });
    // An empty selection means all available values, including after obsolete choices are removed.
    if (available && retained.size) available = available.filter(row => retained.has(String(row[field]).toLowerCase()));
  });
  return { selected, options, removed };
}
