import { icon } from "./icons.js";
import { SearchMultiSelect } from "./search-multiselect.js";
import { reconcileResultFilters } from "./result-catalog.js";

// Normalize scalar and vector JSON metadata without changing source names or units.
function array(value) {
  return value == null ? [] : Array.isArray(value) ? value : [value];
}

// Expand literal R-style integer lists only; result queries never evaluate text as code.
function integers(text, field, minimum, maximum) {
  if (!text.trim()) return null;
  const values = [];
  for (const item of text.trim().split(/[,\s]+/)) {
    const match = item.match(/^(-?\d+)(?::(-?\d+))?$/);
    if (!match) throw new Error(`${field}: use integers or ranges such as 8:18.`);
    const first = Number(match[1]), last = Number(match[2] ?? match[1]);
    if (Math.min(first,last) < minimum || Math.max(first,last) > maximum || Math.abs(last-first) > 10000) {
      throw new Error(`${field}: values must be between ${minimum} and ${maximum}.`);
    }
    for (let n = first; first <= last ? n <= last : n >= last; n += first <= last ? 1 : -1) values.push(n);
  }
  return [...new Set(values)];
}

// Own the SQL query form while the viewer retains geometry, camera and object selection.
export class ResultQueryPanel {
  // Use native form controls and submit semantics for keyboard and assistive technology users.
  constructor(viewer) {
    this.viewer = viewer;
    this.section = document.createElement("details");
    this.section.className = "eplusr-viewer__result-query";
    this.section.open = true;
    const heading = document.createElement("summary");
    heading.textContent = "Result data";
    this.form = document.createElement("form");
    this.form.setAttribute("aria-label", "EnergyPlus result query");
    this.form.noValidate = true;
    this.fields = {};
    this.multiFields = {};
    this.fieldset = document.createElement("fieldset");
    const legend = document.createElement("legend");
    legend.textContent = "Select simulation results";
    legend.className = "eplusr-viewer__sr-only";
    this.fieldset.appendChild(legend);
    this.field("name", "Variables", "select", this.fieldset);
    this.field("environment_name", "Environments", "select", this.fieldset);
    this.field("reporting_frequency", "Reporting frequency", "select", this.fieldset);
    const note = document.createElement("p");
    note.className = "eplusr-viewer__status";
    note.textContent = "Empty selection includes all values.";
    const advanced = document.createElement("details");
    const advancedHeading = document.createElement("summary");
    advancedHeading.textContent = "Keys and time filters";
    advanced.appendChild(advancedHeading);
    this.field("key_value", "Result keys", "select", advanced);
    const grid = document.createElement("div");
    grid.className = "eplusr-viewer__query-grid";
    const numeric = [
      ["month", "Months", 1, 12], ["day", "Days", 1, 31],
      ["hour", "Hours", 0, 24], ["minute", "Minutes", 0, 60],
      ["interval", "Interval (min)", 1, 527040], ["simulation_days", "Simulation days", 1, 366]
    ];
    this.numericFields = numeric;
    numeric.forEach(([name,label]) => this.field(name, label, "input", grid).placeholder = name === "hour" ? "e.g. 8:18" : "All");
    this.field("year", "Year", "input", grid).placeholder = "Source calendar";
    this.field("tz", "Time zone", "input", grid).placeholder = "UTC";
    advanced.appendChild(grid);
    this.field("day_type", "Day types", "select", advanced);
    this.field("period", "Dates / date-times (period)", "input", advanced).placeholder = "2017-07-21, 2017-07-22";
    this.fields.period.title = "Comma-separated dates or date-times, matching report_data(period = ...). Each entry selects that date/time; two entries are not a continuous range.";
    const actions = document.createElement("div");
    actions.className = "eplusr-viewer__button-row";
    this.apply = document.createElement("button");
    this.apply.type = "submit";
    this.apply.textContent = "Apply";
    this.apply.setAttribute("aria-label", "Apply result query");
    this.reset = document.createElement("button");
    this.reset.type = "button";
    this.reset.textContent = "Reset";
    this.reset.setAttribute("aria-label", "Reset query");
    this.reset.addEventListener("click", () => {
      this.paired = false;
      this.keysChanged = true;
      Object.values(this.fields).forEach(control => {
        if (control.tagName === "SELECT") [...control.options].forEach(item => { item.selected = false; });
        else control.value = "";
      });
      this.reconcile(true);
      this.submit();
    });
    actions.append(this.apply,this.reset);
    this.status = document.createElement("p");
    this.status.className = "eplusr-viewer__status";
    this.status.id = `${viewer.container.id}-query-status`;
    this.status.setAttribute("role", "status");
    this.status.setAttribute("aria-live", "polite");
    this.form.setAttribute("aria-describedby", this.status.id);
    this.fieldset.append(note,advanced,actions);
    this.form.append(this.fieldset,this.status);
    this.form.addEventListener("eplusr-multiselect-open", event => {
      Object.values(this.multiFields).forEach(control => { if (control.root !== event.target) control.close(); });
    });
    this.form.addEventListener("submit", event => { event.preventDefault(); this.submit(); });
    this.form.addEventListener("change", event => {
      if ([this.fields.name,this.fields.key_value].includes(event.target)) this.keysChanged = true;
      event.target.removeAttribute("aria-invalid");
      const removed = this.reconcile();
      this.message((removed.length ? "Removed unavailable selections: " + removed.map(item => this.multiFields[item.field].label + " — " + item.values.join(", ")).join("; ") + ". " : "") + "Changes not applied.", "draft");
    });
    this.section.append(heading,this.form);
    viewer.tabBodies.results.prepend(this.section);
    this.configure(null);
  }

  // Build an associated label for every query control and retain source field names.
  field(name, label, tag, parent) {
    if (tag === "select") {
      const select = document.createElement("select");
      select.name = name;
      select.multiple = true;
      const control = new SearchMultiSelect(select, label, this.viewer.container.id + "-query-" + name);
      this.fields[name] = select;
      this.multiFields[name] = control;
      parent.appendChild(control.root);
      return select;
    }
    const wrapper = document.createElement("label");
    wrapper.className = "eplusr-viewer__field";
    const caption = document.createElement("span");
    caption.textContent = label;
    const control = document.createElement(tag);
    control.name = name;
    control.setAttribute("aria-label", label);
    wrapper.append(caption,control);
    parent.appendChild(wrapper);
    this.fields[name] = control;
    return control;
  }

  // Rebuild available values from the original source rather than locking to the last R query.
  configure(config) {
    this.config = config;
    this.section.hidden = !config;
    if (!config) { this.applied = null; return; }
    this.paired = Boolean(config.paired_keys);
    this.keysChanged = false;
    this.applied = config.filters ?? {};
    this.reconcile(true, this.applied);
    for (const name of Object.keys(this.fields).filter(name => !this.multiFields[name])) this.fields[name].value = array(this.applied[name]).join(", ");
    this.fieldset.disabled = !config.enabled;
    this.setPending(false);
    this.message(config.enabled ? "Applied result query." : "Connect to R to query other source results.", config.enabled ? "applied" : "offline");
  }

  // Cascade source relationships in parent order, keeping valid choices and announcing removed ones.
  reconcile(reset = false, filters = null) {
    if (!this.config) return [];
    const current = filters ?? Object.fromEntries(Object.entries(this.multiFields).map(([name, control]) => [name, [...control.select.selectedOptions].map(option => option.value)]));
    const result = reconcileResultFilters(this.config.catalog, Object.fromEntries(Object.keys(this.multiFields).map(name => [name, array(current[name])])));
    for (const [name, control] of Object.entries(this.multiFields)) control.setOptions(result.options[name], result.selected[name], reset);
    if (!reset && result.removed.some(item => item.field === "key_value")) this.keysChanged = true;
    this.resize();
    return result.removed;
  }

  // Refit an open selection list when the host or inspector viewport changes size.
  resize() {
    Object.values(this.multiFields).forEach(control => control.ensureVisible());
  }

  // Collect report_data selectors while preserving an R dictionary-pair filter unless edited.
  read() {
    const filters = {};
    for (const [name,control] of Object.entries(this.fields)) {
      if (control.tagName === "SELECT") {
        const values = [...control.selectedOptions].map(option => option.value);
        filters[name] = values.length ? values : null;
      }
    }
    for (const [name,label,min,max] of this.numericFields) {
      try { filters[name] = integers(this.fields[name].value,label,min,max); }
      catch (error) { this.fields[name].setAttribute("aria-invalid", "true"); this.fields[name].focus(); throw error; }
    }
    try {
      const year = integers(this.fields.year.value,"Year",1,9999);
      if (year?.length > 1) throw new Error("Year: choose a single year.");
      filters.year = year?.[0] ?? null;
    } catch (error) { this.fields.year.setAttribute("aria-invalid", "true"); this.fields.year.focus(); throw error; }
    filters.tz = this.fields.tz.value.trim() || null;
    const period = this.fields.period.value.trim();
    filters.period = period ? period.split(/\s*,\s*/).map(value => value.replace("T"," ")) : null;
    if (this.paired && !this.keysChanged) delete filters.key_value;
    return filters;
  }

  // Preserve the current numerical display until R accepts and prepares a replacement query.
  submit() {
    if (!this.config?.enabled || this.pending) return;
    try {
      const filters = this.read();
      this.setPending(true);
      this.message("Querying results…", "pending");
      this.viewer.root.dispatchEvent(new CustomEvent("eplusr-result-query", { detail: filters }));
    } catch (error) { this.fail(error.message); }
  }

  // Disable repeated submissions only while a valid request is outstanding.
  setPending(pending) {
    this.pending = pending;
    this.form.setAttribute("aria-busy", String(pending));
    this.apply.disabled = pending;
    this.reset.disabled = pending;
  }

  // Keep routine success compact while announcing full draft, loading and failure messages.
  message(text, state) {
    this.status.dataset.state = state;
    const label = document.createElement("span");
    label.textContent = text;
    label.className = state === "applied" ? "eplusr-viewer__sr-only" : "";
    this.status.replaceChildren(...(state === "applied" ? [icon("check"), label] : [label]));
    if (state === "applied") this.status.dataset.hint = text;
    else delete this.status.dataset.hint;
  }

  // Announce query failure while keeping the entered criteria available for correction.
  fail(message) {
    this.setPending(false);
    this.message(message, "error");
  }
}
