import { icon } from "./icons.js";

// Keep the existing report-data select as the value source and expose searchable native checkboxes.
export class SearchMultiSelect {
  // Own every listener inside this component so removing a widget releases its controls.
  constructor(select, label, id) {
    this.select = select;
    this.label = label;
    this.rows = [];
    this.root = document.createElement("div");
    this.root.className = "eplusr-viewer__multi";
    this.root.setAttribute("role", "group");
    this.root.setAttribute("aria-labelledby", id + "-label");
    const heading = document.createElement("div");
    heading.className = "eplusr-viewer__multi-heading";
    const caption = document.createElement("span");
    caption.id = id + "-label";
    caption.textContent = label;
    this.count = document.createElement("span");
    this.count.className = "eplusr-viewer__multi-count";
    this.clear = document.createElement("button");
    this.clear.type = "button";
    this.clear.className = "eplusr-viewer__multi-clear";
    this.clear.setAttribute("aria-label", "Clear " + label.toLowerCase());
    this.clear.dataset.hint = "Clear all selected " + label.toLowerCase();
    this.clear.append(icon("clear"));
    this.clear.addEventListener("click", () => {
      for (const option of this.select.options) option.selected = false;
      this.search.value = "";
      this.renderOptions();
      this.changed();
      this.search.focus();
    });
    heading.append(caption, this.count, this.clear);

    this.chips = document.createElement("div");
    this.chips.className = "eplusr-viewer__multi-chips";
    this.chips.setAttribute("role", "group");
    this.chips.setAttribute("aria-label", "Selected " + label.toLowerCase());
    const searchBar = document.createElement("div");
    searchBar.className = "eplusr-viewer__multi-search";
    const searchLabel = document.createElement("label");
    searchLabel.className = "eplusr-viewer__sr-only";
    searchLabel.htmlFor = id + "-search";
    searchLabel.textContent = "Search " + label.toLowerCase();
    this.search = document.createElement("input");
    this.search.id = id + "-search";
    this.search.type = "search";
    this.search.name = select.name + "_search";
    this.search.placeholder = "Search " + label.toLowerCase() + "…";
    this.search.autocomplete = "off";
    this.search.spellcheck = false;
    this.search.setAttribute("autocapitalize", "none");
    this.search.setAttribute("autocorrect", "off");
    this.search.setAttribute("aria-controls", id + "-options");
    this.search.addEventListener("focus", () => this.open());
    this.search.addEventListener("click", () => this.open());
    this.search.addEventListener("input", event => {
      event.stopPropagation();
      this.renderOptions();
      this.open();
    });
    // Searching is a local catalog operation, not an unapplied numerical-query change.
    this.search.addEventListener("change", event => event.stopPropagation());
    this.toggle = document.createElement("button");
    this.toggle.type = "button";
    this.toggle.className = "eplusr-viewer__multi-toggle";
    this.toggle.setAttribute("aria-label", "Show options for " + label.toLowerCase());
    this.toggle.setAttribute("aria-controls", id + "-options");
    this.toggle.setAttribute("aria-expanded", "false");
    this.toggle.appendChild(icon("next"));
    this.toggle.addEventListener("click", () => {
      if (this.panel.hidden) { this.search.focus(); this.open(); }
      else this.close();
    });
    searchBar.append(searchLabel, icon("search"), this.search, this.toggle);

    this.panel = document.createElement("div");
    this.panel.id = id + "-options";
    this.panel.className = "eplusr-viewer__multi-panel";
    this.panel.hidden = true;
    this.options = document.createElement("div");
    this.options.className = "eplusr-viewer__multi-options";
    this.options.setAttribute("role", "group");
    this.options.setAttribute("aria-label", label + " options");
    this.summary = document.createElement("p");
    this.summary.className = "eplusr-viewer__multi-summary";
    this.summary.setAttribute("role", "status");
    this.summary.setAttribute("aria-live", "polite");
    this.panel.append(this.options, this.summary);
    // The hidden canonical select keeps existing query parsing and dictionary-pair semantics intact.
    select.hidden = true;
    this.root.append(heading, this.chips, searchBar, this.panel, select);
    // Inline lists stay open on blur so clicking Apply cannot move its target mid-click.
    this.root.addEventListener("keydown", event => this.keydown(event));
  }

  // Synchronize a new R catalog or a reset without reporting a spurious user edit.
  configure() {
    this.search.value = "";
    this.close();
    this.renderOptions();
    this.renderSelection();
  }

  // Update dependent options without clearing search text, closing a list or losing checkbox focus.
  setOptions(values, selected, reset = false) {
    const focused = this.rows.find(row => row.checkbox === document.activeElement)?.option.value;
    const wanted = new Set(selected.map(value => String(value).toLowerCase()));
    const changed = JSON.stringify([...this.select.options].map(option => option.value)) !== JSON.stringify(values);
    if (changed) {
      this.select.replaceChildren(...values.map(value => {
        const option = document.createElement("option");
        option.value = option.textContent = option.title = value;
        option.selected = wanted.has(String(value).toLowerCase());
        return option;
      }));
      this.renderOptions();
    } else {
      for (const option of this.select.options) option.selected = wanted.has(option.value.toLowerCase());
    }
    if (reset) this.configure();
    else this.renderSelection();
    if (changed && focused != null) {
      const next = this.rows.find(row => row.option.value === focused)?.checkbox;
      (next ?? this.search).focus();
    }
  }

  // Filter source names literally and case-insensitively; search never removes selected values.
  renderOptions() {
    const terms = this.search.value.trim().toLocaleLowerCase().split(/\s+/).filter(Boolean);
    const matches = [...this.select.options].filter(option => terms.every(term => option.text.toLocaleLowerCase().includes(term)));
    this.options.replaceChildren();
    this.rows = matches.map(option => {
      const label = document.createElement("label");
      label.className = "eplusr-viewer__multi-option";
      label.title = option.text;
      const checkbox = document.createElement("input");
      checkbox.type = "checkbox";
      checkbox.name = this.select.name + "_choice";
      checkbox.checked = option.selected;
      checkbox.value = option.value;
      const text = document.createElement("span");
      text.textContent = option.text;
      checkbox.addEventListener("change", event => {
        event.stopPropagation();
        option.selected = checkbox.checked;
        this.changed();
      });
      label.append(checkbox, text);
      this.options.appendChild(label);
      return { option, checkbox };
    });
    if (!matches.length) {
      const empty = document.createElement("p");
      empty.className = "eplusr-viewer__multi-empty";
      empty.textContent = "No matching options.";
      this.options.appendChild(empty);
    }
    this.summary.textContent = `${matches.length} of ${this.select.options.length} options`;
  }

  // Show all chosen names as removable, wrapping tags even when their search matches are hidden.
  renderSelection() {
    const selected = [...this.select.selectedOptions];
    this.count.textContent = `${selected.length} selected`;
    this.count.hidden = selected.length <= 1;
    this.clear.hidden = selected.length === 0;
    this.clear.disabled = selected.length === 0;
    this.chips.replaceChildren();
    if (!selected.length) {
      const all = document.createElement("span");
      all.className = "eplusr-viewer__multi-all";
      all.textContent = "All values · no restriction";
      this.chips.appendChild(all);
    }
    selected.forEach((option, index) => {
      const chip = document.createElement("button");
      chip.type = "button";
      chip.className = "eplusr-viewer__multi-chip";
      chip.title = option.text;
      chip.setAttribute("aria-label", `Remove ${option.text} from ${this.label.toLowerCase()}`);
      const name = document.createElement("span");
      name.textContent = option.text;
      chip.append(name, icon("clear"));
      chip.addEventListener("click", () => {
        option.selected = false;
        this.changed();
        // A removed tag must not leave keyboard focus on a detached element.
        const remaining = this.chips.querySelectorAll("button");
        if (remaining.length) remaining[Math.min(index, remaining.length - 1)].focus();
        else this.search.focus();
      });
      this.chips.appendChild(chip);
    });
    for (const row of this.rows) row.checkbox.checked = row.option.selected;
  }

  // Send only canonical selection changes through the query form's existing draft handling.
  changed() {
    this.renderSelection();
    this.select.dispatchEvent(new Event("change", { bubbles: true }));
  }

  // Expand in the normal panel flow so options cannot overlap or escape a narrow drawer.
  open() {
    if (this.panel.hidden) {
      this.panel.hidden = false;
      this.toggle.setAttribute("aria-expanded", "true");
      this.root.dispatchEvent(new CustomEvent("eplusr-multiselect-open", { bubbles: true }));
    }
    this.ensureVisible();
  }

  // Reserve space for search and feedback, then scroll the bounded choices into the panel.
  ensureVisible() {
    const scroller = this.root.closest(".eplusr-viewer__tab-body");
    if (this.panel.hidden || !scroller?.clientHeight) return;
    this.options.style.maxHeight = Math.max(44, Math.min(192, scroller.clientHeight - 80)) + "px";
    const viewport = scroller.getBoundingClientRect();
    const bottom = this.panel.getBoundingClientRect().bottom;
    if (bottom > viewport.bottom - 6) scroller.scrollTop += bottom - viewport.bottom + 6;
    const top = this.search.getBoundingClientRect().top;
    if (top < viewport.top + 4) scroller.scrollTop -= viewport.top + 4 - top;
  }

  // Keep the selected tags and search text visible when the option disclosure closes.
  close() {
    this.panel.hidden = true;
    this.toggle.setAttribute("aria-expanded", "false");
  }

  // Keep checkbox navigation and Escape local, including when a model measurement is active.
  keydown(event) {
    event.stopPropagation();
    if (event.isComposing) return;
    if (event.key === "Escape") {
      event.preventDefault();
      this.search.focus();
      this.close();
      return;
    }
    if (event.target === this.search && ["Enter", "ArrowDown"].includes(event.key)) {
      event.preventDefault();
      this.open();
      this.rows[0]?.checkbox.focus();
      return;
    }
    const index = this.rows.findIndex(row => row.checkbox === event.target);
    if (index < 0) return;
    const target = { ArrowDown: index + 1, ArrowUp: index - 1, Home: 0, End: this.rows.length - 1 }[event.key];
    if (target === undefined) return;
    event.preventDefault();
    if (target < 0) this.search.focus();
    else this.rows[Math.min(target, this.rows.length - 1)].checkbox.focus();
  }
}
