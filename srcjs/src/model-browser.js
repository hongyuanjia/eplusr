import { icon, buttonIcon } from "./icons.js";

// Create model labels as text, never HTML, even when source names contain markup.
function node(tag, className = "", text = "") {
  const item = document.createElement(tag);
  item.className = className; item.textContent = text;
  return item;
}

// Browse the actual Zone/Space/surface hierarchy without imposing invented storeys.
export class ModelBrowser {
  // Keep tree search separate from geometric filtering and expose native keyboard navigation.
  constructor(viewer, workspace) {
    this.viewer = viewer; this.workspace = workspace; this.nodes = []; this.expanded = new Set();
    this.root = node("aside", "eplusr-viewer__model-browser");
    this.root.id = viewer.panel.id + "-model-browser";
    this.root.setAttribute("aria-label", "Model browser");
    const header = node("div", "eplusr-viewer__pane-header");
    const heading = node("h2", "", "Model browser");
    const close = workspace.button("Close model browser", "clear", () => workspace.showBrowser(false), true);
    header.append(heading, close);
    const searchField = node("div", "eplusr-viewer__tree-search");
    this.search = node("input"); this.search.type = "search";
    this.search.placeholder = "Find in model tree";
    this.search.setAttribute("aria-label", "Find in model tree");
    const clear = workspace.button("Clear tree search", "clear", () => { this.search.value = ""; this.filter(); this.search.focus(); }, true);
    searchField.append(this.search, clear);
    this.status = node("p", "eplusr-viewer__tree-status");
    this.status.setAttribute("role", "status");
    this.tree = node("ul", "eplusr-viewer__tree");
    this.tree.setAttribute("role", "tree");
    this.tree.setAttribute("aria-label", "Zones and surfaces");
    this.tree.setAttribute("aria-multiselectable", "true");
    this.root.append(header, searchField, this.status, this.tree);
    this.search.addEventListener("input", () => this.filter());
    this.search.addEventListener("keydown", event => {
      if (event.key !== "Enter") return;
      event.preventDefault(); event.stopPropagation();
      const leaves = this.visibleNodes().filter(item => item.key.startsWith("object:"));
      const target = leaves.find(item => item.indices.some(index => this.viewer.isObjectVisible(index))) ?? leaves[0];
      if (target) this.focus(target);
    });
    this.root.addEventListener("keydown", event => {
      if (event.key === "Escape" && !event.defaultPrevented) {
        event.preventDefault(); workspace.showBrowser(false);
      }
      event.stopPropagation();
    });
  }

  // Rebuild hierarchy only when the source model changes, retaining stable expanded branches.
  build() {
    this.nodes = []; this.tree.replaceChildren();
    const groups = new Map();
    this.viewer.objects.forEach((object, index) => {
      const zone = object.zone_name || "Unassigned objects";
      if (!groups.has(zone)) groups.set(zone, new Map());
      const space = object.space_name || "";
      if (!groups.get(zone).has(space)) groups.get(zone).set(space, []);
      groups.get(zone).get(space).push(index);
    });
    for (const [zone, spaces] of groups) {
      const indices = [...spaces.values()].flat();
      const branch = this.add(zone, "zone:" + zone, indices, null, this.tree, "Zone");
      for (const [space, members] of spaces) {
        const parent = space ? this.add(space, "space:" + zone + ":" + space, members, branch, branch.children, "Space") : branch;
        for (const index of members) {
          const object = this.viewer.objects[index];
          const short = object.name?.startsWith(zone + "_") ? object.name.slice(zone.length + 1) : object.name;
          this.add(short || "Object " + (index + 1), "object:" + (object.id ?? index), [index], parent, parent.children, object.surface_type || object.class_name || "Surface");
        }
      }
    }
    this.filter(); this.sync();
  }

  // Make a roving tree item with distinct expansion, selection and visibility actions.
  add(label, key, indices, parent, container, kind) {
    const item = { key, indices, parent, label, kind, level: (parent?.level ?? 0) + 1 };
    item.li = node("li"); item.li.setAttribute("role", "none");
    item.row = node("div", "eplusr-viewer__tree-row");
    item.row.setAttribute("role", "treeitem"); item.row.tabIndex = -1;
    item.row.setAttribute("aria-level", String(item.level));
    item.sourceName = indices.length === 1 ? this.viewer.objects[indices[0]].name : label;
    item.row.title = item.sourceName;
    item.row.setAttribute("aria-label", kind + ": " + item.row.title);
    item.twist = this.workspace.button("Expand " + label, "next", () => this.expand(item, !this.expanded.has(key)), true);
    item.twist.tabIndex = -1;
    item.twist.addEventListener("click", event => event.stopPropagation());
    const name = node("span", "eplusr-viewer__tree-name", label.replaceAll("_", " "));
    name.prepend(icon(key.startsWith("object:") ? "object" : "layers"));
    item.count = node("span", "eplusr-viewer__tree-count");
    item.eye = this.workspace.button("Hide " + label, "eye", () => this.visibility(item), true);
    item.eye.tabIndex = -1; item.eye.addEventListener("click", event => event.stopPropagation());
    const labelStack = node("span", "eplusr-viewer__tree-label");
    item.reason = node("span", "eplusr-viewer__tree-reason");
    item.reason.id = this.viewer.panel.id + "-tree-reason-" + this.nodes.length;
    item.reason.hidden = true;
    labelStack.append(name, item.reason);
    item.row.append(item.twist, labelStack, item.count, item.eye);
    item.children = node("ul"); item.children.setAttribute("role", "group");
    item.li.append(item.row, item.children); container.append(item.li);
    item.row.addEventListener("click", event => this.select(item, event));
    item.row.addEventListener("focus", () => this.focus(item, false));
    item.row.addEventListener("keydown", event => this.keydown(item, event));
    this.nodes.push(item); return item;
  }

  // Search the tree's source names without changing model visibility, camera or filters.
  filter() {
    const query = this.search.value.trim().toLowerCase();
    this.tree.dataset.searching = String(Boolean(query));
    for (const item of [...this.nodes].reverse()) {
      const object = item.key.startsWith("object:") ? this.viewer.objects[item.indices[0]] : null;
      const own = [item.label, item.kind, item.sourceName, object?.construction_name, object?.class].join(" ").toLowerCase().includes(query);
      item.match = !query || own || this.ancestors(item).some(parent => parent.label.toLowerCase().includes(query))
        || [...item.children.children].some(child => !child.hidden);
      item.li.hidden = !item.match;
      const branch = item.children.children.length > 0;
      item.twist.hidden = !branch;
      item.children.hidden = !branch || !(query ? item.match : this.expanded.has(item.key));
      if (branch) item.row.setAttribute("aria-expanded", String(!item.children.hidden));
    }
    this.updateSearchSummary();
    const rows = this.visibleNodes();
    this.nodes.forEach(item => { item.row.tabIndex = item === rows[0] ? 0 : -1; });
  }

  // Search counts include hidden source objects while exposing the currently drawable subset.
  updateSearchSummary() {
    const query = this.search.value.trim();
    const matches = this.nodes.filter(item => item.key.startsWith("object:") && item.match);
    const visible = matches.filter(item => this.viewer.isObjectVisible(item.indices[0])).length;
    this.status.textContent = query ? matches.length + " matching surfaces · " + visible + " visible"
      : this.viewer.objects.length + " surfaces · " + this.tree.children.length + " groups";
  }

  // Expand an existing branch while keeping the selected object independent of disclosure.
  expand(item, expanded) {
    if (!item.children.children.length) return;
    if (expanded) this.expanded.add(item.key); else this.expanded.delete(item.key);
    item.children.hidden = !expanded;
    item.row.setAttribute("aria-expanded", String(expanded));
    item.twist.setAttribute("aria-label", (expanded ? "Collapse " : "Expand ") + item.label);
    this.focus(item);
  }

  // Follow the same additive-selection convention as the Three.js viewport.
  select(item, event) {
    let members = item.indices.filter(index => this.viewer.isObjectVisible(index));
    if (event.shiftKey && this.anchor && item.key.startsWith("object:") && this.anchor.key.startsWith("object:")) {
      const leaves = this.visibleNodes().filter(entry => entry.key.startsWith("object:"));
      const start = leaves.indexOf(this.anchor), end = leaves.indexOf(item);
      if (start >= 0 && end >= 0) members = leaves.slice(Math.min(start,end), Math.max(start,end) + 1).flatMap(entry => entry.indices).filter(index => this.viewer.isObjectVisible(index));
    }
    if (!members.length) {
      const reasons = [...new Set(item.indices.flatMap(index => this.viewer.visibilityReasons(index)))];
      this.status.textContent = "Hidden · " + reasons.join(" · ") + ". Adjust the matching visibility filter to locate.";
      this.focus(item); return;
    }
    const additive = event.ctrlKey || event.metaKey || event.shiftKey;
    if (!additive) this.viewer.selected.clear();
    const remove = additive && !event.shiftKey && members.every(index => this.viewer.selected.has(index));
    members.forEach(index => remove ? this.viewer.selected.delete(index) : this.viewer.selected.add(index));
    if (!event.shiftKey) this.anchor = item;
    this.viewer.updateColors();
    if (!additive) this.viewer.fitView(new Set(members));
    this.viewer.emitSelection(); this.focus(item);
  }

  // Visibility controls cannot silently clear a Zone/component filter in order to reveal an object.
  visibility(item) {
    const visible = item.indices.some(index => this.viewer.isObjectVisible(index));
    item.indices.forEach(index => visible ? this.viewer.hidden.add(index) : this.viewer.hidden.delete(index));
    this.viewer.applyVisibilityChange();
    if (!visible && !item.indices.some(index => this.viewer.isObjectVisible(index))) {
      this.status.textContent = "Still hidden by a filter or isolation. Clear filters to reveal.";
    }
  }

  // Enumerate the rendered rows, including only descendants of expanded branches.
  visibleNodes() {
    return this.nodes.filter(item => !item.li.hidden && !this.ancestors(item).some(parent => parent.li.hidden || parent.children.hidden));
  }

  // Walk parent links without depending on source-model depth.
  ancestors(item) {
    const parents = [];
    for (let current = item.parent; current; current = current.parent) parents.push(current);
    return parents;
  }

  // Maintain one keyboard tab stop while keeping tree scrolling local to its viewport.
  focus(item, move = true) {
    this.nodes.forEach(entry => { entry.row.tabIndex = entry === item ? 0 : -1; });
    if (move) { item.row.focus(); item.row.scrollIntoView({ block: "nearest", inline: "nearest" }); }
  }

  // Implement the standard tree traversal keys and explicit selection/visibility shortcuts.
  keydown(item, event) {
    const rows = this.visibleNodes(), index = rows.indexOf(item);
    let target;
    if (event.key === "ArrowDown") target = rows[Math.min(rows.length - 1,index + 1)];
    else if (event.key === "ArrowUp") target = rows[Math.max(0,index - 1)];
    else if (event.key === "Home") target = rows[0];
    else if (event.key === "End") target = rows.at(-1);
    else if (event.key === "ArrowRight") {
      if (item.children.hidden) this.expand(item,true);
      else target = rows[index + 1]?.parent === item ? rows[index + 1] : item;
    } else if (event.key === "ArrowLeft") {
      if (item.children.children.length && !item.children.hidden) this.expand(item,false);
      else target = item.parent;
    } else if (event.key === "Enter" || event.key === " ") this.select(item,event);
    else if (event.key.toLowerCase() === "h") this.visibility(item);
    else return;
    event.preventDefault(); event.stopPropagation();
    if (target) this.focus(target);
  }

  // Mirror R commands, canvas picking and filters without recreating focused tree rows.
  sync() {
    for (const item of this.nodes) {
      const visible = item.indices.filter(index => this.viewer.isObjectVisible(index)).length;
      const selected = item.indices.filter(index => this.viewer.selected.has(index)).length;
      item.row.setAttribute("aria-selected", String(selected > 0 && selected === visible));
      item.row.dataset.partial = String(selected > 0 && selected < visible);
      item.row.dataset.hidden = String(!visible);
      item.count.textContent = item.indices.length > 1 ? visible + "/" + item.indices.length : "";
      item.reason.hidden = visible > 0;
      if (!visible) {
        const reasons = [...new Set(item.indices.flatMap(index => this.viewer.visibilityReasons(index)))];
        item.reason.textContent = "Hidden · " + reasons.join(" · ");
        item.row.setAttribute("aria-describedby", item.reason.id);
        item.row.title = item.sourceName + " · " + item.reason.textContent;
      } else {
        item.row.removeAttribute("aria-describedby");
        item.row.title = item.sourceName;
      }
      buttonIcon(item.eye, visible ? "eye" : "hide", (visible ? "Hide " : "Show ") + item.label, true);
      item.eye.setAttribute("aria-pressed", String(Boolean(visible)));
    }
    this.updateSearchSummary();
  }
}
