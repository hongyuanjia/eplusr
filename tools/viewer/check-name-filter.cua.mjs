// Exercise name filtering and tree location on the real htmlwidget through documented CUA actions.
export async function checkNameFilter(tab, viewport, fs, output) {
  const checks=[];
  // Assert rendered behavior, keeping all interaction on native controls and accessible locators.
  function check(name,pass,detail=null) { checks.push({name,pass,detail}); if(!pass)throw Error(name+': '+JSON.stringify(detail)); }
  // Use stable accessible command names instead of widget-generated IDs.
  async function click(name) { await tab.playwright.getByRole('button',{name,exact:typeof name==='string'}).click(); }
  // Read only DOM state that users can observe, without accessing the widget implementation.
  async function state() { return tab.playwright.evaluate(()=>({
    visible:document.querySelector('.eplusr-viewer__scope').innerText,
    selected:document.querySelector('.eplusr-viewer__selection-count').textContent,
    chips:document.querySelector('.eplusr-viewer__filter-chips').innerText,
    name:document.querySelector('[aria-label="Name contains"]').value,
    status:document.querySelector('[id$="-name-filter-status"]').textContent,
    tree:document.querySelector('.eplusr-viewer__tree-status').textContent,
    task:document.querySelector('.eplusr-viewer__task-pane h2').textContent,
    pane:document.querySelector('.eplusr-viewer__task-pane').getBoundingClientRect().toJSON(),
    root:document.querySelector('.eplusr-viewer').getBoundingClientRect().toJSON(),
    overflow:document.documentElement.scrollWidth>innerWidth
  })); }
  // Save visual evidence at both normal and narrow container sizes.
  async function shot(name) { await fs.writeFile(output+'/'+name+'.jpg',await tab.screenshot({fullPage:false})); }
  await fs.mkdir(output,{recursive:true});
  await viewport.set({width:1440,height:900});
  await tab.playwright.getByRole("tab",{name:"Model",exact:true}).click();
  await click("Show all");
  if (!(await tab.playwright.getByRole("searchbox",{name:"Find in model tree",exact:true}).isVisible())) await click("Model tree");
  await click("Components");
  await tab.playwright.getByRole("checkbox",{name:"Roofs / ceilings",exact:true}).uncheck();
  await click("Filter objects");
  await tab.playwright.getByLabel("Name contains",{exact:true}).fill("Basement_Ceiling_2");
  await tab.playwright.getByRole("searchbox",{name:"Find in model tree",exact:true}).fill("Basement_Ceiling_2");
  let current=await state();
  check('excluded-name-count',current.status.includes('0 of 1')&&current.status.includes('roof / ceiling'),current);
  check('tree-matches-versus-visible',current.tree==='1 matching surfaces · 0 visible',current);
  const hidden=tab.playwright.getByRole('treeitem',{name:'Ceiling: Basement_Ceiling_2',exact:true});
  await hidden.click(); current=await state();
  check('hidden-click-explains',current.tree.includes('Hidden · Type filter (roof / ceiling)'));
  check('hidden-click-keeps-filter',current.visible.includes('0 / 142')&&current.name==='Basement_Ceiling_2'&&current.selected.includes('0 selected'));
  await shot('hidden-match-clicked');
  await click(/^Remove filter: Types:/); current=await state();
  check('remove-conflicting-criterion-reveals',current.visible.includes('1 / 142')&&current.status.includes('1 of 1'));
  await hidden.click();
  check('tree-locates-visible-leaf',(await state()).selected.includes('1 selected'));
  await click('Clear name filter'); current=await state();
  check('clear-name-preserves-selection',current.selected.includes('1 selected')&&current.visible.includes('142 / 142'));
  await tab.playwright.getByLabel('Zone',{exact:true}).selectOption('Basement');
  await tab.playwright.getByLabel('Name contains',{exact:true}).fill('ceiling_2'); current=await state();
  check('live-case-insensitive-intersection',current.visible.includes('1 / 142')&&current.chips.includes('Basement')&&current.chips.includes('ceiling_2'));
  await click(/^Remove filter: Name contains:/); current=await state();
  check('name-chip-clears-only-name',current.visible.includes('10 / 142')&&current.name===''&&current.chips.includes('Basement')&&current.selected.includes('1 selected'));
  await tab.playwright.getByLabel('Name contains',{exact:true}).fill('does-not-exist'); current=await state();
  check('no-name-match-clear-path',current.visible.includes('0 / 142')&&current.status==='No object names match.'&&await tab.playwright.getByRole('button',{name:'Clear name filter',exact:true}).isVisible());
  await tab.playwright.getByLabel('Name contains',{exact:true}).press('Escape'); current=await state();
  check('escape-clears-only-name',current.visible.includes('10 / 142')&&current.name===''&&current.task==='Visibility filters'&&await tab.playwright.getByRole('heading',{name:'Visibility filters',exact:true}).isVisible());
  await tab.playwright.getByLabel('Name contains',{exact:true}).fill('wall'); await click('Clear filters'); current=await state();
  check('clear-filters-syncs-field',current.name===''&&current.visible.includes('142 / 142')&&!current.chips);
  await tab.playwright.getByLabel('Name contains',{exact:true}).fill('wall'); await click('Show all'); current=await state();
  check('show-all-syncs-field',current.name===''&&current.visible.includes('142 / 142')&&!current.chips);
  await hidden.click();
  await click('Selection mode');
  await tab.playwright.getByLabel('Pick as',{exact:true}).selectOption('space'); current=await state();
  check('selection-mode-preserves-current-selection',current.selected.includes('1 selected')&&current.visible.includes('142 / 142'));
  await tab.playwright.getByLabel('Pick as',{exact:true}).selectOption('surface'); await click('Filter objects');
  check('picking-removed-from-visibility-task',!(await tab.playwright.getByLabel('Pick as',{exact:true}).isVisible()));
  await click('Clear tree search');
  const before=await state();
  await tab.playwright.getByRole('searchbox',{name:'Find in model tree',exact:true}).fill('Core_top'); current=await state();
  check('tree-search-independent',current.visible===before.visible&&current.selected===before.selected&&current.chips===before.chips&&current.tree.includes('6 matching'));
  await click('Clear tree search');
  for(const [width,height] of [[760,650],[360,640],[320,640]]) {
    await viewport.set({width,height});
    await click('Filter objects');
    await tab.playwright.getByLabel('Name contains',{exact:true}).fill('Basement_Ceiling_2'); current=await state();
    check('filter-drawer-bounds-'+width,!current.overflow&&current.pane.left>=current.root.left&&current.pane.right<=current.root.right+1&&current.status.includes('1 of 1'),current);
    await shot('name-filter-'+width);
    await click('Model tree');
    await tab.playwright.getByRole('searchbox',{name:'Find in model tree',exact:true}).fill('Basement');
    check('tree-search-narrow-'+width,(await state()).tree.includes('10 matching surfaces · 1 visible'));
    await shot('tree-'+width);
    await click('Clear tree search');
  }
  await viewport.set({width:1440,height:900});
  await tab.playwright.getByRole('tab',{name:'Model',exact:true}).click();
  await click('Filter objects'); await click('Show all');
  await fs.writeFile(output+'/name-filter-checks.json',JSON.stringify(checks,null,2));
  return checks;
}
