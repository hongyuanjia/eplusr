// Supply the archived model's serialized object metadata and original SQL temperature rows.
export async function checkResultFilter(tab, viewport, fs, output, objects, rows) {
  const checks=[];
  // Fail on observable behavior and retain the source values used for comparison.
  function check(name,pass,detail=null){checks.push({name,pass,detail});if(!pass)throw Error(name+': '+JSON.stringify(detail));}
  // Keep all actions on the documented CUA surface.
  async function click(name){await tab.playwright.getByRole('button',{name,exact:typeof name==='string'}).click();}
  // Read actual visibility from synchronized tree rows without accessing viewer runtime objects.
  async function state(){return tab.playwright.evaluate(()=>({
    names:[...document.querySelectorAll('.eplusr-viewer__tree-row:not([aria-expanded])[data-hidden="false"]')].map(n=>n.title).sort(),
    selected:document.querySelector('.eplusr-viewer__selection-count').textContent,
    labels:document.querySelector('.eplusr-viewer__legend-labels').innerText,
    chips:document.querySelector('.eplusr-viewer__filter-chips').innerText,
    canvas:document.querySelector('canvas').getBoundingClientRect().toJSON(),
    pane:document.querySelector('.eplusr-viewer__task-pane').getBoundingClientRect().toJSON(),
    root:document.querySelector('.eplusr-viewer').getBoundingClientRect().toJSON(),
    overflow:document.documentElement.scrollWidth>innerWidth,
    focused:document.activeElement?.getAttribute('aria-label')
  }));}
  // Model expectations come from original SQL values, including the float32 transport precision.
  function expected(hour,bounds,name=''){
    const values=new Map(rows.filter(row=>row.hour===hour).map(row=>[row.zone.toLowerCase(),Math.fround(row.value)]));
    return objects.filter(object=>object.name.toLowerCase().includes(name.toLowerCase()))
      .filter(object=>{const value=values.get(object.zone_name?.toLowerCase());return Number.isFinite(value)&&(bounds[0]==null||value>=bounds[0])&&(bounds[1]==null||value<=bounds[1]);})
      .map(object=>object.name).sort();
  }
  // Use explicit form submission so incomplete numeric drafts never change visible geometry.
  async function bounds(minimum,maximum){
    await tab.playwright.getByLabel('Minimum visible value',{exact:true}).fill(minimum??'');
    await tab.playwright.getByLabel('Maximum visible value',{exact:true}).fill(maximum??'');
    await click('Apply result value filter');
  }
  // Jump through the same native time control a user operates and wait for the real provider frame.
  async function hour(value){
    await tab.playwright.getByRole('tab',{name:'Results',exact:true}).click();await click('Time settings');
    const label='07-21 '+String(value).padStart(2,'0')+':00';
    await tab.playwright.getByLabel('Jump to result time',{exact:true}).fill(label);
    await tab.playwright.getByLabel('Jump to result time',{exact:true}).press('Enter');
    await tab.playwright.locator('.eplusr-viewer__legend-time').filter({hasText:label}).waitFor({state:'visible'});
    await tab.playwright.locator('.eplusr-viewer--loading').waitFor({state:'hidden'});
  }
  // Save the actual browser viewport after each supported layout transition.
  async function shot(name){await fs.writeFile(output+'/'+name+'.jpg',await tab.screenshot({fullPage:false}));}
  await fs.mkdir(output,{recursive:true});await viewport.set({width:1440,height:900});
  await tab.playwright.getByRole('tab',{name:'Model',exact:true}).click();await click('Show all');
  await click('Filter objects');await tab.playwright.getByLabel('Name contains',{exact:true}).fill('Perimeter_top');
  check('filtering-does-not-invent-selection',(await state()).selected.includes('0 selected'));
  check('fit-visible-enabled-without-selection',await tab.playwright.getByRole('button',{name:'Fit visible objects',exact:true}).isEnabled());
  check('fit-selection-remains-explicit',!(await tab.playwright.getByRole('button',{name:'Fit selection',exact:true}).isEnabled()));
  await click('Fit visible objects');await click('Clear name filter');
  await hour(6);await click('Edit result value filter');const original=await state();
  await bounds('23.7','24');let current=await state();
  check('inclusive-interval-matches-real-SQL',JSON.stringify(current.names)===JSON.stringify(expected(6,[23.7,24])),current.names);
  check('filter-keeps-color-scale',current.labels===original.labels);
  check('result-chip-visible',current.chips.includes('23.7 – 24'));
  const previous=current.names;
  await bounds('30','24');current=await state();
  check('invalid-interval-retains-last-valid-geometry',JSON.stringify(current.names)===JSON.stringify(previous));
  check('invalid-interval-explained',(await tab.playwright.locator('[aria-label="Result value interval"] [role=status]').innerText()).includes('minimum must not exceed'));
  await bounds('23.7','24');
  for(const value of [1,12,24,6]){
    await hour(value);current=await state();
    check('frame-'+value+'-matches-real-SQL',JSON.stringify(current.names)===JSON.stringify(expected(value,[23.7,24])),current.names);
    check('frame-'+value+'-retains-color-scale',current.labels===original.labels);
  }
  await click('Edit result value filter');await click('Reset result value filter');
  check('reset-restores-geometry',(await state()).names.length===objects.length);
  await bounds('20','40');
  await tab.playwright.getByRole('tab',{name:'Model',exact:true}).click();
  if(!(await tab.playwright.getByRole('searchbox',{name:'Find in model tree',exact:true}).isVisible()))await click('Model tree');
  await tab.playwright.getByRole('treeitem',{name:'Zone: Perimeter_top_ZN_1',exact:true}).click();
  await hour(3);
  check('uncached-frame-preserves-valid-selection',(await state()).selected.includes('7 selected'));
  await hour(6);
  await tab.playwright.getByRole('tab',{name:'Model',exact:true}).click();await click('Clear selection');
  await click('Edit result value filter');await click('Reset result value filter');
  const minimum=tab.playwright.getByRole('slider',{name:'Minimum visible result',exact:true});
  await minimum.press('ArrowRight');current=await state();
  check('native-thumb-keeps-keyboard-focus',current.focused==='Minimum visible result');
  check('native-thumb-creates-real-filter',current.chips.includes('Result values:'));
  await minimum.press('Home');check('thumb-endpoint-clears-bound',!(await state()).chips.includes('Result values:'));
  await bounds('23.7','24');
  await tab.playwright.getByRole('tab',{name:'Model',exact:true}).click();await click('Filter objects');
  await tab.playwright.getByLabel('Name contains',{exact:true}).fill('Perimeter_top');current=await state();
  check('numeric-and-name-intersect',JSON.stringify(current.names)===JSON.stringify(expected(6,[23.7,24],'Perimeter_top')),current.names);
  await click(/^Remove filter: Result values:/);current=await state();
  check('numeric-chip-clears-only-numeric',current.names.length===28&&current.chips.includes('Perimeter_top'));
  await click('Clear name filter');
  await tab.playwright.getByRole('tab',{name:'View',exact:true}).click();await click('Diagnostics');
  check('source-mismatches-visible',(await tab.playwright.locator('.eplusr-viewer__diagnostic-list').innerText()).includes('0.04 m'));
  check('exactly-five-source-boundaries',(await tab.playwright.locator('.eplusr-viewer__diagnostic-summary').innerText()).includes('5 of 5'));
  await shot('source-boundaries');
  await tab.playwright.getByRole('tab',{name:'Results',exact:true}).click();await click('Select result data');
  check('source-variable-scope-explained',(await tab.playwright.locator('.eplusr-viewer__source-summary').innerText()).includes('1 spatial / 51 SQL variables'));
  await click('Edit result value filter');await bounds('23.7','24');
  // A new environment keeps thresholds for the same quantity and units, then recalculates visibility.
  for(const [term,label] of [['HTG','CHICAGO ANN HTG 99.6% CONDNS DB'],['CLG','CHICAGO ANN CLG .4% CONDNS WB=>MDB']]){
    await click('Select result data');
    if(await tab.playwright.getByRole('button',{name:'Clear environments',exact:true}).isVisible())await click('Clear environments');
    await tab.playwright.getByRole('searchbox',{name:'Search environments',exact:true}).fill(term);
    await tab.playwright.getByRole('checkbox',{name:label,exact:true}).check();
    await tab.playwright.getByRole('searchbox',{name:'Search environments',exact:true}).press('Escape');
    await click('Apply result query');
    await tab.playwright.locator('.eplusr-viewer__result-query [role=status]').filter({hasText:'Applied result query.'}).waitFor({state:'visible'});
    await tab.playwright.locator('.eplusr-viewer--loading').waitFor({state:'hidden'});
    current=await state();
    check('environment-'+term+'-retains-interval',current.chips.includes('23.7 – 24'));
    check('environment-'+term+'-recalculates-results',JSON.stringify(current.names)===JSON.stringify(term==='HTG'?[]:expected(1,[23.7,24])),current.names);
  }
  await hour(6);
  for(const [width,height] of [[1440,900],[760,650],[360,640],[320,640],[760,460]]){
    await viewport.set({width,height});await tab.playwright.getByRole('tab',{name:'Results',exact:true}).click();
    await click('Filter result values');current=await state();
    check('interval-panel-bounds-'+width+'x'+height,!current.overflow&&current.pane.left>=current.root.left&&current.pane.right<=current.root.right+1,current);
    await click('Close task panel');
    check('legend-thumb-accessible-'+width+'x'+height,await minimum.isVisible());
    await shot('result-filter-'+width+'x'+height);
  }
  await viewport.set({width:1440,height:900});
  await fs.writeFile(output+'/checks.json',JSON.stringify(checks,null,2));return checks;
}
