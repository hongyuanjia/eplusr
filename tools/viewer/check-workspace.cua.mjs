// Exercise the real htmlwidget through the host's CUA tab, with DOM reads only for assertions.
export async function checkWorkspace(tab, viewport, fs, output) {
  const checks = [];
  // Fail at the observable user-facing state instead of assuming a successful click.
  function check(name, pass, detail = null) {
    checks.push({ name, pass, detail });
    if (!pass) throw new Error(name + ': ' + JSON.stringify(detail));
  }
  // Keep all actions on the documented CUA locator surface.
  async function click(name) { await tab.playwright.getByRole('button', {name, exact:true}).click(); }
  // Change command pages without touching the underlying task or result state.
  async function taskTab(name) { await tab.playwright.getByRole('tab', {name, exact:true}).click(); }
  // Read only rendered DOM, including bounds used by responsive assertions.
  async function state() {
    return tab.playwright.evaluate(() => {
      const root=document.querySelector('.eplusr-viewer');
      const pane=document.querySelector('.eplusr-viewer__task-pane');
      return { selected:document.querySelector('.eplusr-viewer__selection-count').textContent,
        scope:document.querySelector('.eplusr-viewer__scope').innerText,
        timeline:!document.querySelector('.eplusr-viewer__timeline').hidden,
        tree:!document.querySelector('.eplusr-viewer__model-browser').hidden,
        task:!pane.hidden, title:pane.querySelector('h2').textContent,
        clean:root.classList.contains('eplusr-viewer--model-only'),
        canvas:document.querySelector('canvas').getBoundingClientRect().toJSON(),
        root:root.getBoundingClientRect().toJSON(), pane:pane.getBoundingClientRect().toJSON(),
        overflow:document.documentElement.scrollWidth>innerWidth,
        key:document.querySelector('.eplusr-viewer__legend').innerText,
        playing:[...document.querySelector('.eplusr-viewer__timeline').querySelectorAll('button')].some(b=>b.getAttribute('aria-label')==='Pause')};
    });
  }
  // Capture the exact visible browser surface for review, without injected scene commands.
  async function shot(name) {
    await fs.writeFile(output+'/'+name+'.jpg',await tab.screenshot({fullPage:false}));
  }
  await fs.mkdir(output,{recursive:true});
  await viewport.set({width:1440,height:900});
  await tab.reload();
  await tab.getAXState({emit:false});
  await taskTab('Model'); await click('Show all');
  if(await tab.playwright.getByRole('button',{name:'Clear selection',exact:true}).isEnabled()) await click('Clear selection');
  for(let round=1;round<=3;round++) {
    await tab.playwright.getByRole('treeitem',{name:'Zone: Perimeter_top_ZN_1',exact:true}).click();
    await tab.playwright.getByRole('treeitem',{name:'Zone: Perimeter_top_ZN_2',exact:true}).click({modifiers:['ControlOrMeta']});
    check('tree-multiselect-'+round,(await state()).selected.includes('14'));
    await click('Properties');
    check('properties-'+round,(await tab.playwright.locator('.eplusr-viewer__selection').innerText()).includes('14'));
    await click('Close task panel'); await click('Clear selection');
    await click('Hide Perimeter_top_ZN_1');
    check('hide-zone-'+round,(await state()).scope.includes('135 / 142'));
    await click('Show Perimeter_top_ZN_1');
    check('show-zone-'+round,(await state()).scope.includes('142 / 142'));
  }
  await taskTab('Results'); await click('Select result data');
  await tab.playwright.getByText('Keys and time filters',{exact:true}).click();
  await tab.playwright.getByLabel('Hours',{exact:true}).fill('8:18');
  const scrollBefore=await tab.playwright.evaluate(()=>document.querySelector('[id$="-task-results"]').scrollTop);
  await click('Color scale'); await click('Select result data');
  check('draft-survives-task-switch',await tab.playwright.evaluate(()=>document.querySelector('[name=hour]').value==='8:18'));
  check('scroll-survives-task-switch',(await tab.playwright.evaluate(()=>document.querySelector('[id$="-task-results"]').scrollTop))===scrollBefore);
  await tab.playwright.getByLabel('Hours',{exact:true}).fill('99'); await click('Apply result query');
  check('invalid-query-retains-old-display',(await tab.playwright.locator('.eplusr-viewer__result-query [role=status]').innerText()).includes('between 0 and 24'));
  await tab.playwright.getByLabel('Hours',{exact:true}).fill('');
  await tab.playwright.getByText('Keys and time filters',{exact:true}).click();
  for(let round=1;round<=3;round++) {
    for(const [term,label,min] of [['HTG','CHICAGO ANN HTG 99.6% CONDNS DB','10.56'],['CLG','CHICAGO ANN CLG .4% CONDNS WB=>MDB','22.25']]) {
      await click('Clear environments');
      await tab.playwright.getByRole('searchbox',{name:'Search environments',exact:true}).fill(term);
      await tab.playwright.getByRole('checkbox',{name:label,exact:true}).check();
      await tab.playwright.getByRole('searchbox',{name:'Search environments',exact:true}).press('Escape');
      check('list-escape-retains-task-'+round+'-'+term,(await state()).task);
      await click('Apply result query');
      await tab.playwright.locator('.eplusr-viewer__result-query [role=status]').filter({hasText:'Applied result query.'}).waitFor({state:'visible'});
      check('real-query-'+round+'-'+term,(await state()).key.includes(min));
    }
  }
  // Reload a changed query to verify metadata and provider frames reconnect as one result set.
  await click('Clear environments');
  await tab.playwright.getByRole('searchbox',{name:'Search environments',exact:true}).fill('HTG');
  await tab.playwright.getByRole('checkbox',{name:'CHICAGO ANN HTG 99.6% CONDNS DB',exact:true}).check();
  await tab.playwright.getByRole('searchbox',{name:'Search environments',exact:true}).press('Escape');
  await click('Apply result query');
  await tab.playwright.locator('.eplusr-viewer__result-query [role=status]').filter({hasText:'Applied result query.'}).waitFor({state:'visible'});
  await tab.reload();
  await tab.playwright.locator('.eplusr-viewer__legend').filter({hasText:'10.56'}).waitFor({state:'visible'});
  check('reload-restores-query-metadata',(await state()).key.includes('10.56'));
  await taskTab('Results'); await click('Time settings'); await click('Timeline');
  check('reload-restores-query-calendar',(await tab.playwright.locator('.eplusr-viewer__timeline').innerText()).includes('01-21'));
  for(let round=1;round<=3;round++) {
    await click('Play'); await tab.playwright.getByRole('button',{name:'Pause',exact:true}).waitFor({state:'visible'});
    await click('Hide timeline'); const current=await state();
    check('hide-pauses-'+round,!current.timeline&&!current.playing);
    await click('Timeline');
  }
  await click('Hide timeline'); await click('Select result data');
  await click('Clear environments');
  await tab.playwright.getByRole('searchbox',{name:'Search environments',exact:true}).fill('CLG');
  await tab.playwright.getByRole('checkbox',{name:'CHICAGO ANN CLG .4% CONDNS WB=>MDB',exact:true}).check();
  await tab.playwright.getByRole('searchbox',{name:'Search environments',exact:true}).press('Escape');
  await click('Apply result query');
  await tab.playwright.locator('.eplusr-viewer__result-query [role=status]').filter({hasText:'Applied result query.'}).waitFor({state:'visible'});
  await shot('desktop-result-task');
  for(const [width,height] of [[760,650],[480,650],[760,460],[360,640]]) {
    await viewport.set({width,height});
    await taskTab('Results'); await click('Select result data'); let current=await state();
    check('one-drawer-'+width+'x'+height,current.task&&!current.tree);
    check('task-bounds-'+width+'x'+height,!current.overflow&&current.pane.right<=current.root.right+1&&current.pane.left>=current.root.left&&current.canvas.height>200,current);
    await shot('results-'+width+'x'+height);
    await click('Model tree'); current=await state();
    check('tree-replaces-task-'+width+'x'+height,current.tree&&!current.task);
    await tab.playwright.getByRole('searchbox',{name:'Find in model tree',exact:true}).fill('Perimeter_top_ZN_1');
    check('search-'+width+'x'+height,(await tab.playwright.locator('.eplusr-viewer__tree-status').innerText()).includes('7 matching'));
    await shot('tree-'+width+'x'+height);
    await click('Clear tree search'); await click('Close model browser');
    await click('Model only'); current=await state();
    check('clean-'+width+'x'+height,current.clean&&!current.timeline&&!current.overflow);
    check('clean-overlays-'+width+'x'+height,!(await tab.playwright.locator('.eplusr-viewer__navigation').isVisible())&&!(await tab.playwright.locator('.eplusr-viewer__legend').isVisible()));
    await shot('clean-'+width+'x'+height);
    await click('Show controls');
  }
  await viewport.set({width:1440,height:900});
  await taskTab('Results'); await click('Select result data');
  await shot('desktop-result-task-final');
  await fs.writeFile(output+'/checks.json',JSON.stringify(checks,null,2));
  return checks;
}
