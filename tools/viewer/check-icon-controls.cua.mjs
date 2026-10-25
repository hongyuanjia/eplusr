// Verify icon-only controls through real CUA actions and read-only DOM assertions.
export async function checkIconControls(tab, viewport, fs, output) {
  const checks=[];
  // Keep assertions tied to visible state rather than implementation method calls.
  function check(name,pass,detail=null) { checks.push({name,pass,detail}); if(!pass)throw Error(name); }
  // Reuse the same accessible names after removing visible command labels.
  async function click(name) { await tab.playwright.getByRole('button',{name,exact:true}).click(); }
  // Capture bounded tooltips at the supported host sizes.
  async function picture(name) { await fs.writeFile(output+'/'+name+'.jpg',await tab.screenshot({fullPage:false})); }
  await fs.mkdir(output,{recursive:true});
  await viewport.set({width:1440,height:900});
  await tab.playwright.getByRole('tab',{name:'Results',exact:true}).click();
  await click('Select result data');
  check('commands-have-hidden-labels',await tab.playwright.evaluate(()=>[...document.querySelectorAll('.eplusr-viewer__ribbon button[aria-label] .eplusr-viewer__button-label')].every(n=>n.getBoundingClientRect().width<=1)));
  check('distinct-result-icons',await tab.playwright.evaluate(()=>{
    const labels=['Select result data','Color scale','Glass / color by','Time settings','Timeline'];
    return new Set(labels.map(label=>document.querySelector('button[aria-label="'+label+'"]').querySelector('svg').outerHTML)).size===labels.length;
  }));
  for(let round=1;round<=3;round++) {
    await tab.playwright.getByRole('button',{name:'Model tree',exact:true}).press('Tab');
    await tab.playwright.getByRole('tooltip').waitFor({state:'visible'});
    check('keyboard-hint-'+round,(await tab.playwright.getByRole('tooltip').innerText()).includes('Focus on the 3D'));
    await tab.playwright.getByRole('button',{name:'Model only',exact:true}).press('Escape');
    check('escape-only-hint-'+round,await tab.playwright.evaluate(()=>document.querySelector('.eplusr-viewer__command-hint').hidden&&!document.querySelector('.eplusr-viewer__task-pane').hidden));
  }
  check('concise-query-actions',await tab.playwright.evaluate(()=>document.querySelector('.eplusr-viewer__result-query button[type=submit]').textContent==='Apply'));
  check('no-repeated-query-title',await tab.playwright.evaluate(()=>document.querySelector('.eplusr-viewer__result-query legend').getBoundingClientRect().width<=1));
  check('current-time-visible',await tab.playwright.locator('.eplusr-viewer__legend-time').isVisible());
  await picture('results-desktop');
  await tab.playwright.getByRole('tab',{name:'View',exact:true}).click();
  await click('Section plane');
  await tab.playwright.getByRole('checkbox',{name:'Enable clipping plane',exact:true}).check();
  check('section-feedback',await tab.playwright.locator('.eplusr-viewer__tool-feedback').isVisible());
  await tab.playwright.getByRole('checkbox',{name:'Enable clipping plane',exact:true}).uncheck();
  await click('Exploded view');
  await tab.playwright.getByLabel('Explode by',{exact:true}).selectOption('zone');
  await tab.playwright.getByRole('slider',{name:'Explode distance',exact:true}).press('ArrowRight');
  await click('Section plane'); await click('Exploded view');
  check('separate-task-draft',await tab.playwright.evaluate(()=>document.querySelector('[aria-label="Explode by"]').value==='zone'));
  await tab.playwright.getByLabel('Explode by',{exact:true}).selectOption('none');
  await click('Close task panel');
  check('idle-guidance-hidden',!(await tab.playwright.locator('.eplusr-viewer__tool-feedback').isVisible()));
  await tab.playwright.locator('.eplusr-viewer').press('?');
  check('help-shortcut',await tab.playwright.getByRole('heading',{name:'Navigation help',exact:true}).isVisible());
  await click('Close task panel');
  await picture('view-desktop');
  for(const [width,height] of [[760,650],[480,650],[760,460],[360,640],[320,640]]) {
    await viewport.set({width,height});
    await tab.playwright.getByRole('tab',{name:'Results',exact:true}).click();
    await click('Select result data');
    check('R-badge-in-bounds-'+width+'x'+height,await tab.playwright.evaluate(()=>{
      const root=document.querySelector('.eplusr-viewer').getBoundingClientRect();
      const badge=document.querySelector('.eplusr-viewer__connection').getBoundingClientRect();
      return badge.left>=root.left&&badge.right<=root.right;
    }));
    await picture('results-'+width+'x'+height);
    await tab.playwright.getByRole('button',{name:'Model tree',exact:true}).press('Tab');
    await tab.playwright.getByRole('tooltip').waitFor({state:'visible'});
    check('hint-in-bounds-'+width+'x'+height,await tab.playwright.evaluate(()=>{
      const root=document.querySelector('.eplusr-viewer').getBoundingClientRect();
      const hint=document.querySelector('.eplusr-viewer__command-hint').getBoundingClientRect();
      return hint.left>=root.left&&hint.right<=root.right+1&&hint.top>=root.top&&hint.bottom<=root.bottom;
    }));
    await picture('hint-'+width+'x'+height);
    await tab.playwright.getByRole('button',{name:'Model only',exact:true}).press('Escape');
  }
  await viewport.set({width:1440,height:900});
  await fs.writeFile(output+'/icon-checks.json',JSON.stringify(checks,null,2));
  return checks;
}
