// Exercise actual source winding through transparent/opaque panes and both camera projections.
export async function checkBackFaces(tab,viewport,fs,output){
  const checks=[];
  // Keep every assertion attached to a user-visible outcome.
  function check(name,pass,detail=null){checks.push({name,pass,detail});if(!pass)throw Error(name+': '+JSON.stringify(detail));}
  // Use semantic commands, never injected viewer methods.
  async function click(name){await tab.playwright.getByRole('button',{name,exact:true}).click();}
  // Inspect just the rendered selection state and current canvas bounds.
  async function state(){return tab.playwright.evaluate(()=>({selected:document.querySelector('.eplusr-viewer__selection-count').textContent,canvas:document.querySelector('canvas').getBoundingClientRect().toJSON()}));}
  // Pick the center of the isolated pane, keeping coordinates grounded in its current canvas.
  async function pickCenter(){const s=await state();await tab.click([s.canvas.x+s.canvas.width/2,s.canvas.y+s.canvas.height/2]);await tab.getAXState({emit:false});return state();}
  // Standard axis views must remain standard directions even when the target faces away.
  async function camera(view,projection){
    await tab.playwright.getByRole('tab',{name:'View',exact:true}).click();await click('Camera');
    await tab.playwright.getByLabel('Projection',{exact:true}).selectOption(projection);
    await tab.playwright.getByLabel('Camera view',{exact:true}).selectOption(view);
    await click('Close task panel');await tab.getAXState({emit:false});
  }
  // Save visible evidence before and after culling a selected pane, including its former highlight.
  async function shot(name){await fs.writeFile(output+'/'+name+'.jpg',await tab.screenshot({fullPage:false}));}
  await fs.mkdir(output,{recursive:true});await viewport.set({width:1440,height:900});
  await tab.playwright.getByRole('tab',{name:'Model',exact:true}).click();await click('Show all');
  await click('Appearance');await tab.playwright.getByRole('checkbox',{name:'Show back faces',exact:true}).uncheck();
  await tab.playwright.getByLabel('Window appearance',{exact:true}).selectOption('glass');await click('Close task panel');
  await tab.playwright.getByRole('tab',{name:'View',exact:true}).click();
  const axesWereOn=await tab.playwright.getByRole('button',{name:'Axes',exact:true}).getAttribute('aria-pressed')==='true';
  if(axesWereOn)await click('Axes');
  await tab.playwright.getByRole('tab',{name:'Model',exact:true}).click();
  await click('Filter objects');await tab.playwright.getByLabel('Name contains',{exact:true}).fill('Perimeter_top_ZN_1_Wall_South_Window');await click('Close task panel');
  for(const projection of ['perspective','orthographic']){
    await camera('front',projection);
    await fs.writeFile(output+'/canvas-'+projection+'.json',JSON.stringify((await state()).canvas));
    check('front-pane-pick-'+projection,(await pickCenter()).selected.includes('1 selected'));await shot('front-'+projection);
    await camera('back',projection);await shot('back-selected-'+projection);
    check('back-pane-not-pickable-'+projection,(await pickCenter()).selected.includes('0 selected'));await shot('back-'+projection);
  }
  // Free orbit must refresh boundary/highlight culling, not only named camera views.
  await camera('front','perspective');await pickCenter();
  const orbit=(await state()).canvas;
  await tab.drag([orbit.x+orbit.width/2,orbit.y+orbit.height/2],[orbit.x+orbit.width/2+orbit.height/2,orbit.y+orbit.height/2]);
  await tab.getAXState({emit:false});await shot('orbit-back-selected');
  check('free-orbit-culls-back-picking',(await pickCenter()).selected.includes('0 selected'));
  await camera('back','orthographic');
  await tab.playwright.getByRole('tab',{name:'Model',exact:true}).click();await click('Appearance');
  await tab.playwright.getByRole('checkbox',{name:'Show back faces',exact:true}).check();await click('Close task panel');
  check('explicit-inspection-allows-back',(await pickCenter()).selected.includes('1 selected'));
  await click('Appearance');await tab.playwright.getByRole('checkbox',{name:'Show back faces',exact:true}).uncheck();
  await tab.playwright.getByLabel('Window appearance',{exact:true}).selectOption('opaque');await click('Close task panel');
  check('opaque-pane-also-culls-back',(await pickCenter()).selected.includes('0 selected'));
  await click('Filter objects');await click('Fit visible objects');await click('Close task panel');
  check('explicit-fit-reveals-back-facing-target',(await pickCenter()).selected.includes('1 selected'));await shot('fit-reveals-front');
  await click('Appearance');await tab.playwright.getByLabel('Window appearance',{exact:true}).selectOption('glass');await click('Close task panel');
  await click('Show all');await camera('iso','perspective');
  if(axesWereOn)await click('Axes');
  await fs.writeFile(output+'/checks.json',JSON.stringify(checks,null,2));return checks;
}
