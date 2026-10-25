import {buttonIcon} from "./icons.js";
import {displayUnit} from "./presentation.js";

// Treat missing values as unknown, never as zero or as a numeric match.
export function matchesResultValue(value, bounds) {
  return !bounds || (Number.isFinite(value) &&
    (bounds[0] == null || value >= bounds[0]) && (bounds[1] == null || value <= bounds[1]));
}

// Validate an inclusive visibility interval separately from the color scale's display limits.
export function validateResultBounds(minimum, maximum) {
  const values = [minimum, maximum].map(value => value == null || value === "" ? null
    : ["number", "string"].includes(typeof value) && String(value).trim() ? Number(value) : NaN);
  if (values.some(value => value !== null && !Number.isFinite(value))) throw Error("Use finite numbers for the result filter.");
  if (values[0] !== null && values[1] !== null && values[0] > values[1]) throw Error("The minimum must not exceed the maximum.");
  return values.every(value => value === null) ? null : values;
}

// Keep the two native thumb controls alive while results, selections and color-key text refresh.
export class ResultValueFilter {
  // Expose direct manipulation on the legend and exact numeric editing in the task pane.
  constructor(viewer) {
    this.viewer = viewer;
    this.track = document.createElement("div");
    this.track.className = "eplusr-viewer__legend-gradient eplusr-viewer__value-track";
    this.shades = [document.createElement("span"),document.createElement("span")];
    this.shades.forEach(item => { item.className="eplusr-viewer__value-shade"; this.track.append(item); });
    this.sliders = ["Minimum visible result", "Maximum visible result"].map((label,index) => {
      const input=document.createElement("input");
      input.type="range"; input.setAttribute("aria-label",label);
      input.addEventListener("input",()=>{
        const bounds=[...(viewer.results?.visibilityRange ?? [null,null])];
        const range=viewer.results.range ?? viewer.results.sourceRange;
        let value=range[0]+Number(input.value)/1000*(range[1]-range[0]),other=bounds[1-index];
        if(other!==null) value=index===0 ? Math.min(value,other) : Math.max(value,other);
        // Returning a handle to its color-key endpoint removes only that bound.
        bounds[index]=Number(input.value)===(index===0?0:1000)?null:value;
        viewer.setResultValueFilter(...bounds);
      });
      this.track.append(input); return input;
    });
    this.actions=document.createElement("span"); this.actions.className="eplusr-viewer__value-actions";
    this.open=document.createElement("button"); this.open.type="button";
    buttonIcon(this.open,"filters","Edit result value filter",true);
    this.open.addEventListener("click",()=>viewer.ribbon.open("value-filter"));
    this.reset=document.createElement("button"); this.reset.type="button";
    buttonIcon(this.reset,"clear","Clear result value filter",true);
    this.reset.addEventListener("click",()=>viewer.setResultValueFilter(null,null));
    this.actions.append(this.open,this.reset);
    this.fit=document.createElement("button"); this.fit.type="button";
    buttonIcon(this.fit,"fit","Fit visible objects",true);
    this.fit.addEventListener("click",()=>viewer.fitView(null,{reveal:true}));
    this.summary=document.createElement("div"); this.summary.className="eplusr-viewer__legend-summary";
    this.panel=document.createElement("form");
    this.panel.setAttribute("aria-label","Result value interval");
    this.panel.addEventListener("submit",event=>{event.preventDefault();viewer.setResultValueFilter(...this.inputs.map(input=>input.value));});
    const note=document.createElement("p"); note.className="eplusr-viewer__status";
    note.textContent="Only values inside this inclusive interval remain visible. The color scale stays unchanged; missing values are excluded while filtering.";
    this.panel.append(note);
    this.inputs=["Minimum visible value","Maximum visible value"].map(label=>{
      const wrapper=document.createElement("label"); wrapper.className="eplusr-viewer__field";
      const text=document.createElement("span"); text.textContent=label;
      const input=document.createElement("input"); input.type="number"; input.step="any";
      input.setAttribute("aria-label",label);
      input.addEventListener("input",()=>{
        this.draft=true; this.error=null;
        this.inputs.forEach(item=>item.removeAttribute("aria-invalid"));
        this.status.textContent="Changes not applied.";
      });
      wrapper.append(text,input); this.panel.append(wrapper); return input;
    });
    const apply=document.createElement("button"); apply.type="submit"; apply.textContent="Apply";
    apply.setAttribute("aria-label","Apply result value filter");
    const reset=document.createElement("button"); reset.type="button"; reset.textContent="Reset filter";
    reset.setAttribute("aria-label","Reset result value filter");
    reset.addEventListener("click",()=>viewer.setResultValueFilter(null,null));
    this.status=document.createElement("p"); this.status.className="eplusr-viewer__status";
    this.status.setAttribute("role","status"); const actions=document.createElement("div"); actions.className="eplusr-viewer__button-row"; actions.append(apply,reset,this.fit);
    this.panel.append(actions,this.status);
  }

  // Keep validation local to the edited fields and retain the last valid visible scene.
  fail(message) {
    this.error=message;
    this.status.textContent=message;
    this.inputs.forEach(input=>input.setAttribute("aria-invalid","true"));
  }

  // Synchronize thumb positions and accessible values without rebuilding focused input elements.
  sync(result, gradient) {
    this.panel.querySelectorAll("input,button").forEach(control=>{control.disabled=!result;});
    if(!result){this.status.textContent="Attach results to filter numerical values.";return;}
    const range=result.range ?? result.sourceRange, bounds=result.visibilityRange;
    this.fit.disabled=!this.viewer.visibleObjects?.size;
    const span=range[1]-range[0], unit=displayUnit(result.units);
    this.track.style.background=`linear-gradient(90deg, ${gradient})`;
    const fractions=[0,1];
    this.sliders.forEach((input,index)=>{
      const value=bounds?.[index] ?? range[index];
      const shown=Math.max(range[0],Math.min(range[1],value));
      // Integer thumb positions avoid floating-point step snapping at either endpoint.
      input.min="0"; input.max="1000"; input.step="1";
      input.value=String(span>0?Math.round((shown-range[0])/span*1000):index*1000); input.disabled=span<=0;
      input.setAttribute("aria-valuetext",`${value} ${unit}${bounds?.[index]==null?" · unbounded":""}`);
      fractions[index]=span>0?(shown-range[0])/span:index;
    });
    this.shades[0].style.width=(fractions[0]*100)+"%";
    this.shades[1].style.width=((1-fractions[1])*100)+"%";
    this.shades[1].style.right="0";
    this.reset.hidden=!bounds;
    this.summary.hidden=!bounds;
    // Label rounding is visual only; comparisons keep the original threshold precision.
    const format=value=>value==null?null:Number(value.toPrecision(6));
    this.summary.textContent=bounds?`Visible: ${format(bounds[0])??"−∞"} – ${format(bounds[1])??"+∞"} ${unit}`:"";
    this.inputs.forEach((input,index)=>{
      if(!this.error)input.removeAttribute("aria-invalid");
      if(!this.error&&!this.draft&&document.activeElement!==input)input.value=bounds?.[index]??"";
      input.placeholder=`Unbounded${unit?" ("+unit+")":""}`;
    });
    this.status.textContent=this.error ?? (this.draft?"Changes not applied.":null) ?? (bounds?"Values outside the interval are hidden.":"All result values are included.");
  }
}
