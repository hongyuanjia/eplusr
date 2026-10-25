(function(){"use strict";class hc{constructor(e,t){this.viewer=e,this.config=t,this.closed=!1,this.applying=!1,this.queue=Promise.resolve(),this.reportTimer=null,this.status=e.connectionStatus.root,this.setStatus("Connecting to R…","connecting"),e.toolbar.appendChild(this.status),this.onSelection=()=>{this.applying||this.report("selection")},this.onChange=()=>this.scheduleReport(),this.onQuery=i=>{if(this.closed||this.socket.readyState!==WebSocket.OPEN){e.queryPanel.fail("R connection is unavailable. The displayed results are unchanged.");return}this.socket.send(JSON.stringify({type:"query_results",scene_id:t.scene_id,filters:i.detail}))},this.onPageHide=()=>this.dispose(),e.container.addEventListener("eplusr-selection",this.onSelection),e.root.addEventListener("change",this.onChange),e.root.addEventListener("click",this.onChange),e.root.addEventListener("eplusr-frame",this.onChange),e.root.addEventListener("eplusr-view",this.onChange),e.root.addEventListener("eplusr-result-query",this.onQuery),e.controls.addEventListener("end",this.onChange),window.addEventListener("pagehide",this.onPageHide),this.socket=new WebSocket(t.websocket),this.socket.addEventListener("open",()=>{this.setStatus("Connected to R","connected"),this.report()}),this.socket.addEventListener("close",()=>{this.setStatus("R connection closed","closed"),e.queryPanel.pending&&e.queryPanel.fail("R connection closed before the query completed. Reopen the live widget to reconnect.")}),this.socket.addEventListener("error",()=>{this.setStatus("R connection unavailable","error")}),this.socket.addEventListener("message",i=>{this.queue=this.queue.then(()=>this.receive(JSON.parse(i.data))).catch(s=>this.report("error",{error:s.message}))})}setStatus(e,t){this.viewer.connectionStatus.set(e,t)}scheduleReport(){this.closed||this.applying||(window.clearTimeout(this.reportTimer),this.reportTimer=window.setTimeout(()=>this.report(),40))}report(e="state",t={}){if(this.closed||this.socket.readyState!==WebSocket.OPEN)return;const i=this.viewer.readState();this.socket.send(JSON.stringify({type:e,scene_id:this.config.scene_id,selected_ids:i.selected_ids,state:i,...t}))}async receive(e){if(!(this.closed||e.scene_id!==this.config.scene_id)){this.applying=!0;try{const t=e.args??{};e.command==="time"?await this.viewer.setTime(t.frame??0):e.command==="variable"?await this.viewer.setResultVariable(t.index??0):e.command==="aggregation"?await this.viewer.setAggregation(t.value??"frame"):e.command==="results"?await this.viewer.setResults(t.results):this.viewer.applyCommand(e.command,t),this.report("ack",{sequence:e.sequence})}finally{this.applying=!1}}}dispose(){this.closed||(this.closed=!0,window.clearTimeout(this.reportTimer),this.viewer.container.removeEventListener("eplusr-selection",this.onSelection),this.viewer.root.removeEventListener("change",this.onChange),this.viewer.root.removeEventListener("click",this.onChange),this.viewer.root.removeEventListener("eplusr-frame",this.onChange),this.viewer.root.removeEventListener("eplusr-view",this.onChange),this.viewer.root.removeEventListener("eplusr-result-query",this.onQuery),this.viewer.controls.removeEventListener("end",this.onChange),window.removeEventListener("pagehide",this.onPageHide),this.socket.close(1e3,"Viewer disposed"),this.setStatus("R connection closed","closed"))}}/**
 * @license
 * Copyright 2010-2026 Three.js Authors
 * SPDX-License-Identifier: MIT
 */const Zs="184",nn={ROTATE:0,DOLLY:1,PAN:2},sn={ROTATE:0,PAN:1,DOLLY_PAN:2,DOLLY_ROTATE:3},uc=0,Za=1,dc=2,Qn=1,fc=2,Un=3,ni=0,vt=1,It=2,gi=0,rn=1,Ka=2,Ja=3,Qa=4,pc=5,Wi=100,mc=101,gc=102,_c=103,vc=104,xc=200,bc=201,Sc=202,Mc=203,Ks=204,Js=205,yc=206,Ec=207,wc=208,Tc=209,Ac=210,Cc=211,Rc=212,Pc=213,Lc=214,Qs=0,er=1,tr=2,an=3,ir=4,nr=5,sr=6,rr=7,eo=0,Dc=1,Ic=2,si=0,to=1,io=2,no=3,so=4,ro=5,ao=6,oo=7,lo=300,Xi=301,on=302,ar=303,or=304,es=306,lr=1e3,_i=1001,cr=1002,Tt=1003,Nc=1004,ts=1005,Ct=1006,hr=1007,ji=1008,kt=1009,co=1010,ho=1011,Fn=1012,ur=1013,ri=1014,ai=1015,vi=1016,dr=1017,fr=1018,On=1020,uo=35902,fo=35899,po=1021,mo=1022,Qt=1023,xi=1026,qi=1027,go=1028,pr=1029,Yi=1030,mr=1031,gr=1033,is=33776,ns=33777,ss=33778,rs=33779,_r=35840,vr=35841,xr=35842,br=35843,Sr=36196,Mr=37492,yr=37496,Er=37488,wr=37489,as=37490,Tr=37491,Ar=37808,Cr=37809,Rr=37810,Pr=37811,Lr=37812,Dr=37813,Ir=37814,Nr=37815,Ur=37816,Fr=37817,Or=37818,Br=37819,zr=37820,kr=37821,Vr=36492,Gr=36494,Hr=36495,Wr=36283,Xr=36284,os=36285,jr=36286,Uc=3200,qr=0,Fc=1,Ci="",jt="srgb",ls="srgb-linear",cs="linear",Qe="srgb",ln=7680,_o=519,Oc=512,Bc=513,zc=514,Yr=515,kc=516,Vc=517,$r=518,Gc=519,vo=35044,hs=35048,xo="300 es",oi=2e3,Bn=2001;function Hc(n){for(let e=n.length-1;e>=0;--e)if(n[e]>=65535)return!0;return!1}function us(n){return document.createElementNS("http://www.w3.org/1999/xhtml",n)}function Wc(){const n=us("canvas");return n.style.display="block",n}const bo={};function So(...n){const e="THREE."+n.shift();console.log(e,...n)}function Mo(n){const e=n[0];if(typeof e=="string"&&e.startsWith("TSL:")){const t=n[1];t&&t.isStackTrace?n[0]+=" "+t.getLocation():n[1]='Stack trace not available. Enable "THREE.Node.captureStackTrace" to capture stack traces.'}return n}function Ce(...n){n=Mo(n);const e="THREE."+n.shift();{const t=n[0];t&&t.isStackTrace?console.warn(t.getError(e)):console.warn(e,...n)}}function je(...n){n=Mo(n);const e="THREE."+n.shift();{const t=n[0];t&&t.isStackTrace?console.error(t.getError(e)):console.error(e,...n)}}function Zr(...n){const e=n.join(" ");e in bo||(bo[e]=!0,Ce(...n))}function Xc(n,e,t){return new Promise(function(i,s){function r(){switch(n.clientWaitSync(e,n.SYNC_FLUSH_COMMANDS_BIT,0)){case n.WAIT_FAILED:s();break;case n.TIMEOUT_EXPIRED:setTimeout(r,t);break;default:i()}}setTimeout(r,t)})}const jc={[Qs]:er,[tr]:sr,[ir]:rr,[an]:nr,[er]:Qs,[sr]:tr,[rr]:ir,[nr]:an};class Ri{addEventListener(e,t){this._listeners===void 0&&(this._listeners={});const i=this._listeners;i[e]===void 0&&(i[e]=[]),i[e].indexOf(t)===-1&&i[e].push(t)}hasEventListener(e,t){const i=this._listeners;return i===void 0?!1:i[e]!==void 0&&i[e].indexOf(t)!==-1}removeEventListener(e,t){const i=this._listeners;if(i===void 0)return;const s=i[e];if(s!==void 0){const r=s.indexOf(t);r!==-1&&s.splice(r,1)}}dispatchEvent(e){const t=this._listeners;if(t===void 0)return;const i=t[e.type];if(i!==void 0){e.target=this;const s=i.slice(0);for(let r=0,a=s.length;r<a;r++)s[r].call(this,e);e.target=null}}}const Rt=["00","01","02","03","04","05","06","07","08","09","0a","0b","0c","0d","0e","0f","10","11","12","13","14","15","16","17","18","19","1a","1b","1c","1d","1e","1f","20","21","22","23","24","25","26","27","28","29","2a","2b","2c","2d","2e","2f","30","31","32","33","34","35","36","37","38","39","3a","3b","3c","3d","3e","3f","40","41","42","43","44","45","46","47","48","49","4a","4b","4c","4d","4e","4f","50","51","52","53","54","55","56","57","58","59","5a","5b","5c","5d","5e","5f","60","61","62","63","64","65","66","67","68","69","6a","6b","6c","6d","6e","6f","70","71","72","73","74","75","76","77","78","79","7a","7b","7c","7d","7e","7f","80","81","82","83","84","85","86","87","88","89","8a","8b","8c","8d","8e","8f","90","91","92","93","94","95","96","97","98","99","9a","9b","9c","9d","9e","9f","a0","a1","a2","a3","a4","a5","a6","a7","a8","a9","aa","ab","ac","ad","ae","af","b0","b1","b2","b3","b4","b5","b6","b7","b8","b9","ba","bb","bc","bd","be","bf","c0","c1","c2","c3","c4","c5","c6","c7","c8","c9","ca","cb","cc","cd","ce","cf","d0","d1","d2","d3","d4","d5","d6","d7","d8","d9","da","db","dc","dd","de","df","e0","e1","e2","e3","e4","e5","e6","e7","e8","e9","ea","eb","ec","ed","ee","ef","f0","f1","f2","f3","f4","f5","f6","f7","f8","f9","fa","fb","fc","fd","fe","ff"];let yo=1234567;const zn=Math.PI/180,kn=180/Math.PI;function cn(){const n=Math.random()*4294967295|0,e=Math.random()*4294967295|0,t=Math.random()*4294967295|0,i=Math.random()*4294967295|0;return(Rt[n&255]+Rt[n>>8&255]+Rt[n>>16&255]+Rt[n>>24&255]+"-"+Rt[e&255]+Rt[e>>8&255]+"-"+Rt[e>>16&15|64]+Rt[e>>24&255]+"-"+Rt[t&63|128]+Rt[t>>8&255]+"-"+Rt[t>>16&255]+Rt[t>>24&255]+Rt[i&255]+Rt[i>>8&255]+Rt[i>>16&255]+Rt[i>>24&255]).toLowerCase()}function Ge(n,e,t){return Math.max(e,Math.min(t,n))}function Kr(n,e){return(n%e+e)%e}function qc(n,e,t,i,s){return i+(n-e)*(s-i)/(t-e)}function Yc(n,e,t){return n!==e?(t-n)/(e-n):0}function Vn(n,e,t){return(1-t)*n+t*e}function $c(n,e,t,i){return Vn(n,e,1-Math.exp(-t*i))}function Zc(n,e=1){return e-Math.abs(Kr(n,e*2)-e)}function Kc(n,e,t){return n<=e?0:n>=t?1:(n=(n-e)/(t-e),n*n*(3-2*n))}function Jc(n,e,t){return n<=e?0:n>=t?1:(n=(n-e)/(t-e),n*n*n*(n*(n*6-15)+10))}function Qc(n,e){return n+Math.floor(Math.random()*(e-n+1))}function eh(n,e){return n+Math.random()*(e-n)}function th(n){return n*(.5-Math.random())}function ih(n){n!==void 0&&(yo=n);let e=yo+=1831565813;return e=Math.imul(e^e>>>15,e|1),e^=e+Math.imul(e^e>>>7,e|61),((e^e>>>14)>>>0)/4294967296}function nh(n){return n*zn}function sh(n){return n*kn}function rh(n){return(n&n-1)===0&&n!==0}function ah(n){return Math.pow(2,Math.ceil(Math.log(n)/Math.LN2))}function oh(n){return Math.pow(2,Math.floor(Math.log(n)/Math.LN2))}function lh(n,e,t,i,s){const r=Math.cos,a=Math.sin,o=r(t/2),l=a(t/2),c=r((e+i)/2),h=a((e+i)/2),d=r((e-i)/2),u=a((e-i)/2),m=r((i-e)/2),_=a((i-e)/2);switch(s){case"XYX":n.set(o*h,l*d,l*u,o*c);break;case"YZY":n.set(l*u,o*h,l*d,o*c);break;case"ZXZ":n.set(l*d,l*u,o*h,o*c);break;case"XZX":n.set(o*h,l*_,l*m,o*c);break;case"YXY":n.set(l*m,o*h,l*_,o*c);break;case"ZYZ":n.set(l*_,l*m,o*h,o*c);break;default:Ce("MathUtils: .setQuaternionFromProperEuler() encountered an unknown order: "+s)}}function hn(n,e){switch(e.constructor){case Float32Array:return n;case Uint32Array:return n/4294967295;case Uint16Array:return n/65535;case Uint8Array:return n/255;case Int32Array:return Math.max(n/2147483647,-1);case Int16Array:return Math.max(n/32767,-1);case Int8Array:return Math.max(n/127,-1);default:throw new Error("Invalid component type.")}}function Nt(n,e){switch(e.constructor){case Float32Array:return n;case Uint32Array:return Math.round(n*4294967295);case Uint16Array:return Math.round(n*65535);case Uint8Array:return Math.round(n*255);case Int32Array:return Math.round(n*2147483647);case Int16Array:return Math.round(n*32767);case Int8Array:return Math.round(n*127);default:throw new Error("Invalid component type.")}}const Jr={DEG2RAD:zn,RAD2DEG:kn,generateUUID:cn,clamp:Ge,euclideanModulo:Kr,mapLinear:qc,inverseLerp:Yc,lerp:Vn,damp:$c,pingpong:Zc,smoothstep:Kc,smootherstep:Jc,randInt:Qc,randFloat:eh,randFloatSpread:th,seededRandom:ih,degToRad:nh,radToDeg:sh,isPowerOfTwo:rh,ceilPowerOfTwo:ah,floorPowerOfTwo:oh,setQuaternionFromProperEuler:lh,normalize:Nt,denormalize:hn},Ha=class Ha{constructor(e=0,t=0){this.x=e,this.y=t}get width(){return this.x}set width(e){this.x=e}get height(){return this.y}set height(e){this.y=e}set(e,t){return this.x=e,this.y=t,this}setScalar(e){return this.x=e,this.y=e,this}setX(e){return this.x=e,this}setY(e){return this.y=e,this}setComponent(e,t){switch(e){case 0:this.x=t;break;case 1:this.y=t;break;default:throw new Error("index is out of range: "+e)}return this}getComponent(e){switch(e){case 0:return this.x;case 1:return this.y;default:throw new Error("index is out of range: "+e)}}clone(){return new this.constructor(this.x,this.y)}copy(e){return this.x=e.x,this.y=e.y,this}add(e){return this.x+=e.x,this.y+=e.y,this}addScalar(e){return this.x+=e,this.y+=e,this}addVectors(e,t){return this.x=e.x+t.x,this.y=e.y+t.y,this}addScaledVector(e,t){return this.x+=e.x*t,this.y+=e.y*t,this}sub(e){return this.x-=e.x,this.y-=e.y,this}subScalar(e){return this.x-=e,this.y-=e,this}subVectors(e,t){return this.x=e.x-t.x,this.y=e.y-t.y,this}multiply(e){return this.x*=e.x,this.y*=e.y,this}multiplyScalar(e){return this.x*=e,this.y*=e,this}divide(e){return this.x/=e.x,this.y/=e.y,this}divideScalar(e){return this.multiplyScalar(1/e)}applyMatrix3(e){const t=this.x,i=this.y,s=e.elements;return this.x=s[0]*t+s[3]*i+s[6],this.y=s[1]*t+s[4]*i+s[7],this}min(e){return this.x=Math.min(this.x,e.x),this.y=Math.min(this.y,e.y),this}max(e){return this.x=Math.max(this.x,e.x),this.y=Math.max(this.y,e.y),this}clamp(e,t){return this.x=Ge(this.x,e.x,t.x),this.y=Ge(this.y,e.y,t.y),this}clampScalar(e,t){return this.x=Ge(this.x,e,t),this.y=Ge(this.y,e,t),this}clampLength(e,t){const i=this.length();return this.divideScalar(i||1).multiplyScalar(Ge(i,e,t))}floor(){return this.x=Math.floor(this.x),this.y=Math.floor(this.y),this}ceil(){return this.x=Math.ceil(this.x),this.y=Math.ceil(this.y),this}round(){return this.x=Math.round(this.x),this.y=Math.round(this.y),this}roundToZero(){return this.x=Math.trunc(this.x),this.y=Math.trunc(this.y),this}negate(){return this.x=-this.x,this.y=-this.y,this}dot(e){return this.x*e.x+this.y*e.y}cross(e){return this.x*e.y-this.y*e.x}lengthSq(){return this.x*this.x+this.y*this.y}length(){return Math.sqrt(this.x*this.x+this.y*this.y)}manhattanLength(){return Math.abs(this.x)+Math.abs(this.y)}normalize(){return this.divideScalar(this.length()||1)}angle(){return Math.atan2(-this.y,-this.x)+Math.PI}angleTo(e){const t=Math.sqrt(this.lengthSq()*e.lengthSq());if(t===0)return Math.PI/2;const i=this.dot(e)/t;return Math.acos(Ge(i,-1,1))}distanceTo(e){return Math.sqrt(this.distanceToSquared(e))}distanceToSquared(e){const t=this.x-e.x,i=this.y-e.y;return t*t+i*i}manhattanDistanceTo(e){return Math.abs(this.x-e.x)+Math.abs(this.y-e.y)}setLength(e){return this.normalize().multiplyScalar(e)}lerp(e,t){return this.x+=(e.x-this.x)*t,this.y+=(e.y-this.y)*t,this}lerpVectors(e,t,i){return this.x=e.x+(t.x-e.x)*i,this.y=e.y+(t.y-e.y)*i,this}equals(e){return e.x===this.x&&e.y===this.y}fromArray(e,t=0){return this.x=e[t],this.y=e[t+1],this}toArray(e=[],t=0){return e[t]=this.x,e[t+1]=this.y,e}fromBufferAttribute(e,t){return this.x=e.getX(t),this.y=e.getY(t),this}rotateAround(e,t){const i=Math.cos(t),s=Math.sin(t),r=this.x-e.x,a=this.y-e.y;return this.x=r*i-a*s+e.x,this.y=r*s+a*i+e.y,this}random(){return this.x=Math.random(),this.y=Math.random(),this}*[Symbol.iterator](){yield this.x,yield this.y}};Ha.prototype.isVector2=!0;let Ne=Ha;class Pi{constructor(e=0,t=0,i=0,s=1){this.isQuaternion=!0,this._x=e,this._y=t,this._z=i,this._w=s}static slerpFlat(e,t,i,s,r,a,o){let l=i[s+0],c=i[s+1],h=i[s+2],d=i[s+3],u=r[a+0],m=r[a+1],_=r[a+2],x=r[a+3];if(d!==x||l!==u||c!==m||h!==_){let p=l*u+c*m+h*_+d*x;p<0&&(u=-u,m=-m,_=-_,x=-x,p=-p);let f=1-o;if(p<.9995){const M=Math.acos(p),y=Math.sin(M);f=Math.sin(f*M)/y,o=Math.sin(o*M)/y,l=l*f+u*o,c=c*f+m*o,h=h*f+_*o,d=d*f+x*o}else{l=l*f+u*o,c=c*f+m*o,h=h*f+_*o,d=d*f+x*o;const M=1/Math.sqrt(l*l+c*c+h*h+d*d);l*=M,c*=M,h*=M,d*=M}}e[t]=l,e[t+1]=c,e[t+2]=h,e[t+3]=d}static multiplyQuaternionsFlat(e,t,i,s,r,a){const o=i[s],l=i[s+1],c=i[s+2],h=i[s+3],d=r[a],u=r[a+1],m=r[a+2],_=r[a+3];return e[t]=o*_+h*d+l*m-c*u,e[t+1]=l*_+h*u+c*d-o*m,e[t+2]=c*_+h*m+o*u-l*d,e[t+3]=h*_-o*d-l*u-c*m,e}get x(){return this._x}set x(e){this._x=e,this._onChangeCallback()}get y(){return this._y}set y(e){this._y=e,this._onChangeCallback()}get z(){return this._z}set z(e){this._z=e,this._onChangeCallback()}get w(){return this._w}set w(e){this._w=e,this._onChangeCallback()}set(e,t,i,s){return this._x=e,this._y=t,this._z=i,this._w=s,this._onChangeCallback(),this}clone(){return new this.constructor(this._x,this._y,this._z,this._w)}copy(e){return this._x=e.x,this._y=e.y,this._z=e.z,this._w=e.w,this._onChangeCallback(),this}setFromEuler(e,t=!0){const i=e._x,s=e._y,r=e._z,a=e._order,o=Math.cos,l=Math.sin,c=o(i/2),h=o(s/2),d=o(r/2),u=l(i/2),m=l(s/2),_=l(r/2);switch(a){case"XYZ":this._x=u*h*d+c*m*_,this._y=c*m*d-u*h*_,this._z=c*h*_+u*m*d,this._w=c*h*d-u*m*_;break;case"YXZ":this._x=u*h*d+c*m*_,this._y=c*m*d-u*h*_,this._z=c*h*_-u*m*d,this._w=c*h*d+u*m*_;break;case"ZXY":this._x=u*h*d-c*m*_,this._y=c*m*d+u*h*_,this._z=c*h*_+u*m*d,this._w=c*h*d-u*m*_;break;case"ZYX":this._x=u*h*d-c*m*_,this._y=c*m*d+u*h*_,this._z=c*h*_-u*m*d,this._w=c*h*d+u*m*_;break;case"YZX":this._x=u*h*d+c*m*_,this._y=c*m*d+u*h*_,this._z=c*h*_-u*m*d,this._w=c*h*d-u*m*_;break;case"XZY":this._x=u*h*d-c*m*_,this._y=c*m*d-u*h*_,this._z=c*h*_+u*m*d,this._w=c*h*d+u*m*_;break;default:Ce("Quaternion: .setFromEuler() encountered an unknown order: "+a)}return t===!0&&this._onChangeCallback(),this}setFromAxisAngle(e,t){const i=t/2,s=Math.sin(i);return this._x=e.x*s,this._y=e.y*s,this._z=e.z*s,this._w=Math.cos(i),this._onChangeCallback(),this}setFromRotationMatrix(e){const t=e.elements,i=t[0],s=t[4],r=t[8],a=t[1],o=t[5],l=t[9],c=t[2],h=t[6],d=t[10],u=i+o+d;if(u>0){const m=.5/Math.sqrt(u+1);this._w=.25/m,this._x=(h-l)*m,this._y=(r-c)*m,this._z=(a-s)*m}else if(i>o&&i>d){const m=2*Math.sqrt(1+i-o-d);this._w=(h-l)/m,this._x=.25*m,this._y=(s+a)/m,this._z=(r+c)/m}else if(o>d){const m=2*Math.sqrt(1+o-i-d);this._w=(r-c)/m,this._x=(s+a)/m,this._y=.25*m,this._z=(l+h)/m}else{const m=2*Math.sqrt(1+d-i-o);this._w=(a-s)/m,this._x=(r+c)/m,this._y=(l+h)/m,this._z=.25*m}return this._onChangeCallback(),this}setFromUnitVectors(e,t){let i=e.dot(t)+1;return i<1e-8?(i=0,Math.abs(e.x)>Math.abs(e.z)?(this._x=-e.y,this._y=e.x,this._z=0,this._w=i):(this._x=0,this._y=-e.z,this._z=e.y,this._w=i)):(this._x=e.y*t.z-e.z*t.y,this._y=e.z*t.x-e.x*t.z,this._z=e.x*t.y-e.y*t.x,this._w=i),this.normalize()}angleTo(e){return 2*Math.acos(Math.abs(Ge(this.dot(e),-1,1)))}rotateTowards(e,t){const i=this.angleTo(e);if(i===0)return this;const s=Math.min(1,t/i);return this.slerp(e,s),this}identity(){return this.set(0,0,0,1)}invert(){return this.conjugate()}conjugate(){return this._x*=-1,this._y*=-1,this._z*=-1,this._onChangeCallback(),this}dot(e){return this._x*e._x+this._y*e._y+this._z*e._z+this._w*e._w}lengthSq(){return this._x*this._x+this._y*this._y+this._z*this._z+this._w*this._w}length(){return Math.sqrt(this._x*this._x+this._y*this._y+this._z*this._z+this._w*this._w)}normalize(){let e=this.length();return e===0?(this._x=0,this._y=0,this._z=0,this._w=1):(e=1/e,this._x=this._x*e,this._y=this._y*e,this._z=this._z*e,this._w=this._w*e),this._onChangeCallback(),this}multiply(e){return this.multiplyQuaternions(this,e)}premultiply(e){return this.multiplyQuaternions(e,this)}multiplyQuaternions(e,t){const i=e._x,s=e._y,r=e._z,a=e._w,o=t._x,l=t._y,c=t._z,h=t._w;return this._x=i*h+a*o+s*c-r*l,this._y=s*h+a*l+r*o-i*c,this._z=r*h+a*c+i*l-s*o,this._w=a*h-i*o-s*l-r*c,this._onChangeCallback(),this}slerp(e,t){let i=e._x,s=e._y,r=e._z,a=e._w,o=this.dot(e);o<0&&(i=-i,s=-s,r=-r,a=-a,o=-o);let l=1-t;if(o<.9995){const c=Math.acos(o),h=Math.sin(c);l=Math.sin(l*c)/h,t=Math.sin(t*c)/h,this._x=this._x*l+i*t,this._y=this._y*l+s*t,this._z=this._z*l+r*t,this._w=this._w*l+a*t,this._onChangeCallback()}else this._x=this._x*l+i*t,this._y=this._y*l+s*t,this._z=this._z*l+r*t,this._w=this._w*l+a*t,this.normalize();return this}slerpQuaternions(e,t,i){return this.copy(e).slerp(t,i)}random(){const e=2*Math.PI*Math.random(),t=2*Math.PI*Math.random(),i=Math.random(),s=Math.sqrt(1-i),r=Math.sqrt(i);return this.set(s*Math.sin(e),s*Math.cos(e),r*Math.sin(t),r*Math.cos(t))}equals(e){return e._x===this._x&&e._y===this._y&&e._z===this._z&&e._w===this._w}fromArray(e,t=0){return this._x=e[t],this._y=e[t+1],this._z=e[t+2],this._w=e[t+3],this._onChangeCallback(),this}toArray(e=[],t=0){return e[t]=this._x,e[t+1]=this._y,e[t+2]=this._z,e[t+3]=this._w,e}fromBufferAttribute(e,t){return this._x=e.getX(t),this._y=e.getY(t),this._z=e.getZ(t),this._w=e.getW(t),this._onChangeCallback(),this}toJSON(){return this.toArray()}_onChange(e){return this._onChangeCallback=e,this}_onChangeCallback(){}*[Symbol.iterator](){yield this._x,yield this._y,yield this._z,yield this._w}}const Wa=class Wa{constructor(e=0,t=0,i=0){this.x=e,this.y=t,this.z=i}set(e,t,i){return i===void 0&&(i=this.z),this.x=e,this.y=t,this.z=i,this}setScalar(e){return this.x=e,this.y=e,this.z=e,this}setX(e){return this.x=e,this}setY(e){return this.y=e,this}setZ(e){return this.z=e,this}setComponent(e,t){switch(e){case 0:this.x=t;break;case 1:this.y=t;break;case 2:this.z=t;break;default:throw new Error("index is out of range: "+e)}return this}getComponent(e){switch(e){case 0:return this.x;case 1:return this.y;case 2:return this.z;default:throw new Error("index is out of range: "+e)}}clone(){return new this.constructor(this.x,this.y,this.z)}copy(e){return this.x=e.x,this.y=e.y,this.z=e.z,this}add(e){return this.x+=e.x,this.y+=e.y,this.z+=e.z,this}addScalar(e){return this.x+=e,this.y+=e,this.z+=e,this}addVectors(e,t){return this.x=e.x+t.x,this.y=e.y+t.y,this.z=e.z+t.z,this}addScaledVector(e,t){return this.x+=e.x*t,this.y+=e.y*t,this.z+=e.z*t,this}sub(e){return this.x-=e.x,this.y-=e.y,this.z-=e.z,this}subScalar(e){return this.x-=e,this.y-=e,this.z-=e,this}subVectors(e,t){return this.x=e.x-t.x,this.y=e.y-t.y,this.z=e.z-t.z,this}multiply(e){return this.x*=e.x,this.y*=e.y,this.z*=e.z,this}multiplyScalar(e){return this.x*=e,this.y*=e,this.z*=e,this}multiplyVectors(e,t){return this.x=e.x*t.x,this.y=e.y*t.y,this.z=e.z*t.z,this}applyEuler(e){return this.applyQuaternion(Eo.setFromEuler(e))}applyAxisAngle(e,t){return this.applyQuaternion(Eo.setFromAxisAngle(e,t))}applyMatrix3(e){const t=this.x,i=this.y,s=this.z,r=e.elements;return this.x=r[0]*t+r[3]*i+r[6]*s,this.y=r[1]*t+r[4]*i+r[7]*s,this.z=r[2]*t+r[5]*i+r[8]*s,this}applyNormalMatrix(e){return this.applyMatrix3(e).normalize()}applyMatrix4(e){const t=this.x,i=this.y,s=this.z,r=e.elements,a=1/(r[3]*t+r[7]*i+r[11]*s+r[15]);return this.x=(r[0]*t+r[4]*i+r[8]*s+r[12])*a,this.y=(r[1]*t+r[5]*i+r[9]*s+r[13])*a,this.z=(r[2]*t+r[6]*i+r[10]*s+r[14])*a,this}applyQuaternion(e){const t=this.x,i=this.y,s=this.z,r=e.x,a=e.y,o=e.z,l=e.w,c=2*(a*s-o*i),h=2*(o*t-r*s),d=2*(r*i-a*t);return this.x=t+l*c+a*d-o*h,this.y=i+l*h+o*c-r*d,this.z=s+l*d+r*h-a*c,this}project(e){return this.applyMatrix4(e.matrixWorldInverse).applyMatrix4(e.projectionMatrix)}unproject(e){return this.applyMatrix4(e.projectionMatrixInverse).applyMatrix4(e.matrixWorld)}transformDirection(e){const t=this.x,i=this.y,s=this.z,r=e.elements;return this.x=r[0]*t+r[4]*i+r[8]*s,this.y=r[1]*t+r[5]*i+r[9]*s,this.z=r[2]*t+r[6]*i+r[10]*s,this.normalize()}divide(e){return this.x/=e.x,this.y/=e.y,this.z/=e.z,this}divideScalar(e){return this.multiplyScalar(1/e)}min(e){return this.x=Math.min(this.x,e.x),this.y=Math.min(this.y,e.y),this.z=Math.min(this.z,e.z),this}max(e){return this.x=Math.max(this.x,e.x),this.y=Math.max(this.y,e.y),this.z=Math.max(this.z,e.z),this}clamp(e,t){return this.x=Ge(this.x,e.x,t.x),this.y=Ge(this.y,e.y,t.y),this.z=Ge(this.z,e.z,t.z),this}clampScalar(e,t){return this.x=Ge(this.x,e,t),this.y=Ge(this.y,e,t),this.z=Ge(this.z,e,t),this}clampLength(e,t){const i=this.length();return this.divideScalar(i||1).multiplyScalar(Ge(i,e,t))}floor(){return this.x=Math.floor(this.x),this.y=Math.floor(this.y),this.z=Math.floor(this.z),this}ceil(){return this.x=Math.ceil(this.x),this.y=Math.ceil(this.y),this.z=Math.ceil(this.z),this}round(){return this.x=Math.round(this.x),this.y=Math.round(this.y),this.z=Math.round(this.z),this}roundToZero(){return this.x=Math.trunc(this.x),this.y=Math.trunc(this.y),this.z=Math.trunc(this.z),this}negate(){return this.x=-this.x,this.y=-this.y,this.z=-this.z,this}dot(e){return this.x*e.x+this.y*e.y+this.z*e.z}lengthSq(){return this.x*this.x+this.y*this.y+this.z*this.z}length(){return Math.sqrt(this.x*this.x+this.y*this.y+this.z*this.z)}manhattanLength(){return Math.abs(this.x)+Math.abs(this.y)+Math.abs(this.z)}normalize(){return this.divideScalar(this.length()||1)}setLength(e){return this.normalize().multiplyScalar(e)}lerp(e,t){return this.x+=(e.x-this.x)*t,this.y+=(e.y-this.y)*t,this.z+=(e.z-this.z)*t,this}lerpVectors(e,t,i){return this.x=e.x+(t.x-e.x)*i,this.y=e.y+(t.y-e.y)*i,this.z=e.z+(t.z-e.z)*i,this}cross(e){return this.crossVectors(this,e)}crossVectors(e,t){const i=e.x,s=e.y,r=e.z,a=t.x,o=t.y,l=t.z;return this.x=s*l-r*o,this.y=r*a-i*l,this.z=i*o-s*a,this}projectOnVector(e){const t=e.lengthSq();if(t===0)return this.set(0,0,0);const i=e.dot(this)/t;return this.copy(e).multiplyScalar(i)}projectOnPlane(e){return Qr.copy(this).projectOnVector(e),this.sub(Qr)}reflect(e){return this.sub(Qr.copy(e).multiplyScalar(2*this.dot(e)))}angleTo(e){const t=Math.sqrt(this.lengthSq()*e.lengthSq());if(t===0)return Math.PI/2;const i=this.dot(e)/t;return Math.acos(Ge(i,-1,1))}distanceTo(e){return Math.sqrt(this.distanceToSquared(e))}distanceToSquared(e){const t=this.x-e.x,i=this.y-e.y,s=this.z-e.z;return t*t+i*i+s*s}manhattanDistanceTo(e){return Math.abs(this.x-e.x)+Math.abs(this.y-e.y)+Math.abs(this.z-e.z)}setFromSpherical(e){return this.setFromSphericalCoords(e.radius,e.phi,e.theta)}setFromSphericalCoords(e,t,i){const s=Math.sin(t)*e;return this.x=s*Math.sin(i),this.y=Math.cos(t)*e,this.z=s*Math.cos(i),this}setFromCylindrical(e){return this.setFromCylindricalCoords(e.radius,e.theta,e.y)}setFromCylindricalCoords(e,t,i){return this.x=e*Math.sin(t),this.y=i,this.z=e*Math.cos(t),this}setFromMatrixPosition(e){const t=e.elements;return this.x=t[12],this.y=t[13],this.z=t[14],this}setFromMatrixScale(e){const t=this.setFromMatrixColumn(e,0).length(),i=this.setFromMatrixColumn(e,1).length(),s=this.setFromMatrixColumn(e,2).length();return this.x=t,this.y=i,this.z=s,this}setFromMatrixColumn(e,t){return this.fromArray(e.elements,t*4)}setFromMatrix3Column(e,t){return this.fromArray(e.elements,t*3)}setFromEuler(e){return this.x=e._x,this.y=e._y,this.z=e._z,this}setFromColor(e){return this.x=e.r,this.y=e.g,this.z=e.b,this}equals(e){return e.x===this.x&&e.y===this.y&&e.z===this.z}fromArray(e,t=0){return this.x=e[t],this.y=e[t+1],this.z=e[t+2],this}toArray(e=[],t=0){return e[t]=this.x,e[t+1]=this.y,e[t+2]=this.z,e}fromBufferAttribute(e,t){return this.x=e.getX(t),this.y=e.getY(t),this.z=e.getZ(t),this}random(){return this.x=Math.random(),this.y=Math.random(),this.z=Math.random(),this}randomDirection(){const e=Math.random()*Math.PI*2,t=Math.random()*2-1,i=Math.sqrt(1-t*t);return this.x=i*Math.cos(e),this.y=t,this.z=i*Math.sin(e),this}*[Symbol.iterator](){yield this.x,yield this.y,yield this.z}};Wa.prototype.isVector3=!0;let A=Wa;const Qr=new A,Eo=new Pi,Xa=class Xa{constructor(e,t,i,s,r,a,o,l,c){this.elements=[1,0,0,0,1,0,0,0,1],e!==void 0&&this.set(e,t,i,s,r,a,o,l,c)}set(e,t,i,s,r,a,o,l,c){const h=this.elements;return h[0]=e,h[1]=s,h[2]=o,h[3]=t,h[4]=r,h[5]=l,h[6]=i,h[7]=a,h[8]=c,this}identity(){return this.set(1,0,0,0,1,0,0,0,1),this}copy(e){const t=this.elements,i=e.elements;return t[0]=i[0],t[1]=i[1],t[2]=i[2],t[3]=i[3],t[4]=i[4],t[5]=i[5],t[6]=i[6],t[7]=i[7],t[8]=i[8],this}extractBasis(e,t,i){return e.setFromMatrix3Column(this,0),t.setFromMatrix3Column(this,1),i.setFromMatrix3Column(this,2),this}setFromMatrix4(e){const t=e.elements;return this.set(t[0],t[4],t[8],t[1],t[5],t[9],t[2],t[6],t[10]),this}multiply(e){return this.multiplyMatrices(this,e)}premultiply(e){return this.multiplyMatrices(e,this)}multiplyMatrices(e,t){const i=e.elements,s=t.elements,r=this.elements,a=i[0],o=i[3],l=i[6],c=i[1],h=i[4],d=i[7],u=i[2],m=i[5],_=i[8],x=s[0],p=s[3],f=s[6],M=s[1],y=s[4],E=s[7],L=s[2],w=s[5],D=s[8];return r[0]=a*x+o*M+l*L,r[3]=a*p+o*y+l*w,r[6]=a*f+o*E+l*D,r[1]=c*x+h*M+d*L,r[4]=c*p+h*y+d*w,r[7]=c*f+h*E+d*D,r[2]=u*x+m*M+_*L,r[5]=u*p+m*y+_*w,r[8]=u*f+m*E+_*D,this}multiplyScalar(e){const t=this.elements;return t[0]*=e,t[3]*=e,t[6]*=e,t[1]*=e,t[4]*=e,t[7]*=e,t[2]*=e,t[5]*=e,t[8]*=e,this}determinant(){const e=this.elements,t=e[0],i=e[1],s=e[2],r=e[3],a=e[4],o=e[5],l=e[6],c=e[7],h=e[8];return t*a*h-t*o*c-i*r*h+i*o*l+s*r*c-s*a*l}invert(){const e=this.elements,t=e[0],i=e[1],s=e[2],r=e[3],a=e[4],o=e[5],l=e[6],c=e[7],h=e[8],d=h*a-o*c,u=o*l-h*r,m=c*r-a*l,_=t*d+i*u+s*m;if(_===0)return this.set(0,0,0,0,0,0,0,0,0);const x=1/_;return e[0]=d*x,e[1]=(s*c-h*i)*x,e[2]=(o*i-s*a)*x,e[3]=u*x,e[4]=(h*t-s*l)*x,e[5]=(s*r-o*t)*x,e[6]=m*x,e[7]=(i*l-c*t)*x,e[8]=(a*t-i*r)*x,this}transpose(){let e;const t=this.elements;return e=t[1],t[1]=t[3],t[3]=e,e=t[2],t[2]=t[6],t[6]=e,e=t[5],t[5]=t[7],t[7]=e,this}getNormalMatrix(e){return this.setFromMatrix4(e).invert().transpose()}transposeIntoArray(e){const t=this.elements;return e[0]=t[0],e[1]=t[3],e[2]=t[6],e[3]=t[1],e[4]=t[4],e[5]=t[7],e[6]=t[2],e[7]=t[5],e[8]=t[8],this}setUvTransform(e,t,i,s,r,a,o){const l=Math.cos(r),c=Math.sin(r);return this.set(i*l,i*c,-i*(l*a+c*o)+a+e,-s*c,s*l,-s*(-c*a+l*o)+o+t,0,0,1),this}scale(e,t){return this.premultiply(ea.makeScale(e,t)),this}rotate(e){return this.premultiply(ea.makeRotation(-e)),this}translate(e,t){return this.premultiply(ea.makeTranslation(e,t)),this}makeTranslation(e,t){return e.isVector2?this.set(1,0,e.x,0,1,e.y,0,0,1):this.set(1,0,e,0,1,t,0,0,1),this}makeRotation(e){const t=Math.cos(e),i=Math.sin(e);return this.set(t,-i,0,i,t,0,0,0,1),this}makeScale(e,t){return this.set(e,0,0,0,t,0,0,0,1),this}equals(e){const t=this.elements,i=e.elements;for(let s=0;s<9;s++)if(t[s]!==i[s])return!1;return!0}fromArray(e,t=0){for(let i=0;i<9;i++)this.elements[i]=e[i+t];return this}toArray(e=[],t=0){const i=this.elements;return e[t]=i[0],e[t+1]=i[1],e[t+2]=i[2],e[t+3]=i[3],e[t+4]=i[4],e[t+5]=i[5],e[t+6]=i[6],e[t+7]=i[7],e[t+8]=i[8],e}clone(){return new this.constructor().fromArray(this.elements)}};Xa.prototype.isMatrix3=!0;let Ue=Xa;const ea=new Ue,wo=new Ue().set(.4123908,.3575843,.1804808,.212639,.7151687,.0721923,.0193308,.1191948,.9505322),To=new Ue().set(3.2409699,-1.5373832,-.4986108,-.9692436,1.8759675,.0415551,.0556301,-.203977,1.0569715);function ch(){const n={enabled:!0,workingColorSpace:ls,spaces:{},convert:function(s,r,a){return this.enabled===!1||r===a||!r||!a||(this.spaces[r].transfer===Qe&&(s.r=bi(s.r),s.g=bi(s.g),s.b=bi(s.b)),this.spaces[r].primaries!==this.spaces[a].primaries&&(s.applyMatrix3(this.spaces[r].toXYZ),s.applyMatrix3(this.spaces[a].fromXYZ)),this.spaces[a].transfer===Qe&&(s.r=un(s.r),s.g=un(s.g),s.b=un(s.b))),s},workingToColorSpace:function(s,r){return this.convert(s,this.workingColorSpace,r)},colorSpaceToWorking:function(s,r){return this.convert(s,r,this.workingColorSpace)},getPrimaries:function(s){return this.spaces[s].primaries},getTransfer:function(s){return s===Ci?cs:this.spaces[s].transfer},getToneMappingMode:function(s){return this.spaces[s].outputColorSpaceConfig.toneMappingMode||"standard"},getLuminanceCoefficients:function(s,r=this.workingColorSpace){return s.fromArray(this.spaces[r].luminanceCoefficients)},define:function(s){Object.assign(this.spaces,s)},_getMatrix:function(s,r,a){return s.copy(this.spaces[r].toXYZ).multiply(this.spaces[a].fromXYZ)},_getDrawingBufferColorSpace:function(s){return this.spaces[s].outputColorSpaceConfig.drawingBufferColorSpace},_getUnpackColorSpace:function(s=this.workingColorSpace){return this.spaces[s].workingColorSpaceConfig.unpackColorSpace},fromWorkingColorSpace:function(s,r){return Zr("ColorManagement: .fromWorkingColorSpace() has been renamed to .workingToColorSpace()."),n.workingToColorSpace(s,r)},toWorkingColorSpace:function(s,r){return Zr("ColorManagement: .toWorkingColorSpace() has been renamed to .colorSpaceToWorking()."),n.colorSpaceToWorking(s,r)}},e=[.64,.33,.3,.6,.15,.06],t=[.2126,.7152,.0722],i=[.3127,.329];return n.define({[ls]:{primaries:e,whitePoint:i,transfer:cs,toXYZ:wo,fromXYZ:To,luminanceCoefficients:t,workingColorSpaceConfig:{unpackColorSpace:jt},outputColorSpaceConfig:{drawingBufferColorSpace:jt}},[jt]:{primaries:e,whitePoint:i,transfer:Qe,toXYZ:wo,fromXYZ:To,luminanceCoefficients:t,outputColorSpaceConfig:{drawingBufferColorSpace:jt}}}),n}const Xe=ch();function bi(n){return n<.04045?n*.0773993808:Math.pow(n*.9478672986+.0521327014,2.4)}function un(n){return n<.0031308?n*12.92:1.055*Math.pow(n,.41666)-.055}let dn;class hh{static getDataURL(e,t="image/png"){if(/^data:/i.test(e.src)||typeof HTMLCanvasElement>"u")return e.src;let i;if(e instanceof HTMLCanvasElement)i=e;else{dn===void 0&&(dn=us("canvas")),dn.width=e.width,dn.height=e.height;const s=dn.getContext("2d");e instanceof ImageData?s.putImageData(e,0,0):s.drawImage(e,0,0,e.width,e.height),i=dn}return i.toDataURL(t)}static sRGBToLinear(e){if(typeof HTMLImageElement<"u"&&e instanceof HTMLImageElement||typeof HTMLCanvasElement<"u"&&e instanceof HTMLCanvasElement||typeof ImageBitmap<"u"&&e instanceof ImageBitmap){const t=us("canvas");t.width=e.width,t.height=e.height;const i=t.getContext("2d");i.drawImage(e,0,0,e.width,e.height);const s=i.getImageData(0,0,e.width,e.height),r=s.data;for(let a=0;a<r.length;a++)r[a]=bi(r[a]/255)*255;return i.putImageData(s,0,0),t}else if(e.data){const t=e.data.slice(0);for(let i=0;i<t.length;i++)t instanceof Uint8Array||t instanceof Uint8ClampedArray?t[i]=Math.floor(bi(t[i]/255)*255):t[i]=bi(t[i]);return{data:t,width:e.width,height:e.height}}else return Ce("ImageUtils.sRGBToLinear(): Unsupported image type. No color space conversion applied."),e}}let uh=0;class ta{constructor(e=null){this.isSource=!0,Object.defineProperty(this,"id",{value:uh++}),this.uuid=cn(),this.data=e,this.dataReady=!0,this.version=0}getSize(e){const t=this.data;return typeof HTMLVideoElement<"u"&&t instanceof HTMLVideoElement?e.set(t.videoWidth,t.videoHeight,0):typeof VideoFrame<"u"&&t instanceof VideoFrame?e.set(t.displayWidth,t.displayHeight,0):t!==null?e.set(t.width,t.height,t.depth||0):e.set(0,0,0),e}set needsUpdate(e){e===!0&&this.version++}toJSON(e){const t=e===void 0||typeof e=="string";if(!t&&e.images[this.uuid]!==void 0)return e.images[this.uuid];const i={uuid:this.uuid,url:""},s=this.data;if(s!==null){let r;if(Array.isArray(s)){r=[];for(let a=0,o=s.length;a<o;a++)s[a].isDataTexture?r.push(ia(s[a].image)):r.push(ia(s[a]))}else r=ia(s);i.url=r}return t||(e.images[this.uuid]=i),i}}function ia(n){return typeof HTMLImageElement<"u"&&n instanceof HTMLImageElement||typeof HTMLCanvasElement<"u"&&n instanceof HTMLCanvasElement||typeof ImageBitmap<"u"&&n instanceof ImageBitmap?hh.getDataURL(n):n.data?{data:Array.from(n.data),width:n.width,height:n.height,type:n.data.constructor.name}:(Ce("Texture: Unable to serialize Texture."),{})}let dh=0;const na=new A;class Ut extends Ri{constructor(e=Ut.DEFAULT_IMAGE,t=Ut.DEFAULT_MAPPING,i=_i,s=_i,r=Ct,a=ji,o=Qt,l=kt,c=Ut.DEFAULT_ANISOTROPY,h=Ci){super(),this.isTexture=!0,Object.defineProperty(this,"id",{value:dh++}),this.uuid=cn(),this.name="",this.source=new ta(e),this.mipmaps=[],this.mapping=t,this.channel=0,this.wrapS=i,this.wrapT=s,this.magFilter=r,this.minFilter=a,this.anisotropy=c,this.format=o,this.internalFormat=null,this.type=l,this.offset=new Ne(0,0),this.repeat=new Ne(1,1),this.center=new Ne(0,0),this.rotation=0,this.matrixAutoUpdate=!0,this.matrix=new Ue,this.generateMipmaps=!0,this.premultiplyAlpha=!1,this.flipY=!0,this.unpackAlignment=4,this.colorSpace=h,this.userData={},this.updateRanges=[],this.version=0,this.onUpdate=null,this.renderTarget=null,this.isRenderTargetTexture=!1,this.isArrayTexture=!!(e&&e.depth&&e.depth>1),this.pmremVersion=0,this.normalized=!1}get width(){return this.source.getSize(na).x}get height(){return this.source.getSize(na).y}get depth(){return this.source.getSize(na).z}get image(){return this.source.data}set image(e){this.source.data=e}updateMatrix(){this.matrix.setUvTransform(this.offset.x,this.offset.y,this.repeat.x,this.repeat.y,this.rotation,this.center.x,this.center.y)}addUpdateRange(e,t){this.updateRanges.push({start:e,count:t})}clearUpdateRanges(){this.updateRanges.length=0}clone(){return new this.constructor().copy(this)}copy(e){return this.name=e.name,this.source=e.source,this.mipmaps=e.mipmaps.slice(0),this.mapping=e.mapping,this.channel=e.channel,this.wrapS=e.wrapS,this.wrapT=e.wrapT,this.magFilter=e.magFilter,this.minFilter=e.minFilter,this.anisotropy=e.anisotropy,this.format=e.format,this.internalFormat=e.internalFormat,this.type=e.type,this.normalized=e.normalized,this.offset.copy(e.offset),this.repeat.copy(e.repeat),this.center.copy(e.center),this.rotation=e.rotation,this.matrixAutoUpdate=e.matrixAutoUpdate,this.matrix.copy(e.matrix),this.generateMipmaps=e.generateMipmaps,this.premultiplyAlpha=e.premultiplyAlpha,this.flipY=e.flipY,this.unpackAlignment=e.unpackAlignment,this.colorSpace=e.colorSpace,this.renderTarget=e.renderTarget,this.isRenderTargetTexture=e.isRenderTargetTexture,this.isArrayTexture=e.isArrayTexture,this.userData=JSON.parse(JSON.stringify(e.userData)),this.needsUpdate=!0,this}setValues(e){for(const t in e){const i=e[t];if(i===void 0){Ce(`Texture.setValues(): parameter '${t}' has value of undefined.`);continue}const s=this[t];if(s===void 0){Ce(`Texture.setValues(): property '${t}' does not exist.`);continue}s&&i&&s.isVector2&&i.isVector2||s&&i&&s.isVector3&&i.isVector3||s&&i&&s.isMatrix3&&i.isMatrix3?s.copy(i):this[t]=i}}toJSON(e){const t=e===void 0||typeof e=="string";if(!t&&e.textures[this.uuid]!==void 0)return e.textures[this.uuid];const i={metadata:{version:4.7,type:"Texture",generator:"Texture.toJSON"},uuid:this.uuid,name:this.name,image:this.source.toJSON(e).uuid,mapping:this.mapping,channel:this.channel,repeat:[this.repeat.x,this.repeat.y],offset:[this.offset.x,this.offset.y],center:[this.center.x,this.center.y],rotation:this.rotation,wrap:[this.wrapS,this.wrapT],format:this.format,internalFormat:this.internalFormat,type:this.type,normalized:this.normalized,colorSpace:this.colorSpace,minFilter:this.minFilter,magFilter:this.magFilter,anisotropy:this.anisotropy,flipY:this.flipY,generateMipmaps:this.generateMipmaps,premultiplyAlpha:this.premultiplyAlpha,unpackAlignment:this.unpackAlignment};return Object.keys(this.userData).length>0&&(i.userData=this.userData),t||(e.textures[this.uuid]=i),i}dispose(){this.dispatchEvent({type:"dispose"})}transformUv(e){if(this.mapping!==lo)return e;if(e.applyMatrix3(this.matrix),e.x<0||e.x>1)switch(this.wrapS){case lr:e.x=e.x-Math.floor(e.x);break;case _i:e.x=e.x<0?0:1;break;case cr:Math.abs(Math.floor(e.x)%2)===1?e.x=Math.ceil(e.x)-e.x:e.x=e.x-Math.floor(e.x);break}if(e.y<0||e.y>1)switch(this.wrapT){case lr:e.y=e.y-Math.floor(e.y);break;case _i:e.y=e.y<0?0:1;break;case cr:Math.abs(Math.floor(e.y)%2)===1?e.y=Math.ceil(e.y)-e.y:e.y=e.y-Math.floor(e.y);break}return this.flipY&&(e.y=1-e.y),e}set needsUpdate(e){e===!0&&(this.version++,this.source.needsUpdate=!0)}set needsPMREMUpdate(e){e===!0&&this.pmremVersion++}}Ut.DEFAULT_IMAGE=null,Ut.DEFAULT_MAPPING=lo,Ut.DEFAULT_ANISOTROPY=1;const ja=class ja{constructor(e=0,t=0,i=0,s=1){this.x=e,this.y=t,this.z=i,this.w=s}get width(){return this.z}set width(e){this.z=e}get height(){return this.w}set height(e){this.w=e}set(e,t,i,s){return this.x=e,this.y=t,this.z=i,this.w=s,this}setScalar(e){return this.x=e,this.y=e,this.z=e,this.w=e,this}setX(e){return this.x=e,this}setY(e){return this.y=e,this}setZ(e){return this.z=e,this}setW(e){return this.w=e,this}setComponent(e,t){switch(e){case 0:this.x=t;break;case 1:this.y=t;break;case 2:this.z=t;break;case 3:this.w=t;break;default:throw new Error("index is out of range: "+e)}return this}getComponent(e){switch(e){case 0:return this.x;case 1:return this.y;case 2:return this.z;case 3:return this.w;default:throw new Error("index is out of range: "+e)}}clone(){return new this.constructor(this.x,this.y,this.z,this.w)}copy(e){return this.x=e.x,this.y=e.y,this.z=e.z,this.w=e.w!==void 0?e.w:1,this}add(e){return this.x+=e.x,this.y+=e.y,this.z+=e.z,this.w+=e.w,this}addScalar(e){return this.x+=e,this.y+=e,this.z+=e,this.w+=e,this}addVectors(e,t){return this.x=e.x+t.x,this.y=e.y+t.y,this.z=e.z+t.z,this.w=e.w+t.w,this}addScaledVector(e,t){return this.x+=e.x*t,this.y+=e.y*t,this.z+=e.z*t,this.w+=e.w*t,this}sub(e){return this.x-=e.x,this.y-=e.y,this.z-=e.z,this.w-=e.w,this}subScalar(e){return this.x-=e,this.y-=e,this.z-=e,this.w-=e,this}subVectors(e,t){return this.x=e.x-t.x,this.y=e.y-t.y,this.z=e.z-t.z,this.w=e.w-t.w,this}multiply(e){return this.x*=e.x,this.y*=e.y,this.z*=e.z,this.w*=e.w,this}multiplyScalar(e){return this.x*=e,this.y*=e,this.z*=e,this.w*=e,this}applyMatrix4(e){const t=this.x,i=this.y,s=this.z,r=this.w,a=e.elements;return this.x=a[0]*t+a[4]*i+a[8]*s+a[12]*r,this.y=a[1]*t+a[5]*i+a[9]*s+a[13]*r,this.z=a[2]*t+a[6]*i+a[10]*s+a[14]*r,this.w=a[3]*t+a[7]*i+a[11]*s+a[15]*r,this}divide(e){return this.x/=e.x,this.y/=e.y,this.z/=e.z,this.w/=e.w,this}divideScalar(e){return this.multiplyScalar(1/e)}setAxisAngleFromQuaternion(e){this.w=2*Math.acos(e.w);const t=Math.sqrt(1-e.w*e.w);return t<1e-4?(this.x=1,this.y=0,this.z=0):(this.x=e.x/t,this.y=e.y/t,this.z=e.z/t),this}setAxisAngleFromRotationMatrix(e){let t,i,s,r;const l=e.elements,c=l[0],h=l[4],d=l[8],u=l[1],m=l[5],_=l[9],x=l[2],p=l[6],f=l[10];if(Math.abs(h-u)<.01&&Math.abs(d-x)<.01&&Math.abs(_-p)<.01){if(Math.abs(h+u)<.1&&Math.abs(d+x)<.1&&Math.abs(_+p)<.1&&Math.abs(c+m+f-3)<.1)return this.set(1,0,0,0),this;t=Math.PI;const y=(c+1)/2,E=(m+1)/2,L=(f+1)/2,w=(h+u)/4,D=(d+x)/4,b=(_+p)/4;return y>E&&y>L?y<.01?(i=0,s=.707106781,r=.707106781):(i=Math.sqrt(y),s=w/i,r=D/i):E>L?E<.01?(i=.707106781,s=0,r=.707106781):(s=Math.sqrt(E),i=w/s,r=b/s):L<.01?(i=.707106781,s=.707106781,r=0):(r=Math.sqrt(L),i=D/r,s=b/r),this.set(i,s,r,t),this}let M=Math.sqrt((p-_)*(p-_)+(d-x)*(d-x)+(u-h)*(u-h));return Math.abs(M)<.001&&(M=1),this.x=(p-_)/M,this.y=(d-x)/M,this.z=(u-h)/M,this.w=Math.acos((c+m+f-1)/2),this}setFromMatrixPosition(e){const t=e.elements;return this.x=t[12],this.y=t[13],this.z=t[14],this.w=t[15],this}min(e){return this.x=Math.min(this.x,e.x),this.y=Math.min(this.y,e.y),this.z=Math.min(this.z,e.z),this.w=Math.min(this.w,e.w),this}max(e){return this.x=Math.max(this.x,e.x),this.y=Math.max(this.y,e.y),this.z=Math.max(this.z,e.z),this.w=Math.max(this.w,e.w),this}clamp(e,t){return this.x=Ge(this.x,e.x,t.x),this.y=Ge(this.y,e.y,t.y),this.z=Ge(this.z,e.z,t.z),this.w=Ge(this.w,e.w,t.w),this}clampScalar(e,t){return this.x=Ge(this.x,e,t),this.y=Ge(this.y,e,t),this.z=Ge(this.z,e,t),this.w=Ge(this.w,e,t),this}clampLength(e,t){const i=this.length();return this.divideScalar(i||1).multiplyScalar(Ge(i,e,t))}floor(){return this.x=Math.floor(this.x),this.y=Math.floor(this.y),this.z=Math.floor(this.z),this.w=Math.floor(this.w),this}ceil(){return this.x=Math.ceil(this.x),this.y=Math.ceil(this.y),this.z=Math.ceil(this.z),this.w=Math.ceil(this.w),this}round(){return this.x=Math.round(this.x),this.y=Math.round(this.y),this.z=Math.round(this.z),this.w=Math.round(this.w),this}roundToZero(){return this.x=Math.trunc(this.x),this.y=Math.trunc(this.y),this.z=Math.trunc(this.z),this.w=Math.trunc(this.w),this}negate(){return this.x=-this.x,this.y=-this.y,this.z=-this.z,this.w=-this.w,this}dot(e){return this.x*e.x+this.y*e.y+this.z*e.z+this.w*e.w}lengthSq(){return this.x*this.x+this.y*this.y+this.z*this.z+this.w*this.w}length(){return Math.sqrt(this.x*this.x+this.y*this.y+this.z*this.z+this.w*this.w)}manhattanLength(){return Math.abs(this.x)+Math.abs(this.y)+Math.abs(this.z)+Math.abs(this.w)}normalize(){return this.divideScalar(this.length()||1)}setLength(e){return this.normalize().multiplyScalar(e)}lerp(e,t){return this.x+=(e.x-this.x)*t,this.y+=(e.y-this.y)*t,this.z+=(e.z-this.z)*t,this.w+=(e.w-this.w)*t,this}lerpVectors(e,t,i){return this.x=e.x+(t.x-e.x)*i,this.y=e.y+(t.y-e.y)*i,this.z=e.z+(t.z-e.z)*i,this.w=e.w+(t.w-e.w)*i,this}equals(e){return e.x===this.x&&e.y===this.y&&e.z===this.z&&e.w===this.w}fromArray(e,t=0){return this.x=e[t],this.y=e[t+1],this.z=e[t+2],this.w=e[t+3],this}toArray(e=[],t=0){return e[t]=this.x,e[t+1]=this.y,e[t+2]=this.z,e[t+3]=this.w,e}fromBufferAttribute(e,t){return this.x=e.getX(t),this.y=e.getY(t),this.z=e.getZ(t),this.w=e.getW(t),this}random(){return this.x=Math.random(),this.y=Math.random(),this.z=Math.random(),this.w=Math.random(),this}*[Symbol.iterator](){yield this.x,yield this.y,yield this.z,yield this.w}};ja.prototype.isVector4=!0;let ht=ja;class fh extends Ri{constructor(e=1,t=1,i={}){super(),i=Object.assign({generateMipmaps:!1,internalFormat:null,minFilter:Ct,depthBuffer:!0,stencilBuffer:!1,resolveDepthBuffer:!0,resolveStencilBuffer:!0,depthTexture:null,samples:0,count:1,depth:1,multiview:!1},i),this.isRenderTarget=!0,this.width=e,this.height=t,this.depth=i.depth,this.scissor=new ht(0,0,e,t),this.scissorTest=!1,this.viewport=new ht(0,0,e,t),this.textures=[];const s={width:e,height:t,depth:i.depth},r=new Ut(s),a=i.count;for(let o=0;o<a;o++)this.textures[o]=r.clone(),this.textures[o].isRenderTargetTexture=!0,this.textures[o].renderTarget=this;this._setTextureOptions(i),this.depthBuffer=i.depthBuffer,this.stencilBuffer=i.stencilBuffer,this.resolveDepthBuffer=i.resolveDepthBuffer,this.resolveStencilBuffer=i.resolveStencilBuffer,this._depthTexture=null,this.depthTexture=i.depthTexture,this.samples=i.samples,this.multiview=i.multiview}_setTextureOptions(e={}){const t={minFilter:Ct,generateMipmaps:!1,flipY:!1,internalFormat:null};e.mapping!==void 0&&(t.mapping=e.mapping),e.wrapS!==void 0&&(t.wrapS=e.wrapS),e.wrapT!==void 0&&(t.wrapT=e.wrapT),e.wrapR!==void 0&&(t.wrapR=e.wrapR),e.magFilter!==void 0&&(t.magFilter=e.magFilter),e.minFilter!==void 0&&(t.minFilter=e.minFilter),e.format!==void 0&&(t.format=e.format),e.type!==void 0&&(t.type=e.type),e.anisotropy!==void 0&&(t.anisotropy=e.anisotropy),e.colorSpace!==void 0&&(t.colorSpace=e.colorSpace),e.flipY!==void 0&&(t.flipY=e.flipY),e.generateMipmaps!==void 0&&(t.generateMipmaps=e.generateMipmaps),e.internalFormat!==void 0&&(t.internalFormat=e.internalFormat);for(let i=0;i<this.textures.length;i++)this.textures[i].setValues(t)}get texture(){return this.textures[0]}set texture(e){this.textures[0]=e}set depthTexture(e){this._depthTexture!==null&&(this._depthTexture.renderTarget=null),e!==null&&(e.renderTarget=this),this._depthTexture=e}get depthTexture(){return this._depthTexture}setSize(e,t,i=1){if(this.width!==e||this.height!==t||this.depth!==i){this.width=e,this.height=t,this.depth=i;for(let s=0,r=this.textures.length;s<r;s++)this.textures[s].image.width=e,this.textures[s].image.height=t,this.textures[s].image.depth=i,this.textures[s].isData3DTexture!==!0&&(this.textures[s].isArrayTexture=this.textures[s].image.depth>1);this.dispose()}this.viewport.set(0,0,e,t),this.scissor.set(0,0,e,t)}clone(){return new this.constructor().copy(this)}copy(e){this.width=e.width,this.height=e.height,this.depth=e.depth,this.scissor.copy(e.scissor),this.scissorTest=e.scissorTest,this.viewport.copy(e.viewport),this.textures.length=0;for(let t=0,i=e.textures.length;t<i;t++){this.textures[t]=e.textures[t].clone(),this.textures[t].isRenderTargetTexture=!0,this.textures[t].renderTarget=this;const s=Object.assign({},e.textures[t].image);this.textures[t].source=new ta(s)}return this.depthBuffer=e.depthBuffer,this.stencilBuffer=e.stencilBuffer,this.resolveDepthBuffer=e.resolveDepthBuffer,this.resolveStencilBuffer=e.resolveStencilBuffer,e.depthTexture!==null&&(this.depthTexture=e.depthTexture.clone()),this.samples=e.samples,this.multiview=e.multiview,this}dispose(){this.dispatchEvent({type:"dispose"})}}class li extends fh{constructor(e=1,t=1,i={}){super(e,t,i),this.isWebGLRenderTarget=!0}}class Ao extends Ut{constructor(e=null,t=1,i=1,s=1){super(null),this.isDataArrayTexture=!0,this.image={data:e,width:t,height:i,depth:s},this.magFilter=Tt,this.minFilter=Tt,this.wrapR=_i,this.generateMipmaps=!1,this.flipY=!1,this.unpackAlignment=1,this.layerUpdates=new Set}addLayerUpdate(e){this.layerUpdates.add(e)}clearLayerUpdates(){this.layerUpdates.clear()}}class ph extends Ut{constructor(e=null,t=1,i=1,s=1){super(null),this.isData3DTexture=!0,this.image={data:e,width:t,height:i,depth:s},this.magFilter=Tt,this.minFilter=Tt,this.wrapR=_i,this.generateMipmaps=!1,this.flipY=!1,this.unpackAlignment=1}}const qs=class qs{constructor(e,t,i,s,r,a,o,l,c,h,d,u,m,_,x,p){this.elements=[1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1],e!==void 0&&this.set(e,t,i,s,r,a,o,l,c,h,d,u,m,_,x,p)}set(e,t,i,s,r,a,o,l,c,h,d,u,m,_,x,p){const f=this.elements;return f[0]=e,f[4]=t,f[8]=i,f[12]=s,f[1]=r,f[5]=a,f[9]=o,f[13]=l,f[2]=c,f[6]=h,f[10]=d,f[14]=u,f[3]=m,f[7]=_,f[11]=x,f[15]=p,this}identity(){return this.set(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),this}clone(){return new qs().fromArray(this.elements)}copy(e){const t=this.elements,i=e.elements;return t[0]=i[0],t[1]=i[1],t[2]=i[2],t[3]=i[3],t[4]=i[4],t[5]=i[5],t[6]=i[6],t[7]=i[7],t[8]=i[8],t[9]=i[9],t[10]=i[10],t[11]=i[11],t[12]=i[12],t[13]=i[13],t[14]=i[14],t[15]=i[15],this}copyPosition(e){const t=this.elements,i=e.elements;return t[12]=i[12],t[13]=i[13],t[14]=i[14],this}setFromMatrix3(e){const t=e.elements;return this.set(t[0],t[3],t[6],0,t[1],t[4],t[7],0,t[2],t[5],t[8],0,0,0,0,1),this}extractBasis(e,t,i){return this.determinant()===0?(e.set(1,0,0),t.set(0,1,0),i.set(0,0,1),this):(e.setFromMatrixColumn(this,0),t.setFromMatrixColumn(this,1),i.setFromMatrixColumn(this,2),this)}makeBasis(e,t,i){return this.set(e.x,t.x,i.x,0,e.y,t.y,i.y,0,e.z,t.z,i.z,0,0,0,0,1),this}extractRotation(e){if(e.determinant()===0)return this.identity();const t=this.elements,i=e.elements,s=1/fn.setFromMatrixColumn(e,0).length(),r=1/fn.setFromMatrixColumn(e,1).length(),a=1/fn.setFromMatrixColumn(e,2).length();return t[0]=i[0]*s,t[1]=i[1]*s,t[2]=i[2]*s,t[3]=0,t[4]=i[4]*r,t[5]=i[5]*r,t[6]=i[6]*r,t[7]=0,t[8]=i[8]*a,t[9]=i[9]*a,t[10]=i[10]*a,t[11]=0,t[12]=0,t[13]=0,t[14]=0,t[15]=1,this}makeRotationFromEuler(e){const t=this.elements,i=e.x,s=e.y,r=e.z,a=Math.cos(i),o=Math.sin(i),l=Math.cos(s),c=Math.sin(s),h=Math.cos(r),d=Math.sin(r);if(e.order==="XYZ"){const u=a*h,m=a*d,_=o*h,x=o*d;t[0]=l*h,t[4]=-l*d,t[8]=c,t[1]=m+_*c,t[5]=u-x*c,t[9]=-o*l,t[2]=x-u*c,t[6]=_+m*c,t[10]=a*l}else if(e.order==="YXZ"){const u=l*h,m=l*d,_=c*h,x=c*d;t[0]=u+x*o,t[4]=_*o-m,t[8]=a*c,t[1]=a*d,t[5]=a*h,t[9]=-o,t[2]=m*o-_,t[6]=x+u*o,t[10]=a*l}else if(e.order==="ZXY"){const u=l*h,m=l*d,_=c*h,x=c*d;t[0]=u-x*o,t[4]=-a*d,t[8]=_+m*o,t[1]=m+_*o,t[5]=a*h,t[9]=x-u*o,t[2]=-a*c,t[6]=o,t[10]=a*l}else if(e.order==="ZYX"){const u=a*h,m=a*d,_=o*h,x=o*d;t[0]=l*h,t[4]=_*c-m,t[8]=u*c+x,t[1]=l*d,t[5]=x*c+u,t[9]=m*c-_,t[2]=-c,t[6]=o*l,t[10]=a*l}else if(e.order==="YZX"){const u=a*l,m=a*c,_=o*l,x=o*c;t[0]=l*h,t[4]=x-u*d,t[8]=_*d+m,t[1]=d,t[5]=a*h,t[9]=-o*h,t[2]=-c*h,t[6]=m*d+_,t[10]=u-x*d}else if(e.order==="XZY"){const u=a*l,m=a*c,_=o*l,x=o*c;t[0]=l*h,t[4]=-d,t[8]=c*h,t[1]=u*d+x,t[5]=a*h,t[9]=m*d-_,t[2]=_*d-m,t[6]=o*h,t[10]=x*d+u}return t[3]=0,t[7]=0,t[11]=0,t[12]=0,t[13]=0,t[14]=0,t[15]=1,this}makeRotationFromQuaternion(e){return this.compose(mh,e,gh)}lookAt(e,t,i){const s=this.elements;return Vt.subVectors(e,t),Vt.lengthSq()===0&&(Vt.z=1),Vt.normalize(),Li.crossVectors(i,Vt),Li.lengthSq()===0&&(Math.abs(i.z)===1?Vt.x+=1e-4:Vt.z+=1e-4,Vt.normalize(),Li.crossVectors(i,Vt)),Li.normalize(),ds.crossVectors(Vt,Li),s[0]=Li.x,s[4]=ds.x,s[8]=Vt.x,s[1]=Li.y,s[5]=ds.y,s[9]=Vt.y,s[2]=Li.z,s[6]=ds.z,s[10]=Vt.z,this}multiply(e){return this.multiplyMatrices(this,e)}premultiply(e){return this.multiplyMatrices(e,this)}multiplyMatrices(e,t){const i=e.elements,s=t.elements,r=this.elements,a=i[0],o=i[4],l=i[8],c=i[12],h=i[1],d=i[5],u=i[9],m=i[13],_=i[2],x=i[6],p=i[10],f=i[14],M=i[3],y=i[7],E=i[11],L=i[15],w=s[0],D=s[4],b=s[8],T=s[12],U=s[1],C=s[5],z=s[9],X=s[13],j=s[2],N=s[6],V=s[10],H=s[14],K=s[3],ee=s[7],ce=s[11],be=s[15];return r[0]=a*w+o*U+l*j+c*K,r[4]=a*D+o*C+l*N+c*ee,r[8]=a*b+o*z+l*V+c*ce,r[12]=a*T+o*X+l*H+c*be,r[1]=h*w+d*U+u*j+m*K,r[5]=h*D+d*C+u*N+m*ee,r[9]=h*b+d*z+u*V+m*ce,r[13]=h*T+d*X+u*H+m*be,r[2]=_*w+x*U+p*j+f*K,r[6]=_*D+x*C+p*N+f*ee,r[10]=_*b+x*z+p*V+f*ce,r[14]=_*T+x*X+p*H+f*be,r[3]=M*w+y*U+E*j+L*K,r[7]=M*D+y*C+E*N+L*ee,r[11]=M*b+y*z+E*V+L*ce,r[15]=M*T+y*X+E*H+L*be,this}multiplyScalar(e){const t=this.elements;return t[0]*=e,t[4]*=e,t[8]*=e,t[12]*=e,t[1]*=e,t[5]*=e,t[9]*=e,t[13]*=e,t[2]*=e,t[6]*=e,t[10]*=e,t[14]*=e,t[3]*=e,t[7]*=e,t[11]*=e,t[15]*=e,this}determinant(){const e=this.elements,t=e[0],i=e[4],s=e[8],r=e[12],a=e[1],o=e[5],l=e[9],c=e[13],h=e[2],d=e[6],u=e[10],m=e[14],_=e[3],x=e[7],p=e[11],f=e[15],M=l*m-c*u,y=o*m-c*d,E=o*u-l*d,L=a*m-c*h,w=a*u-l*h,D=a*d-o*h;return t*(x*M-p*y+f*E)-i*(_*M-p*L+f*w)+s*(_*y-x*L+f*D)-r*(_*E-x*w+p*D)}transpose(){const e=this.elements;let t;return t=e[1],e[1]=e[4],e[4]=t,t=e[2],e[2]=e[8],e[8]=t,t=e[6],e[6]=e[9],e[9]=t,t=e[3],e[3]=e[12],e[12]=t,t=e[7],e[7]=e[13],e[13]=t,t=e[11],e[11]=e[14],e[14]=t,this}setPosition(e,t,i){const s=this.elements;return e.isVector3?(s[12]=e.x,s[13]=e.y,s[14]=e.z):(s[12]=e,s[13]=t,s[14]=i),this}invert(){const e=this.elements,t=e[0],i=e[1],s=e[2],r=e[3],a=e[4],o=e[5],l=e[6],c=e[7],h=e[8],d=e[9],u=e[10],m=e[11],_=e[12],x=e[13],p=e[14],f=e[15],M=t*o-i*a,y=t*l-s*a,E=t*c-r*a,L=i*l-s*o,w=i*c-r*o,D=s*c-r*l,b=h*x-d*_,T=h*p-u*_,U=h*f-m*_,C=d*p-u*x,z=d*f-m*x,X=u*f-m*p,j=M*X-y*z+E*C+L*U-w*T+D*b;if(j===0)return this.set(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);const N=1/j;return e[0]=(o*X-l*z+c*C)*N,e[1]=(s*z-i*X-r*C)*N,e[2]=(x*D-p*w+f*L)*N,e[3]=(u*w-d*D-m*L)*N,e[4]=(l*U-a*X-c*T)*N,e[5]=(t*X-s*U+r*T)*N,e[6]=(p*E-_*D-f*y)*N,e[7]=(h*D-u*E+m*y)*N,e[8]=(a*z-o*U+c*b)*N,e[9]=(i*U-t*z-r*b)*N,e[10]=(_*w-x*E+f*M)*N,e[11]=(d*E-h*w-m*M)*N,e[12]=(o*T-a*C-l*b)*N,e[13]=(t*C-i*T+s*b)*N,e[14]=(x*y-_*L-p*M)*N,e[15]=(h*L-d*y+u*M)*N,this}scale(e){const t=this.elements,i=e.x,s=e.y,r=e.z;return t[0]*=i,t[4]*=s,t[8]*=r,t[1]*=i,t[5]*=s,t[9]*=r,t[2]*=i,t[6]*=s,t[10]*=r,t[3]*=i,t[7]*=s,t[11]*=r,this}getMaxScaleOnAxis(){const e=this.elements,t=e[0]*e[0]+e[1]*e[1]+e[2]*e[2],i=e[4]*e[4]+e[5]*e[5]+e[6]*e[6],s=e[8]*e[8]+e[9]*e[9]+e[10]*e[10];return Math.sqrt(Math.max(t,i,s))}makeTranslation(e,t,i){return e.isVector3?this.set(1,0,0,e.x,0,1,0,e.y,0,0,1,e.z,0,0,0,1):this.set(1,0,0,e,0,1,0,t,0,0,1,i,0,0,0,1),this}makeRotationX(e){const t=Math.cos(e),i=Math.sin(e);return this.set(1,0,0,0,0,t,-i,0,0,i,t,0,0,0,0,1),this}makeRotationY(e){const t=Math.cos(e),i=Math.sin(e);return this.set(t,0,i,0,0,1,0,0,-i,0,t,0,0,0,0,1),this}makeRotationZ(e){const t=Math.cos(e),i=Math.sin(e);return this.set(t,-i,0,0,i,t,0,0,0,0,1,0,0,0,0,1),this}makeRotationAxis(e,t){const i=Math.cos(t),s=Math.sin(t),r=1-i,a=e.x,o=e.y,l=e.z,c=r*a,h=r*o;return this.set(c*a+i,c*o-s*l,c*l+s*o,0,c*o+s*l,h*o+i,h*l-s*a,0,c*l-s*o,h*l+s*a,r*l*l+i,0,0,0,0,1),this}makeScale(e,t,i){return this.set(e,0,0,0,0,t,0,0,0,0,i,0,0,0,0,1),this}makeShear(e,t,i,s,r,a){return this.set(1,i,r,0,e,1,a,0,t,s,1,0,0,0,0,1),this}compose(e,t,i){const s=this.elements,r=t._x,a=t._y,o=t._z,l=t._w,c=r+r,h=a+a,d=o+o,u=r*c,m=r*h,_=r*d,x=a*h,p=a*d,f=o*d,M=l*c,y=l*h,E=l*d,L=i.x,w=i.y,D=i.z;return s[0]=(1-(x+f))*L,s[1]=(m+E)*L,s[2]=(_-y)*L,s[3]=0,s[4]=(m-E)*w,s[5]=(1-(u+f))*w,s[6]=(p+M)*w,s[7]=0,s[8]=(_+y)*D,s[9]=(p-M)*D,s[10]=(1-(u+x))*D,s[11]=0,s[12]=e.x,s[13]=e.y,s[14]=e.z,s[15]=1,this}decompose(e,t,i){const s=this.elements;e.x=s[12],e.y=s[13],e.z=s[14];const r=this.determinant();if(r===0)return i.set(1,1,1),t.identity(),this;let a=fn.set(s[0],s[1],s[2]).length();const o=fn.set(s[4],s[5],s[6]).length(),l=fn.set(s[8],s[9],s[10]).length();r<0&&(a=-a),ei.copy(this);const c=1/a,h=1/o,d=1/l;return ei.elements[0]*=c,ei.elements[1]*=c,ei.elements[2]*=c,ei.elements[4]*=h,ei.elements[5]*=h,ei.elements[6]*=h,ei.elements[8]*=d,ei.elements[9]*=d,ei.elements[10]*=d,t.setFromRotationMatrix(ei),i.x=a,i.y=o,i.z=l,this}makePerspective(e,t,i,s,r,a,o=oi,l=!1){const c=this.elements,h=2*r/(t-e),d=2*r/(i-s),u=(t+e)/(t-e),m=(i+s)/(i-s);let _,x;if(l)_=r/(a-r),x=a*r/(a-r);else if(o===oi)_=-(a+r)/(a-r),x=-2*a*r/(a-r);else if(o===Bn)_=-a/(a-r),x=-a*r/(a-r);else throw new Error("THREE.Matrix4.makePerspective(): Invalid coordinate system: "+o);return c[0]=h,c[4]=0,c[8]=u,c[12]=0,c[1]=0,c[5]=d,c[9]=m,c[13]=0,c[2]=0,c[6]=0,c[10]=_,c[14]=x,c[3]=0,c[7]=0,c[11]=-1,c[15]=0,this}makeOrthographic(e,t,i,s,r,a,o=oi,l=!1){const c=this.elements,h=2/(t-e),d=2/(i-s),u=-(t+e)/(t-e),m=-(i+s)/(i-s);let _,x;if(l)_=1/(a-r),x=a/(a-r);else if(o===oi)_=-2/(a-r),x=-(a+r)/(a-r);else if(o===Bn)_=-1/(a-r),x=-r/(a-r);else throw new Error("THREE.Matrix4.makeOrthographic(): Invalid coordinate system: "+o);return c[0]=h,c[4]=0,c[8]=0,c[12]=u,c[1]=0,c[5]=d,c[9]=0,c[13]=m,c[2]=0,c[6]=0,c[10]=_,c[14]=x,c[3]=0,c[7]=0,c[11]=0,c[15]=1,this}equals(e){const t=this.elements,i=e.elements;for(let s=0;s<16;s++)if(t[s]!==i[s])return!1;return!0}fromArray(e,t=0){for(let i=0;i<16;i++)this.elements[i]=e[i+t];return this}toArray(e=[],t=0){const i=this.elements;return e[t]=i[0],e[t+1]=i[1],e[t+2]=i[2],e[t+3]=i[3],e[t+4]=i[4],e[t+5]=i[5],e[t+6]=i[6],e[t+7]=i[7],e[t+8]=i[8],e[t+9]=i[9],e[t+10]=i[10],e[t+11]=i[11],e[t+12]=i[12],e[t+13]=i[13],e[t+14]=i[14],e[t+15]=i[15],e}};qs.prototype.isMatrix4=!0;let lt=qs;const fn=new A,ei=new lt,mh=new A(0,0,0),gh=new A(1,1,1),Li=new A,ds=new A,Vt=new A,Co=new lt,Ro=new Pi;class Di{constructor(e=0,t=0,i=0,s=Di.DEFAULT_ORDER){this.isEuler=!0,this._x=e,this._y=t,this._z=i,this._order=s}get x(){return this._x}set x(e){this._x=e,this._onChangeCallback()}get y(){return this._y}set y(e){this._y=e,this._onChangeCallback()}get z(){return this._z}set z(e){this._z=e,this._onChangeCallback()}get order(){return this._order}set order(e){this._order=e,this._onChangeCallback()}set(e,t,i,s=this._order){return this._x=e,this._y=t,this._z=i,this._order=s,this._onChangeCallback(),this}clone(){return new this.constructor(this._x,this._y,this._z,this._order)}copy(e){return this._x=e._x,this._y=e._y,this._z=e._z,this._order=e._order,this._onChangeCallback(),this}setFromRotationMatrix(e,t=this._order,i=!0){const s=e.elements,r=s[0],a=s[4],o=s[8],l=s[1],c=s[5],h=s[9],d=s[2],u=s[6],m=s[10];switch(t){case"XYZ":this._y=Math.asin(Ge(o,-1,1)),Math.abs(o)<.9999999?(this._x=Math.atan2(-h,m),this._z=Math.atan2(-a,r)):(this._x=Math.atan2(u,c),this._z=0);break;case"YXZ":this._x=Math.asin(-Ge(h,-1,1)),Math.abs(h)<.9999999?(this._y=Math.atan2(o,m),this._z=Math.atan2(l,c)):(this._y=Math.atan2(-d,r),this._z=0);break;case"ZXY":this._x=Math.asin(Ge(u,-1,1)),Math.abs(u)<.9999999?(this._y=Math.atan2(-d,m),this._z=Math.atan2(-a,c)):(this._y=0,this._z=Math.atan2(l,r));break;case"ZYX":this._y=Math.asin(-Ge(d,-1,1)),Math.abs(d)<.9999999?(this._x=Math.atan2(u,m),this._z=Math.atan2(l,r)):(this._x=0,this._z=Math.atan2(-a,c));break;case"YZX":this._z=Math.asin(Ge(l,-1,1)),Math.abs(l)<.9999999?(this._x=Math.atan2(-h,c),this._y=Math.atan2(-d,r)):(this._x=0,this._y=Math.atan2(o,m));break;case"XZY":this._z=Math.asin(-Ge(a,-1,1)),Math.abs(a)<.9999999?(this._x=Math.atan2(u,c),this._y=Math.atan2(o,r)):(this._x=Math.atan2(-h,m),this._y=0);break;default:Ce("Euler: .setFromRotationMatrix() encountered an unknown order: "+t)}return this._order=t,i===!0&&this._onChangeCallback(),this}setFromQuaternion(e,t,i){return Co.makeRotationFromQuaternion(e),this.setFromRotationMatrix(Co,t,i)}setFromVector3(e,t=this._order){return this.set(e.x,e.y,e.z,t)}reorder(e){return Ro.setFromEuler(this),this.setFromQuaternion(Ro,e)}equals(e){return e._x===this._x&&e._y===this._y&&e._z===this._z&&e._order===this._order}fromArray(e){return this._x=e[0],this._y=e[1],this._z=e[2],e[3]!==void 0&&(this._order=e[3]),this._onChangeCallback(),this}toArray(e=[],t=0){return e[t]=this._x,e[t+1]=this._y,e[t+2]=this._z,e[t+3]=this._order,e}_onChange(e){return this._onChangeCallback=e,this}_onChangeCallback(){}*[Symbol.iterator](){yield this._x,yield this._y,yield this._z,yield this._order}}Di.DEFAULT_ORDER="XYZ";let sa=class{constructor(){this.mask=1}set(e){this.mask=(1<<e|0)>>>0}enable(e){this.mask|=1<<e|0}enableAll(){this.mask=-1}toggle(e){this.mask^=1<<e|0}disable(e){this.mask&=~(1<<e|0)}disableAll(){this.mask=0}test(e){return(this.mask&e.mask)!==0}isEnabled(e){return(this.mask&(1<<e|0))!==0}},_h=0;const Po=new A,pn=new Pi,Si=new lt,fs=new A,Gn=new A,vh=new A,xh=new Pi,Lo=new A(1,0,0),Do=new A(0,1,0),Io=new A(0,0,1),No={type:"added"},bh={type:"removed"},mn={type:"childadded",child:null},ra={type:"childremoved",child:null};class xt extends Ri{constructor(){super(),this.isObject3D=!0,Object.defineProperty(this,"id",{value:_h++}),this.uuid=cn(),this.name="",this.type="Object3D",this.parent=null,this.children=[],this.up=xt.DEFAULT_UP.clone();const e=new A,t=new Di,i=new Pi,s=new A(1,1,1);function r(){i.setFromEuler(t,!1)}function a(){t.setFromQuaternion(i,void 0,!1)}t._onChange(r),i._onChange(a),Object.defineProperties(this,{position:{configurable:!0,enumerable:!0,value:e},rotation:{configurable:!0,enumerable:!0,value:t},quaternion:{configurable:!0,enumerable:!0,value:i},scale:{configurable:!0,enumerable:!0,value:s},modelViewMatrix:{value:new lt},normalMatrix:{value:new Ue}}),this.matrix=new lt,this.matrixWorld=new lt,this.matrixAutoUpdate=xt.DEFAULT_MATRIX_AUTO_UPDATE,this.matrixWorldAutoUpdate=xt.DEFAULT_MATRIX_WORLD_AUTO_UPDATE,this.matrixWorldNeedsUpdate=!1,this.layers=new sa,this.visible=!0,this.castShadow=!1,this.receiveShadow=!1,this.frustumCulled=!0,this.renderOrder=0,this.animations=[],this.customDepthMaterial=void 0,this.customDistanceMaterial=void 0,this.static=!1,this.userData={},this.pivot=null}onBeforeShadow(){}onAfterShadow(){}onBeforeRender(){}onAfterRender(){}applyMatrix4(e){this.matrixAutoUpdate&&this.updateMatrix(),this.matrix.premultiply(e),this.matrix.decompose(this.position,this.quaternion,this.scale)}applyQuaternion(e){return this.quaternion.premultiply(e),this}setRotationFromAxisAngle(e,t){this.quaternion.setFromAxisAngle(e,t)}setRotationFromEuler(e){this.quaternion.setFromEuler(e,!0)}setRotationFromMatrix(e){this.quaternion.setFromRotationMatrix(e)}setRotationFromQuaternion(e){this.quaternion.copy(e)}rotateOnAxis(e,t){return pn.setFromAxisAngle(e,t),this.quaternion.multiply(pn),this}rotateOnWorldAxis(e,t){return pn.setFromAxisAngle(e,t),this.quaternion.premultiply(pn),this}rotateX(e){return this.rotateOnAxis(Lo,e)}rotateY(e){return this.rotateOnAxis(Do,e)}rotateZ(e){return this.rotateOnAxis(Io,e)}translateOnAxis(e,t){return Po.copy(e).applyQuaternion(this.quaternion),this.position.add(Po.multiplyScalar(t)),this}translateX(e){return this.translateOnAxis(Lo,e)}translateY(e){return this.translateOnAxis(Do,e)}translateZ(e){return this.translateOnAxis(Io,e)}localToWorld(e){return this.updateWorldMatrix(!0,!1),e.applyMatrix4(this.matrixWorld)}worldToLocal(e){return this.updateWorldMatrix(!0,!1),e.applyMatrix4(Si.copy(this.matrixWorld).invert())}lookAt(e,t,i){e.isVector3?fs.copy(e):fs.set(e,t,i);const s=this.parent;this.updateWorldMatrix(!0,!1),Gn.setFromMatrixPosition(this.matrixWorld),this.isCamera||this.isLight?Si.lookAt(Gn,fs,this.up):Si.lookAt(fs,Gn,this.up),this.quaternion.setFromRotationMatrix(Si),s&&(Si.extractRotation(s.matrixWorld),pn.setFromRotationMatrix(Si),this.quaternion.premultiply(pn.invert()))}add(e){if(arguments.length>1){for(let t=0;t<arguments.length;t++)this.add(arguments[t]);return this}return e===this?(je("Object3D.add: object can't be added as a child of itself.",e),this):(e&&e.isObject3D?(e.removeFromParent(),e.parent=this,this.children.push(e),e.dispatchEvent(No),mn.child=e,this.dispatchEvent(mn),mn.child=null):je("Object3D.add: object not an instance of THREE.Object3D.",e),this)}remove(e){if(arguments.length>1){for(let i=0;i<arguments.length;i++)this.remove(arguments[i]);return this}const t=this.children.indexOf(e);return t!==-1&&(e.parent=null,this.children.splice(t,1),e.dispatchEvent(bh),ra.child=e,this.dispatchEvent(ra),ra.child=null),this}removeFromParent(){const e=this.parent;return e!==null&&e.remove(this),this}clear(){return this.remove(...this.children)}attach(e){return this.updateWorldMatrix(!0,!1),Si.copy(this.matrixWorld).invert(),e.parent!==null&&(e.parent.updateWorldMatrix(!0,!1),Si.multiply(e.parent.matrixWorld)),e.applyMatrix4(Si),e.removeFromParent(),e.parent=this,this.children.push(e),e.updateWorldMatrix(!1,!0),e.dispatchEvent(No),mn.child=e,this.dispatchEvent(mn),mn.child=null,this}getObjectById(e){return this.getObjectByProperty("id",e)}getObjectByName(e){return this.getObjectByProperty("name",e)}getObjectByProperty(e,t){if(this[e]===t)return this;for(let i=0,s=this.children.length;i<s;i++){const a=this.children[i].getObjectByProperty(e,t);if(a!==void 0)return a}}getObjectsByProperty(e,t,i=[]){this[e]===t&&i.push(this);const s=this.children;for(let r=0,a=s.length;r<a;r++)s[r].getObjectsByProperty(e,t,i);return i}getWorldPosition(e){return this.updateWorldMatrix(!0,!1),e.setFromMatrixPosition(this.matrixWorld)}getWorldQuaternion(e){return this.updateWorldMatrix(!0,!1),this.matrixWorld.decompose(Gn,e,vh),e}getWorldScale(e){return this.updateWorldMatrix(!0,!1),this.matrixWorld.decompose(Gn,xh,e),e}getWorldDirection(e){this.updateWorldMatrix(!0,!1);const t=this.matrixWorld.elements;return e.set(t[8],t[9],t[10]).normalize()}raycast(){}traverse(e){e(this);const t=this.children;for(let i=0,s=t.length;i<s;i++)t[i].traverse(e)}traverseVisible(e){if(this.visible===!1)return;e(this);const t=this.children;for(let i=0,s=t.length;i<s;i++)t[i].traverseVisible(e)}traverseAncestors(e){const t=this.parent;t!==null&&(e(t),t.traverseAncestors(e))}updateMatrix(){this.matrix.compose(this.position,this.quaternion,this.scale);const e=this.pivot;if(e!==null){const t=e.x,i=e.y,s=e.z,r=this.matrix.elements;r[12]+=t-r[0]*t-r[4]*i-r[8]*s,r[13]+=i-r[1]*t-r[5]*i-r[9]*s,r[14]+=s-r[2]*t-r[6]*i-r[10]*s}this.matrixWorldNeedsUpdate=!0}updateMatrixWorld(e){this.matrixAutoUpdate&&this.updateMatrix(),(this.matrixWorldNeedsUpdate||e)&&(this.matrixWorldAutoUpdate===!0&&(this.parent===null?this.matrixWorld.copy(this.matrix):this.matrixWorld.multiplyMatrices(this.parent.matrixWorld,this.matrix)),this.matrixWorldNeedsUpdate=!1,e=!0);const t=this.children;for(let i=0,s=t.length;i<s;i++)t[i].updateMatrixWorld(e)}updateWorldMatrix(e,t){const i=this.parent;if(e===!0&&i!==null&&i.updateWorldMatrix(!0,!1),this.matrixAutoUpdate&&this.updateMatrix(),this.matrixWorldAutoUpdate===!0&&(this.parent===null?this.matrixWorld.copy(this.matrix):this.matrixWorld.multiplyMatrices(this.parent.matrixWorld,this.matrix)),t===!0){const s=this.children;for(let r=0,a=s.length;r<a;r++)s[r].updateWorldMatrix(!1,!0)}}toJSON(e){const t=e===void 0||typeof e=="string",i={};t&&(e={geometries:{},materials:{},textures:{},images:{},shapes:{},skeletons:{},animations:{},nodes:{}},i.metadata={version:4.7,type:"Object",generator:"Object3D.toJSON"});const s={};s.uuid=this.uuid,s.type=this.type,this.name!==""&&(s.name=this.name),this.castShadow===!0&&(s.castShadow=!0),this.receiveShadow===!0&&(s.receiveShadow=!0),this.visible===!1&&(s.visible=!1),this.frustumCulled===!1&&(s.frustumCulled=!1),this.renderOrder!==0&&(s.renderOrder=this.renderOrder),this.static!==!1&&(s.static=this.static),Object.keys(this.userData).length>0&&(s.userData=this.userData),s.layers=this.layers.mask,s.matrix=this.matrix.toArray(),s.up=this.up.toArray(),this.pivot!==null&&(s.pivot=this.pivot.toArray()),this.matrixAutoUpdate===!1&&(s.matrixAutoUpdate=!1),this.morphTargetDictionary!==void 0&&(s.morphTargetDictionary=Object.assign({},this.morphTargetDictionary)),this.morphTargetInfluences!==void 0&&(s.morphTargetInfluences=this.morphTargetInfluences.slice()),this.isInstancedMesh&&(s.type="InstancedMesh",s.count=this.count,s.instanceMatrix=this.instanceMatrix.toJSON(),this.instanceColor!==null&&(s.instanceColor=this.instanceColor.toJSON())),this.isBatchedMesh&&(s.type="BatchedMesh",s.perObjectFrustumCulled=this.perObjectFrustumCulled,s.sortObjects=this.sortObjects,s.drawRanges=this._drawRanges,s.reservedRanges=this._reservedRanges,s.geometryInfo=this._geometryInfo.map(o=>({...o,boundingBox:o.boundingBox?o.boundingBox.toJSON():void 0,boundingSphere:o.boundingSphere?o.boundingSphere.toJSON():void 0})),s.instanceInfo=this._instanceInfo.map(o=>({...o})),s.availableInstanceIds=this._availableInstanceIds.slice(),s.availableGeometryIds=this._availableGeometryIds.slice(),s.nextIndexStart=this._nextIndexStart,s.nextVertexStart=this._nextVertexStart,s.geometryCount=this._geometryCount,s.maxInstanceCount=this._maxInstanceCount,s.maxVertexCount=this._maxVertexCount,s.maxIndexCount=this._maxIndexCount,s.geometryInitialized=this._geometryInitialized,s.matricesTexture=this._matricesTexture.toJSON(e),s.indirectTexture=this._indirectTexture.toJSON(e),this._colorsTexture!==null&&(s.colorsTexture=this._colorsTexture.toJSON(e)),this.boundingSphere!==null&&(s.boundingSphere=this.boundingSphere.toJSON()),this.boundingBox!==null&&(s.boundingBox=this.boundingBox.toJSON()));function r(o,l){return o[l.uuid]===void 0&&(o[l.uuid]=l.toJSON(e)),l.uuid}if(this.isScene)this.background&&(this.background.isColor?s.background=this.background.toJSON():this.background.isTexture&&(s.background=this.background.toJSON(e).uuid)),this.environment&&this.environment.isTexture&&this.environment.isRenderTargetTexture!==!0&&(s.environment=this.environment.toJSON(e).uuid);else if(this.isMesh||this.isLine||this.isPoints){s.geometry=r(e.geometries,this.geometry);const o=this.geometry.parameters;if(o!==void 0&&o.shapes!==void 0){const l=o.shapes;if(Array.isArray(l))for(let c=0,h=l.length;c<h;c++){const d=l[c];r(e.shapes,d)}else r(e.shapes,l)}}if(this.isSkinnedMesh&&(s.bindMode=this.bindMode,s.bindMatrix=this.bindMatrix.toArray(),this.skeleton!==void 0&&(r(e.skeletons,this.skeleton),s.skeleton=this.skeleton.uuid)),this.material!==void 0)if(Array.isArray(this.material)){const o=[];for(let l=0,c=this.material.length;l<c;l++)o.push(r(e.materials,this.material[l]));s.material=o}else s.material=r(e.materials,this.material);if(this.children.length>0){s.children=[];for(let o=0;o<this.children.length;o++)s.children.push(this.children[o].toJSON(e).object)}if(this.animations.length>0){s.animations=[];for(let o=0;o<this.animations.length;o++){const l=this.animations[o];s.animations.push(r(e.animations,l))}}if(t){const o=a(e.geometries),l=a(e.materials),c=a(e.textures),h=a(e.images),d=a(e.shapes),u=a(e.skeletons),m=a(e.animations),_=a(e.nodes);o.length>0&&(i.geometries=o),l.length>0&&(i.materials=l),c.length>0&&(i.textures=c),h.length>0&&(i.images=h),d.length>0&&(i.shapes=d),u.length>0&&(i.skeletons=u),m.length>0&&(i.animations=m),_.length>0&&(i.nodes=_)}return i.object=s,i;function a(o){const l=[];for(const c in o){const h=o[c];delete h.metadata,l.push(h)}return l}}clone(e){return new this.constructor().copy(this,e)}copy(e,t=!0){if(this.name=e.name,this.up.copy(e.up),this.position.copy(e.position),this.rotation.order=e.rotation.order,this.quaternion.copy(e.quaternion),this.scale.copy(e.scale),this.pivot=e.pivot!==null?e.pivot.clone():null,this.matrix.copy(e.matrix),this.matrixWorld.copy(e.matrixWorld),this.matrixAutoUpdate=e.matrixAutoUpdate,this.matrixWorldAutoUpdate=e.matrixWorldAutoUpdate,this.matrixWorldNeedsUpdate=e.matrixWorldNeedsUpdate,this.layers.mask=e.layers.mask,this.visible=e.visible,this.castShadow=e.castShadow,this.receiveShadow=e.receiveShadow,this.frustumCulled=e.frustumCulled,this.renderOrder=e.renderOrder,this.static=e.static,this.animations=e.animations.slice(),this.userData=JSON.parse(JSON.stringify(e.userData)),t===!0)for(let i=0;i<e.children.length;i++){const s=e.children[i];this.add(s.clone())}return this}}xt.DEFAULT_UP=new A(0,1,0),xt.DEFAULT_MATRIX_AUTO_UPDATE=!0,xt.DEFAULT_MATRIX_WORLD_AUTO_UPDATE=!0;class ps extends xt{constructor(){super(),this.isGroup=!0,this.type="Group"}}const Sh={type:"move"};class aa{constructor(){this._targetRay=null,this._grip=null,this._hand=null}getHandSpace(){return this._hand===null&&(this._hand=new ps,this._hand.matrixAutoUpdate=!1,this._hand.visible=!1,this._hand.joints={},this._hand.inputState={pinching:!1}),this._hand}getTargetRaySpace(){return this._targetRay===null&&(this._targetRay=new ps,this._targetRay.matrixAutoUpdate=!1,this._targetRay.visible=!1,this._targetRay.hasLinearVelocity=!1,this._targetRay.linearVelocity=new A,this._targetRay.hasAngularVelocity=!1,this._targetRay.angularVelocity=new A),this._targetRay}getGripSpace(){return this._grip===null&&(this._grip=new ps,this._grip.matrixAutoUpdate=!1,this._grip.visible=!1,this._grip.hasLinearVelocity=!1,this._grip.linearVelocity=new A,this._grip.hasAngularVelocity=!1,this._grip.angularVelocity=new A,this._grip.eventsEnabled=!1),this._grip}dispatchEvent(e){return this._targetRay!==null&&this._targetRay.dispatchEvent(e),this._grip!==null&&this._grip.dispatchEvent(e),this._hand!==null&&this._hand.dispatchEvent(e),this}connect(e){if(e&&e.hand){const t=this._hand;if(t)for(const i of e.hand.values())this._getHandJoint(t,i)}return this.dispatchEvent({type:"connected",data:e}),this}disconnect(e){return this.dispatchEvent({type:"disconnected",data:e}),this._targetRay!==null&&(this._targetRay.visible=!1),this._grip!==null&&(this._grip.visible=!1),this._hand!==null&&(this._hand.visible=!1),this}update(e,t,i){let s=null,r=null,a=null;const o=this._targetRay,l=this._grip,c=this._hand;if(e&&t.session.visibilityState!=="visible-blurred"){if(c&&e.hand){a=!0;for(const x of e.hand.values()){const p=t.getJointPose(x,i),f=this._getHandJoint(c,x);p!==null&&(f.matrix.fromArray(p.transform.matrix),f.matrix.decompose(f.position,f.rotation,f.scale),f.matrixWorldNeedsUpdate=!0,f.jointRadius=p.radius),f.visible=p!==null}const h=c.joints["index-finger-tip"],d=c.joints["thumb-tip"],u=h.position.distanceTo(d.position),m=.02,_=.005;c.inputState.pinching&&u>m+_?(c.inputState.pinching=!1,this.dispatchEvent({type:"pinchend",handedness:e.handedness,target:this})):!c.inputState.pinching&&u<=m-_&&(c.inputState.pinching=!0,this.dispatchEvent({type:"pinchstart",handedness:e.handedness,target:this}))}else l!==null&&e.gripSpace&&(r=t.getPose(e.gripSpace,i),r!==null&&(l.matrix.fromArray(r.transform.matrix),l.matrix.decompose(l.position,l.rotation,l.scale),l.matrixWorldNeedsUpdate=!0,r.linearVelocity?(l.hasLinearVelocity=!0,l.linearVelocity.copy(r.linearVelocity)):l.hasLinearVelocity=!1,r.angularVelocity?(l.hasAngularVelocity=!0,l.angularVelocity.copy(r.angularVelocity)):l.hasAngularVelocity=!1,l.eventsEnabled&&l.dispatchEvent({type:"gripUpdated",data:e,target:this})));o!==null&&(s=t.getPose(e.targetRaySpace,i),s===null&&r!==null&&(s=r),s!==null&&(o.matrix.fromArray(s.transform.matrix),o.matrix.decompose(o.position,o.rotation,o.scale),o.matrixWorldNeedsUpdate=!0,s.linearVelocity?(o.hasLinearVelocity=!0,o.linearVelocity.copy(s.linearVelocity)):o.hasLinearVelocity=!1,s.angularVelocity?(o.hasAngularVelocity=!0,o.angularVelocity.copy(s.angularVelocity)):o.hasAngularVelocity=!1,this.dispatchEvent(Sh)))}return o!==null&&(o.visible=s!==null),l!==null&&(l.visible=r!==null),c!==null&&(c.visible=a!==null),this}_getHandJoint(e,t){if(e.joints[t.jointName]===void 0){const i=new ps;i.matrixAutoUpdate=!1,i.visible=!1,e.joints[t.jointName]=i,e.add(i)}return e.joints[t.jointName]}}const Uo={aliceblue:15792383,antiquewhite:16444375,aqua:65535,aquamarine:8388564,azure:15794175,beige:16119260,bisque:16770244,black:0,blanchedalmond:16772045,blue:255,blueviolet:9055202,brown:10824234,burlywood:14596231,cadetblue:6266528,chartreuse:8388352,chocolate:13789470,coral:16744272,cornflowerblue:6591981,cornsilk:16775388,crimson:14423100,cyan:65535,darkblue:139,darkcyan:35723,darkgoldenrod:12092939,darkgray:11119017,darkgreen:25600,darkgrey:11119017,darkkhaki:12433259,darkmagenta:9109643,darkolivegreen:5597999,darkorange:16747520,darkorchid:10040012,darkred:9109504,darksalmon:15308410,darkseagreen:9419919,darkslateblue:4734347,darkslategray:3100495,darkslategrey:3100495,darkturquoise:52945,darkviolet:9699539,deeppink:16716947,deepskyblue:49151,dimgray:6908265,dimgrey:6908265,dodgerblue:2003199,firebrick:11674146,floralwhite:16775920,forestgreen:2263842,fuchsia:16711935,gainsboro:14474460,ghostwhite:16316671,gold:16766720,goldenrod:14329120,gray:8421504,green:32768,greenyellow:11403055,grey:8421504,honeydew:15794160,hotpink:16738740,indianred:13458524,indigo:4915330,ivory:16777200,khaki:15787660,lavender:15132410,lavenderblush:16773365,lawngreen:8190976,lemonchiffon:16775885,lightblue:11393254,lightcoral:15761536,lightcyan:14745599,lightgoldenrodyellow:16448210,lightgray:13882323,lightgreen:9498256,lightgrey:13882323,lightpink:16758465,lightsalmon:16752762,lightseagreen:2142890,lightskyblue:8900346,lightslategray:7833753,lightslategrey:7833753,lightsteelblue:11584734,lightyellow:16777184,lime:65280,limegreen:3329330,linen:16445670,magenta:16711935,maroon:8388608,mediumaquamarine:6737322,mediumblue:205,mediumorchid:12211667,mediumpurple:9662683,mediumseagreen:3978097,mediumslateblue:8087790,mediumspringgreen:64154,mediumturquoise:4772300,mediumvioletred:13047173,midnightblue:1644912,mintcream:16121850,mistyrose:16770273,moccasin:16770229,navajowhite:16768685,navy:128,oldlace:16643558,olive:8421376,olivedrab:7048739,orange:16753920,orangered:16729344,orchid:14315734,palegoldenrod:15657130,palegreen:10025880,paleturquoise:11529966,palevioletred:14381203,papayawhip:16773077,peachpuff:16767673,peru:13468991,pink:16761035,plum:14524637,powderblue:11591910,purple:8388736,rebeccapurple:6697881,red:16711680,rosybrown:12357519,royalblue:4286945,saddlebrown:9127187,salmon:16416882,sandybrown:16032864,seagreen:3050327,seashell:16774638,sienna:10506797,silver:12632256,skyblue:8900331,slateblue:6970061,slategray:7372944,slategrey:7372944,snow:16775930,springgreen:65407,steelblue:4620980,tan:13808780,teal:32896,thistle:14204888,tomato:16737095,turquoise:4251856,violet:15631086,wheat:16113331,white:16777215,whitesmoke:16119285,yellow:16776960,yellowgreen:10145074},Ii={h:0,s:0,l:0},ms={h:0,s:0,l:0};function oa(n,e,t){return t<0&&(t+=1),t>1&&(t-=1),t<1/6?n+(e-n)*6*t:t<1/2?e:t<2/3?n+(e-n)*6*(2/3-t):n}class Re{constructor(e,t,i){return this.isColor=!0,this.r=1,this.g=1,this.b=1,this.set(e,t,i)}set(e,t,i){if(t===void 0&&i===void 0){const s=e;s&&s.isColor?this.copy(s):typeof s=="number"?this.setHex(s):typeof s=="string"&&this.setStyle(s)}else this.setRGB(e,t,i);return this}setScalar(e){return this.r=e,this.g=e,this.b=e,this}setHex(e,t=jt){return e=Math.floor(e),this.r=(e>>16&255)/255,this.g=(e>>8&255)/255,this.b=(e&255)/255,Xe.colorSpaceToWorking(this,t),this}setRGB(e,t,i,s=Xe.workingColorSpace){return this.r=e,this.g=t,this.b=i,Xe.colorSpaceToWorking(this,s),this}setHSL(e,t,i,s=Xe.workingColorSpace){if(e=Kr(e,1),t=Ge(t,0,1),i=Ge(i,0,1),t===0)this.r=this.g=this.b=i;else{const r=i<=.5?i*(1+t):i+t-i*t,a=2*i-r;this.r=oa(a,r,e+1/3),this.g=oa(a,r,e),this.b=oa(a,r,e-1/3)}return Xe.colorSpaceToWorking(this,s),this}setStyle(e,t=jt){function i(r){r!==void 0&&parseFloat(r)<1&&Ce("Color: Alpha component of "+e+" will be ignored.")}let s;if(s=/^(\w+)\(([^\)]*)\)/.exec(e)){let r;const a=s[1],o=s[2];switch(a){case"rgb":case"rgba":if(r=/^\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*(?:,\s*(\d*\.?\d+)\s*)?$/.exec(o))return i(r[4]),this.setRGB(Math.min(255,parseInt(r[1],10))/255,Math.min(255,parseInt(r[2],10))/255,Math.min(255,parseInt(r[3],10))/255,t);if(r=/^\s*(\d+)\%\s*,\s*(\d+)\%\s*,\s*(\d+)\%\s*(?:,\s*(\d*\.?\d+)\s*)?$/.exec(o))return i(r[4]),this.setRGB(Math.min(100,parseInt(r[1],10))/100,Math.min(100,parseInt(r[2],10))/100,Math.min(100,parseInt(r[3],10))/100,t);break;case"hsl":case"hsla":if(r=/^\s*(\d*\.?\d+)\s*,\s*(\d*\.?\d+)\%\s*,\s*(\d*\.?\d+)\%\s*(?:,\s*(\d*\.?\d+)\s*)?$/.exec(o))return i(r[4]),this.setHSL(parseFloat(r[1])/360,parseFloat(r[2])/100,parseFloat(r[3])/100,t);break;default:Ce("Color: Unknown color model "+e)}}else if(s=/^\#([A-Fa-f\d]+)$/.exec(e)){const r=s[1],a=r.length;if(a===3)return this.setRGB(parseInt(r.charAt(0),16)/15,parseInt(r.charAt(1),16)/15,parseInt(r.charAt(2),16)/15,t);if(a===6)return this.setHex(parseInt(r,16),t);Ce("Color: Invalid hex color "+e)}else if(e&&e.length>0)return this.setColorName(e,t);return this}setColorName(e,t=jt){const i=Uo[e.toLowerCase()];return i!==void 0?this.setHex(i,t):Ce("Color: Unknown color "+e),this}clone(){return new this.constructor(this.r,this.g,this.b)}copy(e){return this.r=e.r,this.g=e.g,this.b=e.b,this}copySRGBToLinear(e){return this.r=bi(e.r),this.g=bi(e.g),this.b=bi(e.b),this}copyLinearToSRGB(e){return this.r=un(e.r),this.g=un(e.g),this.b=un(e.b),this}convertSRGBToLinear(){return this.copySRGBToLinear(this),this}convertLinearToSRGB(){return this.copyLinearToSRGB(this),this}getHex(e=jt){return Xe.workingToColorSpace(Pt.copy(this),e),Math.round(Ge(Pt.r*255,0,255))*65536+Math.round(Ge(Pt.g*255,0,255))*256+Math.round(Ge(Pt.b*255,0,255))}getHexString(e=jt){return("000000"+this.getHex(e).toString(16)).slice(-6)}getHSL(e,t=Xe.workingColorSpace){Xe.workingToColorSpace(Pt.copy(this),t);const i=Pt.r,s=Pt.g,r=Pt.b,a=Math.max(i,s,r),o=Math.min(i,s,r);let l,c;const h=(o+a)/2;if(o===a)l=0,c=0;else{const d=a-o;switch(c=h<=.5?d/(a+o):d/(2-a-o),a){case i:l=(s-r)/d+(s<r?6:0);break;case s:l=(r-i)/d+2;break;case r:l=(i-s)/d+4;break}l/=6}return e.h=l,e.s=c,e.l=h,e}getRGB(e,t=Xe.workingColorSpace){return Xe.workingToColorSpace(Pt.copy(this),t),e.r=Pt.r,e.g=Pt.g,e.b=Pt.b,e}getStyle(e=jt){Xe.workingToColorSpace(Pt.copy(this),e);const t=Pt.r,i=Pt.g,s=Pt.b;return e!==jt?`color(${e} ${t.toFixed(3)} ${i.toFixed(3)} ${s.toFixed(3)})`:`rgb(${Math.round(t*255)},${Math.round(i*255)},${Math.round(s*255)})`}offsetHSL(e,t,i){return this.getHSL(Ii),this.setHSL(Ii.h+e,Ii.s+t,Ii.l+i)}add(e){return this.r+=e.r,this.g+=e.g,this.b+=e.b,this}addColors(e,t){return this.r=e.r+t.r,this.g=e.g+t.g,this.b=e.b+t.b,this}addScalar(e){return this.r+=e,this.g+=e,this.b+=e,this}sub(e){return this.r=Math.max(0,this.r-e.r),this.g=Math.max(0,this.g-e.g),this.b=Math.max(0,this.b-e.b),this}multiply(e){return this.r*=e.r,this.g*=e.g,this.b*=e.b,this}multiplyScalar(e){return this.r*=e,this.g*=e,this.b*=e,this}lerp(e,t){return this.r+=(e.r-this.r)*t,this.g+=(e.g-this.g)*t,this.b+=(e.b-this.b)*t,this}lerpColors(e,t,i){return this.r=e.r+(t.r-e.r)*i,this.g=e.g+(t.g-e.g)*i,this.b=e.b+(t.b-e.b)*i,this}lerpHSL(e,t){this.getHSL(Ii),e.getHSL(ms);const i=Vn(Ii.h,ms.h,t),s=Vn(Ii.s,ms.s,t),r=Vn(Ii.l,ms.l,t);return this.setHSL(i,s,r),this}setFromVector3(e){return this.r=e.x,this.g=e.y,this.b=e.z,this}applyMatrix3(e){const t=this.r,i=this.g,s=this.b,r=e.elements;return this.r=r[0]*t+r[3]*i+r[6]*s,this.g=r[1]*t+r[4]*i+r[7]*s,this.b=r[2]*t+r[5]*i+r[8]*s,this}equals(e){return e.r===this.r&&e.g===this.g&&e.b===this.b}fromArray(e,t=0){return this.r=e[t],this.g=e[t+1],this.b=e[t+2],this}toArray(e=[],t=0){return e[t]=this.r,e[t+1]=this.g,e[t+2]=this.b,e}fromBufferAttribute(e,t){return this.r=e.getX(t),this.g=e.getY(t),this.b=e.getZ(t),this}toJSON(){return this.getHex()}*[Symbol.iterator](){yield this.r,yield this.g,yield this.b}}const Pt=new Re;Re.NAMES=Uo;class Mh extends xt{constructor(){super(),this.isScene=!0,this.type="Scene",this.background=null,this.environment=null,this.fog=null,this.backgroundBlurriness=0,this.backgroundIntensity=1,this.backgroundRotation=new Di,this.environmentIntensity=1,this.environmentRotation=new Di,this.overrideMaterial=null,typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("observe",{detail:this}))}copy(e,t){return super.copy(e,t),e.background!==null&&(this.background=e.background.clone()),e.environment!==null&&(this.environment=e.environment.clone()),e.fog!==null&&(this.fog=e.fog.clone()),this.backgroundBlurriness=e.backgroundBlurriness,this.backgroundIntensity=e.backgroundIntensity,this.backgroundRotation.copy(e.backgroundRotation),this.environmentIntensity=e.environmentIntensity,this.environmentRotation.copy(e.environmentRotation),e.overrideMaterial!==null&&(this.overrideMaterial=e.overrideMaterial.clone()),this.matrixAutoUpdate=e.matrixAutoUpdate,this}toJSON(e){const t=super.toJSON(e);return this.fog!==null&&(t.object.fog=this.fog.toJSON()),this.backgroundBlurriness>0&&(t.object.backgroundBlurriness=this.backgroundBlurriness),this.backgroundIntensity!==1&&(t.object.backgroundIntensity=this.backgroundIntensity),t.object.backgroundRotation=this.backgroundRotation.toArray(),this.environmentIntensity!==1&&(t.object.environmentIntensity=this.environmentIntensity),t.object.environmentRotation=this.environmentRotation.toArray(),t}}const ti=new A,Mi=new A,la=new A,yi=new A,gn=new A,_n=new A,Fo=new A,ca=new A,ha=new A,ua=new A,da=new ht,fa=new ht,pa=new ht;class qt{constructor(e=new A,t=new A,i=new A){this.a=e,this.b=t,this.c=i}static getNormal(e,t,i,s){s.subVectors(i,t),ti.subVectors(e,t),s.cross(ti);const r=s.lengthSq();return r>0?s.multiplyScalar(1/Math.sqrt(r)):s.set(0,0,0)}static getBarycoord(e,t,i,s,r){ti.subVectors(s,t),Mi.subVectors(i,t),la.subVectors(e,t);const a=ti.dot(ti),o=ti.dot(Mi),l=ti.dot(la),c=Mi.dot(Mi),h=Mi.dot(la),d=a*c-o*o;if(d===0)return r.set(0,0,0),null;const u=1/d,m=(c*l-o*h)*u,_=(a*h-o*l)*u;return r.set(1-m-_,_,m)}static containsPoint(e,t,i,s){return this.getBarycoord(e,t,i,s,yi)===null?!1:yi.x>=0&&yi.y>=0&&yi.x+yi.y<=1}static getInterpolation(e,t,i,s,r,a,o,l){return this.getBarycoord(e,t,i,s,yi)===null?(l.x=0,l.y=0,"z"in l&&(l.z=0),"w"in l&&(l.w=0),null):(l.setScalar(0),l.addScaledVector(r,yi.x),l.addScaledVector(a,yi.y),l.addScaledVector(o,yi.z),l)}static getInterpolatedAttribute(e,t,i,s,r,a){return da.setScalar(0),fa.setScalar(0),pa.setScalar(0),da.fromBufferAttribute(e,t),fa.fromBufferAttribute(e,i),pa.fromBufferAttribute(e,s),a.setScalar(0),a.addScaledVector(da,r.x),a.addScaledVector(fa,r.y),a.addScaledVector(pa,r.z),a}static isFrontFacing(e,t,i,s){return ti.subVectors(i,t),Mi.subVectors(e,t),ti.cross(Mi).dot(s)<0}set(e,t,i){return this.a.copy(e),this.b.copy(t),this.c.copy(i),this}setFromPointsAndIndices(e,t,i,s){return this.a.copy(e[t]),this.b.copy(e[i]),this.c.copy(e[s]),this}setFromAttributeAndIndices(e,t,i,s){return this.a.fromBufferAttribute(e,t),this.b.fromBufferAttribute(e,i),this.c.fromBufferAttribute(e,s),this}clone(){return new this.constructor().copy(this)}copy(e){return this.a.copy(e.a),this.b.copy(e.b),this.c.copy(e.c),this}getArea(){return ti.subVectors(this.c,this.b),Mi.subVectors(this.a,this.b),ti.cross(Mi).length()*.5}getMidpoint(e){return e.addVectors(this.a,this.b).add(this.c).multiplyScalar(1/3)}getNormal(e){return qt.getNormal(this.a,this.b,this.c,e)}getPlane(e){return e.setFromCoplanarPoints(this.a,this.b,this.c)}getBarycoord(e,t){return qt.getBarycoord(e,this.a,this.b,this.c,t)}getInterpolation(e,t,i,s,r){return qt.getInterpolation(e,this.a,this.b,this.c,t,i,s,r)}containsPoint(e){return qt.containsPoint(e,this.a,this.b,this.c)}isFrontFacing(e){return qt.isFrontFacing(this.a,this.b,this.c,e)}intersectsBox(e){return e.intersectsTriangle(this)}closestPointToPoint(e,t){const i=this.a,s=this.b,r=this.c;let a,o;gn.subVectors(s,i),_n.subVectors(r,i),ca.subVectors(e,i);const l=gn.dot(ca),c=_n.dot(ca);if(l<=0&&c<=0)return t.copy(i);ha.subVectors(e,s);const h=gn.dot(ha),d=_n.dot(ha);if(h>=0&&d<=h)return t.copy(s);const u=l*d-h*c;if(u<=0&&l>=0&&h<=0)return a=l/(l-h),t.copy(i).addScaledVector(gn,a);ua.subVectors(e,r);const m=gn.dot(ua),_=_n.dot(ua);if(_>=0&&m<=_)return t.copy(r);const x=m*c-l*_;if(x<=0&&c>=0&&_<=0)return o=c/(c-_),t.copy(i).addScaledVector(_n,o);const p=h*_-m*d;if(p<=0&&d-h>=0&&m-_>=0)return Fo.subVectors(r,s),o=(d-h)/(d-h+(m-_)),t.copy(s).addScaledVector(Fo,o);const f=1/(p+x+u);return a=x*f,o=u*f,t.copy(i).addScaledVector(gn,a).addScaledVector(_n,o)}equals(e){return e.a.equals(this.a)&&e.b.equals(this.b)&&e.c.equals(this.c)}}class Ni{constructor(e=new A(1/0,1/0,1/0),t=new A(-1/0,-1/0,-1/0)){this.isBox3=!0,this.min=e,this.max=t}set(e,t){return this.min.copy(e),this.max.copy(t),this}setFromArray(e){this.makeEmpty();for(let t=0,i=e.length;t<i;t+=3)this.expandByPoint(ii.fromArray(e,t));return this}setFromBufferAttribute(e){this.makeEmpty();for(let t=0,i=e.count;t<i;t++)this.expandByPoint(ii.fromBufferAttribute(e,t));return this}setFromPoints(e){this.makeEmpty();for(let t=0,i=e.length;t<i;t++)this.expandByPoint(e[t]);return this}setFromCenterAndSize(e,t){const i=ii.copy(t).multiplyScalar(.5);return this.min.copy(e).sub(i),this.max.copy(e).add(i),this}setFromObject(e,t=!1){return this.makeEmpty(),this.expandByObject(e,t)}clone(){return new this.constructor().copy(this)}copy(e){return this.min.copy(e.min),this.max.copy(e.max),this}makeEmpty(){return this.min.x=this.min.y=this.min.z=1/0,this.max.x=this.max.y=this.max.z=-1/0,this}isEmpty(){return this.max.x<this.min.x||this.max.y<this.min.y||this.max.z<this.min.z}getCenter(e){return this.isEmpty()?e.set(0,0,0):e.addVectors(this.min,this.max).multiplyScalar(.5)}getSize(e){return this.isEmpty()?e.set(0,0,0):e.subVectors(this.max,this.min)}expandByPoint(e){return this.min.min(e),this.max.max(e),this}expandByVector(e){return this.min.sub(e),this.max.add(e),this}expandByScalar(e){return this.min.addScalar(-e),this.max.addScalar(e),this}expandByObject(e,t=!1){e.updateWorldMatrix(!1,!1);const i=e.geometry;if(i!==void 0){const r=i.getAttribute("position");if(t===!0&&r!==void 0&&e.isInstancedMesh!==!0)for(let a=0,o=r.count;a<o;a++)e.isMesh===!0?e.getVertexPosition(a,ii):ii.fromBufferAttribute(r,a),ii.applyMatrix4(e.matrixWorld),this.expandByPoint(ii);else e.boundingBox!==void 0?(e.boundingBox===null&&e.computeBoundingBox(),gs.copy(e.boundingBox)):(i.boundingBox===null&&i.computeBoundingBox(),gs.copy(i.boundingBox)),gs.applyMatrix4(e.matrixWorld),this.union(gs)}const s=e.children;for(let r=0,a=s.length;r<a;r++)this.expandByObject(s[r],t);return this}containsPoint(e){return e.x>=this.min.x&&e.x<=this.max.x&&e.y>=this.min.y&&e.y<=this.max.y&&e.z>=this.min.z&&e.z<=this.max.z}containsBox(e){return this.min.x<=e.min.x&&e.max.x<=this.max.x&&this.min.y<=e.min.y&&e.max.y<=this.max.y&&this.min.z<=e.min.z&&e.max.z<=this.max.z}getParameter(e,t){return t.set((e.x-this.min.x)/(this.max.x-this.min.x),(e.y-this.min.y)/(this.max.y-this.min.y),(e.z-this.min.z)/(this.max.z-this.min.z))}intersectsBox(e){return e.max.x>=this.min.x&&e.min.x<=this.max.x&&e.max.y>=this.min.y&&e.min.y<=this.max.y&&e.max.z>=this.min.z&&e.min.z<=this.max.z}intersectsSphere(e){return this.clampPoint(e.center,ii),ii.distanceToSquared(e.center)<=e.radius*e.radius}intersectsPlane(e){let t,i;return e.normal.x>0?(t=e.normal.x*this.min.x,i=e.normal.x*this.max.x):(t=e.normal.x*this.max.x,i=e.normal.x*this.min.x),e.normal.y>0?(t+=e.normal.y*this.min.y,i+=e.normal.y*this.max.y):(t+=e.normal.y*this.max.y,i+=e.normal.y*this.min.y),e.normal.z>0?(t+=e.normal.z*this.min.z,i+=e.normal.z*this.max.z):(t+=e.normal.z*this.max.z,i+=e.normal.z*this.min.z),t<=-e.constant&&i>=-e.constant}intersectsTriangle(e){if(this.isEmpty())return!1;this.getCenter(Hn),_s.subVectors(this.max,Hn),vn.subVectors(e.a,Hn),xn.subVectors(e.b,Hn),bn.subVectors(e.c,Hn),Ui.subVectors(xn,vn),Fi.subVectors(bn,xn),$i.subVectors(vn,bn);let t=[0,-Ui.z,Ui.y,0,-Fi.z,Fi.y,0,-$i.z,$i.y,Ui.z,0,-Ui.x,Fi.z,0,-Fi.x,$i.z,0,-$i.x,-Ui.y,Ui.x,0,-Fi.y,Fi.x,0,-$i.y,$i.x,0];return!ma(t,vn,xn,bn,_s)||(t=[1,0,0,0,1,0,0,0,1],!ma(t,vn,xn,bn,_s))?!1:(vs.crossVectors(Ui,Fi),t=[vs.x,vs.y,vs.z],ma(t,vn,xn,bn,_s))}clampPoint(e,t){return t.copy(e).clamp(this.min,this.max)}distanceToPoint(e){return this.clampPoint(e,ii).distanceTo(e)}getBoundingSphere(e){return this.isEmpty()?e.makeEmpty():(this.getCenter(e.center),e.radius=this.getSize(ii).length()*.5),e}intersect(e){return this.min.max(e.min),this.max.min(e.max),this.isEmpty()&&this.makeEmpty(),this}union(e){return this.min.min(e.min),this.max.max(e.max),this}applyMatrix4(e){return this.isEmpty()?this:(Ei[0].set(this.min.x,this.min.y,this.min.z).applyMatrix4(e),Ei[1].set(this.min.x,this.min.y,this.max.z).applyMatrix4(e),Ei[2].set(this.min.x,this.max.y,this.min.z).applyMatrix4(e),Ei[3].set(this.min.x,this.max.y,this.max.z).applyMatrix4(e),Ei[4].set(this.max.x,this.min.y,this.min.z).applyMatrix4(e),Ei[5].set(this.max.x,this.min.y,this.max.z).applyMatrix4(e),Ei[6].set(this.max.x,this.max.y,this.min.z).applyMatrix4(e),Ei[7].set(this.max.x,this.max.y,this.max.z).applyMatrix4(e),this.setFromPoints(Ei),this)}translate(e){return this.min.add(e),this.max.add(e),this}equals(e){return e.min.equals(this.min)&&e.max.equals(this.max)}toJSON(){return{min:this.min.toArray(),max:this.max.toArray()}}fromJSON(e){return this.min.fromArray(e.min),this.max.fromArray(e.max),this}}const Ei=[new A,new A,new A,new A,new A,new A,new A,new A],ii=new A,gs=new Ni,vn=new A,xn=new A,bn=new A,Ui=new A,Fi=new A,$i=new A,Hn=new A,_s=new A,vs=new A,Zi=new A;function ma(n,e,t,i,s){for(let r=0,a=n.length-3;r<=a;r+=3){Zi.fromArray(n,r);const o=s.x*Math.abs(Zi.x)+s.y*Math.abs(Zi.y)+s.z*Math.abs(Zi.z),l=e.dot(Zi),c=t.dot(Zi),h=i.dot(Zi);if(Math.max(-Math.max(l,c,h),Math.min(l,c,h))>o)return!1}return!0}const gt=new A,xs=new Ne;let yh=0;class Lt extends Ri{constructor(e,t,i=!1){if(super(),Array.isArray(e))throw new TypeError("THREE.BufferAttribute: array should be a Typed Array.");this.isBufferAttribute=!0,Object.defineProperty(this,"id",{value:yh++}),this.name="",this.array=e,this.itemSize=t,this.count=e!==void 0?e.length/t:0,this.normalized=i,this.usage=vo,this.updateRanges=[],this.gpuType=ai,this.version=0}onUploadCallback(){}set needsUpdate(e){e===!0&&this.version++}setUsage(e){return this.usage=e,this}addUpdateRange(e,t){this.updateRanges.push({start:e,count:t})}clearUpdateRanges(){this.updateRanges.length=0}copy(e){return this.name=e.name,this.array=new e.array.constructor(e.array),this.itemSize=e.itemSize,this.count=e.count,this.normalized=e.normalized,this.usage=e.usage,this.gpuType=e.gpuType,this}copyAt(e,t,i){e*=this.itemSize,i*=t.itemSize;for(let s=0,r=this.itemSize;s<r;s++)this.array[e+s]=t.array[i+s];return this}copyArray(e){return this.array.set(e),this}applyMatrix3(e){if(this.itemSize===2)for(let t=0,i=this.count;t<i;t++)xs.fromBufferAttribute(this,t),xs.applyMatrix3(e),this.setXY(t,xs.x,xs.y);else if(this.itemSize===3)for(let t=0,i=this.count;t<i;t++)gt.fromBufferAttribute(this,t),gt.applyMatrix3(e),this.setXYZ(t,gt.x,gt.y,gt.z);return this}applyMatrix4(e){for(let t=0,i=this.count;t<i;t++)gt.fromBufferAttribute(this,t),gt.applyMatrix4(e),this.setXYZ(t,gt.x,gt.y,gt.z);return this}applyNormalMatrix(e){for(let t=0,i=this.count;t<i;t++)gt.fromBufferAttribute(this,t),gt.applyNormalMatrix(e),this.setXYZ(t,gt.x,gt.y,gt.z);return this}transformDirection(e){for(let t=0,i=this.count;t<i;t++)gt.fromBufferAttribute(this,t),gt.transformDirection(e),this.setXYZ(t,gt.x,gt.y,gt.z);return this}set(e,t=0){return this.array.set(e,t),this}getComponent(e,t){let i=this.array[e*this.itemSize+t];return this.normalized&&(i=hn(i,this.array)),i}setComponent(e,t,i){return this.normalized&&(i=Nt(i,this.array)),this.array[e*this.itemSize+t]=i,this}getX(e){let t=this.array[e*this.itemSize];return this.normalized&&(t=hn(t,this.array)),t}setX(e,t){return this.normalized&&(t=Nt(t,this.array)),this.array[e*this.itemSize]=t,this}getY(e){let t=this.array[e*this.itemSize+1];return this.normalized&&(t=hn(t,this.array)),t}setY(e,t){return this.normalized&&(t=Nt(t,this.array)),this.array[e*this.itemSize+1]=t,this}getZ(e){let t=this.array[e*this.itemSize+2];return this.normalized&&(t=hn(t,this.array)),t}setZ(e,t){return this.normalized&&(t=Nt(t,this.array)),this.array[e*this.itemSize+2]=t,this}getW(e){let t=this.array[e*this.itemSize+3];return this.normalized&&(t=hn(t,this.array)),t}setW(e,t){return this.normalized&&(t=Nt(t,this.array)),this.array[e*this.itemSize+3]=t,this}setXY(e,t,i){return e*=this.itemSize,this.normalized&&(t=Nt(t,this.array),i=Nt(i,this.array)),this.array[e+0]=t,this.array[e+1]=i,this}setXYZ(e,t,i,s){return e*=this.itemSize,this.normalized&&(t=Nt(t,this.array),i=Nt(i,this.array),s=Nt(s,this.array)),this.array[e+0]=t,this.array[e+1]=i,this.array[e+2]=s,this}setXYZW(e,t,i,s,r){return e*=this.itemSize,this.normalized&&(t=Nt(t,this.array),i=Nt(i,this.array),s=Nt(s,this.array),r=Nt(r,this.array)),this.array[e+0]=t,this.array[e+1]=i,this.array[e+2]=s,this.array[e+3]=r,this}onUpload(e){return this.onUploadCallback=e,this}clone(){return new this.constructor(this.array,this.itemSize).copy(this)}toJSON(){const e={itemSize:this.itemSize,type:this.array.constructor.name,array:Array.from(this.array),normalized:this.normalized};return this.name!==""&&(e.name=this.name),this.usage!==vo&&(e.usage=this.usage),e}dispose(){this.dispatchEvent({type:"dispose"})}}class Oo extends Lt{constructor(e,t,i){super(new Uint16Array(e),t,i)}}class Bo extends Lt{constructor(e,t,i){super(new Uint32Array(e),t,i)}}class Ft extends Lt{constructor(e,t,i){super(new Float32Array(e),t,i)}}const Eh=new Ni,Wn=new A,ga=new A;class Sn{constructor(e=new A,t=-1){this.isSphere=!0,this.center=e,this.radius=t}set(e,t){return this.center.copy(e),this.radius=t,this}setFromPoints(e,t){const i=this.center;t!==void 0?i.copy(t):Eh.setFromPoints(e).getCenter(i);let s=0;for(let r=0,a=e.length;r<a;r++)s=Math.max(s,i.distanceToSquared(e[r]));return this.radius=Math.sqrt(s),this}copy(e){return this.center.copy(e.center),this.radius=e.radius,this}isEmpty(){return this.radius<0}makeEmpty(){return this.center.set(0,0,0),this.radius=-1,this}containsPoint(e){return e.distanceToSquared(this.center)<=this.radius*this.radius}distanceToPoint(e){return e.distanceTo(this.center)-this.radius}intersectsSphere(e){const t=this.radius+e.radius;return e.center.distanceToSquared(this.center)<=t*t}intersectsBox(e){return e.intersectsSphere(this)}intersectsPlane(e){return Math.abs(e.distanceToPoint(this.center))<=this.radius}clampPoint(e,t){const i=this.center.distanceToSquared(e);return t.copy(e),i>this.radius*this.radius&&(t.sub(this.center).normalize(),t.multiplyScalar(this.radius).add(this.center)),t}getBoundingBox(e){return this.isEmpty()?(e.makeEmpty(),e):(e.set(this.center,this.center),e.expandByScalar(this.radius),e)}applyMatrix4(e){return this.center.applyMatrix4(e),this.radius=this.radius*e.getMaxScaleOnAxis(),this}translate(e){return this.center.add(e),this}expandByPoint(e){if(this.isEmpty())return this.center.copy(e),this.radius=0,this;Wn.subVectors(e,this.center);const t=Wn.lengthSq();if(t>this.radius*this.radius){const i=Math.sqrt(t),s=(i-this.radius)*.5;this.center.addScaledVector(Wn,s/i),this.radius+=s}return this}union(e){return e.isEmpty()?this:this.isEmpty()?(this.copy(e),this):(this.center.equals(e.center)===!0?this.radius=Math.max(this.radius,e.radius):(ga.subVectors(e.center,this.center).setLength(e.radius),this.expandByPoint(Wn.copy(e.center).add(ga)),this.expandByPoint(Wn.copy(e.center).sub(ga))),this)}equals(e){return e.center.equals(this.center)&&e.radius===this.radius}clone(){return new this.constructor().copy(this)}toJSON(){return{radius:this.radius,center:this.center.toArray()}}fromJSON(e){return this.radius=e.radius,this.center.fromArray(e.center),this}}let wh=0;const Yt=new lt,_a=new xt,Mn=new A,Gt=new Ni,Xn=new Ni,wt=new A;class _t extends Ri{constructor(){super(),this.isBufferGeometry=!0,Object.defineProperty(this,"id",{value:wh++}),this.uuid=cn(),this.name="",this.type="BufferGeometry",this.index=null,this.indirect=null,this.indirectOffset=0,this.attributes={},this.morphAttributes={},this.morphTargetsRelative=!1,this.groups=[],this.boundingBox=null,this.boundingSphere=null,this.drawRange={start:0,count:1/0},this.userData={}}getIndex(){return this.index}setIndex(e){return Array.isArray(e)?this.index=new(Hc(e)?Bo:Oo)(e,1):this.index=e,this}setIndirect(e,t=0){return this.indirect=e,this.indirectOffset=t,this}getIndirect(){return this.indirect}getAttribute(e){return this.attributes[e]}setAttribute(e,t){return this.attributes[e]=t,this}deleteAttribute(e){return delete this.attributes[e],this}hasAttribute(e){return this.attributes[e]!==void 0}addGroup(e,t,i=0){this.groups.push({start:e,count:t,materialIndex:i})}clearGroups(){this.groups=[]}setDrawRange(e,t){this.drawRange.start=e,this.drawRange.count=t}applyMatrix4(e){const t=this.attributes.position;t!==void 0&&(t.applyMatrix4(e),t.needsUpdate=!0);const i=this.attributes.normal;if(i!==void 0){const r=new Ue().getNormalMatrix(e);i.applyNormalMatrix(r),i.needsUpdate=!0}const s=this.attributes.tangent;return s!==void 0&&(s.transformDirection(e),s.needsUpdate=!0),this.boundingBox!==null&&this.computeBoundingBox(),this.boundingSphere!==null&&this.computeBoundingSphere(),this}applyQuaternion(e){return Yt.makeRotationFromQuaternion(e),this.applyMatrix4(Yt),this}rotateX(e){return Yt.makeRotationX(e),this.applyMatrix4(Yt),this}rotateY(e){return Yt.makeRotationY(e),this.applyMatrix4(Yt),this}rotateZ(e){return Yt.makeRotationZ(e),this.applyMatrix4(Yt),this}translate(e,t,i){return Yt.makeTranslation(e,t,i),this.applyMatrix4(Yt),this}scale(e,t,i){return Yt.makeScale(e,t,i),this.applyMatrix4(Yt),this}lookAt(e){return _a.lookAt(e),_a.updateMatrix(),this.applyMatrix4(_a.matrix),this}center(){return this.computeBoundingBox(),this.boundingBox.getCenter(Mn).negate(),this.translate(Mn.x,Mn.y,Mn.z),this}setFromPoints(e){const t=this.getAttribute("position");if(t===void 0){const i=[];for(let s=0,r=e.length;s<r;s++){const a=e[s];i.push(a.x,a.y,a.z||0)}this.setAttribute("position",new Ft(i,3))}else{const i=Math.min(e.length,t.count);for(let s=0;s<i;s++){const r=e[s];t.setXYZ(s,r.x,r.y,r.z||0)}e.length>t.count&&Ce("BufferGeometry: Buffer size too small for points data. Use .dispose() and create a new geometry."),t.needsUpdate=!0}return this}computeBoundingBox(){this.boundingBox===null&&(this.boundingBox=new Ni);const e=this.attributes.position,t=this.morphAttributes.position;if(e&&e.isGLBufferAttribute){je("BufferGeometry.computeBoundingBox(): GLBufferAttribute requires a manual bounding box.",this),this.boundingBox.set(new A(-1/0,-1/0,-1/0),new A(1/0,1/0,1/0));return}if(e!==void 0){if(this.boundingBox.setFromBufferAttribute(e),t)for(let i=0,s=t.length;i<s;i++){const r=t[i];Gt.setFromBufferAttribute(r),this.morphTargetsRelative?(wt.addVectors(this.boundingBox.min,Gt.min),this.boundingBox.expandByPoint(wt),wt.addVectors(this.boundingBox.max,Gt.max),this.boundingBox.expandByPoint(wt)):(this.boundingBox.expandByPoint(Gt.min),this.boundingBox.expandByPoint(Gt.max))}}else this.boundingBox.makeEmpty();(isNaN(this.boundingBox.min.x)||isNaN(this.boundingBox.min.y)||isNaN(this.boundingBox.min.z))&&je('BufferGeometry.computeBoundingBox(): Computed min/max have NaN values. The "position" attribute is likely to have NaN values.',this)}computeBoundingSphere(){this.boundingSphere===null&&(this.boundingSphere=new Sn);const e=this.attributes.position,t=this.morphAttributes.position;if(e&&e.isGLBufferAttribute){je("BufferGeometry.computeBoundingSphere(): GLBufferAttribute requires a manual bounding sphere.",this),this.boundingSphere.set(new A,1/0);return}if(e){const i=this.boundingSphere.center;if(Gt.setFromBufferAttribute(e),t)for(let r=0,a=t.length;r<a;r++){const o=t[r];Xn.setFromBufferAttribute(o),this.morphTargetsRelative?(wt.addVectors(Gt.min,Xn.min),Gt.expandByPoint(wt),wt.addVectors(Gt.max,Xn.max),Gt.expandByPoint(wt)):(Gt.expandByPoint(Xn.min),Gt.expandByPoint(Xn.max))}Gt.getCenter(i);let s=0;for(let r=0,a=e.count;r<a;r++)wt.fromBufferAttribute(e,r),s=Math.max(s,i.distanceToSquared(wt));if(t)for(let r=0,a=t.length;r<a;r++){const o=t[r],l=this.morphTargetsRelative;for(let c=0,h=o.count;c<h;c++)wt.fromBufferAttribute(o,c),l&&(Mn.fromBufferAttribute(e,c),wt.add(Mn)),s=Math.max(s,i.distanceToSquared(wt))}this.boundingSphere.radius=Math.sqrt(s),isNaN(this.boundingSphere.radius)&&je('BufferGeometry.computeBoundingSphere(): Computed radius is NaN. The "position" attribute is likely to have NaN values.',this)}}computeTangents(){const e=this.index,t=this.attributes;if(e===null||t.position===void 0||t.normal===void 0||t.uv===void 0){je("BufferGeometry: .computeTangents() failed. Missing required attributes (index, position, normal or uv)");return}const i=t.position,s=t.normal,r=t.uv;this.hasAttribute("tangent")===!1&&this.setAttribute("tangent",new Lt(new Float32Array(4*i.count),4));const a=this.getAttribute("tangent"),o=[],l=[];for(let b=0;b<i.count;b++)o[b]=new A,l[b]=new A;const c=new A,h=new A,d=new A,u=new Ne,m=new Ne,_=new Ne,x=new A,p=new A;function f(b,T,U){c.fromBufferAttribute(i,b),h.fromBufferAttribute(i,T),d.fromBufferAttribute(i,U),u.fromBufferAttribute(r,b),m.fromBufferAttribute(r,T),_.fromBufferAttribute(r,U),h.sub(c),d.sub(c),m.sub(u),_.sub(u);const C=1/(m.x*_.y-_.x*m.y);isFinite(C)&&(x.copy(h).multiplyScalar(_.y).addScaledVector(d,-m.y).multiplyScalar(C),p.copy(d).multiplyScalar(m.x).addScaledVector(h,-_.x).multiplyScalar(C),o[b].add(x),o[T].add(x),o[U].add(x),l[b].add(p),l[T].add(p),l[U].add(p))}let M=this.groups;M.length===0&&(M=[{start:0,count:e.count}]);for(let b=0,T=M.length;b<T;++b){const U=M[b],C=U.start,z=U.count;for(let X=C,j=C+z;X<j;X+=3)f(e.getX(X+0),e.getX(X+1),e.getX(X+2))}const y=new A,E=new A,L=new A,w=new A;function D(b){L.fromBufferAttribute(s,b),w.copy(L);const T=o[b];y.copy(T),y.sub(L.multiplyScalar(L.dot(T))).normalize(),E.crossVectors(w,T);const C=E.dot(l[b])<0?-1:1;a.setXYZW(b,y.x,y.y,y.z,C)}for(let b=0,T=M.length;b<T;++b){const U=M[b],C=U.start,z=U.count;for(let X=C,j=C+z;X<j;X+=3)D(e.getX(X+0)),D(e.getX(X+1)),D(e.getX(X+2))}}computeVertexNormals(){const e=this.index,t=this.getAttribute("position");if(t!==void 0){let i=this.getAttribute("normal");if(i===void 0)i=new Lt(new Float32Array(t.count*3),3),this.setAttribute("normal",i);else for(let u=0,m=i.count;u<m;u++)i.setXYZ(u,0,0,0);const s=new A,r=new A,a=new A,o=new A,l=new A,c=new A,h=new A,d=new A;if(e)for(let u=0,m=e.count;u<m;u+=3){const _=e.getX(u+0),x=e.getX(u+1),p=e.getX(u+2);s.fromBufferAttribute(t,_),r.fromBufferAttribute(t,x),a.fromBufferAttribute(t,p),h.subVectors(a,r),d.subVectors(s,r),h.cross(d),o.fromBufferAttribute(i,_),l.fromBufferAttribute(i,x),c.fromBufferAttribute(i,p),o.add(h),l.add(h),c.add(h),i.setXYZ(_,o.x,o.y,o.z),i.setXYZ(x,l.x,l.y,l.z),i.setXYZ(p,c.x,c.y,c.z)}else for(let u=0,m=t.count;u<m;u+=3)s.fromBufferAttribute(t,u+0),r.fromBufferAttribute(t,u+1),a.fromBufferAttribute(t,u+2),h.subVectors(a,r),d.subVectors(s,r),h.cross(d),i.setXYZ(u+0,h.x,h.y,h.z),i.setXYZ(u+1,h.x,h.y,h.z),i.setXYZ(u+2,h.x,h.y,h.z);this.normalizeNormals(),i.needsUpdate=!0}}normalizeNormals(){const e=this.attributes.normal;for(let t=0,i=e.count;t<i;t++)wt.fromBufferAttribute(e,t),wt.normalize(),e.setXYZ(t,wt.x,wt.y,wt.z)}toNonIndexed(){function e(o,l){const c=o.array,h=o.itemSize,d=o.normalized,u=new c.constructor(l.length*h);let m=0,_=0;for(let x=0,p=l.length;x<p;x++){o.isInterleavedBufferAttribute?m=l[x]*o.data.stride+o.offset:m=l[x]*h;for(let f=0;f<h;f++)u[_++]=c[m++]}return new Lt(u,h,d)}if(this.index===null)return Ce("BufferGeometry.toNonIndexed(): BufferGeometry is already non-indexed."),this;const t=new _t,i=this.index.array,s=this.attributes;for(const o in s){const l=s[o],c=e(l,i);t.setAttribute(o,c)}const r=this.morphAttributes;for(const o in r){const l=[],c=r[o];for(let h=0,d=c.length;h<d;h++){const u=c[h],m=e(u,i);l.push(m)}t.morphAttributes[o]=l}t.morphTargetsRelative=this.morphTargetsRelative;const a=this.groups;for(let o=0,l=a.length;o<l;o++){const c=a[o];t.addGroup(c.start,c.count,c.materialIndex)}return t}toJSON(){const e={metadata:{version:4.7,type:"BufferGeometry",generator:"BufferGeometry.toJSON"}};if(e.uuid=this.uuid,e.type=this.type,this.name!==""&&(e.name=this.name),Object.keys(this.userData).length>0&&(e.userData=this.userData),this.parameters!==void 0){const l=this.parameters;for(const c in l)l[c]!==void 0&&(e[c]=l[c]);return e}e.data={attributes:{}};const t=this.index;t!==null&&(e.data.index={type:t.array.constructor.name,array:Array.prototype.slice.call(t.array)});const i=this.attributes;for(const l in i){const c=i[l];e.data.attributes[l]=c.toJSON(e.data)}const s={};let r=!1;for(const l in this.morphAttributes){const c=this.morphAttributes[l],h=[];for(let d=0,u=c.length;d<u;d++){const m=c[d];h.push(m.toJSON(e.data))}h.length>0&&(s[l]=h,r=!0)}r&&(e.data.morphAttributes=s,e.data.morphTargetsRelative=this.morphTargetsRelative);const a=this.groups;a.length>0&&(e.data.groups=JSON.parse(JSON.stringify(a)));const o=this.boundingSphere;return o!==null&&(e.data.boundingSphere=o.toJSON()),e}clone(){return new this.constructor().copy(this)}copy(e){this.index=null,this.attributes={},this.morphAttributes={},this.groups=[],this.boundingBox=null,this.boundingSphere=null;const t={};this.name=e.name;const i=e.index;i!==null&&this.setIndex(i.clone());const s=e.attributes;for(const c in s){const h=s[c];this.setAttribute(c,h.clone(t))}const r=e.morphAttributes;for(const c in r){const h=[],d=r[c];for(let u=0,m=d.length;u<m;u++)h.push(d[u].clone(t));this.morphAttributes[c]=h}this.morphTargetsRelative=e.morphTargetsRelative;const a=e.groups;for(let c=0,h=a.length;c<h;c++){const d=a[c];this.addGroup(d.start,d.count,d.materialIndex)}const o=e.boundingBox;o!==null&&(this.boundingBox=o.clone());const l=e.boundingSphere;return l!==null&&(this.boundingSphere=l.clone()),this.drawRange.start=e.drawRange.start,this.drawRange.count=e.drawRange.count,this.userData=e.userData,this}dispose(){this.dispatchEvent({type:"dispose"})}}let Th=0;class Ki extends Ri{constructor(){super(),this.isMaterial=!0,Object.defineProperty(this,"id",{value:Th++}),this.uuid=cn(),this.name="",this.type="Material",this.blending=rn,this.side=ni,this.vertexColors=!1,this.opacity=1,this.transparent=!1,this.alphaHash=!1,this.blendSrc=Ks,this.blendDst=Js,this.blendEquation=Wi,this.blendSrcAlpha=null,this.blendDstAlpha=null,this.blendEquationAlpha=null,this.blendColor=new Re(0,0,0),this.blendAlpha=0,this.depthFunc=an,this.depthTest=!0,this.depthWrite=!0,this.stencilWriteMask=255,this.stencilFunc=_o,this.stencilRef=0,this.stencilFuncMask=255,this.stencilFail=ln,this.stencilZFail=ln,this.stencilZPass=ln,this.stencilWrite=!1,this.clippingPlanes=null,this.clipIntersection=!1,this.clipShadows=!1,this.shadowSide=null,this.colorWrite=!0,this.precision=null,this.polygonOffset=!1,this.polygonOffsetFactor=0,this.polygonOffsetUnits=0,this.dithering=!1,this.alphaToCoverage=!1,this.premultipliedAlpha=!1,this.forceSinglePass=!1,this.allowOverride=!0,this.visible=!0,this.toneMapped=!0,this.userData={},this.version=0,this._alphaTest=0}get alphaTest(){return this._alphaTest}set alphaTest(e){this._alphaTest>0!=e>0&&this.version++,this._alphaTest=e}onBeforeRender(){}onBeforeCompile(){}customProgramCacheKey(){return this.onBeforeCompile.toString()}setValues(e){if(e!==void 0)for(const t in e){const i=e[t];if(i===void 0){Ce(`Material: parameter '${t}' has value of undefined.`);continue}const s=this[t];if(s===void 0){Ce(`Material: '${t}' is not a property of THREE.${this.type}.`);continue}s&&s.isColor?s.set(i):s&&s.isVector3&&i&&i.isVector3?s.copy(i):this[t]=i}}toJSON(e){const t=e===void 0||typeof e=="string";t&&(e={textures:{},images:{}});const i={metadata:{version:4.7,type:"Material",generator:"Material.toJSON"}};i.uuid=this.uuid,i.type=this.type,this.name!==""&&(i.name=this.name),this.color&&this.color.isColor&&(i.color=this.color.getHex()),this.roughness!==void 0&&(i.roughness=this.roughness),this.metalness!==void 0&&(i.metalness=this.metalness),this.sheen!==void 0&&(i.sheen=this.sheen),this.sheenColor&&this.sheenColor.isColor&&(i.sheenColor=this.sheenColor.getHex()),this.sheenRoughness!==void 0&&(i.sheenRoughness=this.sheenRoughness),this.emissive&&this.emissive.isColor&&(i.emissive=this.emissive.getHex()),this.emissiveIntensity!==void 0&&this.emissiveIntensity!==1&&(i.emissiveIntensity=this.emissiveIntensity),this.specular&&this.specular.isColor&&(i.specular=this.specular.getHex()),this.specularIntensity!==void 0&&(i.specularIntensity=this.specularIntensity),this.specularColor&&this.specularColor.isColor&&(i.specularColor=this.specularColor.getHex()),this.shininess!==void 0&&(i.shininess=this.shininess),this.clearcoat!==void 0&&(i.clearcoat=this.clearcoat),this.clearcoatRoughness!==void 0&&(i.clearcoatRoughness=this.clearcoatRoughness),this.clearcoatMap&&this.clearcoatMap.isTexture&&(i.clearcoatMap=this.clearcoatMap.toJSON(e).uuid),this.clearcoatRoughnessMap&&this.clearcoatRoughnessMap.isTexture&&(i.clearcoatRoughnessMap=this.clearcoatRoughnessMap.toJSON(e).uuid),this.clearcoatNormalMap&&this.clearcoatNormalMap.isTexture&&(i.clearcoatNormalMap=this.clearcoatNormalMap.toJSON(e).uuid,i.clearcoatNormalScale=this.clearcoatNormalScale.toArray()),this.sheenColorMap&&this.sheenColorMap.isTexture&&(i.sheenColorMap=this.sheenColorMap.toJSON(e).uuid),this.sheenRoughnessMap&&this.sheenRoughnessMap.isTexture&&(i.sheenRoughnessMap=this.sheenRoughnessMap.toJSON(e).uuid),this.dispersion!==void 0&&(i.dispersion=this.dispersion),this.iridescence!==void 0&&(i.iridescence=this.iridescence),this.iridescenceIOR!==void 0&&(i.iridescenceIOR=this.iridescenceIOR),this.iridescenceThicknessRange!==void 0&&(i.iridescenceThicknessRange=this.iridescenceThicknessRange),this.iridescenceMap&&this.iridescenceMap.isTexture&&(i.iridescenceMap=this.iridescenceMap.toJSON(e).uuid),this.iridescenceThicknessMap&&this.iridescenceThicknessMap.isTexture&&(i.iridescenceThicknessMap=this.iridescenceThicknessMap.toJSON(e).uuid),this.anisotropy!==void 0&&(i.anisotropy=this.anisotropy),this.anisotropyRotation!==void 0&&(i.anisotropyRotation=this.anisotropyRotation),this.anisotropyMap&&this.anisotropyMap.isTexture&&(i.anisotropyMap=this.anisotropyMap.toJSON(e).uuid),this.map&&this.map.isTexture&&(i.map=this.map.toJSON(e).uuid),this.matcap&&this.matcap.isTexture&&(i.matcap=this.matcap.toJSON(e).uuid),this.alphaMap&&this.alphaMap.isTexture&&(i.alphaMap=this.alphaMap.toJSON(e).uuid),this.lightMap&&this.lightMap.isTexture&&(i.lightMap=this.lightMap.toJSON(e).uuid,i.lightMapIntensity=this.lightMapIntensity),this.aoMap&&this.aoMap.isTexture&&(i.aoMap=this.aoMap.toJSON(e).uuid,i.aoMapIntensity=this.aoMapIntensity),this.bumpMap&&this.bumpMap.isTexture&&(i.bumpMap=this.bumpMap.toJSON(e).uuid,i.bumpScale=this.bumpScale),this.normalMap&&this.normalMap.isTexture&&(i.normalMap=this.normalMap.toJSON(e).uuid,i.normalMapType=this.normalMapType,i.normalScale=this.normalScale.toArray()),this.displacementMap&&this.displacementMap.isTexture&&(i.displacementMap=this.displacementMap.toJSON(e).uuid,i.displacementScale=this.displacementScale,i.displacementBias=this.displacementBias),this.roughnessMap&&this.roughnessMap.isTexture&&(i.roughnessMap=this.roughnessMap.toJSON(e).uuid),this.metalnessMap&&this.metalnessMap.isTexture&&(i.metalnessMap=this.metalnessMap.toJSON(e).uuid),this.emissiveMap&&this.emissiveMap.isTexture&&(i.emissiveMap=this.emissiveMap.toJSON(e).uuid),this.specularMap&&this.specularMap.isTexture&&(i.specularMap=this.specularMap.toJSON(e).uuid),this.specularIntensityMap&&this.specularIntensityMap.isTexture&&(i.specularIntensityMap=this.specularIntensityMap.toJSON(e).uuid),this.specularColorMap&&this.specularColorMap.isTexture&&(i.specularColorMap=this.specularColorMap.toJSON(e).uuid),this.envMap&&this.envMap.isTexture&&(i.envMap=this.envMap.toJSON(e).uuid,this.combine!==void 0&&(i.combine=this.combine)),this.envMapRotation!==void 0&&(i.envMapRotation=this.envMapRotation.toArray()),this.envMapIntensity!==void 0&&(i.envMapIntensity=this.envMapIntensity),this.reflectivity!==void 0&&(i.reflectivity=this.reflectivity),this.refractionRatio!==void 0&&(i.refractionRatio=this.refractionRatio),this.gradientMap&&this.gradientMap.isTexture&&(i.gradientMap=this.gradientMap.toJSON(e).uuid),this.transmission!==void 0&&(i.transmission=this.transmission),this.transmissionMap&&this.transmissionMap.isTexture&&(i.transmissionMap=this.transmissionMap.toJSON(e).uuid),this.thickness!==void 0&&(i.thickness=this.thickness),this.thicknessMap&&this.thicknessMap.isTexture&&(i.thicknessMap=this.thicknessMap.toJSON(e).uuid),this.attenuationDistance!==void 0&&this.attenuationDistance!==1/0&&(i.attenuationDistance=this.attenuationDistance),this.attenuationColor!==void 0&&(i.attenuationColor=this.attenuationColor.getHex()),this.size!==void 0&&(i.size=this.size),this.shadowSide!==null&&(i.shadowSide=this.shadowSide),this.sizeAttenuation!==void 0&&(i.sizeAttenuation=this.sizeAttenuation),this.blending!==rn&&(i.blending=this.blending),this.side!==ni&&(i.side=this.side),this.vertexColors===!0&&(i.vertexColors=!0),this.opacity<1&&(i.opacity=this.opacity),this.transparent===!0&&(i.transparent=!0),this.blendSrc!==Ks&&(i.blendSrc=this.blendSrc),this.blendDst!==Js&&(i.blendDst=this.blendDst),this.blendEquation!==Wi&&(i.blendEquation=this.blendEquation),this.blendSrcAlpha!==null&&(i.blendSrcAlpha=this.blendSrcAlpha),this.blendDstAlpha!==null&&(i.blendDstAlpha=this.blendDstAlpha),this.blendEquationAlpha!==null&&(i.blendEquationAlpha=this.blendEquationAlpha),this.blendColor&&this.blendColor.isColor&&(i.blendColor=this.blendColor.getHex()),this.blendAlpha!==0&&(i.blendAlpha=this.blendAlpha),this.depthFunc!==an&&(i.depthFunc=this.depthFunc),this.depthTest===!1&&(i.depthTest=this.depthTest),this.depthWrite===!1&&(i.depthWrite=this.depthWrite),this.colorWrite===!1&&(i.colorWrite=this.colorWrite),this.stencilWriteMask!==255&&(i.stencilWriteMask=this.stencilWriteMask),this.stencilFunc!==_o&&(i.stencilFunc=this.stencilFunc),this.stencilRef!==0&&(i.stencilRef=this.stencilRef),this.stencilFuncMask!==255&&(i.stencilFuncMask=this.stencilFuncMask),this.stencilFail!==ln&&(i.stencilFail=this.stencilFail),this.stencilZFail!==ln&&(i.stencilZFail=this.stencilZFail),this.stencilZPass!==ln&&(i.stencilZPass=this.stencilZPass),this.stencilWrite===!0&&(i.stencilWrite=this.stencilWrite),this.rotation!==void 0&&this.rotation!==0&&(i.rotation=this.rotation),this.polygonOffset===!0&&(i.polygonOffset=!0),this.polygonOffsetFactor!==0&&(i.polygonOffsetFactor=this.polygonOffsetFactor),this.polygonOffsetUnits!==0&&(i.polygonOffsetUnits=this.polygonOffsetUnits),this.linewidth!==void 0&&this.linewidth!==1&&(i.linewidth=this.linewidth),this.dashSize!==void 0&&(i.dashSize=this.dashSize),this.gapSize!==void 0&&(i.gapSize=this.gapSize),this.scale!==void 0&&(i.scale=this.scale),this.dithering===!0&&(i.dithering=!0),this.alphaTest>0&&(i.alphaTest=this.alphaTest),this.alphaHash===!0&&(i.alphaHash=!0),this.alphaToCoverage===!0&&(i.alphaToCoverage=!0),this.premultipliedAlpha===!0&&(i.premultipliedAlpha=!0),this.forceSinglePass===!0&&(i.forceSinglePass=!0),this.allowOverride===!1&&(i.allowOverride=!1),this.wireframe===!0&&(i.wireframe=!0),this.wireframeLinewidth>1&&(i.wireframeLinewidth=this.wireframeLinewidth),this.wireframeLinecap!=="round"&&(i.wireframeLinecap=this.wireframeLinecap),this.wireframeLinejoin!=="round"&&(i.wireframeLinejoin=this.wireframeLinejoin),this.flatShading===!0&&(i.flatShading=!0),this.visible===!1&&(i.visible=!1),this.toneMapped===!1&&(i.toneMapped=!1),this.fog===!1&&(i.fog=!1),Object.keys(this.userData).length>0&&(i.userData=this.userData);function s(r){const a=[];for(const o in r){const l=r[o];delete l.metadata,a.push(l)}return a}if(t){const r=s(e.textures),a=s(e.images);r.length>0&&(i.textures=r),a.length>0&&(i.images=a)}return i}clone(){return new this.constructor().copy(this)}copy(e){this.name=e.name,this.blending=e.blending,this.side=e.side,this.vertexColors=e.vertexColors,this.opacity=e.opacity,this.transparent=e.transparent,this.blendSrc=e.blendSrc,this.blendDst=e.blendDst,this.blendEquation=e.blendEquation,this.blendSrcAlpha=e.blendSrcAlpha,this.blendDstAlpha=e.blendDstAlpha,this.blendEquationAlpha=e.blendEquationAlpha,this.blendColor.copy(e.blendColor),this.blendAlpha=e.blendAlpha,this.depthFunc=e.depthFunc,this.depthTest=e.depthTest,this.depthWrite=e.depthWrite,this.stencilWriteMask=e.stencilWriteMask,this.stencilFunc=e.stencilFunc,this.stencilRef=e.stencilRef,this.stencilFuncMask=e.stencilFuncMask,this.stencilFail=e.stencilFail,this.stencilZFail=e.stencilZFail,this.stencilZPass=e.stencilZPass,this.stencilWrite=e.stencilWrite;const t=e.clippingPlanes;let i=null;if(t!==null){const s=t.length;i=new Array(s);for(let r=0;r!==s;++r)i[r]=t[r].clone()}return this.clippingPlanes=i,this.clipIntersection=e.clipIntersection,this.clipShadows=e.clipShadows,this.shadowSide=e.shadowSide,this.colorWrite=e.colorWrite,this.precision=e.precision,this.polygonOffset=e.polygonOffset,this.polygonOffsetFactor=e.polygonOffsetFactor,this.polygonOffsetUnits=e.polygonOffsetUnits,this.dithering=e.dithering,this.alphaTest=e.alphaTest,this.alphaHash=e.alphaHash,this.alphaToCoverage=e.alphaToCoverage,this.premultipliedAlpha=e.premultipliedAlpha,this.forceSinglePass=e.forceSinglePass,this.allowOverride=e.allowOverride,this.visible=e.visible,this.toneMapped=e.toneMapped,this.userData=JSON.parse(JSON.stringify(e.userData)),this}dispose(){this.dispatchEvent({type:"dispose"})}set needsUpdate(e){e===!0&&this.version++}}const wi=new A,va=new A,bs=new A,Oi=new A,xa=new A,Ss=new A,ba=new A;class jn{constructor(e=new A,t=new A(0,0,-1)){this.origin=e,this.direction=t}set(e,t){return this.origin.copy(e),this.direction.copy(t),this}copy(e){return this.origin.copy(e.origin),this.direction.copy(e.direction),this}at(e,t){return t.copy(this.origin).addScaledVector(this.direction,e)}lookAt(e){return this.direction.copy(e).sub(this.origin).normalize(),this}recast(e){return this.origin.copy(this.at(e,wi)),this}closestPointToPoint(e,t){t.subVectors(e,this.origin);const i=t.dot(this.direction);return i<0?t.copy(this.origin):t.copy(this.origin).addScaledVector(this.direction,i)}distanceToPoint(e){return Math.sqrt(this.distanceSqToPoint(e))}distanceSqToPoint(e){const t=wi.subVectors(e,this.origin).dot(this.direction);return t<0?this.origin.distanceToSquared(e):(wi.copy(this.origin).addScaledVector(this.direction,t),wi.distanceToSquared(e))}distanceSqToSegment(e,t,i,s){va.copy(e).add(t).multiplyScalar(.5),bs.copy(t).sub(e).normalize(),Oi.copy(this.origin).sub(va);const r=e.distanceTo(t)*.5,a=-this.direction.dot(bs),o=Oi.dot(this.direction),l=-Oi.dot(bs),c=Oi.lengthSq(),h=Math.abs(1-a*a);let d,u,m,_;if(h>0)if(d=a*l-o,u=a*o-l,_=r*h,d>=0)if(u>=-_)if(u<=_){const x=1/h;d*=x,u*=x,m=d*(d+a*u+2*o)+u*(a*d+u+2*l)+c}else u=r,d=Math.max(0,-(a*u+o)),m=-d*d+u*(u+2*l)+c;else u=-r,d=Math.max(0,-(a*u+o)),m=-d*d+u*(u+2*l)+c;else u<=-_?(d=Math.max(0,-(-a*r+o)),u=d>0?-r:Math.min(Math.max(-r,-l),r),m=-d*d+u*(u+2*l)+c):u<=_?(d=0,u=Math.min(Math.max(-r,-l),r),m=u*(u+2*l)+c):(d=Math.max(0,-(a*r+o)),u=d>0?r:Math.min(Math.max(-r,-l),r),m=-d*d+u*(u+2*l)+c);else u=a>0?-r:r,d=Math.max(0,-(a*u+o)),m=-d*d+u*(u+2*l)+c;return i&&i.copy(this.origin).addScaledVector(this.direction,d),s&&s.copy(va).addScaledVector(bs,u),m}intersectSphere(e,t){wi.subVectors(e.center,this.origin);const i=wi.dot(this.direction),s=wi.dot(wi)-i*i,r=e.radius*e.radius;if(s>r)return null;const a=Math.sqrt(r-s),o=i-a,l=i+a;return l<0?null:o<0?this.at(l,t):this.at(o,t)}intersectsSphere(e){return e.radius<0?!1:this.distanceSqToPoint(e.center)<=e.radius*e.radius}distanceToPlane(e){const t=e.normal.dot(this.direction);if(t===0)return e.distanceToPoint(this.origin)===0?0:null;const i=-(this.origin.dot(e.normal)+e.constant)/t;return i>=0?i:null}intersectPlane(e,t){const i=this.distanceToPlane(e);return i===null?null:this.at(i,t)}intersectsPlane(e){const t=e.distanceToPoint(this.origin);return t===0||e.normal.dot(this.direction)*t<0}intersectBox(e,t){let i,s,r,a,o,l;const c=1/this.direction.x,h=1/this.direction.y,d=1/this.direction.z,u=this.origin;return c>=0?(i=(e.min.x-u.x)*c,s=(e.max.x-u.x)*c):(i=(e.max.x-u.x)*c,s=(e.min.x-u.x)*c),h>=0?(r=(e.min.y-u.y)*h,a=(e.max.y-u.y)*h):(r=(e.max.y-u.y)*h,a=(e.min.y-u.y)*h),i>a||r>s||((r>i||isNaN(i))&&(i=r),(a<s||isNaN(s))&&(s=a),d>=0?(o=(e.min.z-u.z)*d,l=(e.max.z-u.z)*d):(o=(e.max.z-u.z)*d,l=(e.min.z-u.z)*d),i>l||o>s)||((o>i||i!==i)&&(i=o),(l<s||s!==s)&&(s=l),s<0)?null:this.at(i>=0?i:s,t)}intersectsBox(e){return this.intersectBox(e,wi)!==null}intersectTriangle(e,t,i,s,r){xa.subVectors(t,e),Ss.subVectors(i,e),ba.crossVectors(xa,Ss);let a=this.direction.dot(ba),o;if(a>0){if(s)return null;o=1}else if(a<0)o=-1,a=-a;else return null;Oi.subVectors(this.origin,e);const l=o*this.direction.dot(Ss.crossVectors(Oi,Ss));if(l<0)return null;const c=o*this.direction.dot(xa.cross(Oi));if(c<0||l+c>a)return null;const h=-o*Oi.dot(ba);return h<0?null:this.at(h/a,r)}applyMatrix4(e){return this.origin.applyMatrix4(e),this.direction.transformDirection(e),this}equals(e){return e.origin.equals(this.origin)&&e.direction.equals(this.direction)}clone(){return new this.constructor().copy(this)}}class Bi extends Ki{constructor(e){super(),this.isMeshBasicMaterial=!0,this.type="MeshBasicMaterial",this.color=new Re(16777215),this.map=null,this.lightMap=null,this.lightMapIntensity=1,this.aoMap=null,this.aoMapIntensity=1,this.specularMap=null,this.alphaMap=null,this.envMap=null,this.envMapRotation=new Di,this.combine=eo,this.reflectivity=1,this.refractionRatio=.98,this.wireframe=!1,this.wireframeLinewidth=1,this.wireframeLinecap="round",this.wireframeLinejoin="round",this.fog=!0,this.setValues(e)}copy(e){return super.copy(e),this.color.copy(e.color),this.map=e.map,this.lightMap=e.lightMap,this.lightMapIntensity=e.lightMapIntensity,this.aoMap=e.aoMap,this.aoMapIntensity=e.aoMapIntensity,this.specularMap=e.specularMap,this.alphaMap=e.alphaMap,this.envMap=e.envMap,this.envMapRotation.copy(e.envMapRotation),this.combine=e.combine,this.reflectivity=e.reflectivity,this.refractionRatio=e.refractionRatio,this.wireframe=e.wireframe,this.wireframeLinewidth=e.wireframeLinewidth,this.wireframeLinecap=e.wireframeLinecap,this.wireframeLinejoin=e.wireframeLinejoin,this.fog=e.fog,this}}const zo=new lt,Ji=new jn,Ms=new Sn,ko=new A,ys=new A,Es=new A,ws=new A,Sa=new A,Ts=new A,Vo=new A,As=new A;class Ht extends xt{constructor(e=new _t,t=new Bi){super(),this.isMesh=!0,this.type="Mesh",this.geometry=e,this.material=t,this.morphTargetDictionary=void 0,this.morphTargetInfluences=void 0,this.count=1,this.updateMorphTargets()}copy(e,t){return super.copy(e,t),e.morphTargetInfluences!==void 0&&(this.morphTargetInfluences=e.morphTargetInfluences.slice()),e.morphTargetDictionary!==void 0&&(this.morphTargetDictionary=Object.assign({},e.morphTargetDictionary)),this.material=Array.isArray(e.material)?e.material.slice():e.material,this.geometry=e.geometry,this}updateMorphTargets(){const t=this.geometry.morphAttributes,i=Object.keys(t);if(i.length>0){const s=t[i[0]];if(s!==void 0){this.morphTargetInfluences=[],this.morphTargetDictionary={};for(let r=0,a=s.length;r<a;r++){const o=s[r].name||String(r);this.morphTargetInfluences.push(0),this.morphTargetDictionary[o]=r}}}}getVertexPosition(e,t){const i=this.geometry,s=i.attributes.position,r=i.morphAttributes.position,a=i.morphTargetsRelative;t.fromBufferAttribute(s,e);const o=this.morphTargetInfluences;if(r&&o){Ts.set(0,0,0);for(let l=0,c=r.length;l<c;l++){const h=o[l],d=r[l];h!==0&&(Sa.fromBufferAttribute(d,e),a?Ts.addScaledVector(Sa,h):Ts.addScaledVector(Sa.sub(t),h))}t.add(Ts)}return t}raycast(e,t){const i=this.geometry,s=this.material,r=this.matrixWorld;s!==void 0&&(i.boundingSphere===null&&i.computeBoundingSphere(),Ms.copy(i.boundingSphere),Ms.applyMatrix4(r),Ji.copy(e.ray).recast(e.near),!(Ms.containsPoint(Ji.origin)===!1&&(Ji.intersectSphere(Ms,ko)===null||Ji.origin.distanceToSquared(ko)>(e.far-e.near)**2))&&(zo.copy(r).invert(),Ji.copy(e.ray).applyMatrix4(zo),!(i.boundingBox!==null&&Ji.intersectsBox(i.boundingBox)===!1)&&this._computeIntersections(e,t,Ji)))}_computeIntersections(e,t,i){let s;const r=this.geometry,a=this.material,o=r.index,l=r.attributes.position,c=r.attributes.uv,h=r.attributes.uv1,d=r.attributes.normal,u=r.groups,m=r.drawRange;if(o!==null)if(Array.isArray(a))for(let _=0,x=u.length;_<x;_++){const p=u[_],f=a[p.materialIndex],M=Math.max(p.start,m.start),y=Math.min(o.count,Math.min(p.start+p.count,m.start+m.count));for(let E=M,L=y;E<L;E+=3){const w=o.getX(E),D=o.getX(E+1),b=o.getX(E+2);s=Cs(this,f,e,i,c,h,d,w,D,b),s&&(s.faceIndex=Math.floor(E/3),s.face.materialIndex=p.materialIndex,t.push(s))}}else{const _=Math.max(0,m.start),x=Math.min(o.count,m.start+m.count);for(let p=_,f=x;p<f;p+=3){const M=o.getX(p),y=o.getX(p+1),E=o.getX(p+2);s=Cs(this,a,e,i,c,h,d,M,y,E),s&&(s.faceIndex=Math.floor(p/3),t.push(s))}}else if(l!==void 0)if(Array.isArray(a))for(let _=0,x=u.length;_<x;_++){const p=u[_],f=a[p.materialIndex],M=Math.max(p.start,m.start),y=Math.min(l.count,Math.min(p.start+p.count,m.start+m.count));for(let E=M,L=y;E<L;E+=3){const w=E,D=E+1,b=E+2;s=Cs(this,f,e,i,c,h,d,w,D,b),s&&(s.faceIndex=Math.floor(E/3),s.face.materialIndex=p.materialIndex,t.push(s))}}else{const _=Math.max(0,m.start),x=Math.min(l.count,m.start+m.count);for(let p=_,f=x;p<f;p+=3){const M=p,y=p+1,E=p+2;s=Cs(this,a,e,i,c,h,d,M,y,E),s&&(s.faceIndex=Math.floor(p/3),t.push(s))}}}}function Ah(n,e,t,i,s,r,a,o){let l;if(e.side===vt?l=i.intersectTriangle(a,r,s,!0,o):l=i.intersectTriangle(s,r,a,e.side===ni,o),l===null)return null;As.copy(o),As.applyMatrix4(n.matrixWorld);const c=t.ray.origin.distanceTo(As);return c<t.near||c>t.far?null:{distance:c,point:As.clone(),object:n}}function Cs(n,e,t,i,s,r,a,o,l,c){n.getVertexPosition(o,ys),n.getVertexPosition(l,Es),n.getVertexPosition(c,ws);const h=Ah(n,e,t,i,ys,Es,ws,Vo);if(h){const d=new A;qt.getBarycoord(Vo,ys,Es,ws,d),s&&(h.uv=qt.getInterpolatedAttribute(s,o,l,c,d,new Ne)),r&&(h.uv1=qt.getInterpolatedAttribute(r,o,l,c,d,new Ne)),a&&(h.normal=qt.getInterpolatedAttribute(a,o,l,c,d,new A),h.normal.dot(i.direction)>0&&h.normal.multiplyScalar(-1));const u={a:o,b:l,c,normal:new A,materialIndex:0};qt.getNormal(ys,Es,ws,u.normal),h.face=u,h.barycoord=d}return h}class Ch extends Ut{constructor(e=null,t=1,i=1,s,r,a,o,l,c=Tt,h=Tt,d,u){super(null,a,o,l,c,h,s,r,d,u),this.isDataTexture=!0,this.image={data:e,width:t,height:i},this.generateMipmaps=!1,this.flipY=!1,this.unpackAlignment=1}}const Ma=new A,Rh=new A,Ph=new Ue;class Ti{constructor(e=new A(1,0,0),t=0){this.isPlane=!0,this.normal=e,this.constant=t}set(e,t){return this.normal.copy(e),this.constant=t,this}setComponents(e,t,i,s){return this.normal.set(e,t,i),this.constant=s,this}setFromNormalAndCoplanarPoint(e,t){return this.normal.copy(e),this.constant=-t.dot(this.normal),this}setFromCoplanarPoints(e,t,i){const s=Ma.subVectors(i,t).cross(Rh.subVectors(e,t)).normalize();return this.setFromNormalAndCoplanarPoint(s,e),this}copy(e){return this.normal.copy(e.normal),this.constant=e.constant,this}normalize(){const e=1/this.normal.length();return this.normal.multiplyScalar(e),this.constant*=e,this}negate(){return this.constant*=-1,this.normal.negate(),this}distanceToPoint(e){return this.normal.dot(e)+this.constant}distanceToSphere(e){return this.distanceToPoint(e.center)-e.radius}projectPoint(e,t){return t.copy(e).addScaledVector(this.normal,-this.distanceToPoint(e))}intersectLine(e,t,i=!0){const s=e.delta(Ma),r=this.normal.dot(s);if(r===0)return this.distanceToPoint(e.start)===0?t.copy(e.start):null;const a=-(e.start.dot(this.normal)+this.constant)/r;return i===!0&&(a<0||a>1)?null:t.copy(e.start).addScaledVector(s,a)}intersectsLine(e){const t=this.distanceToPoint(e.start),i=this.distanceToPoint(e.end);return t<0&&i>0||i<0&&t>0}intersectsBox(e){return e.intersectsPlane(this)}intersectsSphere(e){return e.intersectsPlane(this)}coplanarPoint(e){return e.copy(this.normal).multiplyScalar(-this.constant)}applyMatrix4(e,t){const i=t||Ph.getNormalMatrix(e),s=this.coplanarPoint(Ma).applyMatrix4(e),r=this.normal.applyMatrix3(i).normalize();return this.constant=-s.dot(r),this}translate(e){return this.constant-=e.dot(this.normal),this}equals(e){return e.normal.equals(this.normal)&&e.constant===this.constant}clone(){return new this.constructor().copy(this)}}const Qi=new Sn,Lh=new Ne(.5,.5),Rs=new A;class ya{constructor(e=new Ti,t=new Ti,i=new Ti,s=new Ti,r=new Ti,a=new Ti){this.planes=[e,t,i,s,r,a]}set(e,t,i,s,r,a){const o=this.planes;return o[0].copy(e),o[1].copy(t),o[2].copy(i),o[3].copy(s),o[4].copy(r),o[5].copy(a),this}copy(e){const t=this.planes;for(let i=0;i<6;i++)t[i].copy(e.planes[i]);return this}setFromProjectionMatrix(e,t=oi,i=!1){const s=this.planes,r=e.elements,a=r[0],o=r[1],l=r[2],c=r[3],h=r[4],d=r[5],u=r[6],m=r[7],_=r[8],x=r[9],p=r[10],f=r[11],M=r[12],y=r[13],E=r[14],L=r[15];if(s[0].setComponents(c-a,m-h,f-_,L-M).normalize(),s[1].setComponents(c+a,m+h,f+_,L+M).normalize(),s[2].setComponents(c+o,m+d,f+x,L+y).normalize(),s[3].setComponents(c-o,m-d,f-x,L-y).normalize(),i)s[4].setComponents(l,u,p,E).normalize(),s[5].setComponents(c-l,m-u,f-p,L-E).normalize();else if(s[4].setComponents(c-l,m-u,f-p,L-E).normalize(),t===oi)s[5].setComponents(c+l,m+u,f+p,L+E).normalize();else if(t===Bn)s[5].setComponents(l,u,p,E).normalize();else throw new Error("THREE.Frustum.setFromProjectionMatrix(): Invalid coordinate system: "+t);return this}intersectsObject(e){if(e.boundingSphere!==void 0)e.boundingSphere===null&&e.computeBoundingSphere(),Qi.copy(e.boundingSphere).applyMatrix4(e.matrixWorld);else{const t=e.geometry;t.boundingSphere===null&&t.computeBoundingSphere(),Qi.copy(t.boundingSphere).applyMatrix4(e.matrixWorld)}return this.intersectsSphere(Qi)}intersectsSprite(e){Qi.center.set(0,0,0);const t=Lh.distanceTo(e.center);return Qi.radius=.7071067811865476+t,Qi.applyMatrix4(e.matrixWorld),this.intersectsSphere(Qi)}intersectsSphere(e){const t=this.planes,i=e.center,s=-e.radius;for(let r=0;r<6;r++)if(t[r].distanceToPoint(i)<s)return!1;return!0}intersectsBox(e){const t=this.planes;for(let i=0;i<6;i++){const s=t[i];if(Rs.x=s.normal.x>0?e.max.x:e.min.x,Rs.y=s.normal.y>0?e.max.y:e.min.y,Rs.z=s.normal.z>0?e.max.z:e.min.z,s.distanceToPoint(Rs)<0)return!1}return!0}containsPoint(e){const t=this.planes;for(let i=0;i<6;i++)if(t[i].distanceToPoint(e)<0)return!1;return!0}clone(){return new this.constructor().copy(this)}}class yn extends Ki{constructor(e){super(),this.isLineBasicMaterial=!0,this.type="LineBasicMaterial",this.color=new Re(16777215),this.map=null,this.linewidth=1,this.linecap="round",this.linejoin="round",this.fog=!0,this.setValues(e)}copy(e){return super.copy(e),this.color.copy(e.color),this.map=e.map,this.linewidth=e.linewidth,this.linecap=e.linecap,this.linejoin=e.linejoin,this.fog=e.fog,this}}const Ps=new A,Ls=new A,Go=new lt,qn=new jn,Ds=new Sn,Ea=new A,Ho=new A;class Wo extends xt{constructor(e=new _t,t=new yn){super(),this.isLine=!0,this.type="Line",this.geometry=e,this.material=t,this.morphTargetDictionary=void 0,this.morphTargetInfluences=void 0,this.updateMorphTargets()}copy(e,t){return super.copy(e,t),this.material=Array.isArray(e.material)?e.material.slice():e.material,this.geometry=e.geometry,this}computeLineDistances(){const e=this.geometry;if(e.index===null){const t=e.attributes.position,i=[0];for(let s=1,r=t.count;s<r;s++)Ps.fromBufferAttribute(t,s-1),Ls.fromBufferAttribute(t,s),i[s]=i[s-1],i[s]+=Ps.distanceTo(Ls);e.setAttribute("lineDistance",new Ft(i,1))}else Ce("Line.computeLineDistances(): Computation only possible with non-indexed BufferGeometry.");return this}raycast(e,t){const i=this.geometry,s=this.matrixWorld,r=e.params.Line.threshold,a=i.drawRange;if(i.boundingSphere===null&&i.computeBoundingSphere(),Ds.copy(i.boundingSphere),Ds.applyMatrix4(s),Ds.radius+=r,e.ray.intersectsSphere(Ds)===!1)return;Go.copy(s).invert(),qn.copy(e.ray).applyMatrix4(Go);const o=r/((this.scale.x+this.scale.y+this.scale.z)/3),l=o*o,c=this.isLineSegments?2:1,h=i.index,u=i.attributes.position;if(h!==null){const m=Math.max(0,a.start),_=Math.min(h.count,a.start+a.count);for(let x=m,p=_-1;x<p;x+=c){const f=h.getX(x),M=h.getX(x+1),y=Is(this,e,qn,l,f,M,x);y&&t.push(y)}if(this.isLineLoop){const x=h.getX(_-1),p=h.getX(m),f=Is(this,e,qn,l,x,p,_-1);f&&t.push(f)}}else{const m=Math.max(0,a.start),_=Math.min(u.count,a.start+a.count);for(let x=m,p=_-1;x<p;x+=c){const f=Is(this,e,qn,l,x,x+1,x);f&&t.push(f)}if(this.isLineLoop){const x=Is(this,e,qn,l,_-1,m,_-1);x&&t.push(x)}}}updateMorphTargets(){const t=this.geometry.morphAttributes,i=Object.keys(t);if(i.length>0){const s=t[i[0]];if(s!==void 0){this.morphTargetInfluences=[],this.morphTargetDictionary={};for(let r=0,a=s.length;r<a;r++){const o=s[r].name||String(r);this.morphTargetInfluences.push(0),this.morphTargetDictionary[o]=r}}}}}function Is(n,e,t,i,s,r,a){const o=n.geometry.attributes.position;if(Ps.fromBufferAttribute(o,s),Ls.fromBufferAttribute(o,r),t.distanceSqToSegment(Ps,Ls,Ea,Ho)>i)return;Ea.applyMatrix4(n.matrixWorld);const c=e.ray.origin.distanceTo(Ea);if(!(c<e.near||c>e.far))return{distance:c,point:Ho.clone().applyMatrix4(n.matrixWorld),index:a,face:null,faceIndex:null,barycoord:null,object:n}}const Xo=new A,jo=new A;class Ns extends Wo{constructor(e,t){super(e,t),this.isLineSegments=!0,this.type="LineSegments"}computeLineDistances(){const e=this.geometry;if(e.index===null){const t=e.attributes.position,i=[];for(let s=0,r=t.count;s<r;s+=2)Xo.fromBufferAttribute(t,s),jo.fromBufferAttribute(t,s+1),i[s]=s===0?0:i[s-1],i[s+1]=i[s]+Xo.distanceTo(jo);e.setAttribute("lineDistance",new Ft(i,1))}else Ce("LineSegments.computeLineDistances(): Computation only possible with non-indexed BufferGeometry.");return this}}class Us extends Ki{constructor(e){super(),this.isPointsMaterial=!0,this.type="PointsMaterial",this.color=new Re(16777215),this.map=null,this.alphaMap=null,this.size=1,this.sizeAttenuation=!0,this.fog=!0,this.setValues(e)}copy(e){return super.copy(e),this.color.copy(e.color),this.map=e.map,this.alphaMap=e.alphaMap,this.size=e.size,this.sizeAttenuation=e.sizeAttenuation,this.fog=e.fog,this}}const qo=new lt,wa=new jn,Fs=new Sn,Os=new A;class Ta extends xt{constructor(e=new _t,t=new Us){super(),this.isPoints=!0,this.type="Points",this.geometry=e,this.material=t,this.morphTargetDictionary=void 0,this.morphTargetInfluences=void 0,this.updateMorphTargets()}copy(e,t){return super.copy(e,t),this.material=Array.isArray(e.material)?e.material.slice():e.material,this.geometry=e.geometry,this}raycast(e,t){const i=this.geometry,s=this.matrixWorld,r=e.params.Points.threshold,a=i.drawRange;if(i.boundingSphere===null&&i.computeBoundingSphere(),Fs.copy(i.boundingSphere),Fs.applyMatrix4(s),Fs.radius+=r,e.ray.intersectsSphere(Fs)===!1)return;qo.copy(s).invert(),wa.copy(e.ray).applyMatrix4(qo);const o=r/((this.scale.x+this.scale.y+this.scale.z)/3),l=o*o,c=i.index,d=i.attributes.position;if(c!==null){const u=Math.max(0,a.start),m=Math.min(c.count,a.start+a.count);for(let _=u,x=m;_<x;_++){const p=c.getX(_);Os.fromBufferAttribute(d,p),Yo(Os,p,l,s,e,t,this)}}else{const u=Math.max(0,a.start),m=Math.min(d.count,a.start+a.count);for(let _=u,x=m;_<x;_++)Os.fromBufferAttribute(d,_),Yo(Os,_,l,s,e,t,this)}}updateMorphTargets(){const t=this.geometry.morphAttributes,i=Object.keys(t);if(i.length>0){const s=t[i[0]];if(s!==void 0){this.morphTargetInfluences=[],this.morphTargetDictionary={};for(let r=0,a=s.length;r<a;r++){const o=s[r].name||String(r);this.morphTargetInfluences.push(0),this.morphTargetDictionary[o]=r}}}}}function Yo(n,e,t,i,s,r,a){const o=wa.distanceSqToPoint(n);if(o<t){const l=new A;wa.closestPointToPoint(n,l),l.applyMatrix4(i);const c=s.ray.origin.distanceTo(l);if(c<s.near||c>s.far)return;r.push({distance:c,distanceToRay:Math.sqrt(o),point:l,index:e,face:null,faceIndex:null,barycoord:null,object:a})}}class $o extends Ut{constructor(e=[],t=Xi,i,s,r,a,o,l,c,h){super(e,t,i,s,r,a,o,l,c,h),this.isCubeTexture=!0,this.flipY=!1}get images(){return this.image}set images(e){this.image=e}}class En extends Ut{constructor(e,t,i=ri,s,r,a,o=Tt,l=Tt,c,h=xi,d=1){if(h!==xi&&h!==qi)throw new Error("DepthTexture format must be either THREE.DepthFormat or THREE.DepthStencilFormat");const u={width:e,height:t,depth:d};super(u,s,r,a,o,l,h,i,c),this.isDepthTexture=!0,this.flipY=!1,this.generateMipmaps=!1,this.compareFunction=null}copy(e){return super.copy(e),this.source=new ta(Object.assign({},e.image)),this.compareFunction=e.compareFunction,this}toJSON(e){const t=super.toJSON(e);return this.compareFunction!==null&&(t.compareFunction=this.compareFunction),t}}class Dh extends En{constructor(e,t=ri,i=Xi,s,r,a=Tt,o=Tt,l,c=xi){const h={width:e,height:e,depth:1},d=[h,h,h,h,h,h];super(e,e,t,i,s,r,a,o,l,c),this.image=d,this.isCubeDepthTexture=!0,this.isCubeTexture=!0}get images(){return this.image}set images(e){this.image=e}}class Zo extends Ut{constructor(e=null){super(),this.sourceTexture=e,this.isExternalTexture=!0}copy(e){return super.copy(e),this.sourceTexture=e.sourceTexture,this}}class Yn extends _t{constructor(e=1,t=1,i=1,s=1,r=1,a=1){super(),this.type="BoxGeometry",this.parameters={width:e,height:t,depth:i,widthSegments:s,heightSegments:r,depthSegments:a};const o=this;s=Math.floor(s),r=Math.floor(r),a=Math.floor(a);const l=[],c=[],h=[],d=[];let u=0,m=0;_("z","y","x",-1,-1,i,t,e,a,r,0),_("z","y","x",1,-1,i,t,-e,a,r,1),_("x","z","y",1,1,e,i,t,s,a,2),_("x","z","y",1,-1,e,i,-t,s,a,3),_("x","y","z",1,-1,e,t,i,s,r,4),_("x","y","z",-1,-1,e,t,-i,s,r,5),this.setIndex(l),this.setAttribute("position",new Ft(c,3)),this.setAttribute("normal",new Ft(h,3)),this.setAttribute("uv",new Ft(d,2));function _(x,p,f,M,y,E,L,w,D,b,T){const U=E/D,C=L/b,z=E/2,X=L/2,j=w/2,N=D+1,V=b+1;let H=0,K=0;const ee=new A;for(let ce=0;ce<V;ce++){const be=ce*C-X;for(let Me=0;Me<N;Me++){const He=Me*U-z;ee[x]=He*M,ee[p]=be*y,ee[f]=j,c.push(ee.x,ee.y,ee.z),ee[x]=0,ee[p]=0,ee[f]=w>0?1:-1,h.push(ee.x,ee.y,ee.z),d.push(Me/D),d.push(1-ce/b),H+=1}}for(let ce=0;ce<b;ce++)for(let be=0;be<D;be++){const Me=u+be+N*ce,He=u+be+N*(ce+1),Ze=u+(be+1)+N*(ce+1),De=u+(be+1)+N*ce;l.push(Me,He,De),l.push(He,Ze,De),K+=6}o.addGroup(m,K,T),m+=K,u+=H}}copy(e){return super.copy(e),this.parameters=Object.assign({},e.parameters),this}static fromJSON(e){return new Yn(e.width,e.height,e.depth,e.widthSegments,e.heightSegments,e.depthSegments)}}class wn extends _t{constructor(e=1,t=1,i=1,s=1){super(),this.type="PlaneGeometry",this.parameters={width:e,height:t,widthSegments:i,heightSegments:s};const r=e/2,a=t/2,o=Math.floor(i),l=Math.floor(s),c=o+1,h=l+1,d=e/o,u=t/l,m=[],_=[],x=[],p=[];for(let f=0;f<h;f++){const M=f*u-a;for(let y=0;y<c;y++){const E=y*d-r;_.push(E,-M,0),x.push(0,0,1),p.push(y/o),p.push(1-f/l)}}for(let f=0;f<l;f++)for(let M=0;M<o;M++){const y=M+c*f,E=M+c*(f+1),L=M+1+c*(f+1),w=M+1+c*f;m.push(y,E,w),m.push(E,L,w)}this.setIndex(m),this.setAttribute("position",new Ft(_,3)),this.setAttribute("normal",new Ft(x,3)),this.setAttribute("uv",new Ft(p,2))}copy(e){return super.copy(e),this.parameters=Object.assign({},e.parameters),this}static fromJSON(e){return new wn(e.width,e.height,e.widthSegments,e.heightSegments)}}function Tn(n){const e={};for(const t in n){e[t]={};for(const i in n[t]){const s=n[t][i];if(Ko(s))s.isRenderTargetTexture?(Ce("UniformsUtils: Textures of render targets cannot be cloned via cloneUniforms() or mergeUniforms()."),e[t][i]=null):e[t][i]=s.clone();else if(Array.isArray(s))if(Ko(s[0])){const r=[];for(let a=0,o=s.length;a<o;a++)r[a]=s[a].clone();e[t][i]=r}else e[t][i]=s.slice();else e[t][i]=s}}return e}function Ot(n){const e={};for(let t=0;t<n.length;t++){const i=Tn(n[t]);for(const s in i)e[s]=i[s]}return e}function Ko(n){return n&&(n.isColor||n.isMatrix3||n.isMatrix4||n.isVector2||n.isVector3||n.isVector4||n.isTexture||n.isQuaternion)}function Ih(n){const e=[];for(let t=0;t<n.length;t++)e.push(n[t].clone());return e}function Jo(n){const e=n.getRenderTarget();return e===null?n.outputColorSpace:e.isXRRenderTarget===!0?e.texture.colorSpace:Xe.workingColorSpace}const Nh={clone:Tn,merge:Ot};var Uh=`void main() {
	gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );
}`,Fh=`void main() {
	gl_FragColor = vec4( 1.0, 0.0, 0.0, 1.0 );
}`;class ci extends Ki{constructor(e){super(),this.isShaderMaterial=!0,this.type="ShaderMaterial",this.defines={},this.uniforms={},this.uniformsGroups=[],this.vertexShader=Uh,this.fragmentShader=Fh,this.linewidth=1,this.wireframe=!1,this.wireframeLinewidth=1,this.fog=!1,this.lights=!1,this.clipping=!1,this.forceSinglePass=!0,this.extensions={clipCullDistance:!1,multiDraw:!1},this.defaultAttributeValues={color:[1,1,1],uv:[0,0],uv1:[0,0]},this.index0AttributeName=void 0,this.uniformsNeedUpdate=!1,this.glslVersion=null,e!==void 0&&this.setValues(e)}copy(e){return super.copy(e),this.fragmentShader=e.fragmentShader,this.vertexShader=e.vertexShader,this.uniforms=Tn(e.uniforms),this.uniformsGroups=Ih(e.uniformsGroups),this.defines=Object.assign({},e.defines),this.wireframe=e.wireframe,this.wireframeLinewidth=e.wireframeLinewidth,this.fog=e.fog,this.lights=e.lights,this.clipping=e.clipping,this.extensions=Object.assign({},e.extensions),this.glslVersion=e.glslVersion,this.defaultAttributeValues=Object.assign({},e.defaultAttributeValues),this.index0AttributeName=e.index0AttributeName,this.uniformsNeedUpdate=e.uniformsNeedUpdate,this}toJSON(e){const t=super.toJSON(e);t.glslVersion=this.glslVersion,t.uniforms={};for(const s in this.uniforms){const a=this.uniforms[s].value;a&&a.isTexture?t.uniforms[s]={type:"t",value:a.toJSON(e).uuid}:a&&a.isColor?t.uniforms[s]={type:"c",value:a.getHex()}:a&&a.isVector2?t.uniforms[s]={type:"v2",value:a.toArray()}:a&&a.isVector3?t.uniforms[s]={type:"v3",value:a.toArray()}:a&&a.isVector4?t.uniforms[s]={type:"v4",value:a.toArray()}:a&&a.isMatrix3?t.uniforms[s]={type:"m3",value:a.toArray()}:a&&a.isMatrix4?t.uniforms[s]={type:"m4",value:a.toArray()}:t.uniforms[s]={value:a}}Object.keys(this.defines).length>0&&(t.defines=this.defines),t.vertexShader=this.vertexShader,t.fragmentShader=this.fragmentShader,t.lights=this.lights,t.clipping=this.clipping;const i={};for(const s in this.extensions)this.extensions[s]===!0&&(i[s]=!0);return Object.keys(i).length>0&&(t.extensions=i),t}}class Oh extends ci{constructor(e){super(e),this.isRawShaderMaterial=!0,this.type="RawShaderMaterial"}}class Bh extends Ki{constructor(e){super(),this.isMeshStandardMaterial=!0,this.type="MeshStandardMaterial",this.defines={STANDARD:""},this.color=new Re(16777215),this.roughness=1,this.metalness=0,this.map=null,this.lightMap=null,this.lightMapIntensity=1,this.aoMap=null,this.aoMapIntensity=1,this.emissive=new Re(0),this.emissiveIntensity=1,this.emissiveMap=null,this.bumpMap=null,this.bumpScale=1,this.normalMap=null,this.normalMapType=qr,this.normalScale=new Ne(1,1),this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.roughnessMap=null,this.metalnessMap=null,this.alphaMap=null,this.envMap=null,this.envMapRotation=new Di,this.envMapIntensity=1,this.wireframe=!1,this.wireframeLinewidth=1,this.wireframeLinecap="round",this.wireframeLinejoin="round",this.flatShading=!1,this.fog=!0,this.setValues(e)}copy(e){return super.copy(e),this.defines={STANDARD:""},this.color.copy(e.color),this.roughness=e.roughness,this.metalness=e.metalness,this.map=e.map,this.lightMap=e.lightMap,this.lightMapIntensity=e.lightMapIntensity,this.aoMap=e.aoMap,this.aoMapIntensity=e.aoMapIntensity,this.emissive.copy(e.emissive),this.emissiveMap=e.emissiveMap,this.emissiveIntensity=e.emissiveIntensity,this.bumpMap=e.bumpMap,this.bumpScale=e.bumpScale,this.normalMap=e.normalMap,this.normalMapType=e.normalMapType,this.normalScale.copy(e.normalScale),this.displacementMap=e.displacementMap,this.displacementScale=e.displacementScale,this.displacementBias=e.displacementBias,this.roughnessMap=e.roughnessMap,this.metalnessMap=e.metalnessMap,this.alphaMap=e.alphaMap,this.envMap=e.envMap,this.envMapRotation.copy(e.envMapRotation),this.envMapIntensity=e.envMapIntensity,this.wireframe=e.wireframe,this.wireframeLinewidth=e.wireframeLinewidth,this.wireframeLinecap=e.wireframeLinecap,this.wireframeLinejoin=e.wireframeLinejoin,this.flatShading=e.flatShading,this.fog=e.fog,this}}class zh extends Ki{constructor(e){super(),this.isMeshDepthMaterial=!0,this.type="MeshDepthMaterial",this.depthPacking=Uc,this.map=null,this.alphaMap=null,this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.wireframe=!1,this.wireframeLinewidth=1,this.setValues(e)}copy(e){return super.copy(e),this.depthPacking=e.depthPacking,this.map=e.map,this.alphaMap=e.alphaMap,this.displacementMap=e.displacementMap,this.displacementScale=e.displacementScale,this.displacementBias=e.displacementBias,this.wireframe=e.wireframe,this.wireframeLinewidth=e.wireframeLinewidth,this}}class kh extends Ki{constructor(e){super(),this.isMeshDistanceMaterial=!0,this.type="MeshDistanceMaterial",this.map=null,this.alphaMap=null,this.displacementMap=null,this.displacementScale=1,this.displacementBias=0,this.setValues(e)}copy(e){return super.copy(e),this.map=e.map,this.alphaMap=e.alphaMap,this.displacementMap=e.displacementMap,this.displacementScale=e.displacementScale,this.displacementBias=e.displacementBias,this}}class Qo extends xt{constructor(e,t=1){super(),this.isLight=!0,this.type="Light",this.color=new Re(e),this.intensity=t}dispose(){this.dispatchEvent({type:"dispose"})}copy(e,t){return super.copy(e,t),this.color.copy(e.color),this.intensity=e.intensity,this}toJSON(e){const t=super.toJSON(e);return t.object.color=this.color.getHex(),t.object.intensity=this.intensity,t}}class Vh extends Qo{constructor(e,t,i){super(e,i),this.isHemisphereLight=!0,this.type="HemisphereLight",this.position.copy(xt.DEFAULT_UP),this.updateMatrix(),this.groundColor=new Re(t)}copy(e,t){return super.copy(e,t),this.groundColor.copy(e.groundColor),this}toJSON(e){const t=super.toJSON(e);return t.object.groundColor=this.groundColor.getHex(),t}}const Aa=new lt,el=new A,tl=new A;class Gh{constructor(e){this.camera=e,this.intensity=1,this.bias=0,this.biasNode=null,this.normalBias=0,this.radius=1,this.blurSamples=8,this.mapSize=new Ne(512,512),this.mapType=kt,this.map=null,this.mapPass=null,this.matrix=new lt,this.autoUpdate=!0,this.needsUpdate=!1,this._frustum=new ya,this._frameExtents=new Ne(1,1),this._viewportCount=1,this._viewports=[new ht(0,0,1,1)]}getViewportCount(){return this._viewportCount}getFrustum(){return this._frustum}updateMatrices(e){const t=this.camera,i=this.matrix;el.setFromMatrixPosition(e.matrixWorld),t.position.copy(el),tl.setFromMatrixPosition(e.target.matrixWorld),t.lookAt(tl),t.updateMatrixWorld(),Aa.multiplyMatrices(t.projectionMatrix,t.matrixWorldInverse),this._frustum.setFromProjectionMatrix(Aa,t.coordinateSystem,t.reversedDepth),t.coordinateSystem===Bn||t.reversedDepth?i.set(.5,0,0,.5,0,.5,0,.5,0,0,1,0,0,0,0,1):i.set(.5,0,0,.5,0,.5,0,.5,0,0,.5,.5,0,0,0,1),i.multiply(Aa)}getViewport(e){return this._viewports[e]}getFrameExtents(){return this._frameExtents}dispose(){this.map&&this.map.dispose(),this.mapPass&&this.mapPass.dispose()}copy(e){return this.camera=e.camera.clone(),this.intensity=e.intensity,this.bias=e.bias,this.radius=e.radius,this.autoUpdate=e.autoUpdate,this.needsUpdate=e.needsUpdate,this.normalBias=e.normalBias,this.blurSamples=e.blurSamples,this.mapSize.copy(e.mapSize),this.biasNode=e.biasNode,this}clone(){return new this.constructor().copy(this)}toJSON(){const e={};return this.intensity!==1&&(e.intensity=this.intensity),this.bias!==0&&(e.bias=this.bias),this.normalBias!==0&&(e.normalBias=this.normalBias),this.radius!==1&&(e.radius=this.radius),(this.mapSize.x!==512||this.mapSize.y!==512)&&(e.mapSize=this.mapSize.toArray()),e.camera=this.camera.toJSON(!1).object,delete e.camera.matrix,e}}const Bs=new A,zs=new Pi,hi=new A;let il=class extends xt{constructor(){super(),this.isCamera=!0,this.type="Camera",this.matrixWorldInverse=new lt,this.projectionMatrix=new lt,this.projectionMatrixInverse=new lt,this.coordinateSystem=oi,this._reversedDepth=!1}get reversedDepth(){return this._reversedDepth}copy(e,t){return super.copy(e,t),this.matrixWorldInverse.copy(e.matrixWorldInverse),this.projectionMatrix.copy(e.projectionMatrix),this.projectionMatrixInverse.copy(e.projectionMatrixInverse),this.coordinateSystem=e.coordinateSystem,this}getWorldDirection(e){return super.getWorldDirection(e).negate()}updateMatrixWorld(e){super.updateMatrixWorld(e),this.matrixWorld.decompose(Bs,zs,hi),hi.x===1&&hi.y===1&&hi.z===1?this.matrixWorldInverse.copy(this.matrixWorld).invert():this.matrixWorldInverse.compose(Bs,zs,hi.set(1,1,1)).invert()}updateWorldMatrix(e,t){super.updateWorldMatrix(e,t),this.matrixWorld.decompose(Bs,zs,hi),hi.x===1&&hi.y===1&&hi.z===1?this.matrixWorldInverse.copy(this.matrixWorld).invert():this.matrixWorldInverse.compose(Bs,zs,hi.set(1,1,1)).invert()}clone(){return new this.constructor().copy(this)}};const zi=new A,nl=new Ne,sl=new Ne;class $t extends il{constructor(e=50,t=1,i=.1,s=2e3){super(),this.isPerspectiveCamera=!0,this.type="PerspectiveCamera",this.fov=e,this.zoom=1,this.near=i,this.far=s,this.focus=10,this.aspect=t,this.view=null,this.filmGauge=35,this.filmOffset=0,this.updateProjectionMatrix()}copy(e,t){return super.copy(e,t),this.fov=e.fov,this.zoom=e.zoom,this.near=e.near,this.far=e.far,this.focus=e.focus,this.aspect=e.aspect,this.view=e.view===null?null:Object.assign({},e.view),this.filmGauge=e.filmGauge,this.filmOffset=e.filmOffset,this}setFocalLength(e){const t=.5*this.getFilmHeight()/e;this.fov=kn*2*Math.atan(t),this.updateProjectionMatrix()}getFocalLength(){const e=Math.tan(zn*.5*this.fov);return .5*this.getFilmHeight()/e}getEffectiveFOV(){return kn*2*Math.atan(Math.tan(zn*.5*this.fov)/this.zoom)}getFilmWidth(){return this.filmGauge*Math.min(this.aspect,1)}getFilmHeight(){return this.filmGauge/Math.max(this.aspect,1)}getViewBounds(e,t,i){zi.set(-1,-1,.5).applyMatrix4(this.projectionMatrixInverse),t.set(zi.x,zi.y).multiplyScalar(-e/zi.z),zi.set(1,1,.5).applyMatrix4(this.projectionMatrixInverse),i.set(zi.x,zi.y).multiplyScalar(-e/zi.z)}getViewSize(e,t){return this.getViewBounds(e,nl,sl),t.subVectors(sl,nl)}setViewOffset(e,t,i,s,r,a){this.aspect=e/t,this.view===null&&(this.view={enabled:!0,fullWidth:1,fullHeight:1,offsetX:0,offsetY:0,width:1,height:1}),this.view.enabled=!0,this.view.fullWidth=e,this.view.fullHeight=t,this.view.offsetX=i,this.view.offsetY=s,this.view.width=r,this.view.height=a,this.updateProjectionMatrix()}clearViewOffset(){this.view!==null&&(this.view.enabled=!1),this.updateProjectionMatrix()}updateProjectionMatrix(){const e=this.near;let t=e*Math.tan(zn*.5*this.fov)/this.zoom,i=2*t,s=this.aspect*i,r=-.5*s;const a=this.view;if(this.view!==null&&this.view.enabled){const l=a.fullWidth,c=a.fullHeight;r+=a.offsetX*s/l,t-=a.offsetY*i/c,s*=a.width/l,i*=a.height/c}const o=this.filmOffset;o!==0&&(r+=e*o/this.getFilmWidth()),this.projectionMatrix.makePerspective(r,r+s,t,t-i,e,this.far,this.coordinateSystem,this.reversedDepth),this.projectionMatrixInverse.copy(this.projectionMatrix).invert()}toJSON(e){const t=super.toJSON(e);return t.object.fov=this.fov,t.object.zoom=this.zoom,t.object.near=this.near,t.object.far=this.far,t.object.focus=this.focus,t.object.aspect=this.aspect,this.view!==null&&(t.object.view=Object.assign({},this.view)),t.object.filmGauge=this.filmGauge,t.object.filmOffset=this.filmOffset,t}}class ks extends il{constructor(e=-1,t=1,i=1,s=-1,r=.1,a=2e3){super(),this.isOrthographicCamera=!0,this.type="OrthographicCamera",this.zoom=1,this.view=null,this.left=e,this.right=t,this.top=i,this.bottom=s,this.near=r,this.far=a,this.updateProjectionMatrix()}copy(e,t){return super.copy(e,t),this.left=e.left,this.right=e.right,this.top=e.top,this.bottom=e.bottom,this.near=e.near,this.far=e.far,this.zoom=e.zoom,this.view=e.view===null?null:Object.assign({},e.view),this}setViewOffset(e,t,i,s,r,a){this.view===null&&(this.view={enabled:!0,fullWidth:1,fullHeight:1,offsetX:0,offsetY:0,width:1,height:1}),this.view.enabled=!0,this.view.fullWidth=e,this.view.fullHeight=t,this.view.offsetX=i,this.view.offsetY=s,this.view.width=r,this.view.height=a,this.updateProjectionMatrix()}clearViewOffset(){this.view!==null&&(this.view.enabled=!1),this.updateProjectionMatrix()}updateProjectionMatrix(){const e=(this.right-this.left)/(2*this.zoom),t=(this.top-this.bottom)/(2*this.zoom),i=(this.right+this.left)/2,s=(this.top+this.bottom)/2;let r=i-e,a=i+e,o=s+t,l=s-t;if(this.view!==null&&this.view.enabled){const c=(this.right-this.left)/this.view.fullWidth/this.zoom,h=(this.top-this.bottom)/this.view.fullHeight/this.zoom;r+=c*this.view.offsetX,a=r+c*this.view.width,o-=h*this.view.offsetY,l=o-h*this.view.height}this.projectionMatrix.makeOrthographic(r,a,o,l,this.near,this.far,this.coordinateSystem,this.reversedDepth),this.projectionMatrixInverse.copy(this.projectionMatrix).invert()}toJSON(e){const t=super.toJSON(e);return t.object.zoom=this.zoom,t.object.left=this.left,t.object.right=this.right,t.object.top=this.top,t.object.bottom=this.bottom,t.object.near=this.near,t.object.far=this.far,this.view!==null&&(t.object.view=Object.assign({},this.view)),t}}class Hh extends Gh{constructor(){super(new ks(-5,5,5,-5,.5,500)),this.isDirectionalLightShadow=!0}}class Wh extends Qo{constructor(e,t){super(e,t),this.isDirectionalLight=!0,this.type="DirectionalLight",this.position.copy(xt.DEFAULT_UP),this.updateMatrix(),this.target=new xt,this.shadow=new Hh}dispose(){super.dispose(),this.shadow.dispose()}copy(e){return super.copy(e),this.target=e.target.clone(),this.shadow=e.shadow.clone(),this}toJSON(e){const t=super.toJSON(e);return t.object.shadow=this.shadow.toJSON(),t.object.target=this.target.uuid,t}}const An=-90,Cn=1;class Xh extends xt{constructor(e,t,i){super(),this.type="CubeCamera",this.renderTarget=i,this.coordinateSystem=null,this.activeMipmapLevel=0;const s=new $t(An,Cn,e,t);s.layers=this.layers,this.add(s);const r=new $t(An,Cn,e,t);r.layers=this.layers,this.add(r);const a=new $t(An,Cn,e,t);a.layers=this.layers,this.add(a);const o=new $t(An,Cn,e,t);o.layers=this.layers,this.add(o);const l=new $t(An,Cn,e,t);l.layers=this.layers,this.add(l);const c=new $t(An,Cn,e,t);c.layers=this.layers,this.add(c)}updateCoordinateSystem(){const e=this.coordinateSystem,t=this.children.concat(),[i,s,r,a,o,l]=t;for(const c of t)this.remove(c);if(e===oi)i.up.set(0,1,0),i.lookAt(1,0,0),s.up.set(0,1,0),s.lookAt(-1,0,0),r.up.set(0,0,-1),r.lookAt(0,1,0),a.up.set(0,0,1),a.lookAt(0,-1,0),o.up.set(0,1,0),o.lookAt(0,0,1),l.up.set(0,1,0),l.lookAt(0,0,-1);else if(e===Bn)i.up.set(0,-1,0),i.lookAt(-1,0,0),s.up.set(0,-1,0),s.lookAt(1,0,0),r.up.set(0,0,1),r.lookAt(0,1,0),a.up.set(0,0,-1),a.lookAt(0,-1,0),o.up.set(0,-1,0),o.lookAt(0,0,1),l.up.set(0,-1,0),l.lookAt(0,0,-1);else throw new Error("THREE.CubeCamera.updateCoordinateSystem(): Invalid coordinate system: "+e);for(const c of t)this.add(c),c.updateMatrixWorld()}update(e,t){this.parent===null&&this.updateMatrixWorld();const{renderTarget:i,activeMipmapLevel:s}=this;this.coordinateSystem!==e.coordinateSystem&&(this.coordinateSystem=e.coordinateSystem,this.updateCoordinateSystem());const[r,a,o,l,c,h]=this.children,d=e.getRenderTarget(),u=e.getActiveCubeFace(),m=e.getActiveMipmapLevel(),_=e.xr.enabled;e.xr.enabled=!1;const x=i.texture.generateMipmaps;i.texture.generateMipmaps=!1;let p=!1;e.isWebGLRenderer===!0?p=e.state.buffers.depth.getReversed():p=e.reversedDepthBuffer,e.setRenderTarget(i,0,s),p&&e.autoClear===!1&&e.clearDepth(),e.render(t,r),e.setRenderTarget(i,1,s),p&&e.autoClear===!1&&e.clearDepth(),e.render(t,a),e.setRenderTarget(i,2,s),p&&e.autoClear===!1&&e.clearDepth(),e.render(t,o),e.setRenderTarget(i,3,s),p&&e.autoClear===!1&&e.clearDepth(),e.render(t,l),e.setRenderTarget(i,4,s),p&&e.autoClear===!1&&e.clearDepth(),e.render(t,c),i.texture.generateMipmaps=x,e.setRenderTarget(i,5,s),p&&e.autoClear===!1&&e.clearDepth(),e.render(t,h),e.setRenderTarget(d,u,m),e.xr.enabled=_,i.texture.needsPMREMUpdate=!0}}class jh extends $t{constructor(e=[]){super(),this.isArrayCamera=!0,this.isMultiViewCamera=!1,this.cameras=e}}const rl=new lt;class qh{constructor(e,t,i=0,s=1/0){this.ray=new jn(e,t),this.near=i,this.far=s,this.camera=null,this.layers=new sa,this.params={Mesh:{},Line:{threshold:1},LOD:{},Points:{threshold:1},Sprite:{}}}set(e,t){this.ray.set(e,t)}setFromCamera(e,t){t.isPerspectiveCamera?(this.ray.origin.setFromMatrixPosition(t.matrixWorld),this.ray.direction.set(e.x,e.y,.5).unproject(t).sub(this.ray.origin).normalize(),this.camera=t):t.isOrthographicCamera?(this.ray.origin.set(e.x,e.y,(t.near+t.far)/(t.near-t.far)).unproject(t),this.ray.direction.set(0,0,-1).transformDirection(t.matrixWorld),this.camera=t):je("Raycaster: Unsupported camera type: "+t.type)}setFromXRController(e){return rl.identity().extractRotation(e.matrixWorld),this.ray.origin.setFromMatrixPosition(e.matrixWorld),this.ray.direction.set(0,0,-1).applyMatrix4(rl),this}intersectObject(e,t=!0,i=[]){return Ca(e,this,i,t),i.sort(al),i}intersectObjects(e,t=!0,i=[]){for(let s=0,r=e.length;s<r;s++)Ca(e[s],this,i,t);return i.sort(al),i}}function al(n,e){return n.distance-e.distance}function Ca(n,e,t,i){let s=!0;if(n.layers.test(e.layers)&&n.raycast(e,t)===!1&&(s=!1),s===!0&&i===!0){const r=n.children;for(let a=0,o=r.length;a<o;a++)Ca(r[a],e,t,!0)}}class ol{constructor(e=1,t=0,i=0){this.radius=e,this.phi=t,this.theta=i}set(e,t,i){return this.radius=e,this.phi=t,this.theta=i,this}copy(e){return this.radius=e.radius,this.phi=e.phi,this.theta=e.theta,this}makeSafe(){return this.phi=Ge(this.phi,1e-6,Math.PI-1e-6),this}setFromVector3(e){return this.setFromCartesianCoords(e.x,e.y,e.z)}setFromCartesianCoords(e,t,i){return this.radius=Math.sqrt(e*e+t*t+i*i),this.radius===0?(this.theta=0,this.phi=0):(this.theta=Math.atan2(e,i),this.phi=Math.acos(Ge(t/this.radius,-1,1))),this}clone(){return new this.constructor().copy(this)}}const qa=class qa{constructor(e,t,i,s){this.elements=[1,0,0,1],e!==void 0&&this.set(e,t,i,s)}identity(){return this.set(1,0,0,1),this}fromArray(e,t=0){for(let i=0;i<4;i++)this.elements[i]=e[i+t];return this}set(e,t,i,s){const r=this.elements;return r[0]=e,r[2]=t,r[1]=i,r[3]=s,this}};qa.prototype.isMatrix2=!0;let ll=qa;class Yh extends Ns{constructor(e=1){const t=[0,0,0,e,0,0,0,0,0,0,e,0,0,0,0,0,0,e],i=[1,0,0,1,.6,0,0,1,0,.6,1,0,0,0,1,0,.6,1],s=new _t;s.setAttribute("position",new Ft(t,3)),s.setAttribute("color",new Ft(i,3));const r=new yn({vertexColors:!0,toneMapped:!1});super(s,r),this.type="AxesHelper"}setColors(e,t,i){const s=new Re,r=this.geometry.attributes.color.array;return s.set(e),s.toArray(r,0),s.toArray(r,3),s.set(t),s.toArray(r,6),s.toArray(r,9),s.set(i),s.toArray(r,12),s.toArray(r,15),this.geometry.attributes.color.needsUpdate=!0,this}dispose(){this.geometry.dispose(),this.material.dispose()}}class $h extends Ri{constructor(e,t=null){super(),this.object=e,this.domElement=t,this.enabled=!0,this.state=-1,this.keys={},this.mouseButtons={LEFT:null,MIDDLE:null,RIGHT:null},this.touches={ONE:null,TWO:null}}connect(e){if(e===void 0){Ce("Controls: connect() now requires an element.");return}this.domElement!==null&&this.disconnect(),this.domElement=e}disconnect(){}dispose(){}update(){}}function cl(n,e,t,i){const s=Zh(i);switch(t){case po:return n*e;case go:return n*e/s.components*s.byteLength;case pr:return n*e/s.components*s.byteLength;case Yi:return n*e*2/s.components*s.byteLength;case mr:return n*e*2/s.components*s.byteLength;case mo:return n*e*3/s.components*s.byteLength;case Qt:return n*e*4/s.components*s.byteLength;case gr:return n*e*4/s.components*s.byteLength;case is:case ns:return Math.floor((n+3)/4)*Math.floor((e+3)/4)*8;case ss:case rs:return Math.floor((n+3)/4)*Math.floor((e+3)/4)*16;case vr:case br:return Math.max(n,16)*Math.max(e,8)/4;case _r:case xr:return Math.max(n,8)*Math.max(e,8)/2;case Sr:case Mr:case Er:case wr:return Math.floor((n+3)/4)*Math.floor((e+3)/4)*8;case yr:case as:case Tr:return Math.floor((n+3)/4)*Math.floor((e+3)/4)*16;case Ar:return Math.floor((n+3)/4)*Math.floor((e+3)/4)*16;case Cr:return Math.floor((n+4)/5)*Math.floor((e+3)/4)*16;case Rr:return Math.floor((n+4)/5)*Math.floor((e+4)/5)*16;case Pr:return Math.floor((n+5)/6)*Math.floor((e+4)/5)*16;case Lr:return Math.floor((n+5)/6)*Math.floor((e+5)/6)*16;case Dr:return Math.floor((n+7)/8)*Math.floor((e+4)/5)*16;case Ir:return Math.floor((n+7)/8)*Math.floor((e+5)/6)*16;case Nr:return Math.floor((n+7)/8)*Math.floor((e+7)/8)*16;case Ur:return Math.floor((n+9)/10)*Math.floor((e+4)/5)*16;case Fr:return Math.floor((n+9)/10)*Math.floor((e+5)/6)*16;case Or:return Math.floor((n+9)/10)*Math.floor((e+7)/8)*16;case Br:return Math.floor((n+9)/10)*Math.floor((e+9)/10)*16;case zr:return Math.floor((n+11)/12)*Math.floor((e+9)/10)*16;case kr:return Math.floor((n+11)/12)*Math.floor((e+11)/12)*16;case Vr:case Gr:case Hr:return Math.ceil(n/4)*Math.ceil(e/4)*16;case Wr:case Xr:return Math.ceil(n/4)*Math.ceil(e/4)*8;case os:case jr:return Math.ceil(n/4)*Math.ceil(e/4)*16}throw new Error(`Unable to determine texture byte length for ${t} format.`)}function Zh(n){switch(n){case kt:case co:return{byteLength:1,components:1};case Fn:case ho:case vi:return{byteLength:2,components:1};case dr:case fr:return{byteLength:2,components:4};case ri:case ur:case ai:return{byteLength:4,components:1};case uo:case fo:return{byteLength:4,components:3}}throw new Error(`Unknown texture type ${n}.`)}typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("register",{detail:{revision:Zs}})),typeof window<"u"&&(window.__THREE__?Ce("WARNING: Multiple instances of Three.js being imported."):window.__THREE__=Zs);/**
 * @license
 * Copyright 2010-2026 Three.js Authors
 * SPDX-License-Identifier: MIT
 */function hl(){let n=null,e=!1,t=null,i=null;function s(r,a){t(r,a),i=n.requestAnimationFrame(s)}return{start:function(){e!==!0&&t!==null&&n!==null&&(i=n.requestAnimationFrame(s),e=!0)},stop:function(){n!==null&&n.cancelAnimationFrame(i),e=!1},setAnimationLoop:function(r){t=r},setContext:function(r){n=r}}}function Kh(n){const e=new WeakMap;function t(o,l){const c=o.array,h=o.usage,d=c.byteLength,u=n.createBuffer();n.bindBuffer(l,u),n.bufferData(l,c,h),o.onUploadCallback();let m;if(c instanceof Float32Array)m=n.FLOAT;else if(typeof Float16Array<"u"&&c instanceof Float16Array)m=n.HALF_FLOAT;else if(c instanceof Uint16Array)o.isFloat16BufferAttribute?m=n.HALF_FLOAT:m=n.UNSIGNED_SHORT;else if(c instanceof Int16Array)m=n.SHORT;else if(c instanceof Uint32Array)m=n.UNSIGNED_INT;else if(c instanceof Int32Array)m=n.INT;else if(c instanceof Int8Array)m=n.BYTE;else if(c instanceof Uint8Array)m=n.UNSIGNED_BYTE;else if(c instanceof Uint8ClampedArray)m=n.UNSIGNED_BYTE;else throw new Error("THREE.WebGLAttributes: Unsupported buffer data format: "+c);return{buffer:u,type:m,bytesPerElement:c.BYTES_PER_ELEMENT,version:o.version,size:d}}function i(o,l,c){const h=l.array,d=l.updateRanges;if(n.bindBuffer(c,o),d.length===0)n.bufferSubData(c,0,h);else{d.sort((m,_)=>m.start-_.start);let u=0;for(let m=1;m<d.length;m++){const _=d[u],x=d[m];x.start<=_.start+_.count+1?_.count=Math.max(_.count,x.start+x.count-_.start):(++u,d[u]=x)}d.length=u+1;for(let m=0,_=d.length;m<_;m++){const x=d[m];n.bufferSubData(c,x.start*h.BYTES_PER_ELEMENT,h,x.start,x.count)}l.clearUpdateRanges()}l.onUploadCallback()}function s(o){return o.isInterleavedBufferAttribute&&(o=o.data),e.get(o)}function r(o){o.isInterleavedBufferAttribute&&(o=o.data);const l=e.get(o);l&&(n.deleteBuffer(l.buffer),e.delete(o))}function a(o,l){if(o.isInterleavedBufferAttribute&&(o=o.data),o.isGLBufferAttribute){const h=e.get(o);(!h||h.version<o.version)&&e.set(o,{buffer:o.buffer,type:o.type,bytesPerElement:o.elementSize,version:o.version});return}const c=e.get(o);if(c===void 0)e.set(o,t(o,l));else if(c.version<o.version){if(c.size!==o.array.byteLength)throw new Error("THREE.WebGLAttributes: The size of the buffer attribute's array buffer does not match the original size. Resizing buffer attributes is not supported.");i(c.buffer,o,l),c.version=o.version}}return{get:s,remove:r,update:a}}var Jh=`#ifdef USE_ALPHAHASH
	if ( diffuseColor.a < getAlphaHashThreshold( vPosition ) ) discard;
#endif`,Qh=`#ifdef USE_ALPHAHASH
	const float ALPHA_HASH_SCALE = 0.05;
	float hash2D( vec2 value ) {
		return fract( 1.0e4 * sin( 17.0 * value.x + 0.1 * value.y ) * ( 0.1 + abs( sin( 13.0 * value.y + value.x ) ) ) );
	}
	float hash3D( vec3 value ) {
		return hash2D( vec2( hash2D( value.xy ), value.z ) );
	}
	float getAlphaHashThreshold( vec3 position ) {
		float maxDeriv = max(
			length( dFdx( position.xyz ) ),
			length( dFdy( position.xyz ) )
		);
		float pixScale = 1.0 / ( ALPHA_HASH_SCALE * maxDeriv );
		vec2 pixScales = vec2(
			exp2( floor( log2( pixScale ) ) ),
			exp2( ceil( log2( pixScale ) ) )
		);
		vec2 alpha = vec2(
			hash3D( floor( pixScales.x * position.xyz ) ),
			hash3D( floor( pixScales.y * position.xyz ) )
		);
		float lerpFactor = fract( log2( pixScale ) );
		float x = ( 1.0 - lerpFactor ) * alpha.x + lerpFactor * alpha.y;
		float a = min( lerpFactor, 1.0 - lerpFactor );
		vec3 cases = vec3(
			x * x / ( 2.0 * a * ( 1.0 - a ) ),
			( x - 0.5 * a ) / ( 1.0 - a ),
			1.0 - ( ( 1.0 - x ) * ( 1.0 - x ) / ( 2.0 * a * ( 1.0 - a ) ) )
		);
		float threshold = ( x < ( 1.0 - a ) )
			? ( ( x < a ) ? cases.x : cases.y )
			: cases.z;
		return clamp( threshold , 1.0e-6, 1.0 );
	}
#endif`,eu=`#ifdef USE_ALPHAMAP
	diffuseColor.a *= texture2D( alphaMap, vAlphaMapUv ).g;
#endif`,tu=`#ifdef USE_ALPHAMAP
	uniform sampler2D alphaMap;
#endif`,iu=`#ifdef USE_ALPHATEST
	#ifdef ALPHA_TO_COVERAGE
	diffuseColor.a = smoothstep( alphaTest, alphaTest + fwidth( diffuseColor.a ), diffuseColor.a );
	if ( diffuseColor.a == 0.0 ) discard;
	#else
	if ( diffuseColor.a < alphaTest ) discard;
	#endif
#endif`,nu=`#ifdef USE_ALPHATEST
	uniform float alphaTest;
#endif`,su=`#ifdef USE_AOMAP
	float ambientOcclusion = ( texture2D( aoMap, vAoMapUv ).r - 1.0 ) * aoMapIntensity + 1.0;
	reflectedLight.indirectDiffuse *= ambientOcclusion;
	#if defined( USE_CLEARCOAT ) 
		clearcoatSpecularIndirect *= ambientOcclusion;
	#endif
	#if defined( USE_SHEEN ) 
		sheenSpecularIndirect *= ambientOcclusion;
	#endif
	#if defined( USE_ENVMAP ) && defined( STANDARD )
		float dotNV = saturate( dot( geometryNormal, geometryViewDir ) );
		reflectedLight.indirectSpecular *= computeSpecularOcclusion( dotNV, ambientOcclusion, material.roughness );
	#endif
#endif`,ru=`#ifdef USE_AOMAP
	uniform sampler2D aoMap;
	uniform float aoMapIntensity;
#endif`,au=`#ifdef USE_BATCHING
	#if ! defined( GL_ANGLE_multi_draw )
	#define gl_DrawID _gl_DrawID
	uniform int _gl_DrawID;
	#endif
	uniform highp sampler2D batchingTexture;
	uniform highp usampler2D batchingIdTexture;
	mat4 getBatchingMatrix( const in float i ) {
		int size = textureSize( batchingTexture, 0 ).x;
		int j = int( i ) * 4;
		int x = j % size;
		int y = j / size;
		vec4 v1 = texelFetch( batchingTexture, ivec2( x, y ), 0 );
		vec4 v2 = texelFetch( batchingTexture, ivec2( x + 1, y ), 0 );
		vec4 v3 = texelFetch( batchingTexture, ivec2( x + 2, y ), 0 );
		vec4 v4 = texelFetch( batchingTexture, ivec2( x + 3, y ), 0 );
		return mat4( v1, v2, v3, v4 );
	}
	float getIndirectIndex( const in int i ) {
		int size = textureSize( batchingIdTexture, 0 ).x;
		int x = i % size;
		int y = i / size;
		return float( texelFetch( batchingIdTexture, ivec2( x, y ), 0 ).r );
	}
#endif
#ifdef USE_BATCHING_COLOR
	uniform sampler2D batchingColorTexture;
	vec4 getBatchingColor( const in float i ) {
		int size = textureSize( batchingColorTexture, 0 ).x;
		int j = int( i );
		int x = j % size;
		int y = j / size;
		return texelFetch( batchingColorTexture, ivec2( x, y ), 0 );
	}
#endif`,ou=`#ifdef USE_BATCHING
	mat4 batchingMatrix = getBatchingMatrix( getIndirectIndex( gl_DrawID ) );
#endif`,lu=`vec3 transformed = vec3( position );
#ifdef USE_ALPHAHASH
	vPosition = vec3( position );
#endif`,cu=`vec3 objectNormal = vec3( normal );
#ifdef USE_TANGENT
	vec3 objectTangent = vec3( tangent.xyz );
#endif`,hu=`float G_BlinnPhong_Implicit( ) {
	return 0.25;
}
float D_BlinnPhong( const in float shininess, const in float dotNH ) {
	return RECIPROCAL_PI * ( shininess * 0.5 + 1.0 ) * pow( dotNH, shininess );
}
vec3 BRDF_BlinnPhong( const in vec3 lightDir, const in vec3 viewDir, const in vec3 normal, const in vec3 specularColor, const in float shininess ) {
	vec3 halfDir = normalize( lightDir + viewDir );
	float dotNH = saturate( dot( normal, halfDir ) );
	float dotVH = saturate( dot( viewDir, halfDir ) );
	vec3 F = F_Schlick( specularColor, 1.0, dotVH );
	float G = G_BlinnPhong_Implicit( );
	float D = D_BlinnPhong( shininess, dotNH );
	return F * ( G * D );
} // validated`,uu=`#ifdef USE_IRIDESCENCE
	const mat3 XYZ_TO_REC709 = mat3(
		 3.2404542, -0.9692660,  0.0556434,
		-1.5371385,  1.8760108, -0.2040259,
		-0.4985314,  0.0415560,  1.0572252
	);
	vec3 Fresnel0ToIor( vec3 fresnel0 ) {
		vec3 sqrtF0 = sqrt( fresnel0 );
		return ( vec3( 1.0 ) + sqrtF0 ) / ( vec3( 1.0 ) - sqrtF0 );
	}
	vec3 IorToFresnel0( vec3 transmittedIor, float incidentIor ) {
		return pow2( ( transmittedIor - vec3( incidentIor ) ) / ( transmittedIor + vec3( incidentIor ) ) );
	}
	float IorToFresnel0( float transmittedIor, float incidentIor ) {
		return pow2( ( transmittedIor - incidentIor ) / ( transmittedIor + incidentIor ));
	}
	vec3 evalSensitivity( float OPD, vec3 shift ) {
		float phase = 2.0 * PI * OPD * 1.0e-9;
		vec3 val = vec3( 5.4856e-13, 4.4201e-13, 5.2481e-13 );
		vec3 pos = vec3( 1.6810e+06, 1.7953e+06, 2.2084e+06 );
		vec3 var = vec3( 4.3278e+09, 9.3046e+09, 6.6121e+09 );
		vec3 xyz = val * sqrt( 2.0 * PI * var ) * cos( pos * phase + shift ) * exp( - pow2( phase ) * var );
		xyz.x += 9.7470e-14 * sqrt( 2.0 * PI * 4.5282e+09 ) * cos( 2.2399e+06 * phase + shift[ 0 ] ) * exp( - 4.5282e+09 * pow2( phase ) );
		xyz /= 1.0685e-7;
		vec3 rgb = XYZ_TO_REC709 * xyz;
		return rgb;
	}
	vec3 evalIridescence( float outsideIOR, float eta2, float cosTheta1, float thinFilmThickness, vec3 baseF0 ) {
		vec3 I;
		float iridescenceIOR = mix( outsideIOR, eta2, smoothstep( 0.0, 0.03, thinFilmThickness ) );
		float sinTheta2Sq = pow2( outsideIOR / iridescenceIOR ) * ( 1.0 - pow2( cosTheta1 ) );
		float cosTheta2Sq = 1.0 - sinTheta2Sq;
		if ( cosTheta2Sq < 0.0 ) {
			return vec3( 1.0 );
		}
		float cosTheta2 = sqrt( cosTheta2Sq );
		float R0 = IorToFresnel0( iridescenceIOR, outsideIOR );
		float R12 = F_Schlick( R0, 1.0, cosTheta1 );
		float T121 = 1.0 - R12;
		float phi12 = 0.0;
		if ( iridescenceIOR < outsideIOR ) phi12 = PI;
		float phi21 = PI - phi12;
		vec3 baseIOR = Fresnel0ToIor( clamp( baseF0, 0.0, 0.9999 ) );		vec3 R1 = IorToFresnel0( baseIOR, iridescenceIOR );
		vec3 R23 = F_Schlick( R1, 1.0, cosTheta2 );
		vec3 phi23 = vec3( 0.0 );
		if ( baseIOR[ 0 ] < iridescenceIOR ) phi23[ 0 ] = PI;
		if ( baseIOR[ 1 ] < iridescenceIOR ) phi23[ 1 ] = PI;
		if ( baseIOR[ 2 ] < iridescenceIOR ) phi23[ 2 ] = PI;
		float OPD = 2.0 * iridescenceIOR * thinFilmThickness * cosTheta2;
		vec3 phi = vec3( phi21 ) + phi23;
		vec3 R123 = clamp( R12 * R23, 1e-5, 0.9999 );
		vec3 r123 = sqrt( R123 );
		vec3 Rs = pow2( T121 ) * R23 / ( vec3( 1.0 ) - R123 );
		vec3 C0 = R12 + Rs;
		I = C0;
		vec3 Cm = Rs - T121;
		for ( int m = 1; m <= 2; ++ m ) {
			Cm *= r123;
			vec3 Sm = 2.0 * evalSensitivity( float( m ) * OPD, float( m ) * phi );
			I += Cm * Sm;
		}
		return max( I, vec3( 0.0 ) );
	}
#endif`,du=`#ifdef USE_BUMPMAP
	uniform sampler2D bumpMap;
	uniform float bumpScale;
	vec2 dHdxy_fwd() {
		vec2 dSTdx = dFdx( vBumpMapUv );
		vec2 dSTdy = dFdy( vBumpMapUv );
		float Hll = bumpScale * texture2D( bumpMap, vBumpMapUv ).x;
		float dBx = bumpScale * texture2D( bumpMap, vBumpMapUv + dSTdx ).x - Hll;
		float dBy = bumpScale * texture2D( bumpMap, vBumpMapUv + dSTdy ).x - Hll;
		return vec2( dBx, dBy );
	}
	vec3 perturbNormalArb( vec3 surf_pos, vec3 surf_norm, vec2 dHdxy, float faceDirection ) {
		vec3 vSigmaX = normalize( dFdx( surf_pos.xyz ) );
		vec3 vSigmaY = normalize( dFdy( surf_pos.xyz ) );
		vec3 vN = surf_norm;
		vec3 R1 = cross( vSigmaY, vN );
		vec3 R2 = cross( vN, vSigmaX );
		float fDet = dot( vSigmaX, R1 ) * faceDirection;
		vec3 vGrad = sign( fDet ) * ( dHdxy.x * R1 + dHdxy.y * R2 );
		return normalize( abs( fDet ) * surf_norm - vGrad );
	}
#endif`,fu=`#if NUM_CLIPPING_PLANES > 0
	vec4 plane;
	#ifdef ALPHA_TO_COVERAGE
		float distanceToPlane, distanceGradient;
		float clipOpacity = 1.0;
		#pragma unroll_loop_start
		for ( int i = 0; i < UNION_CLIPPING_PLANES; i ++ ) {
			plane = clippingPlanes[ i ];
			distanceToPlane = - dot( vClipPosition, plane.xyz ) + plane.w;
			distanceGradient = fwidth( distanceToPlane ) / 2.0;
			clipOpacity *= smoothstep( - distanceGradient, distanceGradient, distanceToPlane );
			if ( clipOpacity == 0.0 ) discard;
		}
		#pragma unroll_loop_end
		#if UNION_CLIPPING_PLANES < NUM_CLIPPING_PLANES
			float unionClipOpacity = 1.0;
			#pragma unroll_loop_start
			for ( int i = UNION_CLIPPING_PLANES; i < NUM_CLIPPING_PLANES; i ++ ) {
				plane = clippingPlanes[ i ];
				distanceToPlane = - dot( vClipPosition, plane.xyz ) + plane.w;
				distanceGradient = fwidth( distanceToPlane ) / 2.0;
				unionClipOpacity *= 1.0 - smoothstep( - distanceGradient, distanceGradient, distanceToPlane );
			}
			#pragma unroll_loop_end
			clipOpacity *= 1.0 - unionClipOpacity;
		#endif
		diffuseColor.a *= clipOpacity;
		if ( diffuseColor.a == 0.0 ) discard;
	#else
		#pragma unroll_loop_start
		for ( int i = 0; i < UNION_CLIPPING_PLANES; i ++ ) {
			plane = clippingPlanes[ i ];
			if ( dot( vClipPosition, plane.xyz ) > plane.w ) discard;
		}
		#pragma unroll_loop_end
		#if UNION_CLIPPING_PLANES < NUM_CLIPPING_PLANES
			bool clipped = true;
			#pragma unroll_loop_start
			for ( int i = UNION_CLIPPING_PLANES; i < NUM_CLIPPING_PLANES; i ++ ) {
				plane = clippingPlanes[ i ];
				clipped = ( dot( vClipPosition, plane.xyz ) > plane.w ) && clipped;
			}
			#pragma unroll_loop_end
			if ( clipped ) discard;
		#endif
	#endif
#endif`,pu=`#if NUM_CLIPPING_PLANES > 0
	varying vec3 vClipPosition;
	uniform vec4 clippingPlanes[ NUM_CLIPPING_PLANES ];
#endif`,mu=`#if NUM_CLIPPING_PLANES > 0
	varying vec3 vClipPosition;
#endif`,gu=`#if NUM_CLIPPING_PLANES > 0
	vClipPosition = - mvPosition.xyz;
#endif`,_u=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA )
	diffuseColor *= vColor;
#endif`,vu=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA )
	varying vec4 vColor;
#endif`,xu=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA ) || defined( USE_INSTANCING_COLOR ) || defined( USE_BATCHING_COLOR )
	varying vec4 vColor;
#endif`,bu=`#if defined( USE_COLOR ) || defined( USE_COLOR_ALPHA ) || defined( USE_INSTANCING_COLOR ) || defined( USE_BATCHING_COLOR )
	vColor = vec4( 1.0 );
#endif
#ifdef USE_COLOR_ALPHA
	vColor *= color;
#elif defined( USE_COLOR )
	vColor.rgb *= color;
#endif
#ifdef USE_INSTANCING_COLOR
	vColor.rgb *= instanceColor.rgb;
#endif
#ifdef USE_BATCHING_COLOR
	vColor *= getBatchingColor( getIndirectIndex( gl_DrawID ) );
#endif`,Su=`#define PI 3.141592653589793
#define PI2 6.283185307179586
#define PI_HALF 1.5707963267948966
#define RECIPROCAL_PI 0.3183098861837907
#define RECIPROCAL_PI2 0.15915494309189535
#define EPSILON 1e-6
#ifndef saturate
#define saturate( a ) clamp( a, 0.0, 1.0 )
#endif
#define whiteComplement( a ) ( 1.0 - saturate( a ) )
float pow2( const in float x ) { return x*x; }
vec3 pow2( const in vec3 x ) { return x*x; }
float pow3( const in float x ) { return x*x*x; }
float pow4( const in float x ) { float x2 = x*x; return x2*x2; }
float max3( const in vec3 v ) { return max( max( v.x, v.y ), v.z ); }
float average( const in vec3 v ) { return dot( v, vec3( 0.3333333 ) ); }
highp float rand( const in vec2 uv ) {
	const highp float a = 12.9898, b = 78.233, c = 43758.5453;
	highp float dt = dot( uv.xy, vec2( a,b ) ), sn = mod( dt, PI );
	return fract( sin( sn ) * c );
}
#ifdef HIGH_PRECISION
	float precisionSafeLength( vec3 v ) { return length( v ); }
#else
	float precisionSafeLength( vec3 v ) {
		float maxComponent = max3( abs( v ) );
		return length( v / maxComponent ) * maxComponent;
	}
#endif
struct IncidentLight {
	vec3 color;
	vec3 direction;
	bool visible;
};
struct ReflectedLight {
	vec3 directDiffuse;
	vec3 directSpecular;
	vec3 indirectDiffuse;
	vec3 indirectSpecular;
};
#ifdef USE_ALPHAHASH
	varying vec3 vPosition;
#endif
vec3 transformDirection( in vec3 dir, in mat4 matrix ) {
	return normalize( ( matrix * vec4( dir, 0.0 ) ).xyz );
}
vec3 inverseTransformDirection( in vec3 dir, in mat4 matrix ) {
	return normalize( ( vec4( dir, 0.0 ) * matrix ).xyz );
}
bool isPerspectiveMatrix( mat4 m ) {
	return m[ 2 ][ 3 ] == - 1.0;
}
vec2 equirectUv( in vec3 dir ) {
	float u = atan( dir.z, dir.x ) * RECIPROCAL_PI2 + 0.5;
	float v = asin( clamp( dir.y, - 1.0, 1.0 ) ) * RECIPROCAL_PI + 0.5;
	return vec2( u, v );
}
vec3 BRDF_Lambert( const in vec3 diffuseColor ) {
	return RECIPROCAL_PI * diffuseColor;
}
vec3 F_Schlick( const in vec3 f0, const in float f90, const in float dotVH ) {
	float fresnel = exp2( ( - 5.55473 * dotVH - 6.98316 ) * dotVH );
	return f0 * ( 1.0 - fresnel ) + ( f90 * fresnel );
}
float F_Schlick( const in float f0, const in float f90, const in float dotVH ) {
	float fresnel = exp2( ( - 5.55473 * dotVH - 6.98316 ) * dotVH );
	return f0 * ( 1.0 - fresnel ) + ( f90 * fresnel );
} // validated`,Mu=`#ifdef ENVMAP_TYPE_CUBE_UV
	#define cubeUV_minMipLevel 4.0
	#define cubeUV_minTileSize 16.0
	float getFace( vec3 direction ) {
		vec3 absDirection = abs( direction );
		float face = - 1.0;
		if ( absDirection.x > absDirection.z ) {
			if ( absDirection.x > absDirection.y )
				face = direction.x > 0.0 ? 0.0 : 3.0;
			else
				face = direction.y > 0.0 ? 1.0 : 4.0;
		} else {
			if ( absDirection.z > absDirection.y )
				face = direction.z > 0.0 ? 2.0 : 5.0;
			else
				face = direction.y > 0.0 ? 1.0 : 4.0;
		}
		return face;
	}
	vec2 getUV( vec3 direction, float face ) {
		vec2 uv;
		if ( face == 0.0 ) {
			uv = vec2( direction.z, direction.y ) / abs( direction.x );
		} else if ( face == 1.0 ) {
			uv = vec2( - direction.x, - direction.z ) / abs( direction.y );
		} else if ( face == 2.0 ) {
			uv = vec2( - direction.x, direction.y ) / abs( direction.z );
		} else if ( face == 3.0 ) {
			uv = vec2( - direction.z, direction.y ) / abs( direction.x );
		} else if ( face == 4.0 ) {
			uv = vec2( - direction.x, direction.z ) / abs( direction.y );
		} else {
			uv = vec2( direction.x, direction.y ) / abs( direction.z );
		}
		return 0.5 * ( uv + 1.0 );
	}
	vec3 bilinearCubeUV( sampler2D envMap, vec3 direction, float mipInt ) {
		float face = getFace( direction );
		float filterInt = max( cubeUV_minMipLevel - mipInt, 0.0 );
		mipInt = max( mipInt, cubeUV_minMipLevel );
		float faceSize = exp2( mipInt );
		highp vec2 uv = getUV( direction, face ) * ( faceSize - 2.0 ) + 1.0;
		if ( face > 2.0 ) {
			uv.y += faceSize;
			face -= 3.0;
		}
		uv.x += face * faceSize;
		uv.x += filterInt * 3.0 * cubeUV_minTileSize;
		uv.y += 4.0 * ( exp2( CUBEUV_MAX_MIP ) - faceSize );
		uv.x *= CUBEUV_TEXEL_WIDTH;
		uv.y *= CUBEUV_TEXEL_HEIGHT;
		#ifdef texture2DGradEXT
			return texture2DGradEXT( envMap, uv, vec2( 0.0 ), vec2( 0.0 ) ).rgb;
		#else
			return texture2D( envMap, uv ).rgb;
		#endif
	}
	#define cubeUV_r0 1.0
	#define cubeUV_m0 - 2.0
	#define cubeUV_r1 0.8
	#define cubeUV_m1 - 1.0
	#define cubeUV_r4 0.4
	#define cubeUV_m4 2.0
	#define cubeUV_r5 0.305
	#define cubeUV_m5 3.0
	#define cubeUV_r6 0.21
	#define cubeUV_m6 4.0
	float roughnessToMip( float roughness ) {
		float mip = 0.0;
		if ( roughness >= cubeUV_r1 ) {
			mip = ( cubeUV_r0 - roughness ) * ( cubeUV_m1 - cubeUV_m0 ) / ( cubeUV_r0 - cubeUV_r1 ) + cubeUV_m0;
		} else if ( roughness >= cubeUV_r4 ) {
			mip = ( cubeUV_r1 - roughness ) * ( cubeUV_m4 - cubeUV_m1 ) / ( cubeUV_r1 - cubeUV_r4 ) + cubeUV_m1;
		} else if ( roughness >= cubeUV_r5 ) {
			mip = ( cubeUV_r4 - roughness ) * ( cubeUV_m5 - cubeUV_m4 ) / ( cubeUV_r4 - cubeUV_r5 ) + cubeUV_m4;
		} else if ( roughness >= cubeUV_r6 ) {
			mip = ( cubeUV_r5 - roughness ) * ( cubeUV_m6 - cubeUV_m5 ) / ( cubeUV_r5 - cubeUV_r6 ) + cubeUV_m5;
		} else {
			mip = - 2.0 * log2( 1.16 * roughness );		}
		return mip;
	}
	vec4 textureCubeUV( sampler2D envMap, vec3 sampleDir, float roughness ) {
		float mip = clamp( roughnessToMip( roughness ), cubeUV_m0, CUBEUV_MAX_MIP );
		float mipF = fract( mip );
		float mipInt = floor( mip );
		vec3 color0 = bilinearCubeUV( envMap, sampleDir, mipInt );
		if ( mipF == 0.0 ) {
			return vec4( color0, 1.0 );
		} else {
			vec3 color1 = bilinearCubeUV( envMap, sampleDir, mipInt + 1.0 );
			return vec4( mix( color0, color1, mipF ), 1.0 );
		}
	}
#endif`,yu=`vec3 transformedNormal = objectNormal;
#ifdef USE_TANGENT
	vec3 transformedTangent = objectTangent;
#endif
#ifdef USE_BATCHING
	mat3 bm = mat3( batchingMatrix );
	transformedNormal /= vec3( dot( bm[ 0 ], bm[ 0 ] ), dot( bm[ 1 ], bm[ 1 ] ), dot( bm[ 2 ], bm[ 2 ] ) );
	transformedNormal = bm * transformedNormal;
	#ifdef USE_TANGENT
		transformedTangent = bm * transformedTangent;
	#endif
#endif
#ifdef USE_INSTANCING
	mat3 im = mat3( instanceMatrix );
	transformedNormal /= vec3( dot( im[ 0 ], im[ 0 ] ), dot( im[ 1 ], im[ 1 ] ), dot( im[ 2 ], im[ 2 ] ) );
	transformedNormal = im * transformedNormal;
	#ifdef USE_TANGENT
		transformedTangent = im * transformedTangent;
	#endif
#endif
transformedNormal = normalMatrix * transformedNormal;
#ifdef FLIP_SIDED
	transformedNormal = - transformedNormal;
#endif
#ifdef USE_TANGENT
	transformedTangent = ( modelViewMatrix * vec4( transformedTangent, 0.0 ) ).xyz;
	#ifdef FLIP_SIDED
		transformedTangent = - transformedTangent;
	#endif
#endif`,Eu=`#ifdef USE_DISPLACEMENTMAP
	uniform sampler2D displacementMap;
	uniform float displacementScale;
	uniform float displacementBias;
#endif`,wu=`#ifdef USE_DISPLACEMENTMAP
	transformed += normalize( objectNormal ) * ( texture2D( displacementMap, vDisplacementMapUv ).x * displacementScale + displacementBias );
#endif`,Tu=`#ifdef USE_EMISSIVEMAP
	vec4 emissiveColor = texture2D( emissiveMap, vEmissiveMapUv );
	#ifdef DECODE_VIDEO_TEXTURE_EMISSIVE
		emissiveColor = sRGBTransferEOTF( emissiveColor );
	#endif
	totalEmissiveRadiance *= emissiveColor.rgb;
#endif`,Au=`#ifdef USE_EMISSIVEMAP
	uniform sampler2D emissiveMap;
#endif`,Cu="gl_FragColor = linearToOutputTexel( gl_FragColor );",Ru=`vec4 LinearTransferOETF( in vec4 value ) {
	return value;
}
vec4 sRGBTransferEOTF( in vec4 value ) {
	return vec4( mix( pow( value.rgb * 0.9478672986 + vec3( 0.0521327014 ), vec3( 2.4 ) ), value.rgb * 0.0773993808, vec3( lessThanEqual( value.rgb, vec3( 0.04045 ) ) ) ), value.a );
}
vec4 sRGBTransferOETF( in vec4 value ) {
	return vec4( mix( pow( value.rgb, vec3( 0.41666 ) ) * 1.055 - vec3( 0.055 ), value.rgb * 12.92, vec3( lessThanEqual( value.rgb, vec3( 0.0031308 ) ) ) ), value.a );
}`,Pu=`#ifdef USE_ENVMAP
	#ifdef ENV_WORLDPOS
		vec3 cameraToFrag;
		if ( isOrthographic ) {
			cameraToFrag = normalize( vec3( - viewMatrix[ 0 ][ 2 ], - viewMatrix[ 1 ][ 2 ], - viewMatrix[ 2 ][ 2 ] ) );
		} else {
			cameraToFrag = normalize( vWorldPosition - cameraPosition );
		}
		vec3 worldNormal = inverseTransformDirection( normal, viewMatrix );
		#ifdef ENVMAP_MODE_REFLECTION
			vec3 reflectVec = reflect( cameraToFrag, worldNormal );
		#else
			vec3 reflectVec = refract( cameraToFrag, worldNormal, refractionRatio );
		#endif
	#else
		vec3 reflectVec = vReflect;
	#endif
	#ifdef ENVMAP_TYPE_CUBE
		vec4 envColor = textureCube( envMap, envMapRotation * reflectVec );
		#ifdef ENVMAP_BLENDING_MULTIPLY
			outgoingLight = mix( outgoingLight, outgoingLight * envColor.xyz, specularStrength * reflectivity );
		#elif defined( ENVMAP_BLENDING_MIX )
			outgoingLight = mix( outgoingLight, envColor.xyz, specularStrength * reflectivity );
		#elif defined( ENVMAP_BLENDING_ADD )
			outgoingLight += envColor.xyz * specularStrength * reflectivity;
		#endif
	#endif
#endif`,Lu=`#ifdef USE_ENVMAP
	uniform float envMapIntensity;
	uniform mat3 envMapRotation;
	#ifdef ENVMAP_TYPE_CUBE
		uniform samplerCube envMap;
	#else
		uniform sampler2D envMap;
	#endif
#endif`,Du=`#ifdef USE_ENVMAP
	uniform float reflectivity;
	#if defined( USE_BUMPMAP ) || defined( USE_NORMALMAP ) || defined( PHONG ) || defined( LAMBERT )
		#define ENV_WORLDPOS
	#endif
	#ifdef ENV_WORLDPOS
		varying vec3 vWorldPosition;
		uniform float refractionRatio;
	#else
		varying vec3 vReflect;
	#endif
#endif`,Iu=`#ifdef USE_ENVMAP
	#if defined( USE_BUMPMAP ) || defined( USE_NORMALMAP ) || defined( PHONG ) || defined( LAMBERT )
		#define ENV_WORLDPOS
	#endif
	#ifdef ENV_WORLDPOS
		
		varying vec3 vWorldPosition;
	#else
		varying vec3 vReflect;
		uniform float refractionRatio;
	#endif
#endif`,Nu=`#ifdef USE_ENVMAP
	#ifdef ENV_WORLDPOS
		vWorldPosition = worldPosition.xyz;
	#else
		vec3 cameraToVertex;
		if ( isOrthographic ) {
			cameraToVertex = normalize( vec3( - viewMatrix[ 0 ][ 2 ], - viewMatrix[ 1 ][ 2 ], - viewMatrix[ 2 ][ 2 ] ) );
		} else {
			cameraToVertex = normalize( worldPosition.xyz - cameraPosition );
		}
		vec3 worldNormal = inverseTransformDirection( transformedNormal, viewMatrix );
		#ifdef ENVMAP_MODE_REFLECTION
			vReflect = reflect( cameraToVertex, worldNormal );
		#else
			vReflect = refract( cameraToVertex, worldNormal, refractionRatio );
		#endif
	#endif
#endif`,Uu=`#ifdef USE_FOG
	vFogDepth = - mvPosition.z;
#endif`,Fu=`#ifdef USE_FOG
	varying float vFogDepth;
#endif`,Ou=`#ifdef USE_FOG
	#ifdef FOG_EXP2
		float fogFactor = 1.0 - exp( - fogDensity * fogDensity * vFogDepth * vFogDepth );
	#else
		float fogFactor = smoothstep( fogNear, fogFar, vFogDepth );
	#endif
	gl_FragColor.rgb = mix( gl_FragColor.rgb, fogColor, fogFactor );
#endif`,Bu=`#ifdef USE_FOG
	uniform vec3 fogColor;
	varying float vFogDepth;
	#ifdef FOG_EXP2
		uniform float fogDensity;
	#else
		uniform float fogNear;
		uniform float fogFar;
	#endif
#endif`,zu=`#ifdef USE_GRADIENTMAP
	uniform sampler2D gradientMap;
#endif
vec3 getGradientIrradiance( vec3 normal, vec3 lightDirection ) {
	float dotNL = dot( normal, lightDirection );
	vec2 coord = vec2( dotNL * 0.5 + 0.5, 0.0 );
	#ifdef USE_GRADIENTMAP
		return vec3( texture2D( gradientMap, coord ).r );
	#else
		vec2 fw = fwidth( coord ) * 0.5;
		return mix( vec3( 0.7 ), vec3( 1.0 ), smoothstep( 0.7 - fw.x, 0.7 + fw.x, coord.x ) );
	#endif
}`,ku=`#ifdef USE_LIGHTMAP
	uniform sampler2D lightMap;
	uniform float lightMapIntensity;
#endif`,Vu=`LambertMaterial material;
material.diffuseColor = diffuseColor.rgb;
material.specularStrength = specularStrength;`,Gu=`varying vec3 vViewPosition;
struct LambertMaterial {
	vec3 diffuseColor;
	float specularStrength;
};
void RE_Direct_Lambert( const in IncidentLight directLight, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in LambertMaterial material, inout ReflectedLight reflectedLight ) {
	float dotNL = saturate( dot( geometryNormal, directLight.direction ) );
	vec3 irradiance = dotNL * directLight.color;
	reflectedLight.directDiffuse += irradiance * BRDF_Lambert( material.diffuseColor );
}
void RE_IndirectDiffuse_Lambert( const in vec3 irradiance, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in LambertMaterial material, inout ReflectedLight reflectedLight ) {
	reflectedLight.indirectDiffuse += irradiance * BRDF_Lambert( material.diffuseColor );
}
#define RE_Direct				RE_Direct_Lambert
#define RE_IndirectDiffuse		RE_IndirectDiffuse_Lambert`,Hu=`uniform bool receiveShadow;
uniform vec3 ambientLightColor;
#if defined( USE_LIGHT_PROBES )
	uniform vec3 lightProbe[ 9 ];
#endif
vec3 shGetIrradianceAt( in vec3 normal, in vec3 shCoefficients[ 9 ] ) {
	float x = normal.x, y = normal.y, z = normal.z;
	vec3 result = shCoefficients[ 0 ] * 0.886227;
	result += shCoefficients[ 1 ] * 2.0 * 0.511664 * y;
	result += shCoefficients[ 2 ] * 2.0 * 0.511664 * z;
	result += shCoefficients[ 3 ] * 2.0 * 0.511664 * x;
	result += shCoefficients[ 4 ] * 2.0 * 0.429043 * x * y;
	result += shCoefficients[ 5 ] * 2.0 * 0.429043 * y * z;
	result += shCoefficients[ 6 ] * ( 0.743125 * z * z - 0.247708 );
	result += shCoefficients[ 7 ] * 2.0 * 0.429043 * x * z;
	result += shCoefficients[ 8 ] * 0.429043 * ( x * x - y * y );
	return result;
}
vec3 getLightProbeIrradiance( const in vec3 lightProbe[ 9 ], const in vec3 normal ) {
	vec3 worldNormal = inverseTransformDirection( normal, viewMatrix );
	vec3 irradiance = shGetIrradianceAt( worldNormal, lightProbe );
	return irradiance;
}
vec3 getAmbientLightIrradiance( const in vec3 ambientLightColor ) {
	vec3 irradiance = ambientLightColor;
	return irradiance;
}
float getDistanceAttenuation( const in float lightDistance, const in float cutoffDistance, const in float decayExponent ) {
	float distanceFalloff = 1.0 / max( pow( lightDistance, decayExponent ), 0.01 );
	if ( cutoffDistance > 0.0 ) {
		distanceFalloff *= pow2( saturate( 1.0 - pow4( lightDistance / cutoffDistance ) ) );
	}
	return distanceFalloff;
}
float getSpotAttenuation( const in float coneCosine, const in float penumbraCosine, const in float angleCosine ) {
	return smoothstep( coneCosine, penumbraCosine, angleCosine );
}
#if NUM_DIR_LIGHTS > 0
	struct DirectionalLight {
		vec3 direction;
		vec3 color;
	};
	uniform DirectionalLight directionalLights[ NUM_DIR_LIGHTS ];
	void getDirectionalLightInfo( const in DirectionalLight directionalLight, out IncidentLight light ) {
		light.color = directionalLight.color;
		light.direction = directionalLight.direction;
		light.visible = true;
	}
#endif
#if NUM_POINT_LIGHTS > 0
	struct PointLight {
		vec3 position;
		vec3 color;
		float distance;
		float decay;
	};
	uniform PointLight pointLights[ NUM_POINT_LIGHTS ];
	void getPointLightInfo( const in PointLight pointLight, const in vec3 geometryPosition, out IncidentLight light ) {
		vec3 lVector = pointLight.position - geometryPosition;
		light.direction = normalize( lVector );
		float lightDistance = length( lVector );
		light.color = pointLight.color;
		light.color *= getDistanceAttenuation( lightDistance, pointLight.distance, pointLight.decay );
		light.visible = ( light.color != vec3( 0.0 ) );
	}
#endif
#if NUM_SPOT_LIGHTS > 0
	struct SpotLight {
		vec3 position;
		vec3 direction;
		vec3 color;
		float distance;
		float decay;
		float coneCos;
		float penumbraCos;
	};
	uniform SpotLight spotLights[ NUM_SPOT_LIGHTS ];
	void getSpotLightInfo( const in SpotLight spotLight, const in vec3 geometryPosition, out IncidentLight light ) {
		vec3 lVector = spotLight.position - geometryPosition;
		light.direction = normalize( lVector );
		float angleCos = dot( light.direction, spotLight.direction );
		float spotAttenuation = getSpotAttenuation( spotLight.coneCos, spotLight.penumbraCos, angleCos );
		if ( spotAttenuation > 0.0 ) {
			float lightDistance = length( lVector );
			light.color = spotLight.color * spotAttenuation;
			light.color *= getDistanceAttenuation( lightDistance, spotLight.distance, spotLight.decay );
			light.visible = ( light.color != vec3( 0.0 ) );
		} else {
			light.color = vec3( 0.0 );
			light.visible = false;
		}
	}
#endif
#if NUM_RECT_AREA_LIGHTS > 0
	struct RectAreaLight {
		vec3 color;
		vec3 position;
		vec3 halfWidth;
		vec3 halfHeight;
	};
	uniform sampler2D ltc_1;	uniform sampler2D ltc_2;
	uniform RectAreaLight rectAreaLights[ NUM_RECT_AREA_LIGHTS ];
#endif
#if NUM_HEMI_LIGHTS > 0
	struct HemisphereLight {
		vec3 direction;
		vec3 skyColor;
		vec3 groundColor;
	};
	uniform HemisphereLight hemisphereLights[ NUM_HEMI_LIGHTS ];
	vec3 getHemisphereLightIrradiance( const in HemisphereLight hemiLight, const in vec3 normal ) {
		float dotNL = dot( normal, hemiLight.direction );
		float hemiDiffuseWeight = 0.5 * dotNL + 0.5;
		vec3 irradiance = mix( hemiLight.groundColor, hemiLight.skyColor, hemiDiffuseWeight );
		return irradiance;
	}
#endif
#include <lightprobes_pars_fragment>`,Wu=`#ifdef USE_ENVMAP
	vec3 getIBLIrradiance( const in vec3 normal ) {
		#ifdef ENVMAP_TYPE_CUBE_UV
			vec3 worldNormal = inverseTransformDirection( normal, viewMatrix );
			vec4 envMapColor = textureCubeUV( envMap, envMapRotation * worldNormal, 1.0 );
			return PI * envMapColor.rgb * envMapIntensity;
		#else
			return vec3( 0.0 );
		#endif
	}
	vec3 getIBLRadiance( const in vec3 viewDir, const in vec3 normal, const in float roughness ) {
		#ifdef ENVMAP_TYPE_CUBE_UV
			vec3 reflectVec = reflect( - viewDir, normal );
			reflectVec = normalize( mix( reflectVec, normal, pow4( roughness ) ) );
			reflectVec = inverseTransformDirection( reflectVec, viewMatrix );
			vec4 envMapColor = textureCubeUV( envMap, envMapRotation * reflectVec, roughness );
			return envMapColor.rgb * envMapIntensity;
		#else
			return vec3( 0.0 );
		#endif
	}
	#ifdef USE_ANISOTROPY
		vec3 getIBLAnisotropyRadiance( const in vec3 viewDir, const in vec3 normal, const in float roughness, const in vec3 bitangent, const in float anisotropy ) {
			#ifdef ENVMAP_TYPE_CUBE_UV
				vec3 bentNormal = cross( bitangent, viewDir );
				bentNormal = normalize( cross( bentNormal, bitangent ) );
				bentNormal = normalize( mix( bentNormal, normal, pow2( pow2( 1.0 - anisotropy * ( 1.0 - roughness ) ) ) ) );
				return getIBLRadiance( viewDir, bentNormal, roughness );
			#else
				return vec3( 0.0 );
			#endif
		}
	#endif
#endif`,Xu=`ToonMaterial material;
material.diffuseColor = diffuseColor.rgb;`,ju=`varying vec3 vViewPosition;
struct ToonMaterial {
	vec3 diffuseColor;
};
void RE_Direct_Toon( const in IncidentLight directLight, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in ToonMaterial material, inout ReflectedLight reflectedLight ) {
	vec3 irradiance = getGradientIrradiance( geometryNormal, directLight.direction ) * directLight.color;
	reflectedLight.directDiffuse += irradiance * BRDF_Lambert( material.diffuseColor );
}
void RE_IndirectDiffuse_Toon( const in vec3 irradiance, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in ToonMaterial material, inout ReflectedLight reflectedLight ) {
	reflectedLight.indirectDiffuse += irradiance * BRDF_Lambert( material.diffuseColor );
}
#define RE_Direct				RE_Direct_Toon
#define RE_IndirectDiffuse		RE_IndirectDiffuse_Toon`,qu=`BlinnPhongMaterial material;
material.diffuseColor = diffuseColor.rgb;
material.specularColor = specular;
material.specularShininess = shininess;
material.specularStrength = specularStrength;`,Yu=`varying vec3 vViewPosition;
struct BlinnPhongMaterial {
	vec3 diffuseColor;
	vec3 specularColor;
	float specularShininess;
	float specularStrength;
};
void RE_Direct_BlinnPhong( const in IncidentLight directLight, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in BlinnPhongMaterial material, inout ReflectedLight reflectedLight ) {
	float dotNL = saturate( dot( geometryNormal, directLight.direction ) );
	vec3 irradiance = dotNL * directLight.color;
	reflectedLight.directDiffuse += irradiance * BRDF_Lambert( material.diffuseColor );
	reflectedLight.directSpecular += irradiance * BRDF_BlinnPhong( directLight.direction, geometryViewDir, geometryNormal, material.specularColor, material.specularShininess ) * material.specularStrength;
}
void RE_IndirectDiffuse_BlinnPhong( const in vec3 irradiance, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in BlinnPhongMaterial material, inout ReflectedLight reflectedLight ) {
	reflectedLight.indirectDiffuse += irradiance * BRDF_Lambert( material.diffuseColor );
}
#define RE_Direct				RE_Direct_BlinnPhong
#define RE_IndirectDiffuse		RE_IndirectDiffuse_BlinnPhong`,$u=`PhysicalMaterial material;
material.diffuseColor = diffuseColor.rgb;
material.diffuseContribution = diffuseColor.rgb * ( 1.0 - metalnessFactor );
material.metalness = metalnessFactor;
vec3 dxy = max( abs( dFdx( nonPerturbedNormal ) ), abs( dFdy( nonPerturbedNormal ) ) );
float geometryRoughness = max( max( dxy.x, dxy.y ), dxy.z );
material.roughness = max( roughnessFactor, 0.0525 );material.roughness += geometryRoughness;
material.roughness = min( material.roughness, 1.0 );
#ifdef IOR
	material.ior = ior;
	#ifdef USE_SPECULAR
		float specularIntensityFactor = specularIntensity;
		vec3 specularColorFactor = specularColor;
		#ifdef USE_SPECULAR_COLORMAP
			specularColorFactor *= texture2D( specularColorMap, vSpecularColorMapUv ).rgb;
		#endif
		#ifdef USE_SPECULAR_INTENSITYMAP
			specularIntensityFactor *= texture2D( specularIntensityMap, vSpecularIntensityMapUv ).a;
		#endif
		material.specularF90 = mix( specularIntensityFactor, 1.0, metalnessFactor );
	#else
		float specularIntensityFactor = 1.0;
		vec3 specularColorFactor = vec3( 1.0 );
		material.specularF90 = 1.0;
	#endif
	material.specularColor = min( pow2( ( material.ior - 1.0 ) / ( material.ior + 1.0 ) ) * specularColorFactor, vec3( 1.0 ) ) * specularIntensityFactor;
	material.specularColorBlended = mix( material.specularColor, diffuseColor.rgb, metalnessFactor );
#else
	material.specularColor = vec3( 0.04 );
	material.specularColorBlended = mix( material.specularColor, diffuseColor.rgb, metalnessFactor );
	material.specularF90 = 1.0;
#endif
#ifdef USE_CLEARCOAT
	material.clearcoat = clearcoat;
	material.clearcoatRoughness = clearcoatRoughness;
	material.clearcoatF0 = vec3( 0.04 );
	material.clearcoatF90 = 1.0;
	#ifdef USE_CLEARCOATMAP
		material.clearcoat *= texture2D( clearcoatMap, vClearcoatMapUv ).x;
	#endif
	#ifdef USE_CLEARCOAT_ROUGHNESSMAP
		material.clearcoatRoughness *= texture2D( clearcoatRoughnessMap, vClearcoatRoughnessMapUv ).y;
	#endif
	material.clearcoat = saturate( material.clearcoat );	material.clearcoatRoughness = max( material.clearcoatRoughness, 0.0525 );
	material.clearcoatRoughness += geometryRoughness;
	material.clearcoatRoughness = min( material.clearcoatRoughness, 1.0 );
#endif
#ifdef USE_DISPERSION
	material.dispersion = dispersion;
#endif
#ifdef USE_IRIDESCENCE
	material.iridescence = iridescence;
	material.iridescenceIOR = iridescenceIOR;
	#ifdef USE_IRIDESCENCEMAP
		material.iridescence *= texture2D( iridescenceMap, vIridescenceMapUv ).r;
	#endif
	#ifdef USE_IRIDESCENCE_THICKNESSMAP
		material.iridescenceThickness = (iridescenceThicknessMaximum - iridescenceThicknessMinimum) * texture2D( iridescenceThicknessMap, vIridescenceThicknessMapUv ).g + iridescenceThicknessMinimum;
	#else
		material.iridescenceThickness = iridescenceThicknessMaximum;
	#endif
#endif
#ifdef USE_SHEEN
	material.sheenColor = sheenColor;
	#ifdef USE_SHEEN_COLORMAP
		material.sheenColor *= texture2D( sheenColorMap, vSheenColorMapUv ).rgb;
	#endif
	material.sheenRoughness = clamp( sheenRoughness, 0.0001, 1.0 );
	#ifdef USE_SHEEN_ROUGHNESSMAP
		material.sheenRoughness *= texture2D( sheenRoughnessMap, vSheenRoughnessMapUv ).a;
	#endif
#endif
#ifdef USE_ANISOTROPY
	#ifdef USE_ANISOTROPYMAP
		mat2 anisotropyMat = mat2( anisotropyVector.x, anisotropyVector.y, - anisotropyVector.y, anisotropyVector.x );
		vec3 anisotropyPolar = texture2D( anisotropyMap, vAnisotropyMapUv ).rgb;
		vec2 anisotropyV = anisotropyMat * normalize( 2.0 * anisotropyPolar.rg - vec2( 1.0 ) ) * anisotropyPolar.b;
	#else
		vec2 anisotropyV = anisotropyVector;
	#endif
	material.anisotropy = length( anisotropyV );
	if( material.anisotropy == 0.0 ) {
		anisotropyV = vec2( 1.0, 0.0 );
	} else {
		anisotropyV /= material.anisotropy;
		material.anisotropy = saturate( material.anisotropy );
	}
	material.alphaT = mix( pow2( material.roughness ), 1.0, pow2( material.anisotropy ) );
	material.anisotropyT = tbn[ 0 ] * anisotropyV.x + tbn[ 1 ] * anisotropyV.y;
	material.anisotropyB = tbn[ 1 ] * anisotropyV.x - tbn[ 0 ] * anisotropyV.y;
#endif`,Zu=`uniform sampler2D dfgLUT;
struct PhysicalMaterial {
	vec3 diffuseColor;
	vec3 diffuseContribution;
	vec3 specularColor;
	vec3 specularColorBlended;
	float roughness;
	float metalness;
	float specularF90;
	float dispersion;
	#ifdef USE_CLEARCOAT
		float clearcoat;
		float clearcoatRoughness;
		vec3 clearcoatF0;
		float clearcoatF90;
	#endif
	#ifdef USE_IRIDESCENCE
		float iridescence;
		float iridescenceIOR;
		float iridescenceThickness;
		vec3 iridescenceFresnel;
		vec3 iridescenceF0;
		vec3 iridescenceFresnelDielectric;
		vec3 iridescenceFresnelMetallic;
	#endif
	#ifdef USE_SHEEN
		vec3 sheenColor;
		float sheenRoughness;
	#endif
	#ifdef IOR
		float ior;
	#endif
	#ifdef USE_TRANSMISSION
		float transmission;
		float transmissionAlpha;
		float thickness;
		float attenuationDistance;
		vec3 attenuationColor;
	#endif
	#ifdef USE_ANISOTROPY
		float anisotropy;
		float alphaT;
		vec3 anisotropyT;
		vec3 anisotropyB;
	#endif
};
vec3 clearcoatSpecularDirect = vec3( 0.0 );
vec3 clearcoatSpecularIndirect = vec3( 0.0 );
vec3 sheenSpecularDirect = vec3( 0.0 );
vec3 sheenSpecularIndirect = vec3(0.0 );
vec3 Schlick_to_F0( const in vec3 f, const in float f90, const in float dotVH ) {
    float x = clamp( 1.0 - dotVH, 0.0, 1.0 );
    float x2 = x * x;
    float x5 = clamp( x * x2 * x2, 0.0, 0.9999 );
    return ( f - vec3( f90 ) * x5 ) / ( 1.0 - x5 );
}
float V_GGX_SmithCorrelated( const in float alpha, const in float dotNL, const in float dotNV ) {
	float a2 = pow2( alpha );
	float gv = dotNL * sqrt( a2 + ( 1.0 - a2 ) * pow2( dotNV ) );
	float gl = dotNV * sqrt( a2 + ( 1.0 - a2 ) * pow2( dotNL ) );
	return 0.5 / max( gv + gl, EPSILON );
}
float D_GGX( const in float alpha, const in float dotNH ) {
	float a2 = pow2( alpha );
	float denom = pow2( dotNH ) * ( a2 - 1.0 ) + 1.0;
	return RECIPROCAL_PI * a2 / pow2( denom );
}
#ifdef USE_ANISOTROPY
	float V_GGX_SmithCorrelated_Anisotropic( const in float alphaT, const in float alphaB, const in float dotTV, const in float dotBV, const in float dotTL, const in float dotBL, const in float dotNV, const in float dotNL ) {
		float gv = dotNL * length( vec3( alphaT * dotTV, alphaB * dotBV, dotNV ) );
		float gl = dotNV * length( vec3( alphaT * dotTL, alphaB * dotBL, dotNL ) );
		return 0.5 / max( gv + gl, EPSILON );
	}
	float D_GGX_Anisotropic( const in float alphaT, const in float alphaB, const in float dotNH, const in float dotTH, const in float dotBH ) {
		float a2 = alphaT * alphaB;
		highp vec3 v = vec3( alphaB * dotTH, alphaT * dotBH, a2 * dotNH );
		highp float v2 = dot( v, v );
		float w2 = a2 / v2;
		return RECIPROCAL_PI * a2 * pow2 ( w2 );
	}
#endif
#ifdef USE_CLEARCOAT
	vec3 BRDF_GGX_Clearcoat( const in vec3 lightDir, const in vec3 viewDir, const in vec3 normal, const in PhysicalMaterial material) {
		vec3 f0 = material.clearcoatF0;
		float f90 = material.clearcoatF90;
		float roughness = material.clearcoatRoughness;
		float alpha = pow2( roughness );
		vec3 halfDir = normalize( lightDir + viewDir );
		float dotNL = saturate( dot( normal, lightDir ) );
		float dotNV = saturate( dot( normal, viewDir ) );
		float dotNH = saturate( dot( normal, halfDir ) );
		float dotVH = saturate( dot( viewDir, halfDir ) );
		vec3 F = F_Schlick( f0, f90, dotVH );
		float V = V_GGX_SmithCorrelated( alpha, dotNL, dotNV );
		float D = D_GGX( alpha, dotNH );
		return F * ( V * D );
	}
#endif
vec3 BRDF_GGX( const in vec3 lightDir, const in vec3 viewDir, const in vec3 normal, const in PhysicalMaterial material ) {
	vec3 f0 = material.specularColorBlended;
	float f90 = material.specularF90;
	float roughness = material.roughness;
	float alpha = pow2( roughness );
	vec3 halfDir = normalize( lightDir + viewDir );
	float dotNL = saturate( dot( normal, lightDir ) );
	float dotNV = saturate( dot( normal, viewDir ) );
	float dotNH = saturate( dot( normal, halfDir ) );
	float dotVH = saturate( dot( viewDir, halfDir ) );
	vec3 F = F_Schlick( f0, f90, dotVH );
	#ifdef USE_IRIDESCENCE
		F = mix( F, material.iridescenceFresnel, material.iridescence );
	#endif
	#ifdef USE_ANISOTROPY
		float dotTL = dot( material.anisotropyT, lightDir );
		float dotTV = dot( material.anisotropyT, viewDir );
		float dotTH = dot( material.anisotropyT, halfDir );
		float dotBL = dot( material.anisotropyB, lightDir );
		float dotBV = dot( material.anisotropyB, viewDir );
		float dotBH = dot( material.anisotropyB, halfDir );
		float V = V_GGX_SmithCorrelated_Anisotropic( material.alphaT, alpha, dotTV, dotBV, dotTL, dotBL, dotNV, dotNL );
		float D = D_GGX_Anisotropic( material.alphaT, alpha, dotNH, dotTH, dotBH );
	#else
		float V = V_GGX_SmithCorrelated( alpha, dotNL, dotNV );
		float D = D_GGX( alpha, dotNH );
	#endif
	return F * ( V * D );
}
vec2 LTC_Uv( const in vec3 N, const in vec3 V, const in float roughness ) {
	const float LUT_SIZE = 64.0;
	const float LUT_SCALE = ( LUT_SIZE - 1.0 ) / LUT_SIZE;
	const float LUT_BIAS = 0.5 / LUT_SIZE;
	float dotNV = saturate( dot( N, V ) );
	vec2 uv = vec2( roughness, sqrt( 1.0 - dotNV ) );
	uv = uv * LUT_SCALE + LUT_BIAS;
	return uv;
}
float LTC_ClippedSphereFormFactor( const in vec3 f ) {
	float l = length( f );
	return max( ( l * l + f.z ) / ( l + 1.0 ), 0.0 );
}
vec3 LTC_EdgeVectorFormFactor( const in vec3 v1, const in vec3 v2 ) {
	float x = dot( v1, v2 );
	float y = abs( x );
	float a = 0.8543985 + ( 0.4965155 + 0.0145206 * y ) * y;
	float b = 3.4175940 + ( 4.1616724 + y ) * y;
	float v = a / b;
	float theta_sintheta = ( x > 0.0 ) ? v : 0.5 * inversesqrt( max( 1.0 - x * x, 1e-7 ) ) - v;
	return cross( v1, v2 ) * theta_sintheta;
}
vec3 LTC_Evaluate( const in vec3 N, const in vec3 V, const in vec3 P, const in mat3 mInv, const in vec3 rectCoords[ 4 ] ) {
	vec3 v1 = rectCoords[ 1 ] - rectCoords[ 0 ];
	vec3 v2 = rectCoords[ 3 ] - rectCoords[ 0 ];
	vec3 lightNormal = cross( v1, v2 );
	if( dot( lightNormal, P - rectCoords[ 0 ] ) < 0.0 ) return vec3( 0.0 );
	vec3 T1, T2;
	T1 = normalize( V - N * dot( V, N ) );
	T2 = - cross( N, T1 );
	mat3 mat = mInv * transpose( mat3( T1, T2, N ) );
	vec3 coords[ 4 ];
	coords[ 0 ] = mat * ( rectCoords[ 0 ] - P );
	coords[ 1 ] = mat * ( rectCoords[ 1 ] - P );
	coords[ 2 ] = mat * ( rectCoords[ 2 ] - P );
	coords[ 3 ] = mat * ( rectCoords[ 3 ] - P );
	coords[ 0 ] = normalize( coords[ 0 ] );
	coords[ 1 ] = normalize( coords[ 1 ] );
	coords[ 2 ] = normalize( coords[ 2 ] );
	coords[ 3 ] = normalize( coords[ 3 ] );
	vec3 vectorFormFactor = vec3( 0.0 );
	vectorFormFactor += LTC_EdgeVectorFormFactor( coords[ 0 ], coords[ 1 ] );
	vectorFormFactor += LTC_EdgeVectorFormFactor( coords[ 1 ], coords[ 2 ] );
	vectorFormFactor += LTC_EdgeVectorFormFactor( coords[ 2 ], coords[ 3 ] );
	vectorFormFactor += LTC_EdgeVectorFormFactor( coords[ 3 ], coords[ 0 ] );
	float result = LTC_ClippedSphereFormFactor( vectorFormFactor );
	return vec3( result );
}
#if defined( USE_SHEEN )
float D_Charlie( float roughness, float dotNH ) {
	float alpha = pow2( roughness );
	float invAlpha = 1.0 / alpha;
	float cos2h = dotNH * dotNH;
	float sin2h = max( 1.0 - cos2h, 0.0078125 );
	return ( 2.0 + invAlpha ) * pow( sin2h, invAlpha * 0.5 ) / ( 2.0 * PI );
}
float V_Neubelt( float dotNV, float dotNL ) {
	return saturate( 1.0 / ( 4.0 * ( dotNL + dotNV - dotNL * dotNV ) ) );
}
vec3 BRDF_Sheen( const in vec3 lightDir, const in vec3 viewDir, const in vec3 normal, vec3 sheenColor, const in float sheenRoughness ) {
	vec3 halfDir = normalize( lightDir + viewDir );
	float dotNL = saturate( dot( normal, lightDir ) );
	float dotNV = saturate( dot( normal, viewDir ) );
	float dotNH = saturate( dot( normal, halfDir ) );
	float D = D_Charlie( sheenRoughness, dotNH );
	float V = V_Neubelt( dotNV, dotNL );
	return sheenColor * ( D * V );
}
#endif
float IBLSheenBRDF( const in vec3 normal, const in vec3 viewDir, const in float roughness ) {
	float dotNV = saturate( dot( normal, viewDir ) );
	float r2 = roughness * roughness;
	float rInv = 1.0 / ( roughness + 0.1 );
	float a = -1.9362 + 1.0678 * roughness + 0.4573 * r2 - 0.8469 * rInv;
	float b = -0.6014 + 0.5538 * roughness - 0.4670 * r2 - 0.1255 * rInv;
	float DG = exp( a * dotNV + b );
	return saturate( DG );
}
vec3 EnvironmentBRDF( const in vec3 normal, const in vec3 viewDir, const in vec3 specularColor, const in float specularF90, const in float roughness ) {
	float dotNV = saturate( dot( normal, viewDir ) );
	vec2 fab = texture2D( dfgLUT, vec2( roughness, dotNV ) ).rg;
	return specularColor * fab.x + specularF90 * fab.y;
}
#ifdef USE_IRIDESCENCE
void computeMultiscatteringIridescence( const in vec3 normal, const in vec3 viewDir, const in vec3 specularColor, const in float specularF90, const in float iridescence, const in vec3 iridescenceF0, const in float roughness, inout vec3 singleScatter, inout vec3 multiScatter ) {
#else
void computeMultiscattering( const in vec3 normal, const in vec3 viewDir, const in vec3 specularColor, const in float specularF90, const in float roughness, inout vec3 singleScatter, inout vec3 multiScatter ) {
#endif
	float dotNV = saturate( dot( normal, viewDir ) );
	vec2 fab = texture2D( dfgLUT, vec2( roughness, dotNV ) ).rg;
	#ifdef USE_IRIDESCENCE
		vec3 Fr = mix( specularColor, iridescenceF0, iridescence );
	#else
		vec3 Fr = specularColor;
	#endif
	vec3 FssEss = Fr * fab.x + specularF90 * fab.y;
	float Ess = fab.x + fab.y;
	float Ems = 1.0 - Ess;
	vec3 Favg = Fr + ( 1.0 - Fr ) * 0.047619;	vec3 Fms = FssEss * Favg / ( 1.0 - Ems * Favg );
	singleScatter += FssEss;
	multiScatter += Fms * Ems;
}
vec3 BRDF_GGX_Multiscatter( const in vec3 lightDir, const in vec3 viewDir, const in vec3 normal, const in PhysicalMaterial material ) {
	vec3 singleScatter = BRDF_GGX( lightDir, viewDir, normal, material );
	float dotNL = saturate( dot( normal, lightDir ) );
	float dotNV = saturate( dot( normal, viewDir ) );
	vec2 dfgV = texture2D( dfgLUT, vec2( material.roughness, dotNV ) ).rg;
	vec2 dfgL = texture2D( dfgLUT, vec2( material.roughness, dotNL ) ).rg;
	vec3 FssEss_V = material.specularColorBlended * dfgV.x + material.specularF90 * dfgV.y;
	vec3 FssEss_L = material.specularColorBlended * dfgL.x + material.specularF90 * dfgL.y;
	float Ess_V = dfgV.x + dfgV.y;
	float Ess_L = dfgL.x + dfgL.y;
	float Ems_V = 1.0 - Ess_V;
	float Ems_L = 1.0 - Ess_L;
	vec3 Favg = material.specularColorBlended + ( 1.0 - material.specularColorBlended ) * 0.047619;
	vec3 Fms = FssEss_V * FssEss_L * Favg / ( 1.0 - Ems_V * Ems_L * Favg + EPSILON );
	float compensationFactor = Ems_V * Ems_L;
	vec3 multiScatter = Fms * compensationFactor;
	return singleScatter + multiScatter;
}
#if NUM_RECT_AREA_LIGHTS > 0
	void RE_Direct_RectArea_Physical( const in RectAreaLight rectAreaLight, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in PhysicalMaterial material, inout ReflectedLight reflectedLight ) {
		vec3 normal = geometryNormal;
		vec3 viewDir = geometryViewDir;
		vec3 position = geometryPosition;
		vec3 lightPos = rectAreaLight.position;
		vec3 halfWidth = rectAreaLight.halfWidth;
		vec3 halfHeight = rectAreaLight.halfHeight;
		vec3 lightColor = rectAreaLight.color;
		float roughness = material.roughness;
		vec3 rectCoords[ 4 ];
		rectCoords[ 0 ] = lightPos + halfWidth - halfHeight;		rectCoords[ 1 ] = lightPos - halfWidth - halfHeight;
		rectCoords[ 2 ] = lightPos - halfWidth + halfHeight;
		rectCoords[ 3 ] = lightPos + halfWidth + halfHeight;
		vec2 uv = LTC_Uv( normal, viewDir, roughness );
		vec4 t1 = texture2D( ltc_1, uv );
		vec4 t2 = texture2D( ltc_2, uv );
		mat3 mInv = mat3(
			vec3( t1.x, 0, t1.y ),
			vec3(    0, 1,    0 ),
			vec3( t1.z, 0, t1.w )
		);
		vec3 fresnel = ( material.specularColorBlended * t2.x + ( material.specularF90 - material.specularColorBlended ) * t2.y );
		reflectedLight.directSpecular += lightColor * fresnel * LTC_Evaluate( normal, viewDir, position, mInv, rectCoords );
		reflectedLight.directDiffuse += lightColor * material.diffuseContribution * LTC_Evaluate( normal, viewDir, position, mat3( 1.0 ), rectCoords );
		#ifdef USE_CLEARCOAT
			vec3 Ncc = geometryClearcoatNormal;
			vec2 uvClearcoat = LTC_Uv( Ncc, viewDir, material.clearcoatRoughness );
			vec4 t1Clearcoat = texture2D( ltc_1, uvClearcoat );
			vec4 t2Clearcoat = texture2D( ltc_2, uvClearcoat );
			mat3 mInvClearcoat = mat3(
				vec3( t1Clearcoat.x, 0, t1Clearcoat.y ),
				vec3(             0, 1,             0 ),
				vec3( t1Clearcoat.z, 0, t1Clearcoat.w )
			);
			vec3 fresnelClearcoat = material.clearcoatF0 * t2Clearcoat.x + ( material.clearcoatF90 - material.clearcoatF0 ) * t2Clearcoat.y;
			clearcoatSpecularDirect += lightColor * fresnelClearcoat * LTC_Evaluate( Ncc, viewDir, position, mInvClearcoat, rectCoords );
		#endif
	}
#endif
void RE_Direct_Physical( const in IncidentLight directLight, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in PhysicalMaterial material, inout ReflectedLight reflectedLight ) {
	float dotNL = saturate( dot( geometryNormal, directLight.direction ) );
	vec3 irradiance = dotNL * directLight.color;
	#ifdef USE_CLEARCOAT
		float dotNLcc = saturate( dot( geometryClearcoatNormal, directLight.direction ) );
		vec3 ccIrradiance = dotNLcc * directLight.color;
		clearcoatSpecularDirect += ccIrradiance * BRDF_GGX_Clearcoat( directLight.direction, geometryViewDir, geometryClearcoatNormal, material );
	#endif
	#ifdef USE_SHEEN
 
 		sheenSpecularDirect += irradiance * BRDF_Sheen( directLight.direction, geometryViewDir, geometryNormal, material.sheenColor, material.sheenRoughness );
 
 		float sheenAlbedoV = IBLSheenBRDF( geometryNormal, geometryViewDir, material.sheenRoughness );
 		float sheenAlbedoL = IBLSheenBRDF( geometryNormal, directLight.direction, material.sheenRoughness );
 
 		float sheenEnergyComp = 1.0 - max3( material.sheenColor ) * max( sheenAlbedoV, sheenAlbedoL );
 
 		irradiance *= sheenEnergyComp;
 
 	#endif
	reflectedLight.directSpecular += irradiance * BRDF_GGX_Multiscatter( directLight.direction, geometryViewDir, geometryNormal, material );
	reflectedLight.directDiffuse += irradiance * BRDF_Lambert( material.diffuseContribution );
}
void RE_IndirectDiffuse_Physical( const in vec3 irradiance, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in PhysicalMaterial material, inout ReflectedLight reflectedLight ) {
	vec3 diffuse = irradiance * BRDF_Lambert( material.diffuseContribution );
	#ifdef USE_SHEEN
		float sheenAlbedo = IBLSheenBRDF( geometryNormal, geometryViewDir, material.sheenRoughness );
		float sheenEnergyComp = 1.0 - max3( material.sheenColor ) * sheenAlbedo;
		diffuse *= sheenEnergyComp;
	#endif
	reflectedLight.indirectDiffuse += diffuse;
}
void RE_IndirectSpecular_Physical( const in vec3 radiance, const in vec3 irradiance, const in vec3 clearcoatRadiance, const in vec3 geometryPosition, const in vec3 geometryNormal, const in vec3 geometryViewDir, const in vec3 geometryClearcoatNormal, const in PhysicalMaterial material, inout ReflectedLight reflectedLight) {
	#ifdef USE_CLEARCOAT
		clearcoatSpecularIndirect += clearcoatRadiance * EnvironmentBRDF( geometryClearcoatNormal, geometryViewDir, material.clearcoatF0, material.clearcoatF90, material.clearcoatRoughness );
	#endif
	#ifdef USE_SHEEN
		sheenSpecularIndirect += irradiance * material.sheenColor * IBLSheenBRDF( geometryNormal, geometryViewDir, material.sheenRoughness ) * RECIPROCAL_PI;
 	#endif
	vec3 singleScatteringDielectric = vec3( 0.0 );
	vec3 multiScatteringDielectric = vec3( 0.0 );
	vec3 singleScatteringMetallic = vec3( 0.0 );
	vec3 multiScatteringMetallic = vec3( 0.0 );
	#ifdef USE_IRIDESCENCE
		computeMultiscatteringIridescence( geometryNormal, geometryViewDir, material.specularColor, material.specularF90, material.iridescence, material.iridescenceFresnelDielectric, material.roughness, singleScatteringDielectric, multiScatteringDielectric );
		computeMultiscatteringIridescence( geometryNormal, geometryViewDir, material.diffuseColor, material.specularF90, material.iridescence, material.iridescenceFresnelMetallic, material.roughness, singleScatteringMetallic, multiScatteringMetallic );
	#else
		computeMultiscattering( geometryNormal, geometryViewDir, material.specularColor, material.specularF90, material.roughness, singleScatteringDielectric, multiScatteringDielectric );
		computeMultiscattering( geometryNormal, geometryViewDir, material.diffuseColor, material.specularF90, material.roughness, singleScatteringMetallic, multiScatteringMetallic );
	#endif
	vec3 singleScattering = mix( singleScatteringDielectric, singleScatteringMetallic, material.metalness );
	vec3 multiScattering = mix( multiScatteringDielectric, multiScatteringMetallic, material.metalness );
	vec3 totalScatteringDielectric = singleScatteringDielectric + multiScatteringDielectric;
	vec3 diffuse = material.diffuseContribution * ( 1.0 - totalScatteringDielectric );
	vec3 cosineWeightedIrradiance = irradiance * RECIPROCAL_PI;
	vec3 indirectSpecular = radiance * singleScattering;
	indirectSpecular += multiScattering * cosineWeightedIrradiance;
	vec3 indirectDiffuse = diffuse * cosineWeightedIrradiance;
	#ifdef USE_SHEEN
		float sheenAlbedo = IBLSheenBRDF( geometryNormal, geometryViewDir, material.sheenRoughness );
		float sheenEnergyComp = 1.0 - max3( material.sheenColor ) * sheenAlbedo;
		indirectSpecular *= sheenEnergyComp;
		indirectDiffuse *= sheenEnergyComp;
	#endif
	reflectedLight.indirectSpecular += indirectSpecular;
	reflectedLight.indirectDiffuse += indirectDiffuse;
}
#define RE_Direct				RE_Direct_Physical
#define RE_Direct_RectArea		RE_Direct_RectArea_Physical
#define RE_IndirectDiffuse		RE_IndirectDiffuse_Physical
#define RE_IndirectSpecular		RE_IndirectSpecular_Physical
float computeSpecularOcclusion( const in float dotNV, const in float ambientOcclusion, const in float roughness ) {
	return saturate( pow( dotNV + ambientOcclusion, exp2( - 16.0 * roughness - 1.0 ) ) - 1.0 + ambientOcclusion );
}`,Ku=`
vec3 geometryPosition = - vViewPosition;
vec3 geometryNormal = normal;
vec3 geometryViewDir = ( isOrthographic ) ? vec3( 0, 0, 1 ) : normalize( vViewPosition );
vec3 geometryClearcoatNormal = vec3( 0.0 );
#ifdef USE_CLEARCOAT
	geometryClearcoatNormal = clearcoatNormal;
#endif
#ifdef USE_IRIDESCENCE
	float dotNVi = saturate( dot( normal, geometryViewDir ) );
	if ( material.iridescenceThickness == 0.0 ) {
		material.iridescence = 0.0;
	} else {
		material.iridescence = saturate( material.iridescence );
	}
	if ( material.iridescence > 0.0 ) {
		material.iridescenceFresnelDielectric = evalIridescence( 1.0, material.iridescenceIOR, dotNVi, material.iridescenceThickness, material.specularColor );
		material.iridescenceFresnelMetallic = evalIridescence( 1.0, material.iridescenceIOR, dotNVi, material.iridescenceThickness, material.diffuseColor );
		material.iridescenceFresnel = mix( material.iridescenceFresnelDielectric, material.iridescenceFresnelMetallic, material.metalness );
		material.iridescenceF0 = Schlick_to_F0( material.iridescenceFresnel, 1.0, dotNVi );
	}
#endif
IncidentLight directLight;
#if ( NUM_POINT_LIGHTS > 0 ) && defined( RE_Direct )
	PointLight pointLight;
	#if defined( USE_SHADOWMAP ) && NUM_POINT_LIGHT_SHADOWS > 0
	PointLightShadow pointLightShadow;
	#endif
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_POINT_LIGHTS; i ++ ) {
		pointLight = pointLights[ i ];
		getPointLightInfo( pointLight, geometryPosition, directLight );
		#if defined( USE_SHADOWMAP ) && ( UNROLLED_LOOP_INDEX < NUM_POINT_LIGHT_SHADOWS ) && ( defined( SHADOWMAP_TYPE_PCF ) || defined( SHADOWMAP_TYPE_BASIC ) )
		pointLightShadow = pointLightShadows[ i ];
		directLight.color *= ( directLight.visible && receiveShadow ) ? getPointShadow( pointShadowMap[ i ], pointLightShadow.shadowMapSize, pointLightShadow.shadowIntensity, pointLightShadow.shadowBias, pointLightShadow.shadowRadius, vPointShadowCoord[ i ], pointLightShadow.shadowCameraNear, pointLightShadow.shadowCameraFar ) : 1.0;
		#endif
		RE_Direct( directLight, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
	}
	#pragma unroll_loop_end
#endif
#if ( NUM_SPOT_LIGHTS > 0 ) && defined( RE_Direct )
	SpotLight spotLight;
	vec4 spotColor;
	vec3 spotLightCoord;
	bool inSpotLightMap;
	#if defined( USE_SHADOWMAP ) && NUM_SPOT_LIGHT_SHADOWS > 0
	SpotLightShadow spotLightShadow;
	#endif
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_SPOT_LIGHTS; i ++ ) {
		spotLight = spotLights[ i ];
		getSpotLightInfo( spotLight, geometryPosition, directLight );
		#if ( UNROLLED_LOOP_INDEX < NUM_SPOT_LIGHT_SHADOWS_WITH_MAPS )
		#define SPOT_LIGHT_MAP_INDEX UNROLLED_LOOP_INDEX
		#elif ( UNROLLED_LOOP_INDEX < NUM_SPOT_LIGHT_SHADOWS )
		#define SPOT_LIGHT_MAP_INDEX NUM_SPOT_LIGHT_MAPS
		#else
		#define SPOT_LIGHT_MAP_INDEX ( UNROLLED_LOOP_INDEX - NUM_SPOT_LIGHT_SHADOWS + NUM_SPOT_LIGHT_SHADOWS_WITH_MAPS )
		#endif
		#if ( SPOT_LIGHT_MAP_INDEX < NUM_SPOT_LIGHT_MAPS )
			spotLightCoord = vSpotLightCoord[ i ].xyz / vSpotLightCoord[ i ].w;
			inSpotLightMap = all( lessThan( abs( spotLightCoord * 2. - 1. ), vec3( 1.0 ) ) );
			spotColor = texture2D( spotLightMap[ SPOT_LIGHT_MAP_INDEX ], spotLightCoord.xy );
			directLight.color = inSpotLightMap ? directLight.color * spotColor.rgb : directLight.color;
		#endif
		#undef SPOT_LIGHT_MAP_INDEX
		#if defined( USE_SHADOWMAP ) && ( UNROLLED_LOOP_INDEX < NUM_SPOT_LIGHT_SHADOWS )
		spotLightShadow = spotLightShadows[ i ];
		directLight.color *= ( directLight.visible && receiveShadow ) ? getShadow( spotShadowMap[ i ], spotLightShadow.shadowMapSize, spotLightShadow.shadowIntensity, spotLightShadow.shadowBias, spotLightShadow.shadowRadius, vSpotLightCoord[ i ] ) : 1.0;
		#endif
		RE_Direct( directLight, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
	}
	#pragma unroll_loop_end
#endif
#if ( NUM_DIR_LIGHTS > 0 ) && defined( RE_Direct )
	DirectionalLight directionalLight;
	#if defined( USE_SHADOWMAP ) && NUM_DIR_LIGHT_SHADOWS > 0
	DirectionalLightShadow directionalLightShadow;
	#endif
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_DIR_LIGHTS; i ++ ) {
		directionalLight = directionalLights[ i ];
		getDirectionalLightInfo( directionalLight, directLight );
		#if defined( USE_SHADOWMAP ) && ( UNROLLED_LOOP_INDEX < NUM_DIR_LIGHT_SHADOWS )
		directionalLightShadow = directionalLightShadows[ i ];
		directLight.color *= ( directLight.visible && receiveShadow ) ? getShadow( directionalShadowMap[ i ], directionalLightShadow.shadowMapSize, directionalLightShadow.shadowIntensity, directionalLightShadow.shadowBias, directionalLightShadow.shadowRadius, vDirectionalShadowCoord[ i ] ) : 1.0;
		#endif
		RE_Direct( directLight, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
	}
	#pragma unroll_loop_end
#endif
#if ( NUM_RECT_AREA_LIGHTS > 0 ) && defined( RE_Direct_RectArea )
	RectAreaLight rectAreaLight;
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_RECT_AREA_LIGHTS; i ++ ) {
		rectAreaLight = rectAreaLights[ i ];
		RE_Direct_RectArea( rectAreaLight, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
	}
	#pragma unroll_loop_end
#endif
#if defined( RE_IndirectDiffuse )
	vec3 iblIrradiance = vec3( 0.0 );
	vec3 irradiance = getAmbientLightIrradiance( ambientLightColor );
	#if defined( USE_LIGHT_PROBES )
		irradiance += getLightProbeIrradiance( lightProbe, geometryNormal );
	#endif
	#if ( NUM_HEMI_LIGHTS > 0 )
		#pragma unroll_loop_start
		for ( int i = 0; i < NUM_HEMI_LIGHTS; i ++ ) {
			irradiance += getHemisphereLightIrradiance( hemisphereLights[ i ], geometryNormal );
		}
		#pragma unroll_loop_end
	#endif
	#ifdef USE_LIGHT_PROBES_GRID
		vec3 probeWorldPos = ( ( vec4( geometryPosition, 1.0 ) - viewMatrix[ 3 ] ) * viewMatrix ).xyz;
		vec3 probeWorldNormal = inverseTransformDirection( geometryNormal, viewMatrix );
		irradiance += getLightProbeGridIrradiance( probeWorldPos, probeWorldNormal );
	#endif
#endif
#if defined( RE_IndirectSpecular )
	vec3 radiance = vec3( 0.0 );
	vec3 clearcoatRadiance = vec3( 0.0 );
#endif`,Ju=`#if defined( RE_IndirectDiffuse )
	#ifdef USE_LIGHTMAP
		vec4 lightMapTexel = texture2D( lightMap, vLightMapUv );
		vec3 lightMapIrradiance = lightMapTexel.rgb * lightMapIntensity;
		irradiance += lightMapIrradiance;
	#endif
	#if defined( USE_ENVMAP ) && defined( ENVMAP_TYPE_CUBE_UV )
		#if defined( STANDARD ) || defined( LAMBERT ) || defined( PHONG )
			iblIrradiance += getIBLIrradiance( geometryNormal );
		#endif
	#endif
#endif
#if defined( USE_ENVMAP ) && defined( RE_IndirectSpecular )
	#ifdef USE_ANISOTROPY
		radiance += getIBLAnisotropyRadiance( geometryViewDir, geometryNormal, material.roughness, material.anisotropyB, material.anisotropy );
	#else
		radiance += getIBLRadiance( geometryViewDir, geometryNormal, material.roughness );
	#endif
	#ifdef USE_CLEARCOAT
		clearcoatRadiance += getIBLRadiance( geometryViewDir, geometryClearcoatNormal, material.clearcoatRoughness );
	#endif
#endif`,Qu=`#if defined( RE_IndirectDiffuse )
	#if defined( LAMBERT ) || defined( PHONG )
		irradiance += iblIrradiance;
	#endif
	RE_IndirectDiffuse( irradiance, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
#endif
#if defined( RE_IndirectSpecular )
	RE_IndirectSpecular( radiance, iblIrradiance, clearcoatRadiance, geometryPosition, geometryNormal, geometryViewDir, geometryClearcoatNormal, material, reflectedLight );
#endif`,ed=`#ifdef USE_LIGHT_PROBES_GRID
uniform highp sampler3D probesSH;
uniform vec3 probesMin;
uniform vec3 probesMax;
uniform vec3 probesResolution;
vec3 getLightProbeGridIrradiance( vec3 worldPos, vec3 worldNormal ) {
	vec3 res = probesResolution;
	vec3 gridRange = probesMax - probesMin;
	vec3 resMinusOne = res - 1.0;
	vec3 probeSpacing = gridRange / resMinusOne;
	vec3 samplePos = worldPos + worldNormal * probeSpacing * 0.5;
	vec3 uvw = clamp( ( samplePos - probesMin ) / gridRange, 0.0, 1.0 );
	uvw = uvw * resMinusOne / res + 0.5 / res;
	float nz          = res.z;
	float paddedSlices = nz + 2.0;
	float atlasDepth  = 7.0 * paddedSlices;
	float uvZBase     = uvw.z * nz + 1.0;
	vec4 s0 = texture( probesSH, vec3( uvw.xy, ( uvZBase                       ) / atlasDepth ) );
	vec4 s1 = texture( probesSH, vec3( uvw.xy, ( uvZBase +       paddedSlices   ) / atlasDepth ) );
	vec4 s2 = texture( probesSH, vec3( uvw.xy, ( uvZBase + 2.0 * paddedSlices   ) / atlasDepth ) );
	vec4 s3 = texture( probesSH, vec3( uvw.xy, ( uvZBase + 3.0 * paddedSlices   ) / atlasDepth ) );
	vec4 s4 = texture( probesSH, vec3( uvw.xy, ( uvZBase + 4.0 * paddedSlices   ) / atlasDepth ) );
	vec4 s5 = texture( probesSH, vec3( uvw.xy, ( uvZBase + 5.0 * paddedSlices   ) / atlasDepth ) );
	vec4 s6 = texture( probesSH, vec3( uvw.xy, ( uvZBase + 6.0 * paddedSlices   ) / atlasDepth ) );
	vec3 c0 = s0.xyz;
	vec3 c1 = vec3( s0.w, s1.xy );
	vec3 c2 = vec3( s1.zw, s2.x );
	vec3 c3 = s2.yzw;
	vec3 c4 = s3.xyz;
	vec3 c5 = vec3( s3.w, s4.xy );
	vec3 c6 = vec3( s4.zw, s5.x );
	vec3 c7 = s5.yzw;
	vec3 c8 = s6.xyz;
	float x = worldNormal.x, y = worldNormal.y, z = worldNormal.z;
	vec3 result = c0 * 0.886227;
	result += c1 * 2.0 * 0.511664 * y;
	result += c2 * 2.0 * 0.511664 * z;
	result += c3 * 2.0 * 0.511664 * x;
	result += c4 * 2.0 * 0.429043 * x * y;
	result += c5 * 2.0 * 0.429043 * y * z;
	result += c6 * ( 0.743125 * z * z - 0.247708 );
	result += c7 * 2.0 * 0.429043 * x * z;
	result += c8 * 0.429043 * ( x * x - y * y );
	return max( result, vec3( 0.0 ) );
}
#endif`,td=`#if defined( USE_LOGARITHMIC_DEPTH_BUFFER )
	gl_FragDepth = vIsPerspective == 0.0 ? gl_FragCoord.z : log2( vFragDepth ) * logDepthBufFC * 0.5;
#endif`,id=`#if defined( USE_LOGARITHMIC_DEPTH_BUFFER )
	uniform float logDepthBufFC;
	varying float vFragDepth;
	varying float vIsPerspective;
#endif`,nd=`#ifdef USE_LOGARITHMIC_DEPTH_BUFFER
	varying float vFragDepth;
	varying float vIsPerspective;
#endif`,sd=`#ifdef USE_LOGARITHMIC_DEPTH_BUFFER
	vFragDepth = 1.0 + gl_Position.w;
	vIsPerspective = float( isPerspectiveMatrix( projectionMatrix ) );
#endif`,rd=`#ifdef USE_MAP
	vec4 sampledDiffuseColor = texture2D( map, vMapUv );
	#ifdef DECODE_VIDEO_TEXTURE
		sampledDiffuseColor = sRGBTransferEOTF( sampledDiffuseColor );
	#endif
	diffuseColor *= sampledDiffuseColor;
#endif`,ad=`#ifdef USE_MAP
	uniform sampler2D map;
#endif`,od=`#if defined( USE_MAP ) || defined( USE_ALPHAMAP )
	#if defined( USE_POINTS_UV )
		vec2 uv = vUv;
	#else
		vec2 uv = ( uvTransform * vec3( gl_PointCoord.x, 1.0 - gl_PointCoord.y, 1 ) ).xy;
	#endif
#endif
#ifdef USE_MAP
	diffuseColor *= texture2D( map, uv );
#endif
#ifdef USE_ALPHAMAP
	diffuseColor.a *= texture2D( alphaMap, uv ).g;
#endif`,ld=`#if defined( USE_POINTS_UV )
	varying vec2 vUv;
#else
	#if defined( USE_MAP ) || defined( USE_ALPHAMAP )
		uniform mat3 uvTransform;
	#endif
#endif
#ifdef USE_MAP
	uniform sampler2D map;
#endif
#ifdef USE_ALPHAMAP
	uniform sampler2D alphaMap;
#endif`,cd=`float metalnessFactor = metalness;
#ifdef USE_METALNESSMAP
	vec4 texelMetalness = texture2D( metalnessMap, vMetalnessMapUv );
	metalnessFactor *= texelMetalness.b;
#endif`,hd=`#ifdef USE_METALNESSMAP
	uniform sampler2D metalnessMap;
#endif`,ud=`#ifdef USE_INSTANCING_MORPH
	float morphTargetInfluences[ MORPHTARGETS_COUNT ];
	float morphTargetBaseInfluence = texelFetch( morphTexture, ivec2( 0, gl_InstanceID ), 0 ).r;
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		morphTargetInfluences[i] =  texelFetch( morphTexture, ivec2( i + 1, gl_InstanceID ), 0 ).r;
	}
#endif`,dd=`#if defined( USE_MORPHCOLORS )
	vColor *= morphTargetBaseInfluence;
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		#if defined( USE_COLOR_ALPHA )
			if ( morphTargetInfluences[ i ] != 0.0 ) vColor += getMorph( gl_VertexID, i, 2 ) * morphTargetInfluences[ i ];
		#elif defined( USE_COLOR )
			if ( morphTargetInfluences[ i ] != 0.0 ) vColor += getMorph( gl_VertexID, i, 2 ).rgb * morphTargetInfluences[ i ];
		#endif
	}
#endif`,fd=`#ifdef USE_MORPHNORMALS
	objectNormal *= morphTargetBaseInfluence;
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		if ( morphTargetInfluences[ i ] != 0.0 ) objectNormal += getMorph( gl_VertexID, i, 1 ).xyz * morphTargetInfluences[ i ];
	}
#endif`,pd=`#ifdef USE_MORPHTARGETS
	#ifndef USE_INSTANCING_MORPH
		uniform float morphTargetBaseInfluence;
		uniform float morphTargetInfluences[ MORPHTARGETS_COUNT ];
	#endif
	uniform sampler2DArray morphTargetsTexture;
	uniform ivec2 morphTargetsTextureSize;
	vec4 getMorph( const in int vertexIndex, const in int morphTargetIndex, const in int offset ) {
		int texelIndex = vertexIndex * MORPHTARGETS_TEXTURE_STRIDE + offset;
		int y = texelIndex / morphTargetsTextureSize.x;
		int x = texelIndex - y * morphTargetsTextureSize.x;
		ivec3 morphUV = ivec3( x, y, morphTargetIndex );
		return texelFetch( morphTargetsTexture, morphUV, 0 );
	}
#endif`,md=`#ifdef USE_MORPHTARGETS
	transformed *= morphTargetBaseInfluence;
	for ( int i = 0; i < MORPHTARGETS_COUNT; i ++ ) {
		if ( morphTargetInfluences[ i ] != 0.0 ) transformed += getMorph( gl_VertexID, i, 0 ).xyz * morphTargetInfluences[ i ];
	}
#endif`,gd=`float faceDirection = gl_FrontFacing ? 1.0 : - 1.0;
#ifdef FLAT_SHADED
	vec3 fdx = dFdx( vViewPosition );
	vec3 fdy = dFdy( vViewPosition );
	vec3 normal = normalize( cross( fdx, fdy ) );
#else
	vec3 normal = normalize( vNormal );
	#ifdef DOUBLE_SIDED
		normal *= faceDirection;
	#endif
#endif
#if defined( USE_NORMALMAP_TANGENTSPACE ) || defined( USE_CLEARCOAT_NORMALMAP ) || defined( USE_ANISOTROPY )
	#ifdef USE_TANGENT
		mat3 tbn = mat3( normalize( vTangent ), normalize( vBitangent ), normal );
	#else
		mat3 tbn = getTangentFrame( - vViewPosition, normal,
		#if defined( USE_NORMALMAP )
			vNormalMapUv
		#elif defined( USE_CLEARCOAT_NORMALMAP )
			vClearcoatNormalMapUv
		#else
			vUv
		#endif
		);
	#endif
	#if defined( DOUBLE_SIDED ) && ! defined( FLAT_SHADED )
		tbn[0] *= faceDirection;
		tbn[1] *= faceDirection;
	#endif
#endif
#ifdef USE_CLEARCOAT_NORMALMAP
	#ifdef USE_TANGENT
		mat3 tbn2 = mat3( normalize( vTangent ), normalize( vBitangent ), normal );
	#else
		mat3 tbn2 = getTangentFrame( - vViewPosition, normal, vClearcoatNormalMapUv );
	#endif
	#if defined( DOUBLE_SIDED ) && ! defined( FLAT_SHADED )
		tbn2[0] *= faceDirection;
		tbn2[1] *= faceDirection;
	#endif
#endif
vec3 nonPerturbedNormal = normal;`,_d=`#ifdef USE_NORMALMAP_OBJECTSPACE
	normal = texture2D( normalMap, vNormalMapUv ).xyz * 2.0 - 1.0;
	#ifdef FLIP_SIDED
		normal = - normal;
	#endif
	#ifdef DOUBLE_SIDED
		normal = normal * faceDirection;
	#endif
	normal = normalize( normalMatrix * normal );
#elif defined( USE_NORMALMAP_TANGENTSPACE )
	vec3 mapN = texture2D( normalMap, vNormalMapUv ).xyz * 2.0 - 1.0;
	#if defined( USE_PACKED_NORMALMAP )
		mapN = vec3( mapN.xy, sqrt( saturate( 1.0 - dot( mapN.xy, mapN.xy ) ) ) );
	#endif
	mapN.xy *= normalScale;
	normal = normalize( tbn * mapN );
#elif defined( USE_BUMPMAP )
	normal = perturbNormalArb( - vViewPosition, normal, dHdxy_fwd(), faceDirection );
#endif`,vd=`#ifndef FLAT_SHADED
	varying vec3 vNormal;
	#ifdef USE_TANGENT
		varying vec3 vTangent;
		varying vec3 vBitangent;
	#endif
#endif`,xd=`#ifndef FLAT_SHADED
	varying vec3 vNormal;
	#ifdef USE_TANGENT
		varying vec3 vTangent;
		varying vec3 vBitangent;
	#endif
#endif`,bd=`#ifndef FLAT_SHADED
	vNormal = normalize( transformedNormal );
	#ifdef USE_TANGENT
		vTangent = normalize( transformedTangent );
		vBitangent = normalize( cross( vNormal, vTangent ) * tangent.w );
	#endif
#endif`,Sd=`#ifdef USE_NORMALMAP
	uniform sampler2D normalMap;
	uniform vec2 normalScale;
#endif
#ifdef USE_NORMALMAP_OBJECTSPACE
	uniform mat3 normalMatrix;
#endif
#if ! defined ( USE_TANGENT ) && ( defined ( USE_NORMALMAP_TANGENTSPACE ) || defined ( USE_CLEARCOAT_NORMALMAP ) || defined( USE_ANISOTROPY ) )
	mat3 getTangentFrame( vec3 eye_pos, vec3 surf_norm, vec2 uv ) {
		vec3 q0 = dFdx( eye_pos.xyz );
		vec3 q1 = dFdy( eye_pos.xyz );
		vec2 st0 = dFdx( uv.st );
		vec2 st1 = dFdy( uv.st );
		vec3 N = surf_norm;
		vec3 q1perp = cross( q1, N );
		vec3 q0perp = cross( N, q0 );
		vec3 T = q1perp * st0.x + q0perp * st1.x;
		vec3 B = q1perp * st0.y + q0perp * st1.y;
		float det = max( dot( T, T ), dot( B, B ) );
		float scale = ( det == 0.0 ) ? 0.0 : inversesqrt( det );
		return mat3( T * scale, B * scale, N );
	}
#endif`,Md=`#ifdef USE_CLEARCOAT
	vec3 clearcoatNormal = nonPerturbedNormal;
#endif`,yd=`#ifdef USE_CLEARCOAT_NORMALMAP
	vec3 clearcoatMapN = texture2D( clearcoatNormalMap, vClearcoatNormalMapUv ).xyz * 2.0 - 1.0;
	clearcoatMapN.xy *= clearcoatNormalScale;
	clearcoatNormal = normalize( tbn2 * clearcoatMapN );
#endif`,Ed=`#ifdef USE_CLEARCOATMAP
	uniform sampler2D clearcoatMap;
#endif
#ifdef USE_CLEARCOAT_NORMALMAP
	uniform sampler2D clearcoatNormalMap;
	uniform vec2 clearcoatNormalScale;
#endif
#ifdef USE_CLEARCOAT_ROUGHNESSMAP
	uniform sampler2D clearcoatRoughnessMap;
#endif`,wd=`#ifdef USE_IRIDESCENCEMAP
	uniform sampler2D iridescenceMap;
#endif
#ifdef USE_IRIDESCENCE_THICKNESSMAP
	uniform sampler2D iridescenceThicknessMap;
#endif`,Td=`#ifdef OPAQUE
diffuseColor.a = 1.0;
#endif
#ifdef USE_TRANSMISSION
diffuseColor.a *= material.transmissionAlpha;
#endif
gl_FragColor = vec4( outgoingLight, diffuseColor.a );`,Ad=`vec3 packNormalToRGB( const in vec3 normal ) {
	return normalize( normal ) * 0.5 + 0.5;
}
vec3 unpackRGBToNormal( const in vec3 rgb ) {
	return 2.0 * rgb.xyz - 1.0;
}
const float PackUpscale = 256. / 255.;const float UnpackDownscale = 255. / 256.;const float ShiftRight8 = 1. / 256.;
const float Inv255 = 1. / 255.;
const vec4 PackFactors = vec4( 1.0, 256.0, 256.0 * 256.0, 256.0 * 256.0 * 256.0 );
const vec2 UnpackFactors2 = vec2( UnpackDownscale, 1.0 / PackFactors.g );
const vec3 UnpackFactors3 = vec3( UnpackDownscale / PackFactors.rg, 1.0 / PackFactors.b );
const vec4 UnpackFactors4 = vec4( UnpackDownscale / PackFactors.rgb, 1.0 / PackFactors.a );
vec4 packDepthToRGBA( const in float v ) {
	if( v <= 0.0 )
		return vec4( 0., 0., 0., 0. );
	if( v >= 1.0 )
		return vec4( 1., 1., 1., 1. );
	float vuf;
	float af = modf( v * PackFactors.a, vuf );
	float bf = modf( vuf * ShiftRight8, vuf );
	float gf = modf( vuf * ShiftRight8, vuf );
	return vec4( vuf * Inv255, gf * PackUpscale, bf * PackUpscale, af );
}
vec3 packDepthToRGB( const in float v ) {
	if( v <= 0.0 )
		return vec3( 0., 0., 0. );
	if( v >= 1.0 )
		return vec3( 1., 1., 1. );
	float vuf;
	float bf = modf( v * PackFactors.b, vuf );
	float gf = modf( vuf * ShiftRight8, vuf );
	return vec3( vuf * Inv255, gf * PackUpscale, bf );
}
vec2 packDepthToRG( const in float v ) {
	if( v <= 0.0 )
		return vec2( 0., 0. );
	if( v >= 1.0 )
		return vec2( 1., 1. );
	float vuf;
	float gf = modf( v * 256., vuf );
	return vec2( vuf * Inv255, gf );
}
float unpackRGBAToDepth( const in vec4 v ) {
	return dot( v, UnpackFactors4 );
}
float unpackRGBToDepth( const in vec3 v ) {
	return dot( v, UnpackFactors3 );
}
float unpackRGToDepth( const in vec2 v ) {
	return v.r * UnpackFactors2.r + v.g * UnpackFactors2.g;
}
vec4 pack2HalfToRGBA( const in vec2 v ) {
	vec4 r = vec4( v.x, fract( v.x * 255.0 ), v.y, fract( v.y * 255.0 ) );
	return vec4( r.x - r.y / 255.0, r.y, r.z - r.w / 255.0, r.w );
}
vec2 unpackRGBATo2Half( const in vec4 v ) {
	return vec2( v.x + ( v.y / 255.0 ), v.z + ( v.w / 255.0 ) );
}
float viewZToOrthographicDepth( const in float viewZ, const in float near, const in float far ) {
	return ( viewZ + near ) / ( near - far );
}
float orthographicDepthToViewZ( const in float depth, const in float near, const in float far ) {
	#ifdef USE_REVERSED_DEPTH_BUFFER
	
		return depth * ( far - near ) - far;
	#else
		return depth * ( near - far ) - near;
	#endif
}
float viewZToPerspectiveDepth( const in float viewZ, const in float near, const in float far ) {
	return ( ( near + viewZ ) * far ) / ( ( far - near ) * viewZ );
}
float perspectiveDepthToViewZ( const in float depth, const in float near, const in float far ) {
	
	#ifdef USE_REVERSED_DEPTH_BUFFER
		return ( near * far ) / ( ( near - far ) * depth - near );
	#else
		return ( near * far ) / ( ( far - near ) * depth - far );
	#endif
}`,Cd=`#ifdef PREMULTIPLIED_ALPHA
	gl_FragColor.rgb *= gl_FragColor.a;
#endif`,Rd=`vec4 mvPosition = vec4( transformed, 1.0 );
#ifdef USE_BATCHING
	mvPosition = batchingMatrix * mvPosition;
#endif
#ifdef USE_INSTANCING
	mvPosition = instanceMatrix * mvPosition;
#endif
mvPosition = modelViewMatrix * mvPosition;
gl_Position = projectionMatrix * mvPosition;`,Pd=`#ifdef DITHERING
	gl_FragColor.rgb = dithering( gl_FragColor.rgb );
#endif`,Ld=`#ifdef DITHERING
	vec3 dithering( vec3 color ) {
		float grid_position = rand( gl_FragCoord.xy );
		vec3 dither_shift_RGB = vec3( 0.25 / 255.0, -0.25 / 255.0, 0.25 / 255.0 );
		dither_shift_RGB = mix( 2.0 * dither_shift_RGB, -2.0 * dither_shift_RGB, grid_position );
		return color + dither_shift_RGB;
	}
#endif`,Dd=`float roughnessFactor = roughness;
#ifdef USE_ROUGHNESSMAP
	vec4 texelRoughness = texture2D( roughnessMap, vRoughnessMapUv );
	roughnessFactor *= texelRoughness.g;
#endif`,Id=`#ifdef USE_ROUGHNESSMAP
	uniform sampler2D roughnessMap;
#endif`,Nd=`#if NUM_SPOT_LIGHT_COORDS > 0
	varying vec4 vSpotLightCoord[ NUM_SPOT_LIGHT_COORDS ];
#endif
#if NUM_SPOT_LIGHT_MAPS > 0
	uniform sampler2D spotLightMap[ NUM_SPOT_LIGHT_MAPS ];
#endif
#ifdef USE_SHADOWMAP
	#if NUM_DIR_LIGHT_SHADOWS > 0
		#if defined( SHADOWMAP_TYPE_PCF )
			uniform sampler2DShadow directionalShadowMap[ NUM_DIR_LIGHT_SHADOWS ];
		#else
			uniform sampler2D directionalShadowMap[ NUM_DIR_LIGHT_SHADOWS ];
		#endif
		varying vec4 vDirectionalShadowCoord[ NUM_DIR_LIGHT_SHADOWS ];
		struct DirectionalLightShadow {
			float shadowIntensity;
			float shadowBias;
			float shadowNormalBias;
			float shadowRadius;
			vec2 shadowMapSize;
		};
		uniform DirectionalLightShadow directionalLightShadows[ NUM_DIR_LIGHT_SHADOWS ];
	#endif
	#if NUM_SPOT_LIGHT_SHADOWS > 0
		#if defined( SHADOWMAP_TYPE_PCF )
			uniform sampler2DShadow spotShadowMap[ NUM_SPOT_LIGHT_SHADOWS ];
		#else
			uniform sampler2D spotShadowMap[ NUM_SPOT_LIGHT_SHADOWS ];
		#endif
		struct SpotLightShadow {
			float shadowIntensity;
			float shadowBias;
			float shadowNormalBias;
			float shadowRadius;
			vec2 shadowMapSize;
		};
		uniform SpotLightShadow spotLightShadows[ NUM_SPOT_LIGHT_SHADOWS ];
	#endif
	#if NUM_POINT_LIGHT_SHADOWS > 0
		#if defined( SHADOWMAP_TYPE_PCF )
			uniform samplerCubeShadow pointShadowMap[ NUM_POINT_LIGHT_SHADOWS ];
		#elif defined( SHADOWMAP_TYPE_BASIC )
			uniform samplerCube pointShadowMap[ NUM_POINT_LIGHT_SHADOWS ];
		#endif
		varying vec4 vPointShadowCoord[ NUM_POINT_LIGHT_SHADOWS ];
		struct PointLightShadow {
			float shadowIntensity;
			float shadowBias;
			float shadowNormalBias;
			float shadowRadius;
			vec2 shadowMapSize;
			float shadowCameraNear;
			float shadowCameraFar;
		};
		uniform PointLightShadow pointLightShadows[ NUM_POINT_LIGHT_SHADOWS ];
	#endif
	#if defined( SHADOWMAP_TYPE_PCF )
		float interleavedGradientNoise( vec2 position ) {
			return fract( 52.9829189 * fract( dot( position, vec2( 0.06711056, 0.00583715 ) ) ) );
		}
		vec2 vogelDiskSample( int sampleIndex, int samplesCount, float phi ) {
			const float goldenAngle = 2.399963229728653;
			float r = sqrt( ( float( sampleIndex ) + 0.5 ) / float( samplesCount ) );
			float theta = float( sampleIndex ) * goldenAngle + phi;
			return vec2( cos( theta ), sin( theta ) ) * r;
		}
	#endif
	#if defined( SHADOWMAP_TYPE_PCF )
		float getShadow( sampler2DShadow shadowMap, vec2 shadowMapSize, float shadowIntensity, float shadowBias, float shadowRadius, vec4 shadowCoord ) {
			float shadow = 1.0;
			shadowCoord.xyz /= shadowCoord.w;
			shadowCoord.z += shadowBias;
			bool inFrustum = shadowCoord.x >= 0.0 && shadowCoord.x <= 1.0 && shadowCoord.y >= 0.0 && shadowCoord.y <= 1.0;
			bool frustumTest = inFrustum && shadowCoord.z <= 1.0;
			if ( frustumTest ) {
				vec2 texelSize = vec2( 1.0 ) / shadowMapSize;
				float radius = shadowRadius * texelSize.x;
				float phi = interleavedGradientNoise( gl_FragCoord.xy ) * PI2;
				shadow = (
					texture( shadowMap, vec3( shadowCoord.xy + vogelDiskSample( 0, 5, phi ) * radius, shadowCoord.z ) ) +
					texture( shadowMap, vec3( shadowCoord.xy + vogelDiskSample( 1, 5, phi ) * radius, shadowCoord.z ) ) +
					texture( shadowMap, vec3( shadowCoord.xy + vogelDiskSample( 2, 5, phi ) * radius, shadowCoord.z ) ) +
					texture( shadowMap, vec3( shadowCoord.xy + vogelDiskSample( 3, 5, phi ) * radius, shadowCoord.z ) ) +
					texture( shadowMap, vec3( shadowCoord.xy + vogelDiskSample( 4, 5, phi ) * radius, shadowCoord.z ) )
				) * 0.2;
			}
			return mix( 1.0, shadow, shadowIntensity );
		}
	#elif defined( SHADOWMAP_TYPE_VSM )
		float getShadow( sampler2D shadowMap, vec2 shadowMapSize, float shadowIntensity, float shadowBias, float shadowRadius, vec4 shadowCoord ) {
			float shadow = 1.0;
			shadowCoord.xyz /= shadowCoord.w;
			#ifdef USE_REVERSED_DEPTH_BUFFER
				shadowCoord.z -= shadowBias;
			#else
				shadowCoord.z += shadowBias;
			#endif
			bool inFrustum = shadowCoord.x >= 0.0 && shadowCoord.x <= 1.0 && shadowCoord.y >= 0.0 && shadowCoord.y <= 1.0;
			bool frustumTest = inFrustum && shadowCoord.z <= 1.0;
			if ( frustumTest ) {
				vec2 distribution = texture2D( shadowMap, shadowCoord.xy ).rg;
				float mean = distribution.x;
				float variance = distribution.y * distribution.y;
				#ifdef USE_REVERSED_DEPTH_BUFFER
					float hard_shadow = step( mean, shadowCoord.z );
				#else
					float hard_shadow = step( shadowCoord.z, mean );
				#endif
				
				if ( hard_shadow == 1.0 ) {
					shadow = 1.0;
				} else {
					variance = max( variance, 0.0000001 );
					float d = shadowCoord.z - mean;
					float p_max = variance / ( variance + d * d );
					p_max = clamp( ( p_max - 0.3 ) / 0.65, 0.0, 1.0 );
					shadow = max( hard_shadow, p_max );
				}
			}
			return mix( 1.0, shadow, shadowIntensity );
		}
	#else
		float getShadow( sampler2D shadowMap, vec2 shadowMapSize, float shadowIntensity, float shadowBias, float shadowRadius, vec4 shadowCoord ) {
			float shadow = 1.0;
			shadowCoord.xyz /= shadowCoord.w;
			#ifdef USE_REVERSED_DEPTH_BUFFER
				shadowCoord.z -= shadowBias;
			#else
				shadowCoord.z += shadowBias;
			#endif
			bool inFrustum = shadowCoord.x >= 0.0 && shadowCoord.x <= 1.0 && shadowCoord.y >= 0.0 && shadowCoord.y <= 1.0;
			bool frustumTest = inFrustum && shadowCoord.z <= 1.0;
			if ( frustumTest ) {
				float depth = texture2D( shadowMap, shadowCoord.xy ).r;
				#ifdef USE_REVERSED_DEPTH_BUFFER
					shadow = step( depth, shadowCoord.z );
				#else
					shadow = step( shadowCoord.z, depth );
				#endif
			}
			return mix( 1.0, shadow, shadowIntensity );
		}
	#endif
	#if NUM_POINT_LIGHT_SHADOWS > 0
	#if defined( SHADOWMAP_TYPE_PCF )
	float getPointShadow( samplerCubeShadow shadowMap, vec2 shadowMapSize, float shadowIntensity, float shadowBias, float shadowRadius, vec4 shadowCoord, float shadowCameraNear, float shadowCameraFar ) {
		float shadow = 1.0;
		vec3 lightToPosition = shadowCoord.xyz;
		vec3 bd3D = normalize( lightToPosition );
		vec3 absVec = abs( lightToPosition );
		float viewSpaceZ = max( max( absVec.x, absVec.y ), absVec.z );
		if ( viewSpaceZ - shadowCameraFar <= 0.0 && viewSpaceZ - shadowCameraNear >= 0.0 ) {
			#ifdef USE_REVERSED_DEPTH_BUFFER
				float dp = ( shadowCameraNear * ( shadowCameraFar - viewSpaceZ ) ) / ( viewSpaceZ * ( shadowCameraFar - shadowCameraNear ) );
				dp -= shadowBias;
			#else
				float dp = ( shadowCameraFar * ( viewSpaceZ - shadowCameraNear ) ) / ( viewSpaceZ * ( shadowCameraFar - shadowCameraNear ) );
				dp += shadowBias;
			#endif
			float texelSize = shadowRadius / shadowMapSize.x;
			vec3 absDir = abs( bd3D );
			vec3 tangent = absDir.x > absDir.z ? vec3( 0.0, 1.0, 0.0 ) : vec3( 1.0, 0.0, 0.0 );
			tangent = normalize( cross( bd3D, tangent ) );
			vec3 bitangent = cross( bd3D, tangent );
			float phi = interleavedGradientNoise( gl_FragCoord.xy ) * PI2;
			vec2 sample0 = vogelDiskSample( 0, 5, phi );
			vec2 sample1 = vogelDiskSample( 1, 5, phi );
			vec2 sample2 = vogelDiskSample( 2, 5, phi );
			vec2 sample3 = vogelDiskSample( 3, 5, phi );
			vec2 sample4 = vogelDiskSample( 4, 5, phi );
			shadow = (
				texture( shadowMap, vec4( bd3D + ( tangent * sample0.x + bitangent * sample0.y ) * texelSize, dp ) ) +
				texture( shadowMap, vec4( bd3D + ( tangent * sample1.x + bitangent * sample1.y ) * texelSize, dp ) ) +
				texture( shadowMap, vec4( bd3D + ( tangent * sample2.x + bitangent * sample2.y ) * texelSize, dp ) ) +
				texture( shadowMap, vec4( bd3D + ( tangent * sample3.x + bitangent * sample3.y ) * texelSize, dp ) ) +
				texture( shadowMap, vec4( bd3D + ( tangent * sample4.x + bitangent * sample4.y ) * texelSize, dp ) )
			) * 0.2;
		}
		return mix( 1.0, shadow, shadowIntensity );
	}
	#elif defined( SHADOWMAP_TYPE_BASIC )
	float getPointShadow( samplerCube shadowMap, vec2 shadowMapSize, float shadowIntensity, float shadowBias, float shadowRadius, vec4 shadowCoord, float shadowCameraNear, float shadowCameraFar ) {
		float shadow = 1.0;
		vec3 lightToPosition = shadowCoord.xyz;
		vec3 absVec = abs( lightToPosition );
		float viewSpaceZ = max( max( absVec.x, absVec.y ), absVec.z );
		if ( viewSpaceZ - shadowCameraFar <= 0.0 && viewSpaceZ - shadowCameraNear >= 0.0 ) {
			float dp = ( shadowCameraFar * ( viewSpaceZ - shadowCameraNear ) ) / ( viewSpaceZ * ( shadowCameraFar - shadowCameraNear ) );
			dp += shadowBias;
			vec3 bd3D = normalize( lightToPosition );
			float depth = textureCube( shadowMap, bd3D ).r;
			#ifdef USE_REVERSED_DEPTH_BUFFER
				depth = 1.0 - depth;
			#endif
			shadow = step( dp, depth );
		}
		return mix( 1.0, shadow, shadowIntensity );
	}
	#endif
	#endif
#endif`,Ud=`#if NUM_SPOT_LIGHT_COORDS > 0
	uniform mat4 spotLightMatrix[ NUM_SPOT_LIGHT_COORDS ];
	varying vec4 vSpotLightCoord[ NUM_SPOT_LIGHT_COORDS ];
#endif
#ifdef USE_SHADOWMAP
	#if NUM_DIR_LIGHT_SHADOWS > 0
		uniform mat4 directionalShadowMatrix[ NUM_DIR_LIGHT_SHADOWS ];
		varying vec4 vDirectionalShadowCoord[ NUM_DIR_LIGHT_SHADOWS ];
		struct DirectionalLightShadow {
			float shadowIntensity;
			float shadowBias;
			float shadowNormalBias;
			float shadowRadius;
			vec2 shadowMapSize;
		};
		uniform DirectionalLightShadow directionalLightShadows[ NUM_DIR_LIGHT_SHADOWS ];
	#endif
	#if NUM_SPOT_LIGHT_SHADOWS > 0
		struct SpotLightShadow {
			float shadowIntensity;
			float shadowBias;
			float shadowNormalBias;
			float shadowRadius;
			vec2 shadowMapSize;
		};
		uniform SpotLightShadow spotLightShadows[ NUM_SPOT_LIGHT_SHADOWS ];
	#endif
	#if NUM_POINT_LIGHT_SHADOWS > 0
		uniform mat4 pointShadowMatrix[ NUM_POINT_LIGHT_SHADOWS ];
		varying vec4 vPointShadowCoord[ NUM_POINT_LIGHT_SHADOWS ];
		struct PointLightShadow {
			float shadowIntensity;
			float shadowBias;
			float shadowNormalBias;
			float shadowRadius;
			vec2 shadowMapSize;
			float shadowCameraNear;
			float shadowCameraFar;
		};
		uniform PointLightShadow pointLightShadows[ NUM_POINT_LIGHT_SHADOWS ];
	#endif
#endif`,Fd=`#if ( defined( USE_SHADOWMAP ) && ( NUM_DIR_LIGHT_SHADOWS > 0 || NUM_POINT_LIGHT_SHADOWS > 0 ) ) || ( NUM_SPOT_LIGHT_COORDS > 0 )
	#ifdef HAS_NORMAL
		vec3 shadowWorldNormal = inverseTransformDirection( transformedNormal, viewMatrix );
	#else
		vec3 shadowWorldNormal = vec3( 0.0 );
	#endif
	vec4 shadowWorldPosition;
#endif
#if defined( USE_SHADOWMAP )
	#if NUM_DIR_LIGHT_SHADOWS > 0
		#pragma unroll_loop_start
		for ( int i = 0; i < NUM_DIR_LIGHT_SHADOWS; i ++ ) {
			shadowWorldPosition = worldPosition + vec4( shadowWorldNormal * directionalLightShadows[ i ].shadowNormalBias, 0 );
			vDirectionalShadowCoord[ i ] = directionalShadowMatrix[ i ] * shadowWorldPosition;
		}
		#pragma unroll_loop_end
	#endif
	#if NUM_POINT_LIGHT_SHADOWS > 0
		#pragma unroll_loop_start
		for ( int i = 0; i < NUM_POINT_LIGHT_SHADOWS; i ++ ) {
			shadowWorldPosition = worldPosition + vec4( shadowWorldNormal * pointLightShadows[ i ].shadowNormalBias, 0 );
			vPointShadowCoord[ i ] = pointShadowMatrix[ i ] * shadowWorldPosition;
		}
		#pragma unroll_loop_end
	#endif
#endif
#if NUM_SPOT_LIGHT_COORDS > 0
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_SPOT_LIGHT_COORDS; i ++ ) {
		shadowWorldPosition = worldPosition;
		#if ( defined( USE_SHADOWMAP ) && UNROLLED_LOOP_INDEX < NUM_SPOT_LIGHT_SHADOWS )
			shadowWorldPosition.xyz += shadowWorldNormal * spotLightShadows[ i ].shadowNormalBias;
		#endif
		vSpotLightCoord[ i ] = spotLightMatrix[ i ] * shadowWorldPosition;
	}
	#pragma unroll_loop_end
#endif`,Od=`float getShadowMask() {
	float shadow = 1.0;
	#ifdef USE_SHADOWMAP
	#if NUM_DIR_LIGHT_SHADOWS > 0
	DirectionalLightShadow directionalLight;
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_DIR_LIGHT_SHADOWS; i ++ ) {
		directionalLight = directionalLightShadows[ i ];
		shadow *= receiveShadow ? getShadow( directionalShadowMap[ i ], directionalLight.shadowMapSize, directionalLight.shadowIntensity, directionalLight.shadowBias, directionalLight.shadowRadius, vDirectionalShadowCoord[ i ] ) : 1.0;
	}
	#pragma unroll_loop_end
	#endif
	#if NUM_SPOT_LIGHT_SHADOWS > 0
	SpotLightShadow spotLight;
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_SPOT_LIGHT_SHADOWS; i ++ ) {
		spotLight = spotLightShadows[ i ];
		shadow *= receiveShadow ? getShadow( spotShadowMap[ i ], spotLight.shadowMapSize, spotLight.shadowIntensity, spotLight.shadowBias, spotLight.shadowRadius, vSpotLightCoord[ i ] ) : 1.0;
	}
	#pragma unroll_loop_end
	#endif
	#if NUM_POINT_LIGHT_SHADOWS > 0 && ( defined( SHADOWMAP_TYPE_PCF ) || defined( SHADOWMAP_TYPE_BASIC ) )
	PointLightShadow pointLight;
	#pragma unroll_loop_start
	for ( int i = 0; i < NUM_POINT_LIGHT_SHADOWS; i ++ ) {
		pointLight = pointLightShadows[ i ];
		shadow *= receiveShadow ? getPointShadow( pointShadowMap[ i ], pointLight.shadowMapSize, pointLight.shadowIntensity, pointLight.shadowBias, pointLight.shadowRadius, vPointShadowCoord[ i ], pointLight.shadowCameraNear, pointLight.shadowCameraFar ) : 1.0;
	}
	#pragma unroll_loop_end
	#endif
	#endif
	return shadow;
}`,Bd=`#ifdef USE_SKINNING
	mat4 boneMatX = getBoneMatrix( skinIndex.x );
	mat4 boneMatY = getBoneMatrix( skinIndex.y );
	mat4 boneMatZ = getBoneMatrix( skinIndex.z );
	mat4 boneMatW = getBoneMatrix( skinIndex.w );
#endif`,zd=`#ifdef USE_SKINNING
	uniform mat4 bindMatrix;
	uniform mat4 bindMatrixInverse;
	uniform highp sampler2D boneTexture;
	mat4 getBoneMatrix( const in float i ) {
		int size = textureSize( boneTexture, 0 ).x;
		int j = int( i ) * 4;
		int x = j % size;
		int y = j / size;
		vec4 v1 = texelFetch( boneTexture, ivec2( x, y ), 0 );
		vec4 v2 = texelFetch( boneTexture, ivec2( x + 1, y ), 0 );
		vec4 v3 = texelFetch( boneTexture, ivec2( x + 2, y ), 0 );
		vec4 v4 = texelFetch( boneTexture, ivec2( x + 3, y ), 0 );
		return mat4( v1, v2, v3, v4 );
	}
#endif`,kd=`#ifdef USE_SKINNING
	vec4 skinVertex = bindMatrix * vec4( transformed, 1.0 );
	vec4 skinned = vec4( 0.0 );
	skinned += boneMatX * skinVertex * skinWeight.x;
	skinned += boneMatY * skinVertex * skinWeight.y;
	skinned += boneMatZ * skinVertex * skinWeight.z;
	skinned += boneMatW * skinVertex * skinWeight.w;
	transformed = ( bindMatrixInverse * skinned ).xyz;
#endif`,Vd=`#ifdef USE_SKINNING
	mat4 skinMatrix = mat4( 0.0 );
	skinMatrix += skinWeight.x * boneMatX;
	skinMatrix += skinWeight.y * boneMatY;
	skinMatrix += skinWeight.z * boneMatZ;
	skinMatrix += skinWeight.w * boneMatW;
	skinMatrix = bindMatrixInverse * skinMatrix * bindMatrix;
	objectNormal = vec4( skinMatrix * vec4( objectNormal, 0.0 ) ).xyz;
	#ifdef USE_TANGENT
		objectTangent = vec4( skinMatrix * vec4( objectTangent, 0.0 ) ).xyz;
	#endif
#endif`,Gd=`float specularStrength;
#ifdef USE_SPECULARMAP
	vec4 texelSpecular = texture2D( specularMap, vSpecularMapUv );
	specularStrength = texelSpecular.r;
#else
	specularStrength = 1.0;
#endif`,Hd=`#ifdef USE_SPECULARMAP
	uniform sampler2D specularMap;
#endif`,Wd=`#if defined( TONE_MAPPING )
	gl_FragColor.rgb = toneMapping( gl_FragColor.rgb );
#endif`,Xd=`#ifndef saturate
#define saturate( a ) clamp( a, 0.0, 1.0 )
#endif
uniform float toneMappingExposure;
vec3 LinearToneMapping( vec3 color ) {
	return saturate( toneMappingExposure * color );
}
vec3 ReinhardToneMapping( vec3 color ) {
	color *= toneMappingExposure;
	return saturate( color / ( vec3( 1.0 ) + color ) );
}
vec3 CineonToneMapping( vec3 color ) {
	color *= toneMappingExposure;
	color = max( vec3( 0.0 ), color - 0.004 );
	return pow( ( color * ( 6.2 * color + 0.5 ) ) / ( color * ( 6.2 * color + 1.7 ) + 0.06 ), vec3( 2.2 ) );
}
vec3 RRTAndODTFit( vec3 v ) {
	vec3 a = v * ( v + 0.0245786 ) - 0.000090537;
	vec3 b = v * ( 0.983729 * v + 0.4329510 ) + 0.238081;
	return a / b;
}
vec3 ACESFilmicToneMapping( vec3 color ) {
	const mat3 ACESInputMat = mat3(
		vec3( 0.59719, 0.07600, 0.02840 ),		vec3( 0.35458, 0.90834, 0.13383 ),
		vec3( 0.04823, 0.01566, 0.83777 )
	);
	const mat3 ACESOutputMat = mat3(
		vec3(  1.60475, -0.10208, -0.00327 ),		vec3( -0.53108,  1.10813, -0.07276 ),
		vec3( -0.07367, -0.00605,  1.07602 )
	);
	color *= toneMappingExposure / 0.6;
	color = ACESInputMat * color;
	color = RRTAndODTFit( color );
	color = ACESOutputMat * color;
	return saturate( color );
}
const mat3 LINEAR_REC2020_TO_LINEAR_SRGB = mat3(
	vec3( 1.6605, - 0.1246, - 0.0182 ),
	vec3( - 0.5876, 1.1329, - 0.1006 ),
	vec3( - 0.0728, - 0.0083, 1.1187 )
);
const mat3 LINEAR_SRGB_TO_LINEAR_REC2020 = mat3(
	vec3( 0.6274, 0.0691, 0.0164 ),
	vec3( 0.3293, 0.9195, 0.0880 ),
	vec3( 0.0433, 0.0113, 0.8956 )
);
vec3 agxDefaultContrastApprox( vec3 x ) {
	vec3 x2 = x * x;
	vec3 x4 = x2 * x2;
	return + 15.5 * x4 * x2
		- 40.14 * x4 * x
		+ 31.96 * x4
		- 6.868 * x2 * x
		+ 0.4298 * x2
		+ 0.1191 * x
		- 0.00232;
}
vec3 AgXToneMapping( vec3 color ) {
	const mat3 AgXInsetMatrix = mat3(
		vec3( 0.856627153315983, 0.137318972929847, 0.11189821299995 ),
		vec3( 0.0951212405381588, 0.761241990602591, 0.0767994186031903 ),
		vec3( 0.0482516061458583, 0.101439036467562, 0.811302368396859 )
	);
	const mat3 AgXOutsetMatrix = mat3(
		vec3( 1.1271005818144368, - 0.1413297634984383, - 0.14132976349843826 ),
		vec3( - 0.11060664309660323, 1.157823702216272, - 0.11060664309660294 ),
		vec3( - 0.016493938717834573, - 0.016493938717834257, 1.2519364065950405 )
	);
	const float AgxMinEv = - 12.47393;	const float AgxMaxEv = 4.026069;
	color *= toneMappingExposure;
	color = LINEAR_SRGB_TO_LINEAR_REC2020 * color;
	color = AgXInsetMatrix * color;
	color = max( color, 1e-10 );	color = log2( color );
	color = ( color - AgxMinEv ) / ( AgxMaxEv - AgxMinEv );
	color = clamp( color, 0.0, 1.0 );
	color = agxDefaultContrastApprox( color );
	color = AgXOutsetMatrix * color;
	color = pow( max( vec3( 0.0 ), color ), vec3( 2.2 ) );
	color = LINEAR_REC2020_TO_LINEAR_SRGB * color;
	color = clamp( color, 0.0, 1.0 );
	return color;
}
vec3 NeutralToneMapping( vec3 color ) {
	const float StartCompression = 0.8 - 0.04;
	const float Desaturation = 0.15;
	color *= toneMappingExposure;
	float x = min( color.r, min( color.g, color.b ) );
	float offset = x < 0.08 ? x - 6.25 * x * x : 0.04;
	color -= offset;
	float peak = max( color.r, max( color.g, color.b ) );
	if ( peak < StartCompression ) return color;
	float d = 1. - StartCompression;
	float newPeak = 1. - d * d / ( peak + d - StartCompression );
	color *= newPeak / peak;
	float g = 1. - 1. / ( Desaturation * ( peak - newPeak ) + 1. );
	return mix( color, vec3( newPeak ), g );
}
vec3 CustomToneMapping( vec3 color ) { return color; }`,jd=`#ifdef USE_TRANSMISSION
	material.transmission = transmission;
	material.transmissionAlpha = 1.0;
	material.thickness = thickness;
	material.attenuationDistance = attenuationDistance;
	material.attenuationColor = attenuationColor;
	#ifdef USE_TRANSMISSIONMAP
		material.transmission *= texture2D( transmissionMap, vTransmissionMapUv ).r;
	#endif
	#ifdef USE_THICKNESSMAP
		material.thickness *= texture2D( thicknessMap, vThicknessMapUv ).g;
	#endif
	vec3 pos = vWorldPosition;
	vec3 v = normalize( cameraPosition - pos );
	vec3 n = inverseTransformDirection( normal, viewMatrix );
	vec4 transmitted = getIBLVolumeRefraction(
		n, v, material.roughness, material.diffuseContribution, material.specularColorBlended, material.specularF90,
		pos, modelMatrix, viewMatrix, projectionMatrix, material.dispersion, material.ior, material.thickness,
		material.attenuationColor, material.attenuationDistance );
	material.transmissionAlpha = mix( material.transmissionAlpha, transmitted.a, material.transmission );
	totalDiffuse = mix( totalDiffuse, transmitted.rgb, material.transmission );
#endif`,qd=`#ifdef USE_TRANSMISSION
	uniform float transmission;
	uniform float thickness;
	uniform float attenuationDistance;
	uniform vec3 attenuationColor;
	#ifdef USE_TRANSMISSIONMAP
		uniform sampler2D transmissionMap;
	#endif
	#ifdef USE_THICKNESSMAP
		uniform sampler2D thicknessMap;
	#endif
	uniform vec2 transmissionSamplerSize;
	uniform sampler2D transmissionSamplerMap;
	uniform mat4 modelMatrix;
	uniform mat4 projectionMatrix;
	varying vec3 vWorldPosition;
	float w0( float a ) {
		return ( 1.0 / 6.0 ) * ( a * ( a * ( - a + 3.0 ) - 3.0 ) + 1.0 );
	}
	float w1( float a ) {
		return ( 1.0 / 6.0 ) * ( a *  a * ( 3.0 * a - 6.0 ) + 4.0 );
	}
	float w2( float a ){
		return ( 1.0 / 6.0 ) * ( a * ( a * ( - 3.0 * a + 3.0 ) + 3.0 ) + 1.0 );
	}
	float w3( float a ) {
		return ( 1.0 / 6.0 ) * ( a * a * a );
	}
	float g0( float a ) {
		return w0( a ) + w1( a );
	}
	float g1( float a ) {
		return w2( a ) + w3( a );
	}
	float h0( float a ) {
		return - 1.0 + w1( a ) / ( w0( a ) + w1( a ) );
	}
	float h1( float a ) {
		return 1.0 + w3( a ) / ( w2( a ) + w3( a ) );
	}
	vec4 bicubic( sampler2D tex, vec2 uv, vec4 texelSize, float lod ) {
		uv = uv * texelSize.zw + 0.5;
		vec2 iuv = floor( uv );
		vec2 fuv = fract( uv );
		float g0x = g0( fuv.x );
		float g1x = g1( fuv.x );
		float h0x = h0( fuv.x );
		float h1x = h1( fuv.x );
		float h0y = h0( fuv.y );
		float h1y = h1( fuv.y );
		vec2 p0 = ( vec2( iuv.x + h0x, iuv.y + h0y ) - 0.5 ) * texelSize.xy;
		vec2 p1 = ( vec2( iuv.x + h1x, iuv.y + h0y ) - 0.5 ) * texelSize.xy;
		vec2 p2 = ( vec2( iuv.x + h0x, iuv.y + h1y ) - 0.5 ) * texelSize.xy;
		vec2 p3 = ( vec2( iuv.x + h1x, iuv.y + h1y ) - 0.5 ) * texelSize.xy;
		return g0( fuv.y ) * ( g0x * textureLod( tex, p0, lod ) + g1x * textureLod( tex, p1, lod ) ) +
			g1( fuv.y ) * ( g0x * textureLod( tex, p2, lod ) + g1x * textureLod( tex, p3, lod ) );
	}
	vec4 textureBicubic( sampler2D sampler, vec2 uv, float lod ) {
		vec2 fLodSize = vec2( textureSize( sampler, int( lod ) ) );
		vec2 cLodSize = vec2( textureSize( sampler, int( lod + 1.0 ) ) );
		vec2 fLodSizeInv = 1.0 / fLodSize;
		vec2 cLodSizeInv = 1.0 / cLodSize;
		vec4 fSample = bicubic( sampler, uv, vec4( fLodSizeInv, fLodSize ), floor( lod ) );
		vec4 cSample = bicubic( sampler, uv, vec4( cLodSizeInv, cLodSize ), ceil( lod ) );
		return mix( fSample, cSample, fract( lod ) );
	}
	vec3 getVolumeTransmissionRay( const in vec3 n, const in vec3 v, const in float thickness, const in float ior, const in mat4 modelMatrix ) {
		vec3 refractionVector = refract( - v, normalize( n ), 1.0 / ior );
		vec3 modelScale;
		modelScale.x = length( vec3( modelMatrix[ 0 ].xyz ) );
		modelScale.y = length( vec3( modelMatrix[ 1 ].xyz ) );
		modelScale.z = length( vec3( modelMatrix[ 2 ].xyz ) );
		return normalize( refractionVector ) * thickness * modelScale;
	}
	float applyIorToRoughness( const in float roughness, const in float ior ) {
		return roughness * clamp( ior * 2.0 - 2.0, 0.0, 1.0 );
	}
	vec4 getTransmissionSample( const in vec2 fragCoord, const in float roughness, const in float ior ) {
		float lod = log2( transmissionSamplerSize.x ) * applyIorToRoughness( roughness, ior );
		return textureBicubic( transmissionSamplerMap, fragCoord.xy, lod );
	}
	vec3 volumeAttenuation( const in float transmissionDistance, const in vec3 attenuationColor, const in float attenuationDistance ) {
		if ( isinf( attenuationDistance ) ) {
			return vec3( 1.0 );
		} else {
			vec3 attenuationCoefficient = -log( attenuationColor ) / attenuationDistance;
			vec3 transmittance = exp( - attenuationCoefficient * transmissionDistance );			return transmittance;
		}
	}
	vec4 getIBLVolumeRefraction( const in vec3 n, const in vec3 v, const in float roughness, const in vec3 diffuseColor,
		const in vec3 specularColor, const in float specularF90, const in vec3 position, const in mat4 modelMatrix,
		const in mat4 viewMatrix, const in mat4 projMatrix, const in float dispersion, const in float ior, const in float thickness,
		const in vec3 attenuationColor, const in float attenuationDistance ) {
		vec4 transmittedLight;
		vec3 transmittance;
		#ifdef USE_DISPERSION
			float halfSpread = ( ior - 1.0 ) * 0.025 * dispersion;
			vec3 iors = vec3( ior - halfSpread, ior, ior + halfSpread );
			for ( int i = 0; i < 3; i ++ ) {
				vec3 transmissionRay = getVolumeTransmissionRay( n, v, thickness, iors[ i ], modelMatrix );
				vec3 refractedRayExit = position + transmissionRay;
				vec4 ndcPos = projMatrix * viewMatrix * vec4( refractedRayExit, 1.0 );
				vec2 refractionCoords = ndcPos.xy / ndcPos.w;
				refractionCoords += 1.0;
				refractionCoords /= 2.0;
				vec4 transmissionSample = getTransmissionSample( refractionCoords, roughness, iors[ i ] );
				transmittedLight[ i ] = transmissionSample[ i ];
				transmittedLight.a += transmissionSample.a;
				transmittance[ i ] = diffuseColor[ i ] * volumeAttenuation( length( transmissionRay ), attenuationColor, attenuationDistance )[ i ];
			}
			transmittedLight.a /= 3.0;
		#else
			vec3 transmissionRay = getVolumeTransmissionRay( n, v, thickness, ior, modelMatrix );
			vec3 refractedRayExit = position + transmissionRay;
			vec4 ndcPos = projMatrix * viewMatrix * vec4( refractedRayExit, 1.0 );
			vec2 refractionCoords = ndcPos.xy / ndcPos.w;
			refractionCoords += 1.0;
			refractionCoords /= 2.0;
			transmittedLight = getTransmissionSample( refractionCoords, roughness, ior );
			transmittance = diffuseColor * volumeAttenuation( length( transmissionRay ), attenuationColor, attenuationDistance );
		#endif
		vec3 attenuatedColor = transmittance * transmittedLight.rgb;
		vec3 F = EnvironmentBRDF( n, v, specularColor, specularF90, roughness );
		float transmittanceFactor = ( transmittance.r + transmittance.g + transmittance.b ) / 3.0;
		return vec4( ( 1.0 - F ) * attenuatedColor, 1.0 - ( 1.0 - transmittedLight.a ) * transmittanceFactor );
	}
#endif`,Yd=`#if defined( USE_UV ) || defined( USE_ANISOTROPY )
	varying vec2 vUv;
#endif
#ifdef USE_MAP
	varying vec2 vMapUv;
#endif
#ifdef USE_ALPHAMAP
	varying vec2 vAlphaMapUv;
#endif
#ifdef USE_LIGHTMAP
	varying vec2 vLightMapUv;
#endif
#ifdef USE_AOMAP
	varying vec2 vAoMapUv;
#endif
#ifdef USE_BUMPMAP
	varying vec2 vBumpMapUv;
#endif
#ifdef USE_NORMALMAP
	varying vec2 vNormalMapUv;
#endif
#ifdef USE_EMISSIVEMAP
	varying vec2 vEmissiveMapUv;
#endif
#ifdef USE_METALNESSMAP
	varying vec2 vMetalnessMapUv;
#endif
#ifdef USE_ROUGHNESSMAP
	varying vec2 vRoughnessMapUv;
#endif
#ifdef USE_ANISOTROPYMAP
	varying vec2 vAnisotropyMapUv;
#endif
#ifdef USE_CLEARCOATMAP
	varying vec2 vClearcoatMapUv;
#endif
#ifdef USE_CLEARCOAT_NORMALMAP
	varying vec2 vClearcoatNormalMapUv;
#endif
#ifdef USE_CLEARCOAT_ROUGHNESSMAP
	varying vec2 vClearcoatRoughnessMapUv;
#endif
#ifdef USE_IRIDESCENCEMAP
	varying vec2 vIridescenceMapUv;
#endif
#ifdef USE_IRIDESCENCE_THICKNESSMAP
	varying vec2 vIridescenceThicknessMapUv;
#endif
#ifdef USE_SHEEN_COLORMAP
	varying vec2 vSheenColorMapUv;
#endif
#ifdef USE_SHEEN_ROUGHNESSMAP
	varying vec2 vSheenRoughnessMapUv;
#endif
#ifdef USE_SPECULARMAP
	varying vec2 vSpecularMapUv;
#endif
#ifdef USE_SPECULAR_COLORMAP
	varying vec2 vSpecularColorMapUv;
#endif
#ifdef USE_SPECULAR_INTENSITYMAP
	varying vec2 vSpecularIntensityMapUv;
#endif
#ifdef USE_TRANSMISSIONMAP
	uniform mat3 transmissionMapTransform;
	varying vec2 vTransmissionMapUv;
#endif
#ifdef USE_THICKNESSMAP
	uniform mat3 thicknessMapTransform;
	varying vec2 vThicknessMapUv;
#endif`,$d=`#if defined( USE_UV ) || defined( USE_ANISOTROPY )
	varying vec2 vUv;
#endif
#ifdef USE_MAP
	uniform mat3 mapTransform;
	varying vec2 vMapUv;
#endif
#ifdef USE_ALPHAMAP
	uniform mat3 alphaMapTransform;
	varying vec2 vAlphaMapUv;
#endif
#ifdef USE_LIGHTMAP
	uniform mat3 lightMapTransform;
	varying vec2 vLightMapUv;
#endif
#ifdef USE_AOMAP
	uniform mat3 aoMapTransform;
	varying vec2 vAoMapUv;
#endif
#ifdef USE_BUMPMAP
	uniform mat3 bumpMapTransform;
	varying vec2 vBumpMapUv;
#endif
#ifdef USE_NORMALMAP
	uniform mat3 normalMapTransform;
	varying vec2 vNormalMapUv;
#endif
#ifdef USE_DISPLACEMENTMAP
	uniform mat3 displacementMapTransform;
	varying vec2 vDisplacementMapUv;
#endif
#ifdef USE_EMISSIVEMAP
	uniform mat3 emissiveMapTransform;
	varying vec2 vEmissiveMapUv;
#endif
#ifdef USE_METALNESSMAP
	uniform mat3 metalnessMapTransform;
	varying vec2 vMetalnessMapUv;
#endif
#ifdef USE_ROUGHNESSMAP
	uniform mat3 roughnessMapTransform;
	varying vec2 vRoughnessMapUv;
#endif
#ifdef USE_ANISOTROPYMAP
	uniform mat3 anisotropyMapTransform;
	varying vec2 vAnisotropyMapUv;
#endif
#ifdef USE_CLEARCOATMAP
	uniform mat3 clearcoatMapTransform;
	varying vec2 vClearcoatMapUv;
#endif
#ifdef USE_CLEARCOAT_NORMALMAP
	uniform mat3 clearcoatNormalMapTransform;
	varying vec2 vClearcoatNormalMapUv;
#endif
#ifdef USE_CLEARCOAT_ROUGHNESSMAP
	uniform mat3 clearcoatRoughnessMapTransform;
	varying vec2 vClearcoatRoughnessMapUv;
#endif
#ifdef USE_SHEEN_COLORMAP
	uniform mat3 sheenColorMapTransform;
	varying vec2 vSheenColorMapUv;
#endif
#ifdef USE_SHEEN_ROUGHNESSMAP
	uniform mat3 sheenRoughnessMapTransform;
	varying vec2 vSheenRoughnessMapUv;
#endif
#ifdef USE_IRIDESCENCEMAP
	uniform mat3 iridescenceMapTransform;
	varying vec2 vIridescenceMapUv;
#endif
#ifdef USE_IRIDESCENCE_THICKNESSMAP
	uniform mat3 iridescenceThicknessMapTransform;
	varying vec2 vIridescenceThicknessMapUv;
#endif
#ifdef USE_SPECULARMAP
	uniform mat3 specularMapTransform;
	varying vec2 vSpecularMapUv;
#endif
#ifdef USE_SPECULAR_COLORMAP
	uniform mat3 specularColorMapTransform;
	varying vec2 vSpecularColorMapUv;
#endif
#ifdef USE_SPECULAR_INTENSITYMAP
	uniform mat3 specularIntensityMapTransform;
	varying vec2 vSpecularIntensityMapUv;
#endif
#ifdef USE_TRANSMISSIONMAP
	uniform mat3 transmissionMapTransform;
	varying vec2 vTransmissionMapUv;
#endif
#ifdef USE_THICKNESSMAP
	uniform mat3 thicknessMapTransform;
	varying vec2 vThicknessMapUv;
#endif`,Zd=`#if defined( USE_UV ) || defined( USE_ANISOTROPY )
	vUv = vec3( uv, 1 ).xy;
#endif
#ifdef USE_MAP
	vMapUv = ( mapTransform * vec3( MAP_UV, 1 ) ).xy;
#endif
#ifdef USE_ALPHAMAP
	vAlphaMapUv = ( alphaMapTransform * vec3( ALPHAMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_LIGHTMAP
	vLightMapUv = ( lightMapTransform * vec3( LIGHTMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_AOMAP
	vAoMapUv = ( aoMapTransform * vec3( AOMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_BUMPMAP
	vBumpMapUv = ( bumpMapTransform * vec3( BUMPMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_NORMALMAP
	vNormalMapUv = ( normalMapTransform * vec3( NORMALMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_DISPLACEMENTMAP
	vDisplacementMapUv = ( displacementMapTransform * vec3( DISPLACEMENTMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_EMISSIVEMAP
	vEmissiveMapUv = ( emissiveMapTransform * vec3( EMISSIVEMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_METALNESSMAP
	vMetalnessMapUv = ( metalnessMapTransform * vec3( METALNESSMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_ROUGHNESSMAP
	vRoughnessMapUv = ( roughnessMapTransform * vec3( ROUGHNESSMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_ANISOTROPYMAP
	vAnisotropyMapUv = ( anisotropyMapTransform * vec3( ANISOTROPYMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_CLEARCOATMAP
	vClearcoatMapUv = ( clearcoatMapTransform * vec3( CLEARCOATMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_CLEARCOAT_NORMALMAP
	vClearcoatNormalMapUv = ( clearcoatNormalMapTransform * vec3( CLEARCOAT_NORMALMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_CLEARCOAT_ROUGHNESSMAP
	vClearcoatRoughnessMapUv = ( clearcoatRoughnessMapTransform * vec3( CLEARCOAT_ROUGHNESSMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_IRIDESCENCEMAP
	vIridescenceMapUv = ( iridescenceMapTransform * vec3( IRIDESCENCEMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_IRIDESCENCE_THICKNESSMAP
	vIridescenceThicknessMapUv = ( iridescenceThicknessMapTransform * vec3( IRIDESCENCE_THICKNESSMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_SHEEN_COLORMAP
	vSheenColorMapUv = ( sheenColorMapTransform * vec3( SHEEN_COLORMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_SHEEN_ROUGHNESSMAP
	vSheenRoughnessMapUv = ( sheenRoughnessMapTransform * vec3( SHEEN_ROUGHNESSMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_SPECULARMAP
	vSpecularMapUv = ( specularMapTransform * vec3( SPECULARMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_SPECULAR_COLORMAP
	vSpecularColorMapUv = ( specularColorMapTransform * vec3( SPECULAR_COLORMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_SPECULAR_INTENSITYMAP
	vSpecularIntensityMapUv = ( specularIntensityMapTransform * vec3( SPECULAR_INTENSITYMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_TRANSMISSIONMAP
	vTransmissionMapUv = ( transmissionMapTransform * vec3( TRANSMISSIONMAP_UV, 1 ) ).xy;
#endif
#ifdef USE_THICKNESSMAP
	vThicknessMapUv = ( thicknessMapTransform * vec3( THICKNESSMAP_UV, 1 ) ).xy;
#endif`,Kd=`#if defined( USE_ENVMAP ) || defined( DISTANCE ) || defined ( USE_SHADOWMAP ) || defined ( USE_TRANSMISSION ) || NUM_SPOT_LIGHT_COORDS > 0
	vec4 worldPosition = vec4( transformed, 1.0 );
	#ifdef USE_BATCHING
		worldPosition = batchingMatrix * worldPosition;
	#endif
	#ifdef USE_INSTANCING
		worldPosition = instanceMatrix * worldPosition;
	#endif
	worldPosition = modelMatrix * worldPosition;
#endif`;const ze={alphahash_fragment:Jh,alphahash_pars_fragment:Qh,alphamap_fragment:eu,alphamap_pars_fragment:tu,alphatest_fragment:iu,alphatest_pars_fragment:nu,aomap_fragment:su,aomap_pars_fragment:ru,batching_pars_vertex:au,batching_vertex:ou,begin_vertex:lu,beginnormal_vertex:cu,bsdfs:hu,iridescence_fragment:uu,bumpmap_pars_fragment:du,clipping_planes_fragment:fu,clipping_planes_pars_fragment:pu,clipping_planes_pars_vertex:mu,clipping_planes_vertex:gu,color_fragment:_u,color_pars_fragment:vu,color_pars_vertex:xu,color_vertex:bu,common:Su,cube_uv_reflection_fragment:Mu,defaultnormal_vertex:yu,displacementmap_pars_vertex:Eu,displacementmap_vertex:wu,emissivemap_fragment:Tu,emissivemap_pars_fragment:Au,colorspace_fragment:Cu,colorspace_pars_fragment:Ru,envmap_fragment:Pu,envmap_common_pars_fragment:Lu,envmap_pars_fragment:Du,envmap_pars_vertex:Iu,envmap_physical_pars_fragment:Wu,envmap_vertex:Nu,fog_vertex:Uu,fog_pars_vertex:Fu,fog_fragment:Ou,fog_pars_fragment:Bu,gradientmap_pars_fragment:zu,lightmap_pars_fragment:ku,lights_lambert_fragment:Vu,lights_lambert_pars_fragment:Gu,lights_pars_begin:Hu,lights_toon_fragment:Xu,lights_toon_pars_fragment:ju,lights_phong_fragment:qu,lights_phong_pars_fragment:Yu,lights_physical_fragment:$u,lights_physical_pars_fragment:Zu,lights_fragment_begin:Ku,lights_fragment_maps:Ju,lights_fragment_end:Qu,lightprobes_pars_fragment:ed,logdepthbuf_fragment:td,logdepthbuf_pars_fragment:id,logdepthbuf_pars_vertex:nd,logdepthbuf_vertex:sd,map_fragment:rd,map_pars_fragment:ad,map_particle_fragment:od,map_particle_pars_fragment:ld,metalnessmap_fragment:cd,metalnessmap_pars_fragment:hd,morphinstance_vertex:ud,morphcolor_vertex:dd,morphnormal_vertex:fd,morphtarget_pars_vertex:pd,morphtarget_vertex:md,normal_fragment_begin:gd,normal_fragment_maps:_d,normal_pars_fragment:vd,normal_pars_vertex:xd,normal_vertex:bd,normalmap_pars_fragment:Sd,clearcoat_normal_fragment_begin:Md,clearcoat_normal_fragment_maps:yd,clearcoat_pars_fragment:Ed,iridescence_pars_fragment:wd,opaque_fragment:Td,packing:Ad,premultiplied_alpha_fragment:Cd,project_vertex:Rd,dithering_fragment:Pd,dithering_pars_fragment:Ld,roughnessmap_fragment:Dd,roughnessmap_pars_fragment:Id,shadowmap_pars_fragment:Nd,shadowmap_pars_vertex:Ud,shadowmap_vertex:Fd,shadowmask_pars_fragment:Od,skinbase_vertex:Bd,skinning_pars_vertex:zd,skinning_vertex:kd,skinnormal_vertex:Vd,specularmap_fragment:Gd,specularmap_pars_fragment:Hd,tonemapping_fragment:Wd,tonemapping_pars_fragment:Xd,transmission_fragment:jd,transmission_pars_fragment:qd,uv_pars_fragment:Yd,uv_pars_vertex:$d,uv_vertex:Zd,worldpos_vertex:Kd,background_vert:`varying vec2 vUv;
uniform mat3 uvTransform;
void main() {
	vUv = ( uvTransform * vec3( uv, 1 ) ).xy;
	gl_Position = vec4( position.xy, 1.0, 1.0 );
}`,background_frag:`uniform sampler2D t2D;
uniform float backgroundIntensity;
varying vec2 vUv;
void main() {
	vec4 texColor = texture2D( t2D, vUv );
	#ifdef DECODE_VIDEO_TEXTURE
		texColor = vec4( mix( pow( texColor.rgb * 0.9478672986 + vec3( 0.0521327014 ), vec3( 2.4 ) ), texColor.rgb * 0.0773993808, vec3( lessThanEqual( texColor.rgb, vec3( 0.04045 ) ) ) ), texColor.w );
	#endif
	texColor.rgb *= backgroundIntensity;
	gl_FragColor = texColor;
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
}`,backgroundCube_vert:`varying vec3 vWorldDirection;
#include <common>
void main() {
	vWorldDirection = transformDirection( position, modelMatrix );
	#include <begin_vertex>
	#include <project_vertex>
	gl_Position.z = gl_Position.w;
}`,backgroundCube_frag:`#ifdef ENVMAP_TYPE_CUBE
	uniform samplerCube envMap;
#elif defined( ENVMAP_TYPE_CUBE_UV )
	uniform sampler2D envMap;
#endif
uniform float backgroundBlurriness;
uniform float backgroundIntensity;
uniform mat3 backgroundRotation;
varying vec3 vWorldDirection;
#include <cube_uv_reflection_fragment>
void main() {
	#ifdef ENVMAP_TYPE_CUBE
		vec4 texColor = textureCube( envMap, backgroundRotation * vWorldDirection );
	#elif defined( ENVMAP_TYPE_CUBE_UV )
		vec4 texColor = textureCubeUV( envMap, backgroundRotation * vWorldDirection, backgroundBlurriness );
	#else
		vec4 texColor = vec4( 0.0, 0.0, 0.0, 1.0 );
	#endif
	texColor.rgb *= backgroundIntensity;
	gl_FragColor = texColor;
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
}`,cube_vert:`varying vec3 vWorldDirection;
#include <common>
void main() {
	vWorldDirection = transformDirection( position, modelMatrix );
	#include <begin_vertex>
	#include <project_vertex>
	gl_Position.z = gl_Position.w;
}`,cube_frag:`uniform samplerCube tCube;
uniform float tFlip;
uniform float opacity;
varying vec3 vWorldDirection;
void main() {
	vec4 texColor = textureCube( tCube, vec3( tFlip * vWorldDirection.x, vWorldDirection.yz ) );
	gl_FragColor = texColor;
	gl_FragColor.a *= opacity;
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
}`,depth_vert:`#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
varying vec2 vHighPrecisionZW;
void main() {
	#include <uv_vertex>
	#include <batching_vertex>
	#include <skinbase_vertex>
	#include <morphinstance_vertex>
	#ifdef USE_DISPLACEMENTMAP
		#include <beginnormal_vertex>
		#include <morphnormal_vertex>
		#include <skinnormal_vertex>
	#endif
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	vHighPrecisionZW = gl_Position.zw;
}`,depth_frag:`#if DEPTH_PACKING == 3200
	uniform float opacity;
#endif
#include <common>
#include <packing>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
varying vec2 vHighPrecisionZW;
void main() {
	vec4 diffuseColor = vec4( 1.0 );
	#include <clipping_planes_fragment>
	#if DEPTH_PACKING == 3200
		diffuseColor.a = opacity;
	#endif
	#include <map_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <logdepthbuf_fragment>
	#ifdef USE_REVERSED_DEPTH_BUFFER
		float fragCoordZ = vHighPrecisionZW[ 0 ] / vHighPrecisionZW[ 1 ];
	#else
		float fragCoordZ = 0.5 * vHighPrecisionZW[ 0 ] / vHighPrecisionZW[ 1 ] + 0.5;
	#endif
	#if DEPTH_PACKING == 3200
		gl_FragColor = vec4( vec3( 1.0 - fragCoordZ ), opacity );
	#elif DEPTH_PACKING == 3201
		gl_FragColor = packDepthToRGBA( fragCoordZ );
	#elif DEPTH_PACKING == 3202
		gl_FragColor = vec4( packDepthToRGB( fragCoordZ ), 1.0 );
	#elif DEPTH_PACKING == 3203
		gl_FragColor = vec4( packDepthToRG( fragCoordZ ), 0.0, 1.0 );
	#endif
}`,distance_vert:`#define DISTANCE
varying vec3 vWorldPosition;
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <batching_vertex>
	#include <skinbase_vertex>
	#include <morphinstance_vertex>
	#ifdef USE_DISPLACEMENTMAP
		#include <beginnormal_vertex>
		#include <morphnormal_vertex>
		#include <skinnormal_vertex>
	#endif
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <worldpos_vertex>
	#include <clipping_planes_vertex>
	vWorldPosition = worldPosition.xyz;
}`,distance_frag:`#define DISTANCE
uniform vec3 referencePosition;
uniform float nearDistance;
uniform float farDistance;
varying vec3 vWorldPosition;
#include <common>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <clipping_planes_pars_fragment>
void main () {
	vec4 diffuseColor = vec4( 1.0 );
	#include <clipping_planes_fragment>
	#include <map_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	float dist = length( vWorldPosition - referencePosition );
	dist = ( dist - nearDistance ) / ( farDistance - nearDistance );
	dist = saturate( dist );
	gl_FragColor = vec4( dist, 0.0, 0.0, 1.0 );
}`,equirect_vert:`varying vec3 vWorldDirection;
#include <common>
void main() {
	vWorldDirection = transformDirection( position, modelMatrix );
	#include <begin_vertex>
	#include <project_vertex>
}`,equirect_frag:`uniform sampler2D tEquirect;
varying vec3 vWorldDirection;
#include <common>
void main() {
	vec3 direction = normalize( vWorldDirection );
	vec2 sampleUV = equirectUv( direction );
	gl_FragColor = texture2D( tEquirect, sampleUV );
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
}`,linedashed_vert:`uniform float scale;
attribute float lineDistance;
varying float vLineDistance;
#include <common>
#include <uv_pars_vertex>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <morphtarget_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	vLineDistance = scale * lineDistance;
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	#include <fog_vertex>
}`,linedashed_frag:`uniform vec3 diffuse;
uniform float opacity;
uniform float dashSize;
uniform float totalSize;
varying float vLineDistance;
#include <common>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <fog_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	if ( mod( vLineDistance, totalSize ) > dashSize ) {
		discard;
	}
	vec3 outgoingLight = vec3( 0.0 );
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	outgoingLight = diffuseColor.rgb;
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
}`,meshbasic_vert:`#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <envmap_pars_vertex>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <batching_vertex>
	#if defined ( USE_ENVMAP ) || defined ( USE_SKINNING )
		#include <beginnormal_vertex>
		#include <morphnormal_vertex>
		#include <skinbase_vertex>
		#include <skinnormal_vertex>
		#include <defaultnormal_vertex>
	#endif
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	#include <worldpos_vertex>
	#include <envmap_vertex>
	#include <fog_vertex>
}`,meshbasic_frag:`uniform vec3 diffuse;
uniform float opacity;
#ifndef FLAT_SHADED
	varying vec3 vNormal;
#endif
#include <common>
#include <dithering_pars_fragment>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <aomap_pars_fragment>
#include <lightmap_pars_fragment>
#include <envmap_common_pars_fragment>
#include <envmap_pars_fragment>
#include <fog_pars_fragment>
#include <specularmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <specularmap_fragment>
	ReflectedLight reflectedLight = ReflectedLight( vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ) );
	#ifdef USE_LIGHTMAP
		vec4 lightMapTexel = texture2D( lightMap, vLightMapUv );
		reflectedLight.indirectDiffuse += lightMapTexel.rgb * lightMapIntensity * RECIPROCAL_PI;
	#else
		reflectedLight.indirectDiffuse += vec3( 1.0 );
	#endif
	#include <aomap_fragment>
	reflectedLight.indirectDiffuse *= diffuseColor.rgb;
	vec3 outgoingLight = reflectedLight.indirectDiffuse;
	#include <envmap_fragment>
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
	#include <dithering_fragment>
}`,meshlambert_vert:`#define LAMBERT
varying vec3 vViewPosition;
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <envmap_pars_vertex>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <normal_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <shadowmap_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <normal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	vViewPosition = - mvPosition.xyz;
	#include <worldpos_vertex>
	#include <envmap_vertex>
	#include <shadowmap_vertex>
	#include <fog_vertex>
}`,meshlambert_frag:`#define LAMBERT
uniform vec3 diffuse;
uniform vec3 emissive;
uniform float opacity;
#include <common>
#include <dithering_pars_fragment>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <aomap_pars_fragment>
#include <lightmap_pars_fragment>
#include <emissivemap_pars_fragment>
#include <cube_uv_reflection_fragment>
#include <envmap_common_pars_fragment>
#include <envmap_pars_fragment>
#include <envmap_physical_pars_fragment>
#include <fog_pars_fragment>
#include <bsdfs>
#include <lights_pars_begin>
#include <normal_pars_fragment>
#include <lights_lambert_pars_fragment>
#include <shadowmap_pars_fragment>
#include <bumpmap_pars_fragment>
#include <normalmap_pars_fragment>
#include <specularmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	ReflectedLight reflectedLight = ReflectedLight( vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ) );
	vec3 totalEmissiveRadiance = emissive;
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <specularmap_fragment>
	#include <normal_fragment_begin>
	#include <normal_fragment_maps>
	#include <emissivemap_fragment>
	#include <lights_lambert_fragment>
	#include <lights_fragment_begin>
	#include <lights_fragment_maps>
	#include <lights_fragment_end>
	#include <aomap_fragment>
	vec3 outgoingLight = reflectedLight.directDiffuse + reflectedLight.indirectDiffuse + totalEmissiveRadiance;
	#include <envmap_fragment>
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
	#include <dithering_fragment>
}`,meshmatcap_vert:`#define MATCAP
varying vec3 vViewPosition;
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <color_pars_vertex>
#include <displacementmap_pars_vertex>
#include <fog_pars_vertex>
#include <normal_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <normal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	#include <fog_vertex>
	vViewPosition = - mvPosition.xyz;
}`,meshmatcap_frag:`#define MATCAP
uniform vec3 diffuse;
uniform float opacity;
uniform sampler2D matcap;
varying vec3 vViewPosition;
#include <common>
#include <dithering_pars_fragment>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <fog_pars_fragment>
#include <normal_pars_fragment>
#include <bumpmap_pars_fragment>
#include <normalmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <normal_fragment_begin>
	#include <normal_fragment_maps>
	vec3 viewDir = normalize( vViewPosition );
	vec3 x = normalize( vec3( viewDir.z, 0.0, - viewDir.x ) );
	vec3 y = cross( viewDir, x );
	vec2 uv = vec2( dot( x, normal ), dot( y, normal ) ) * 0.495 + 0.5;
	#ifdef USE_MATCAP
		vec4 matcapColor = texture2D( matcap, uv );
	#else
		vec4 matcapColor = vec4( vec3( mix( 0.2, 0.8, uv.y ) ), 1.0 );
	#endif
	vec3 outgoingLight = diffuseColor.rgb * matcapColor.rgb;
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
	#include <dithering_fragment>
}`,meshnormal_vert:`#define NORMAL
#if defined( FLAT_SHADED ) || defined( USE_BUMPMAP ) || defined( USE_NORMALMAP_TANGENTSPACE )
	varying vec3 vViewPosition;
#endif
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <normal_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphinstance_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <normal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
#if defined( FLAT_SHADED ) || defined( USE_BUMPMAP ) || defined( USE_NORMALMAP_TANGENTSPACE )
	vViewPosition = - mvPosition.xyz;
#endif
}`,meshnormal_frag:`#define NORMAL
uniform float opacity;
#if defined( FLAT_SHADED ) || defined( USE_BUMPMAP ) || defined( USE_NORMALMAP_TANGENTSPACE )
	varying vec3 vViewPosition;
#endif
#include <uv_pars_fragment>
#include <normal_pars_fragment>
#include <bumpmap_pars_fragment>
#include <normalmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( 0.0, 0.0, 0.0, opacity );
	#include <clipping_planes_fragment>
	#include <logdepthbuf_fragment>
	#include <normal_fragment_begin>
	#include <normal_fragment_maps>
	gl_FragColor = vec4( normalize( normal ) * 0.5 + 0.5, diffuseColor.a );
	#ifdef OPAQUE
		gl_FragColor.a = 1.0;
	#endif
}`,meshphong_vert:`#define PHONG
varying vec3 vViewPosition;
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <envmap_pars_vertex>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <normal_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <shadowmap_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphcolor_vertex>
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphinstance_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <normal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	vViewPosition = - mvPosition.xyz;
	#include <worldpos_vertex>
	#include <envmap_vertex>
	#include <shadowmap_vertex>
	#include <fog_vertex>
}`,meshphong_frag:`#define PHONG
uniform vec3 diffuse;
uniform vec3 emissive;
uniform vec3 specular;
uniform float shininess;
uniform float opacity;
#include <common>
#include <dithering_pars_fragment>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <aomap_pars_fragment>
#include <lightmap_pars_fragment>
#include <emissivemap_pars_fragment>
#include <cube_uv_reflection_fragment>
#include <envmap_common_pars_fragment>
#include <envmap_pars_fragment>
#include <envmap_physical_pars_fragment>
#include <fog_pars_fragment>
#include <bsdfs>
#include <lights_pars_begin>
#include <normal_pars_fragment>
#include <lights_phong_pars_fragment>
#include <shadowmap_pars_fragment>
#include <bumpmap_pars_fragment>
#include <normalmap_pars_fragment>
#include <specularmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	ReflectedLight reflectedLight = ReflectedLight( vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ) );
	vec3 totalEmissiveRadiance = emissive;
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <specularmap_fragment>
	#include <normal_fragment_begin>
	#include <normal_fragment_maps>
	#include <emissivemap_fragment>
	#include <lights_phong_fragment>
	#include <lights_fragment_begin>
	#include <lights_fragment_maps>
	#include <lights_fragment_end>
	#include <aomap_fragment>
	vec3 outgoingLight = reflectedLight.directDiffuse + reflectedLight.indirectDiffuse + reflectedLight.directSpecular + reflectedLight.indirectSpecular + totalEmissiveRadiance;
	#include <envmap_fragment>
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
	#include <dithering_fragment>
}`,meshphysical_vert:`#define STANDARD
varying vec3 vViewPosition;
#ifdef USE_TRANSMISSION
	varying vec3 vWorldPosition;
#endif
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <normal_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <shadowmap_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <normal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	vViewPosition = - mvPosition.xyz;
	#include <worldpos_vertex>
	#include <shadowmap_vertex>
	#include <fog_vertex>
#ifdef USE_TRANSMISSION
	vWorldPosition = worldPosition.xyz;
#endif
}`,meshphysical_frag:`#define STANDARD
#ifdef PHYSICAL
	#define IOR
	#define USE_SPECULAR
#endif
uniform vec3 diffuse;
uniform vec3 emissive;
uniform float roughness;
uniform float metalness;
uniform float opacity;
#ifdef IOR
	uniform float ior;
#endif
#ifdef USE_SPECULAR
	uniform float specularIntensity;
	uniform vec3 specularColor;
	#ifdef USE_SPECULAR_COLORMAP
		uniform sampler2D specularColorMap;
	#endif
	#ifdef USE_SPECULAR_INTENSITYMAP
		uniform sampler2D specularIntensityMap;
	#endif
#endif
#ifdef USE_CLEARCOAT
	uniform float clearcoat;
	uniform float clearcoatRoughness;
#endif
#ifdef USE_DISPERSION
	uniform float dispersion;
#endif
#ifdef USE_IRIDESCENCE
	uniform float iridescence;
	uniform float iridescenceIOR;
	uniform float iridescenceThicknessMinimum;
	uniform float iridescenceThicknessMaximum;
#endif
#ifdef USE_SHEEN
	uniform vec3 sheenColor;
	uniform float sheenRoughness;
	#ifdef USE_SHEEN_COLORMAP
		uniform sampler2D sheenColorMap;
	#endif
	#ifdef USE_SHEEN_ROUGHNESSMAP
		uniform sampler2D sheenRoughnessMap;
	#endif
#endif
#ifdef USE_ANISOTROPY
	uniform vec2 anisotropyVector;
	#ifdef USE_ANISOTROPYMAP
		uniform sampler2D anisotropyMap;
	#endif
#endif
varying vec3 vViewPosition;
#include <common>
#include <dithering_pars_fragment>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <aomap_pars_fragment>
#include <lightmap_pars_fragment>
#include <emissivemap_pars_fragment>
#include <iridescence_fragment>
#include <cube_uv_reflection_fragment>
#include <envmap_common_pars_fragment>
#include <envmap_physical_pars_fragment>
#include <fog_pars_fragment>
#include <lights_pars_begin>
#include <normal_pars_fragment>
#include <lights_physical_pars_fragment>
#include <transmission_pars_fragment>
#include <shadowmap_pars_fragment>
#include <bumpmap_pars_fragment>
#include <normalmap_pars_fragment>
#include <clearcoat_pars_fragment>
#include <iridescence_pars_fragment>
#include <roughnessmap_pars_fragment>
#include <metalnessmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	ReflectedLight reflectedLight = ReflectedLight( vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ) );
	vec3 totalEmissiveRadiance = emissive;
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <roughnessmap_fragment>
	#include <metalnessmap_fragment>
	#include <normal_fragment_begin>
	#include <normal_fragment_maps>
	#include <clearcoat_normal_fragment_begin>
	#include <clearcoat_normal_fragment_maps>
	#include <emissivemap_fragment>
	#include <lights_physical_fragment>
	#include <lights_fragment_begin>
	#include <lights_fragment_maps>
	#include <lights_fragment_end>
	#include <aomap_fragment>
	vec3 totalDiffuse = reflectedLight.directDiffuse + reflectedLight.indirectDiffuse;
	vec3 totalSpecular = reflectedLight.directSpecular + reflectedLight.indirectSpecular;
	#include <transmission_fragment>
	vec3 outgoingLight = totalDiffuse + totalSpecular + totalEmissiveRadiance;
	#ifdef USE_SHEEN
 
		outgoingLight = outgoingLight + sheenSpecularDirect + sheenSpecularIndirect;
 
 	#endif
	#ifdef USE_CLEARCOAT
		float dotNVcc = saturate( dot( geometryClearcoatNormal, geometryViewDir ) );
		vec3 Fcc = F_Schlick( material.clearcoatF0, material.clearcoatF90, dotNVcc );
		outgoingLight = outgoingLight * ( 1.0 - material.clearcoat * Fcc ) + ( clearcoatSpecularDirect + clearcoatSpecularIndirect ) * material.clearcoat;
	#endif
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
	#include <dithering_fragment>
}`,meshtoon_vert:`#define TOON
varying vec3 vViewPosition;
#include <common>
#include <batching_pars_vertex>
#include <uv_pars_vertex>
#include <displacementmap_pars_vertex>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <normal_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <shadowmap_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <normal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <displacementmap_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	vViewPosition = - mvPosition.xyz;
	#include <worldpos_vertex>
	#include <shadowmap_vertex>
	#include <fog_vertex>
}`,meshtoon_frag:`#define TOON
uniform vec3 diffuse;
uniform vec3 emissive;
uniform float opacity;
#include <common>
#include <dithering_pars_fragment>
#include <color_pars_fragment>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <aomap_pars_fragment>
#include <lightmap_pars_fragment>
#include <emissivemap_pars_fragment>
#include <gradientmap_pars_fragment>
#include <fog_pars_fragment>
#include <bsdfs>
#include <lights_pars_begin>
#include <normal_pars_fragment>
#include <lights_toon_pars_fragment>
#include <shadowmap_pars_fragment>
#include <bumpmap_pars_fragment>
#include <normalmap_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	ReflectedLight reflectedLight = ReflectedLight( vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ), vec3( 0.0 ) );
	vec3 totalEmissiveRadiance = emissive;
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <color_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	#include <normal_fragment_begin>
	#include <normal_fragment_maps>
	#include <emissivemap_fragment>
	#include <lights_toon_fragment>
	#include <lights_fragment_begin>
	#include <lights_fragment_maps>
	#include <lights_fragment_end>
	#include <aomap_fragment>
	vec3 outgoingLight = reflectedLight.directDiffuse + reflectedLight.indirectDiffuse + totalEmissiveRadiance;
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
	#include <dithering_fragment>
}`,points_vert:`uniform float size;
uniform float scale;
#include <common>
#include <color_pars_vertex>
#include <fog_pars_vertex>
#include <morphtarget_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
#ifdef USE_POINTS_UV
	varying vec2 vUv;
	uniform mat3 uvTransform;
#endif
void main() {
	#ifdef USE_POINTS_UV
		vUv = ( uvTransform * vec3( uv, 1 ) ).xy;
	#endif
	#include <color_vertex>
	#include <morphinstance_vertex>
	#include <morphcolor_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <project_vertex>
	gl_PointSize = size;
	#ifdef USE_SIZEATTENUATION
		bool isPerspective = isPerspectiveMatrix( projectionMatrix );
		if ( isPerspective ) gl_PointSize *= ( scale / - mvPosition.z );
	#endif
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	#include <worldpos_vertex>
	#include <fog_vertex>
}`,points_frag:`uniform vec3 diffuse;
uniform float opacity;
#include <common>
#include <color_pars_fragment>
#include <map_particle_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <fog_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	vec3 outgoingLight = vec3( 0.0 );
	#include <logdepthbuf_fragment>
	#include <map_particle_fragment>
	#include <color_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	outgoingLight = diffuseColor.rgb;
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
}`,shadow_vert:`#include <common>
#include <batching_pars_vertex>
#include <fog_pars_vertex>
#include <morphtarget_pars_vertex>
#include <skinning_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <shadowmap_pars_vertex>
void main() {
	#include <batching_vertex>
	#include <beginnormal_vertex>
	#include <morphinstance_vertex>
	#include <morphnormal_vertex>
	#include <skinbase_vertex>
	#include <skinnormal_vertex>
	#include <defaultnormal_vertex>
	#include <begin_vertex>
	#include <morphtarget_vertex>
	#include <skinning_vertex>
	#include <project_vertex>
	#include <logdepthbuf_vertex>
	#include <worldpos_vertex>
	#include <shadowmap_vertex>
	#include <fog_vertex>
}`,shadow_frag:`uniform vec3 color;
uniform float opacity;
#include <common>
#include <fog_pars_fragment>
#include <bsdfs>
#include <lights_pars_begin>
#include <logdepthbuf_pars_fragment>
#include <shadowmap_pars_fragment>
#include <shadowmask_pars_fragment>
void main() {
	#include <logdepthbuf_fragment>
	gl_FragColor = vec4( color, opacity * ( 1.0 - getShadowMask() ) );
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
	#include <premultiplied_alpha_fragment>
}`,sprite_vert:`uniform float rotation;
uniform vec2 center;
#include <common>
#include <uv_pars_vertex>
#include <fog_pars_vertex>
#include <logdepthbuf_pars_vertex>
#include <clipping_planes_pars_vertex>
void main() {
	#include <uv_vertex>
	vec4 mvPosition = modelViewMatrix[ 3 ];
	vec2 scale = vec2( length( modelMatrix[ 0 ].xyz ), length( modelMatrix[ 1 ].xyz ) );
	#ifndef USE_SIZEATTENUATION
		bool isPerspective = isPerspectiveMatrix( projectionMatrix );
		if ( isPerspective ) scale *= - mvPosition.z;
	#endif
	vec2 alignedPosition = ( position.xy - ( center - vec2( 0.5 ) ) ) * scale;
	vec2 rotatedPosition;
	rotatedPosition.x = cos( rotation ) * alignedPosition.x - sin( rotation ) * alignedPosition.y;
	rotatedPosition.y = sin( rotation ) * alignedPosition.x + cos( rotation ) * alignedPosition.y;
	mvPosition.xy += rotatedPosition;
	gl_Position = projectionMatrix * mvPosition;
	#include <logdepthbuf_vertex>
	#include <clipping_planes_vertex>
	#include <fog_vertex>
}`,sprite_frag:`uniform vec3 diffuse;
uniform float opacity;
#include <common>
#include <uv_pars_fragment>
#include <map_pars_fragment>
#include <alphamap_pars_fragment>
#include <alphatest_pars_fragment>
#include <alphahash_pars_fragment>
#include <fog_pars_fragment>
#include <logdepthbuf_pars_fragment>
#include <clipping_planes_pars_fragment>
void main() {
	vec4 diffuseColor = vec4( diffuse, opacity );
	#include <clipping_planes_fragment>
	vec3 outgoingLight = vec3( 0.0 );
	#include <logdepthbuf_fragment>
	#include <map_fragment>
	#include <alphamap_fragment>
	#include <alphatest_fragment>
	#include <alphahash_fragment>
	outgoingLight = diffuseColor.rgb;
	#include <opaque_fragment>
	#include <tonemapping_fragment>
	#include <colorspace_fragment>
	#include <fog_fragment>
}`},he={common:{diffuse:{value:new Re(16777215)},opacity:{value:1},map:{value:null},mapTransform:{value:new Ue},alphaMap:{value:null},alphaMapTransform:{value:new Ue},alphaTest:{value:0}},specularmap:{specularMap:{value:null},specularMapTransform:{value:new Ue}},envmap:{envMap:{value:null},envMapRotation:{value:new Ue},reflectivity:{value:1},ior:{value:1.5},refractionRatio:{value:.98},dfgLUT:{value:null}},aomap:{aoMap:{value:null},aoMapIntensity:{value:1},aoMapTransform:{value:new Ue}},lightmap:{lightMap:{value:null},lightMapIntensity:{value:1},lightMapTransform:{value:new Ue}},bumpmap:{bumpMap:{value:null},bumpMapTransform:{value:new Ue},bumpScale:{value:1}},normalmap:{normalMap:{value:null},normalMapTransform:{value:new Ue},normalScale:{value:new Ne(1,1)}},displacementmap:{displacementMap:{value:null},displacementMapTransform:{value:new Ue},displacementScale:{value:1},displacementBias:{value:0}},emissivemap:{emissiveMap:{value:null},emissiveMapTransform:{value:new Ue}},metalnessmap:{metalnessMap:{value:null},metalnessMapTransform:{value:new Ue}},roughnessmap:{roughnessMap:{value:null},roughnessMapTransform:{value:new Ue}},gradientmap:{gradientMap:{value:null}},fog:{fogDensity:{value:25e-5},fogNear:{value:1},fogFar:{value:2e3},fogColor:{value:new Re(16777215)}},lights:{ambientLightColor:{value:[]},lightProbe:{value:[]},directionalLights:{value:[],properties:{direction:{},color:{}}},directionalLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{}}},directionalShadowMatrix:{value:[]},spotLights:{value:[],properties:{color:{},position:{},direction:{},distance:{},coneCos:{},penumbraCos:{},decay:{}}},spotLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{}}},spotLightMap:{value:[]},spotLightMatrix:{value:[]},pointLights:{value:[],properties:{color:{},position:{},decay:{},distance:{}}},pointLightShadows:{value:[],properties:{shadowIntensity:1,shadowBias:{},shadowNormalBias:{},shadowRadius:{},shadowMapSize:{},shadowCameraNear:{},shadowCameraFar:{}}},pointShadowMatrix:{value:[]},hemisphereLights:{value:[],properties:{direction:{},skyColor:{},groundColor:{}}},rectAreaLights:{value:[],properties:{color:{},position:{},width:{},height:{}}},ltc_1:{value:null},ltc_2:{value:null},probesSH:{value:null},probesMin:{value:new A},probesMax:{value:new A},probesResolution:{value:new A}},points:{diffuse:{value:new Re(16777215)},opacity:{value:1},size:{value:1},scale:{value:1},map:{value:null},alphaMap:{value:null},alphaMapTransform:{value:new Ue},alphaTest:{value:0},uvTransform:{value:new Ue}},sprite:{diffuse:{value:new Re(16777215)},opacity:{value:1},center:{value:new Ne(.5,.5)},rotation:{value:0},map:{value:null},mapTransform:{value:new Ue},alphaMap:{value:null},alphaMapTransform:{value:new Ue},alphaTest:{value:0}}},ui={basic:{uniforms:Ot([he.common,he.specularmap,he.envmap,he.aomap,he.lightmap,he.fog]),vertexShader:ze.meshbasic_vert,fragmentShader:ze.meshbasic_frag},lambert:{uniforms:Ot([he.common,he.specularmap,he.envmap,he.aomap,he.lightmap,he.emissivemap,he.bumpmap,he.normalmap,he.displacementmap,he.fog,he.lights,{emissive:{value:new Re(0)},envMapIntensity:{value:1}}]),vertexShader:ze.meshlambert_vert,fragmentShader:ze.meshlambert_frag},phong:{uniforms:Ot([he.common,he.specularmap,he.envmap,he.aomap,he.lightmap,he.emissivemap,he.bumpmap,he.normalmap,he.displacementmap,he.fog,he.lights,{emissive:{value:new Re(0)},specular:{value:new Re(1118481)},shininess:{value:30},envMapIntensity:{value:1}}]),vertexShader:ze.meshphong_vert,fragmentShader:ze.meshphong_frag},standard:{uniforms:Ot([he.common,he.envmap,he.aomap,he.lightmap,he.emissivemap,he.bumpmap,he.normalmap,he.displacementmap,he.roughnessmap,he.metalnessmap,he.fog,he.lights,{emissive:{value:new Re(0)},roughness:{value:1},metalness:{value:0},envMapIntensity:{value:1}}]),vertexShader:ze.meshphysical_vert,fragmentShader:ze.meshphysical_frag},toon:{uniforms:Ot([he.common,he.aomap,he.lightmap,he.emissivemap,he.bumpmap,he.normalmap,he.displacementmap,he.gradientmap,he.fog,he.lights,{emissive:{value:new Re(0)}}]),vertexShader:ze.meshtoon_vert,fragmentShader:ze.meshtoon_frag},matcap:{uniforms:Ot([he.common,he.bumpmap,he.normalmap,he.displacementmap,he.fog,{matcap:{value:null}}]),vertexShader:ze.meshmatcap_vert,fragmentShader:ze.meshmatcap_frag},points:{uniforms:Ot([he.points,he.fog]),vertexShader:ze.points_vert,fragmentShader:ze.points_frag},dashed:{uniforms:Ot([he.common,he.fog,{scale:{value:1},dashSize:{value:1},totalSize:{value:2}}]),vertexShader:ze.linedashed_vert,fragmentShader:ze.linedashed_frag},depth:{uniforms:Ot([he.common,he.displacementmap]),vertexShader:ze.depth_vert,fragmentShader:ze.depth_frag},normal:{uniforms:Ot([he.common,he.bumpmap,he.normalmap,he.displacementmap,{opacity:{value:1}}]),vertexShader:ze.meshnormal_vert,fragmentShader:ze.meshnormal_frag},sprite:{uniforms:Ot([he.sprite,he.fog]),vertexShader:ze.sprite_vert,fragmentShader:ze.sprite_frag},background:{uniforms:{uvTransform:{value:new Ue},t2D:{value:null},backgroundIntensity:{value:1}},vertexShader:ze.background_vert,fragmentShader:ze.background_frag},backgroundCube:{uniforms:{envMap:{value:null},backgroundBlurriness:{value:0},backgroundIntensity:{value:1},backgroundRotation:{value:new Ue}},vertexShader:ze.backgroundCube_vert,fragmentShader:ze.backgroundCube_frag},cube:{uniforms:{tCube:{value:null},tFlip:{value:-1},opacity:{value:1}},vertexShader:ze.cube_vert,fragmentShader:ze.cube_frag},equirect:{uniforms:{tEquirect:{value:null}},vertexShader:ze.equirect_vert,fragmentShader:ze.equirect_frag},distance:{uniforms:Ot([he.common,he.displacementmap,{referencePosition:{value:new A},nearDistance:{value:1},farDistance:{value:1e3}}]),vertexShader:ze.distance_vert,fragmentShader:ze.distance_frag},shadow:{uniforms:Ot([he.lights,he.fog,{color:{value:new Re(0)},opacity:{value:1}}]),vertexShader:ze.shadow_vert,fragmentShader:ze.shadow_frag}};ui.physical={uniforms:Ot([ui.standard.uniforms,{clearcoat:{value:0},clearcoatMap:{value:null},clearcoatMapTransform:{value:new Ue},clearcoatNormalMap:{value:null},clearcoatNormalMapTransform:{value:new Ue},clearcoatNormalScale:{value:new Ne(1,1)},clearcoatRoughness:{value:0},clearcoatRoughnessMap:{value:null},clearcoatRoughnessMapTransform:{value:new Ue},dispersion:{value:0},iridescence:{value:0},iridescenceMap:{value:null},iridescenceMapTransform:{value:new Ue},iridescenceIOR:{value:1.3},iridescenceThicknessMinimum:{value:100},iridescenceThicknessMaximum:{value:400},iridescenceThicknessMap:{value:null},iridescenceThicknessMapTransform:{value:new Ue},sheen:{value:0},sheenColor:{value:new Re(0)},sheenColorMap:{value:null},sheenColorMapTransform:{value:new Ue},sheenRoughness:{value:1},sheenRoughnessMap:{value:null},sheenRoughnessMapTransform:{value:new Ue},transmission:{value:0},transmissionMap:{value:null},transmissionMapTransform:{value:new Ue},transmissionSamplerSize:{value:new Ne},transmissionSamplerMap:{value:null},thickness:{value:0},thicknessMap:{value:null},thicknessMapTransform:{value:new Ue},attenuationDistance:{value:0},attenuationColor:{value:new Re(0)},specularColor:{value:new Re(1,1,1)},specularColorMap:{value:null},specularColorMapTransform:{value:new Ue},specularIntensity:{value:1},specularIntensityMap:{value:null},specularIntensityMapTransform:{value:new Ue},anisotropyVector:{value:new Ne},anisotropyMap:{value:null},anisotropyMapTransform:{value:new Ue}}]),vertexShader:ze.meshphysical_vert,fragmentShader:ze.meshphysical_frag};const Vs={r:0,b:0,g:0},Jd=new lt,ul=new Ue;ul.set(-1,0,0,0,1,0,0,0,1);function Qd(n,e,t,i,s,r){const a=new Re(0);let o=s===!0?0:1,l,c,h=null,d=0,u=null;function m(M){let y=M.isScene===!0?M.background:null;if(y&&y.isTexture){const E=M.backgroundBlurriness>0;y=e.get(y,E)}return y}function _(M){let y=!1;const E=m(M);E===null?p(a,o):E&&E.isColor&&(p(E,1),y=!0);const L=n.xr.getEnvironmentBlendMode();L==="additive"?t.buffers.color.setClear(0,0,0,1,r):L==="alpha-blend"&&t.buffers.color.setClear(0,0,0,0,r),(n.autoClear||y)&&(t.buffers.depth.setTest(!0),t.buffers.depth.setMask(!0),t.buffers.color.setMask(!0),n.clear(n.autoClearColor,n.autoClearDepth,n.autoClearStencil))}function x(M,y){const E=m(y);E&&(E.isCubeTexture||E.mapping===es)?(c===void 0&&(c=new Ht(new Yn(1,1,1),new ci({name:"BackgroundCubeMaterial",uniforms:Tn(ui.backgroundCube.uniforms),vertexShader:ui.backgroundCube.vertexShader,fragmentShader:ui.backgroundCube.fragmentShader,side:vt,depthTest:!1,depthWrite:!1,fog:!1,allowOverride:!1})),c.geometry.deleteAttribute("normal"),c.geometry.deleteAttribute("uv"),c.onBeforeRender=function(L,w,D){this.matrixWorld.copyPosition(D.matrixWorld)},Object.defineProperty(c.material,"envMap",{get:function(){return this.uniforms.envMap.value}}),i.update(c)),c.material.uniforms.envMap.value=E,c.material.uniforms.backgroundBlurriness.value=y.backgroundBlurriness,c.material.uniforms.backgroundIntensity.value=y.backgroundIntensity,c.material.uniforms.backgroundRotation.value.setFromMatrix4(Jd.makeRotationFromEuler(y.backgroundRotation)).transpose(),E.isCubeTexture&&E.isRenderTargetTexture===!1&&c.material.uniforms.backgroundRotation.value.premultiply(ul),c.material.toneMapped=Xe.getTransfer(E.colorSpace)!==Qe,(h!==E||d!==E.version||u!==n.toneMapping)&&(c.material.needsUpdate=!0,h=E,d=E.version,u=n.toneMapping),c.layers.enableAll(),M.unshift(c,c.geometry,c.material,0,0,null)):E&&E.isTexture&&(l===void 0&&(l=new Ht(new wn(2,2),new ci({name:"BackgroundMaterial",uniforms:Tn(ui.background.uniforms),vertexShader:ui.background.vertexShader,fragmentShader:ui.background.fragmentShader,side:ni,depthTest:!1,depthWrite:!1,fog:!1,allowOverride:!1})),l.geometry.deleteAttribute("normal"),Object.defineProperty(l.material,"map",{get:function(){return this.uniforms.t2D.value}}),i.update(l)),l.material.uniforms.t2D.value=E,l.material.uniforms.backgroundIntensity.value=y.backgroundIntensity,l.material.toneMapped=Xe.getTransfer(E.colorSpace)!==Qe,E.matrixAutoUpdate===!0&&E.updateMatrix(),l.material.uniforms.uvTransform.value.copy(E.matrix),(h!==E||d!==E.version||u!==n.toneMapping)&&(l.material.needsUpdate=!0,h=E,d=E.version,u=n.toneMapping),l.layers.enableAll(),M.unshift(l,l.geometry,l.material,0,0,null))}function p(M,y){M.getRGB(Vs,Jo(n)),t.buffers.color.setClear(Vs.r,Vs.g,Vs.b,y,r)}function f(){c!==void 0&&(c.geometry.dispose(),c.material.dispose(),c=void 0),l!==void 0&&(l.geometry.dispose(),l.material.dispose(),l=void 0)}return{getClearColor:function(){return a},setClearColor:function(M,y=1){a.set(M),o=y,p(a,o)},getClearAlpha:function(){return o},setClearAlpha:function(M){o=M,p(a,o)},render:_,addToRenderList:x,dispose:f}}function ef(n,e){const t=n.getParameter(n.MAX_VERTEX_ATTRIBS),i={},s=u(null);let r=s,a=!1;function o(C,z,X,j,N){let V=!1;const H=d(C,j,X,z);r!==H&&(r=H,c(r.object)),V=m(C,j,X,N),V&&_(C,j,X,N),N!==null&&e.update(N,n.ELEMENT_ARRAY_BUFFER),(V||a)&&(a=!1,E(C,z,X,j),N!==null&&n.bindBuffer(n.ELEMENT_ARRAY_BUFFER,e.get(N).buffer))}function l(){return n.createVertexArray()}function c(C){return n.bindVertexArray(C)}function h(C){return n.deleteVertexArray(C)}function d(C,z,X,j){const N=j.wireframe===!0;let V=i[z.id];V===void 0&&(V={},i[z.id]=V);const H=C.isInstancedMesh===!0?C.id:0;let K=V[H];K===void 0&&(K={},V[H]=K);let ee=K[X.id];ee===void 0&&(ee={},K[X.id]=ee);let ce=ee[N];return ce===void 0&&(ce=u(l()),ee[N]=ce),ce}function u(C){const z=[],X=[],j=[];for(let N=0;N<t;N++)z[N]=0,X[N]=0,j[N]=0;return{geometry:null,program:null,wireframe:!1,newAttributes:z,enabledAttributes:X,attributeDivisors:j,object:C,attributes:{},index:null}}function m(C,z,X,j){const N=r.attributes,V=z.attributes;let H=0;const K=X.getAttributes();for(const ee in K)if(K[ee].location>=0){const be=N[ee];let Me=V[ee];if(Me===void 0&&(ee==="instanceMatrix"&&C.instanceMatrix&&(Me=C.instanceMatrix),ee==="instanceColor"&&C.instanceColor&&(Me=C.instanceColor)),be===void 0||be.attribute!==Me||Me&&be.data!==Me.data)return!0;H++}return r.attributesNum!==H||r.index!==j}function _(C,z,X,j){const N={},V=z.attributes;let H=0;const K=X.getAttributes();for(const ee in K)if(K[ee].location>=0){let be=V[ee];be===void 0&&(ee==="instanceMatrix"&&C.instanceMatrix&&(be=C.instanceMatrix),ee==="instanceColor"&&C.instanceColor&&(be=C.instanceColor));const Me={};Me.attribute=be,be&&be.data&&(Me.data=be.data),N[ee]=Me,H++}r.attributes=N,r.attributesNum=H,r.index=j}function x(){const C=r.newAttributes;for(let z=0,X=C.length;z<X;z++)C[z]=0}function p(C){f(C,0)}function f(C,z){const X=r.newAttributes,j=r.enabledAttributes,N=r.attributeDivisors;X[C]=1,j[C]===0&&(n.enableVertexAttribArray(C),j[C]=1),N[C]!==z&&(n.vertexAttribDivisor(C,z),N[C]=z)}function M(){const C=r.newAttributes,z=r.enabledAttributes;for(let X=0,j=z.length;X<j;X++)z[X]!==C[X]&&(n.disableVertexAttribArray(X),z[X]=0)}function y(C,z,X,j,N,V,H){H===!0?n.vertexAttribIPointer(C,z,X,N,V):n.vertexAttribPointer(C,z,X,j,N,V)}function E(C,z,X,j){x();const N=j.attributes,V=X.getAttributes(),H=z.defaultAttributeValues;for(const K in V){const ee=V[K];if(ee.location>=0){let ce=N[K];if(ce===void 0&&(K==="instanceMatrix"&&C.instanceMatrix&&(ce=C.instanceMatrix),K==="instanceColor"&&C.instanceColor&&(ce=C.instanceColor)),ce!==void 0){const be=ce.normalized,Me=ce.itemSize,He=e.get(ce);if(He===void 0)continue;const Ze=He.buffer,De=He.type,Z=He.bytesPerElement,ue=De===n.INT||De===n.UNSIGNED_INT||ce.gpuType===ur;if(ce.isInterleavedBufferAttribute){const te=ce.data,Te=te.stride,Le=ce.offset;if(te.isInstancedInterleavedBuffer){for(let Ae=0;Ae<ee.locationSize;Ae++)f(ee.location+Ae,te.meshPerAttribute);C.isInstancedMesh!==!0&&j._maxInstanceCount===void 0&&(j._maxInstanceCount=te.meshPerAttribute*te.count)}else for(let Ae=0;Ae<ee.locationSize;Ae++)p(ee.location+Ae);n.bindBuffer(n.ARRAY_BUFFER,Ze);for(let Ae=0;Ae<ee.locationSize;Ae++)y(ee.location+Ae,Me/ee.locationSize,De,be,Te*Z,(Le+Me/ee.locationSize*Ae)*Z,ue)}else{if(ce.isInstancedBufferAttribute){for(let te=0;te<ee.locationSize;te++)f(ee.location+te,ce.meshPerAttribute);C.isInstancedMesh!==!0&&j._maxInstanceCount===void 0&&(j._maxInstanceCount=ce.meshPerAttribute*ce.count)}else for(let te=0;te<ee.locationSize;te++)p(ee.location+te);n.bindBuffer(n.ARRAY_BUFFER,Ze);for(let te=0;te<ee.locationSize;te++)y(ee.location+te,Me/ee.locationSize,De,be,Me*Z,Me/ee.locationSize*te*Z,ue)}}else if(H!==void 0){const be=H[K];if(be!==void 0)switch(be.length){case 2:n.vertexAttrib2fv(ee.location,be);break;case 3:n.vertexAttrib3fv(ee.location,be);break;case 4:n.vertexAttrib4fv(ee.location,be);break;default:n.vertexAttrib1fv(ee.location,be)}}}}M()}function L(){T();for(const C in i){const z=i[C];for(const X in z){const j=z[X];for(const N in j){const V=j[N];for(const H in V)h(V[H].object),delete V[H];delete j[N]}}delete i[C]}}function w(C){if(i[C.id]===void 0)return;const z=i[C.id];for(const X in z){const j=z[X];for(const N in j){const V=j[N];for(const H in V)h(V[H].object),delete V[H];delete j[N]}}delete i[C.id]}function D(C){for(const z in i){const X=i[z];for(const j in X){const N=X[j];if(N[C.id]===void 0)continue;const V=N[C.id];for(const H in V)h(V[H].object),delete V[H];delete N[C.id]}}}function b(C){for(const z in i){const X=i[z],j=C.isInstancedMesh===!0?C.id:0,N=X[j];if(N!==void 0){for(const V in N){const H=N[V];for(const K in H)h(H[K].object),delete H[K];delete N[V]}delete X[j],Object.keys(X).length===0&&delete i[z]}}}function T(){U(),a=!0,r!==s&&(r=s,c(r.object))}function U(){s.geometry=null,s.program=null,s.wireframe=!1}return{setup:o,reset:T,resetDefaultState:U,dispose:L,releaseStatesOfGeometry:w,releaseStatesOfObject:b,releaseStatesOfProgram:D,initAttributes:x,enableAttribute:p,disableUnusedAttributes:M}}function tf(n,e,t){let i;function s(l){i=l}function r(l,c){n.drawArrays(i,l,c),t.update(c,i,1)}function a(l,c,h){h!==0&&(n.drawArraysInstanced(i,l,c,h),t.update(c,i,h))}function o(l,c,h){if(h===0)return;e.get("WEBGL_multi_draw").multiDrawArraysWEBGL(i,l,0,c,0,h);let u=0;for(let m=0;m<h;m++)u+=c[m];t.update(u,i,1)}this.setMode=s,this.render=r,this.renderInstances=a,this.renderMultiDraw=o}function nf(n,e,t,i){let s;function r(){if(s!==void 0)return s;if(e.has("EXT_texture_filter_anisotropic")===!0){const D=e.get("EXT_texture_filter_anisotropic");s=n.getParameter(D.MAX_TEXTURE_MAX_ANISOTROPY_EXT)}else s=0;return s}function a(D){return!(D!==Qt&&i.convert(D)!==n.getParameter(n.IMPLEMENTATION_COLOR_READ_FORMAT))}function o(D){const b=D===vi&&(e.has("EXT_color_buffer_half_float")||e.has("EXT_color_buffer_float"));return!(D!==kt&&i.convert(D)!==n.getParameter(n.IMPLEMENTATION_COLOR_READ_TYPE)&&D!==ai&&!b)}function l(D){if(D==="highp"){if(n.getShaderPrecisionFormat(n.VERTEX_SHADER,n.HIGH_FLOAT).precision>0&&n.getShaderPrecisionFormat(n.FRAGMENT_SHADER,n.HIGH_FLOAT).precision>0)return"highp";D="mediump"}return D==="mediump"&&n.getShaderPrecisionFormat(n.VERTEX_SHADER,n.MEDIUM_FLOAT).precision>0&&n.getShaderPrecisionFormat(n.FRAGMENT_SHADER,n.MEDIUM_FLOAT).precision>0?"mediump":"lowp"}let c=t.precision!==void 0?t.precision:"highp";const h=l(c);h!==c&&(Ce("WebGLRenderer:",c,"not supported, using",h,"instead."),c=h);const d=t.logarithmicDepthBuffer===!0,u=t.reversedDepthBuffer===!0&&e.has("EXT_clip_control");t.reversedDepthBuffer===!0&&u===!1&&Ce("WebGLRenderer: Unable to use reversed depth buffer due to missing EXT_clip_control extension. Fallback to default depth buffer.");const m=n.getParameter(n.MAX_TEXTURE_IMAGE_UNITS),_=n.getParameter(n.MAX_VERTEX_TEXTURE_IMAGE_UNITS),x=n.getParameter(n.MAX_TEXTURE_SIZE),p=n.getParameter(n.MAX_CUBE_MAP_TEXTURE_SIZE),f=n.getParameter(n.MAX_VERTEX_ATTRIBS),M=n.getParameter(n.MAX_VERTEX_UNIFORM_VECTORS),y=n.getParameter(n.MAX_VARYING_VECTORS),E=n.getParameter(n.MAX_FRAGMENT_UNIFORM_VECTORS),L=n.getParameter(n.MAX_SAMPLES),w=n.getParameter(n.SAMPLES);return{isWebGL2:!0,getMaxAnisotropy:r,getMaxPrecision:l,textureFormatReadable:a,textureTypeReadable:o,precision:c,logarithmicDepthBuffer:d,reversedDepthBuffer:u,maxTextures:m,maxVertexTextures:_,maxTextureSize:x,maxCubemapSize:p,maxAttributes:f,maxVertexUniforms:M,maxVaryings:y,maxFragmentUniforms:E,maxSamples:L,samples:w}}function sf(n){const e=this;let t=null,i=0,s=!1,r=!1;const a=new Ti,o=new Ue,l={value:null,needsUpdate:!1};this.uniform=l,this.numPlanes=0,this.numIntersection=0,this.init=function(d,u){const m=d.length!==0||u||i!==0||s;return s=u,i=d.length,m},this.beginShadows=function(){r=!0,h(null)},this.endShadows=function(){r=!1},this.setGlobalState=function(d,u){t=h(d,u,0)},this.setState=function(d,u,m){const _=d.clippingPlanes,x=d.clipIntersection,p=d.clipShadows,f=n.get(d);if(!s||_===null||_.length===0||r&&!p)r?h(null):c();else{const M=r?0:i,y=M*4;let E=f.clippingState||null;l.value=E,E=h(_,u,y,m);for(let L=0;L!==y;++L)E[L]=t[L];f.clippingState=E,this.numIntersection=x?this.numPlanes:0,this.numPlanes+=M}};function c(){l.value!==t&&(l.value=t,l.needsUpdate=i>0),e.numPlanes=i,e.numIntersection=0}function h(d,u,m,_){const x=d!==null?d.length:0;let p=null;if(x!==0){if(p=l.value,_!==!0||p===null){const f=m+x*4,M=u.matrixWorldInverse;o.getNormalMatrix(M),(p===null||p.length<f)&&(p=new Float32Array(f));for(let y=0,E=m;y!==x;++y,E+=4)a.copy(d[y]).applyMatrix4(M,o),a.normal.toArray(p,E),p[E+3]=a.constant}l.value=p,l.needsUpdate=!0}return e.numPlanes=x,e.numIntersection=0,p}}const ki=4,dl=[.125,.215,.35,.446,.526,.582],en=20,rf=256,$n=new ks,fl=new Re;let Ra=null,Pa=0,La=0,Da=!1;const af=new A;class pl{constructor(e){this._renderer=e,this._pingPongRenderTarget=null,this._lodMax=0,this._cubeSize=0,this._sizeLods=[],this._sigmas=[],this._lodMeshes=[],this._backgroundBox=null,this._cubemapMaterial=null,this._equirectMaterial=null,this._blurMaterial=null,this._ggxMaterial=null}fromScene(e,t=0,i=.1,s=100,r={}){const{size:a=256,position:o=af}=r;Ra=this._renderer.getRenderTarget(),Pa=this._renderer.getActiveCubeFace(),La=this._renderer.getActiveMipmapLevel(),Da=this._renderer.xr.enabled,this._renderer.xr.enabled=!1,this._setSize(a);const l=this._allocateTargets();return l.depthBuffer=!0,this._sceneToCubeUV(e,i,s,l,o),t>0&&this._blur(l,0,0,t),this._applyPMREM(l),this._cleanup(l),l}fromEquirectangular(e,t=null){return this._fromTexture(e,t)}fromCubemap(e,t=null){return this._fromTexture(e,t)}compileCubemapShader(){this._cubemapMaterial===null&&(this._cubemapMaterial=_l(),this._compileMaterial(this._cubemapMaterial))}compileEquirectangularShader(){this._equirectMaterial===null&&(this._equirectMaterial=gl(),this._compileMaterial(this._equirectMaterial))}dispose(){this._dispose(),this._cubemapMaterial!==null&&this._cubemapMaterial.dispose(),this._equirectMaterial!==null&&this._equirectMaterial.dispose(),this._backgroundBox!==null&&(this._backgroundBox.geometry.dispose(),this._backgroundBox.material.dispose())}_setSize(e){this._lodMax=Math.floor(Math.log2(e)),this._cubeSize=Math.pow(2,this._lodMax)}_dispose(){this._blurMaterial!==null&&this._blurMaterial.dispose(),this._ggxMaterial!==null&&this._ggxMaterial.dispose(),this._pingPongRenderTarget!==null&&this._pingPongRenderTarget.dispose();for(let e=0;e<this._lodMeshes.length;e++)this._lodMeshes[e].geometry.dispose()}_cleanup(e){this._renderer.setRenderTarget(Ra,Pa,La),this._renderer.xr.enabled=Da,e.scissorTest=!1,Rn(e,0,0,e.width,e.height)}_fromTexture(e,t){e.mapping===Xi||e.mapping===on?this._setSize(e.image.length===0?16:e.image[0].width||e.image[0].image.width):this._setSize(e.image.width/4),Ra=this._renderer.getRenderTarget(),Pa=this._renderer.getActiveCubeFace(),La=this._renderer.getActiveMipmapLevel(),Da=this._renderer.xr.enabled,this._renderer.xr.enabled=!1;const i=t||this._allocateTargets();return this._textureToCubeUV(e,i),this._applyPMREM(i),this._cleanup(i),i}_allocateTargets(){const e=3*Math.max(this._cubeSize,112),t=4*this._cubeSize,i={magFilter:Ct,minFilter:Ct,generateMipmaps:!1,type:vi,format:Qt,colorSpace:ls,depthBuffer:!1},s=ml(e,t,i);if(this._pingPongRenderTarget===null||this._pingPongRenderTarget.width!==e||this._pingPongRenderTarget.height!==t){this._pingPongRenderTarget!==null&&this._dispose(),this._pingPongRenderTarget=ml(e,t,i);const{_lodMax:r}=this;({lodMeshes:this._lodMeshes,sizeLods:this._sizeLods,sigmas:this._sigmas}=of(r)),this._blurMaterial=cf(r,e,t),this._ggxMaterial=lf(r,e,t)}return s}_compileMaterial(e){const t=new Ht(new _t,e);this._renderer.compile(t,$n)}_sceneToCubeUV(e,t,i,s,r){const l=new $t(90,1,t,i),c=[1,-1,1,1,1,1],h=[1,1,1,-1,-1,-1],d=this._renderer,u=d.autoClear,m=d.toneMapping;d.getClearColor(fl),d.toneMapping=si,d.autoClear=!1,d.state.buffers.depth.getReversed()&&(d.setRenderTarget(s),d.clearDepth(),d.setRenderTarget(null)),this._backgroundBox===null&&(this._backgroundBox=new Ht(new Yn,new Bi({name:"PMREM.Background",side:vt,depthWrite:!1,depthTest:!1})));const x=this._backgroundBox,p=x.material;let f=!1;const M=e.background;M?M.isColor&&(p.color.copy(M),e.background=null,f=!0):(p.color.copy(fl),f=!0);for(let y=0;y<6;y++){const E=y%3;E===0?(l.up.set(0,c[y],0),l.position.set(r.x,r.y,r.z),l.lookAt(r.x+h[y],r.y,r.z)):E===1?(l.up.set(0,0,c[y]),l.position.set(r.x,r.y,r.z),l.lookAt(r.x,r.y+h[y],r.z)):(l.up.set(0,c[y],0),l.position.set(r.x,r.y,r.z),l.lookAt(r.x,r.y,r.z+h[y]));const L=this._cubeSize;Rn(s,E*L,y>2?L:0,L,L),d.setRenderTarget(s),f&&d.render(x,l),d.render(e,l)}d.toneMapping=m,d.autoClear=u,e.background=M}_textureToCubeUV(e,t){const i=this._renderer,s=e.mapping===Xi||e.mapping===on;s?(this._cubemapMaterial===null&&(this._cubemapMaterial=_l()),this._cubemapMaterial.uniforms.flipEnvMap.value=e.isRenderTargetTexture===!1?-1:1):this._equirectMaterial===null&&(this._equirectMaterial=gl());const r=s?this._cubemapMaterial:this._equirectMaterial,a=this._lodMeshes[0];a.material=r;const o=r.uniforms;o.envMap.value=e;const l=this._cubeSize;Rn(t,0,0,3*l,2*l),i.setRenderTarget(t),i.render(a,$n)}_applyPMREM(e){const t=this._renderer,i=t.autoClear;t.autoClear=!1;const s=this._lodMeshes.length;for(let r=1;r<s;r++)this._applyGGXFilter(e,r-1,r);t.autoClear=i}_applyGGXFilter(e,t,i){const s=this._renderer,r=this._pingPongRenderTarget,a=this._ggxMaterial,o=this._lodMeshes[i];o.material=a;const l=a.uniforms,c=i/(this._lodMeshes.length-1),h=t/(this._lodMeshes.length-1),d=Math.sqrt(c*c-h*h),u=0+c*1.25,m=d*u,{_lodMax:_}=this,x=this._sizeLods[i],p=3*x*(i>_-ki?i-_+ki:0),f=4*(this._cubeSize-x);l.envMap.value=e.texture,l.roughness.value=m,l.mipInt.value=_-t,Rn(r,p,f,3*x,2*x),s.setRenderTarget(r),s.render(o,$n),l.envMap.value=r.texture,l.roughness.value=0,l.mipInt.value=_-i,Rn(e,p,f,3*x,2*x),s.setRenderTarget(e),s.render(o,$n)}_blur(e,t,i,s,r){const a=this._pingPongRenderTarget;this._halfBlur(e,a,t,i,s,"latitudinal",r),this._halfBlur(a,e,i,i,s,"longitudinal",r)}_halfBlur(e,t,i,s,r,a,o){const l=this._renderer,c=this._blurMaterial;a!=="latitudinal"&&a!=="longitudinal"&&je("blur direction must be either latitudinal or longitudinal!");const h=3,d=this._lodMeshes[s];d.material=c;const u=c.uniforms,m=this._sizeLods[i]-1,_=isFinite(r)?Math.PI/(2*m):2*Math.PI/(2*en-1),x=r/_,p=isFinite(r)?1+Math.floor(h*x):en;p>en&&Ce(`sigmaRadians, ${r}, is too large and will clip, as it requested ${p} samples when the maximum is set to ${en}`);const f=[];let M=0;for(let D=0;D<en;++D){const b=D/x,T=Math.exp(-b*b/2);f.push(T),D===0?M+=T:D<p&&(M+=2*T)}for(let D=0;D<f.length;D++)f[D]=f[D]/M;u.envMap.value=e.texture,u.samples.value=p,u.weights.value=f,u.latitudinal.value=a==="latitudinal",o&&(u.poleAxis.value=o);const{_lodMax:y}=this;u.dTheta.value=_,u.mipInt.value=y-i;const E=this._sizeLods[s],L=3*E*(s>y-ki?s-y+ki:0),w=4*(this._cubeSize-E);Rn(t,L,w,3*E,2*E),l.setRenderTarget(t),l.render(d,$n)}}function of(n){const e=[],t=[],i=[];let s=n;const r=n-ki+1+dl.length;for(let a=0;a<r;a++){const o=Math.pow(2,s);e.push(o);let l=1/o;a>n-ki?l=dl[a-n+ki-1]:a===0&&(l=0),t.push(l);const c=1/(o-2),h=-c,d=1+c,u=[h,h,d,h,d,d,h,h,d,d,h,d],m=6,_=6,x=3,p=2,f=1,M=new Float32Array(x*_*m),y=new Float32Array(p*_*m),E=new Float32Array(f*_*m);for(let w=0;w<m;w++){const D=w%3*2/3-1,b=w>2?0:-1,T=[D,b,0,D+2/3,b,0,D+2/3,b+1,0,D,b,0,D+2/3,b+1,0,D,b+1,0];M.set(T,x*_*w),y.set(u,p*_*w);const U=[w,w,w,w,w,w];E.set(U,f*_*w)}const L=new _t;L.setAttribute("position",new Lt(M,x)),L.setAttribute("uv",new Lt(y,p)),L.setAttribute("faceIndex",new Lt(E,f)),i.push(new Ht(L,null)),s>ki&&s--}return{lodMeshes:i,sizeLods:e,sigmas:t}}function ml(n,e,t){const i=new li(n,e,t);return i.texture.mapping=es,i.texture.name="PMREM.cubeUv",i.scissorTest=!0,i}function Rn(n,e,t,i,s){n.viewport.set(e,t,i,s),n.scissor.set(e,t,i,s)}function lf(n,e,t){return new ci({name:"PMREMGGXConvolution",defines:{GGX_SAMPLES:rf,CUBEUV_TEXEL_WIDTH:1/e,CUBEUV_TEXEL_HEIGHT:1/t,CUBEUV_MAX_MIP:`${n}.0`},uniforms:{envMap:{value:null},roughness:{value:0},mipInt:{value:0}},vertexShader:Gs(),fragmentShader:`

			precision highp float;
			precision highp int;

			varying vec3 vOutputDirection;

			uniform sampler2D envMap;
			uniform float roughness;
			uniform float mipInt;

			#define ENVMAP_TYPE_CUBE_UV
			#include <cube_uv_reflection_fragment>

			#define PI 3.14159265359

			// Van der Corput radical inverse
			float radicalInverse_VdC(uint bits) {
				bits = (bits << 16u) | (bits >> 16u);
				bits = ((bits & 0x55555555u) << 1u) | ((bits & 0xAAAAAAAAu) >> 1u);
				bits = ((bits & 0x33333333u) << 2u) | ((bits & 0xCCCCCCCCu) >> 2u);
				bits = ((bits & 0x0F0F0F0Fu) << 4u) | ((bits & 0xF0F0F0F0u) >> 4u);
				bits = ((bits & 0x00FF00FFu) << 8u) | ((bits & 0xFF00FF00u) >> 8u);
				return float(bits) * 2.3283064365386963e-10; // / 0x100000000
			}

			// Hammersley sequence
			vec2 hammersley(uint i, uint N) {
				return vec2(float(i) / float(N), radicalInverse_VdC(i));
			}

			// GGX VNDF importance sampling (Eric Heitz 2018)
			// "Sampling the GGX Distribution of Visible Normals"
			// https://jcgt.org/published/0007/04/01/
			vec3 importanceSampleGGX_VNDF(vec2 Xi, vec3 V, float roughness) {
				float alpha = roughness * roughness;

				// Section 4.1: Orthonormal basis
				vec3 T1 = vec3(1.0, 0.0, 0.0);
				vec3 T2 = cross(V, T1);

				// Section 4.2: Parameterization of projected area
				float r = sqrt(Xi.x);
				float phi = 2.0 * PI * Xi.y;
				float t1 = r * cos(phi);
				float t2 = r * sin(phi);
				float s = 0.5 * (1.0 + V.z);
				t2 = (1.0 - s) * sqrt(1.0 - t1 * t1) + s * t2;

				// Section 4.3: Reprojection onto hemisphere
				vec3 Nh = t1 * T1 + t2 * T2 + sqrt(max(0.0, 1.0 - t1 * t1 - t2 * t2)) * V;

				// Section 3.4: Transform back to ellipsoid configuration
				return normalize(vec3(alpha * Nh.x, alpha * Nh.y, max(0.0, Nh.z)));
			}

			void main() {
				vec3 N = normalize(vOutputDirection);
				vec3 V = N; // Assume view direction equals normal for pre-filtering

				vec3 prefilteredColor = vec3(0.0);
				float totalWeight = 0.0;

				// For very low roughness, just sample the environment directly
				if (roughness < 0.001) {
					gl_FragColor = vec4(bilinearCubeUV(envMap, N, mipInt), 1.0);
					return;
				}

				// Tangent space basis for VNDF sampling
				vec3 up = abs(N.z) < 0.999 ? vec3(0.0, 0.0, 1.0) : vec3(1.0, 0.0, 0.0);
				vec3 tangent = normalize(cross(up, N));
				vec3 bitangent = cross(N, tangent);

				for(uint i = 0u; i < uint(GGX_SAMPLES); i++) {
					vec2 Xi = hammersley(i, uint(GGX_SAMPLES));

					// For PMREM, V = N, so in tangent space V is always (0, 0, 1)
					vec3 H_tangent = importanceSampleGGX_VNDF(Xi, vec3(0.0, 0.0, 1.0), roughness);

					// Transform H back to world space
					vec3 H = normalize(tangent * H_tangent.x + bitangent * H_tangent.y + N * H_tangent.z);
					vec3 L = normalize(2.0 * dot(V, H) * H - V);

					float NdotL = max(dot(N, L), 0.0);

					if(NdotL > 0.0) {
						// Sample environment at fixed mip level
						// VNDF importance sampling handles the distribution filtering
						vec3 sampleColor = bilinearCubeUV(envMap, L, mipInt);

						// Weight by NdotL for the split-sum approximation
						// VNDF PDF naturally accounts for the visible microfacet distribution
						prefilteredColor += sampleColor * NdotL;
						totalWeight += NdotL;
					}
				}

				if (totalWeight > 0.0) {
					prefilteredColor = prefilteredColor / totalWeight;
				}

				gl_FragColor = vec4(prefilteredColor, 1.0);
			}
		`,blending:gi,depthTest:!1,depthWrite:!1})}function cf(n,e,t){const i=new Float32Array(en),s=new A(0,1,0);return new ci({name:"SphericalGaussianBlur",defines:{n:en,CUBEUV_TEXEL_WIDTH:1/e,CUBEUV_TEXEL_HEIGHT:1/t,CUBEUV_MAX_MIP:`${n}.0`},uniforms:{envMap:{value:null},samples:{value:1},weights:{value:i},latitudinal:{value:!1},dTheta:{value:0},mipInt:{value:0},poleAxis:{value:s}},vertexShader:Gs(),fragmentShader:`

			precision mediump float;
			precision mediump int;

			varying vec3 vOutputDirection;

			uniform sampler2D envMap;
			uniform int samples;
			uniform float weights[ n ];
			uniform bool latitudinal;
			uniform float dTheta;
			uniform float mipInt;
			uniform vec3 poleAxis;

			#define ENVMAP_TYPE_CUBE_UV
			#include <cube_uv_reflection_fragment>

			vec3 getSample( float theta, vec3 axis ) {

				float cosTheta = cos( theta );
				// Rodrigues' axis-angle rotation
				vec3 sampleDirection = vOutputDirection * cosTheta
					+ cross( axis, vOutputDirection ) * sin( theta )
					+ axis * dot( axis, vOutputDirection ) * ( 1.0 - cosTheta );

				return bilinearCubeUV( envMap, sampleDirection, mipInt );

			}

			void main() {

				vec3 axis = latitudinal ? poleAxis : cross( poleAxis, vOutputDirection );

				if ( all( equal( axis, vec3( 0.0 ) ) ) ) {

					axis = vec3( vOutputDirection.z, 0.0, - vOutputDirection.x );

				}

				axis = normalize( axis );

				gl_FragColor = vec4( 0.0, 0.0, 0.0, 1.0 );
				gl_FragColor.rgb += weights[ 0 ] * getSample( 0.0, axis );

				for ( int i = 1; i < n; i++ ) {

					if ( i >= samples ) {

						break;

					}

					float theta = dTheta * float( i );
					gl_FragColor.rgb += weights[ i ] * getSample( -1.0 * theta, axis );
					gl_FragColor.rgb += weights[ i ] * getSample( theta, axis );

				}

			}
		`,blending:gi,depthTest:!1,depthWrite:!1})}function gl(){return new ci({name:"EquirectangularToCubeUV",uniforms:{envMap:{value:null}},vertexShader:Gs(),fragmentShader:`

			precision mediump float;
			precision mediump int;

			varying vec3 vOutputDirection;

			uniform sampler2D envMap;

			#include <common>

			void main() {

				vec3 outputDirection = normalize( vOutputDirection );
				vec2 uv = equirectUv( outputDirection );

				gl_FragColor = vec4( texture2D ( envMap, uv ).rgb, 1.0 );

			}
		`,blending:gi,depthTest:!1,depthWrite:!1})}function _l(){return new ci({name:"CubemapToCubeUV",uniforms:{envMap:{value:null},flipEnvMap:{value:-1}},vertexShader:Gs(),fragmentShader:`

			precision mediump float;
			precision mediump int;

			uniform float flipEnvMap;

			varying vec3 vOutputDirection;

			uniform samplerCube envMap;

			void main() {

				gl_FragColor = textureCube( envMap, vec3( flipEnvMap * vOutputDirection.x, vOutputDirection.yz ) );

			}
		`,blending:gi,depthTest:!1,depthWrite:!1})}function Gs(){return`

		precision mediump float;
		precision mediump int;

		attribute float faceIndex;

		varying vec3 vOutputDirection;

		// RH coordinate system; PMREM face-indexing convention
		vec3 getDirection( vec2 uv, float face ) {

			uv = 2.0 * uv - 1.0;

			vec3 direction = vec3( uv, 1.0 );

			if ( face == 0.0 ) {

				direction = direction.zyx; // ( 1, v, u ) pos x

			} else if ( face == 1.0 ) {

				direction = direction.xzy;
				direction.xz *= -1.0; // ( -u, 1, -v ) pos y

			} else if ( face == 2.0 ) {

				direction.x *= -1.0; // ( -u, v, 1 ) pos z

			} else if ( face == 3.0 ) {

				direction = direction.zyx;
				direction.xz *= -1.0; // ( -1, v, -u ) neg x

			} else if ( face == 4.0 ) {

				direction = direction.xzy;
				direction.xy *= -1.0; // ( -u, -1, v ) neg y

			} else if ( face == 5.0 ) {

				direction.z *= -1.0; // ( u, v, -1 ) neg z

			}

			return direction;

		}

		void main() {

			vOutputDirection = getDirection( uv, faceIndex );
			gl_Position = vec4( position, 1.0 );

		}
	`}class vl extends li{constructor(e=1,t={}){super(e,e,t),this.isWebGLCubeRenderTarget=!0;const i={width:e,height:e,depth:1},s=[i,i,i,i,i,i];this.texture=new $o(s),this._setTextureOptions(t),this.texture.isRenderTargetTexture=!0}fromEquirectangularTexture(e,t){this.texture.type=t.type,this.texture.colorSpace=t.colorSpace,this.texture.generateMipmaps=t.generateMipmaps,this.texture.minFilter=t.minFilter,this.texture.magFilter=t.magFilter;const i={uniforms:{tEquirect:{value:null}},vertexShader:`

				varying vec3 vWorldDirection;

				vec3 transformDirection( in vec3 dir, in mat4 matrix ) {

					return normalize( ( matrix * vec4( dir, 0.0 ) ).xyz );

				}

				void main() {

					vWorldDirection = transformDirection( position, modelMatrix );

					#include <begin_vertex>
					#include <project_vertex>

				}
			`,fragmentShader:`

				uniform sampler2D tEquirect;

				varying vec3 vWorldDirection;

				#include <common>

				void main() {

					vec3 direction = normalize( vWorldDirection );

					vec2 sampleUV = equirectUv( direction );

					gl_FragColor = texture2D( tEquirect, sampleUV );

				}
			`},s=new Yn(5,5,5),r=new ci({name:"CubemapFromEquirect",uniforms:Tn(i.uniforms),vertexShader:i.vertexShader,fragmentShader:i.fragmentShader,side:vt,blending:gi});r.uniforms.tEquirect.value=t;const a=new Ht(s,r),o=t.minFilter;return t.minFilter===ji&&(t.minFilter=Ct),new Xh(1,10,this).update(e,a),t.minFilter=o,a.geometry.dispose(),a.material.dispose(),this}clear(e,t=!0,i=!0,s=!0){const r=e.getRenderTarget();for(let a=0;a<6;a++)e.setRenderTarget(this,a),e.clear(t,i,s);e.setRenderTarget(r)}}function hf(n){let e=new WeakMap,t=new WeakMap,i=null;function s(u,m=!1){return u==null?null:m?a(u):r(u)}function r(u){if(u&&u.isTexture){const m=u.mapping;if(m===ar||m===or)if(e.has(u)){const _=e.get(u).texture;return o(_,u.mapping)}else{const _=u.image;if(_&&_.height>0){const x=new vl(_.height);return x.fromEquirectangularTexture(n,u),e.set(u,x),u.addEventListener("dispose",c),o(x.texture,u.mapping)}else return null}}return u}function a(u){if(u&&u.isTexture){const m=u.mapping,_=m===ar||m===or,x=m===Xi||m===on;if(_||x){let p=t.get(u);const f=p!==void 0?p.texture.pmremVersion:0;if(u.isRenderTargetTexture&&u.pmremVersion!==f)return i===null&&(i=new pl(n)),p=_?i.fromEquirectangular(u,p):i.fromCubemap(u,p),p.texture.pmremVersion=u.pmremVersion,t.set(u,p),p.texture;if(p!==void 0)return p.texture;{const M=u.image;return _&&M&&M.height>0||x&&M&&l(M)?(i===null&&(i=new pl(n)),p=_?i.fromEquirectangular(u):i.fromCubemap(u),p.texture.pmremVersion=u.pmremVersion,t.set(u,p),u.addEventListener("dispose",h),p.texture):null}}}return u}function o(u,m){return m===ar?u.mapping=Xi:m===or&&(u.mapping=on),u}function l(u){let m=0;const _=6;for(let x=0;x<_;x++)u[x]!==void 0&&m++;return m===_}function c(u){const m=u.target;m.removeEventListener("dispose",c);const _=e.get(m);_!==void 0&&(e.delete(m),_.dispose())}function h(u){const m=u.target;m.removeEventListener("dispose",h);const _=t.get(m);_!==void 0&&(t.delete(m),_.dispose())}function d(){e=new WeakMap,t=new WeakMap,i!==null&&(i.dispose(),i=null)}return{get:s,dispose:d}}function uf(n){const e={};function t(i){if(e[i]!==void 0)return e[i];const s=n.getExtension(i);return e[i]=s,s}return{has:function(i){return t(i)!==null},init:function(){t("EXT_color_buffer_float"),t("WEBGL_clip_cull_distance"),t("OES_texture_float_linear"),t("EXT_color_buffer_half_float"),t("WEBGL_multisampled_render_to_texture"),t("WEBGL_render_shared_exponent")},get:function(i){const s=t(i);return s===null&&Zr("WebGLRenderer: "+i+" extension not supported."),s}}}function df(n,e,t,i){const s={},r=new WeakMap;function a(d){const u=d.target;u.index!==null&&e.remove(u.index);for(const _ in u.attributes)e.remove(u.attributes[_]);u.removeEventListener("dispose",a),delete s[u.id];const m=r.get(u);m&&(e.remove(m),r.delete(u)),i.releaseStatesOfGeometry(u),u.isInstancedBufferGeometry===!0&&delete u._maxInstanceCount,t.memory.geometries--}function o(d,u){return s[u.id]===!0||(u.addEventListener("dispose",a),s[u.id]=!0,t.memory.geometries++),u}function l(d){const u=d.attributes;for(const m in u)e.update(u[m],n.ARRAY_BUFFER)}function c(d){const u=[],m=d.index,_=d.attributes.position;let x=0;if(_===void 0)return;if(m!==null){const M=m.array;x=m.version;for(let y=0,E=M.length;y<E;y+=3){const L=M[y+0],w=M[y+1],D=M[y+2];u.push(L,w,w,D,D,L)}}else{const M=_.array;x=_.version;for(let y=0,E=M.length/3-1;y<E;y+=3){const L=y+0,w=y+1,D=y+2;u.push(L,w,w,D,D,L)}}const p=new(_.count>=65535?Bo:Oo)(u,1);p.version=x;const f=r.get(d);f&&e.remove(f),r.set(d,p)}function h(d){const u=r.get(d);if(u){const m=d.index;m!==null&&u.version<m.version&&c(d)}else c(d);return r.get(d)}return{get:o,update:l,getWireframeAttribute:h}}function ff(n,e,t){let i;function s(d){i=d}let r,a;function o(d){r=d.type,a=d.bytesPerElement}function l(d,u){n.drawElements(i,u,r,d*a),t.update(u,i,1)}function c(d,u,m){m!==0&&(n.drawElementsInstanced(i,u,r,d*a,m),t.update(u,i,m))}function h(d,u,m){if(m===0)return;e.get("WEBGL_multi_draw").multiDrawElementsWEBGL(i,u,0,r,d,0,m);let x=0;for(let p=0;p<m;p++)x+=u[p];t.update(x,i,1)}this.setMode=s,this.setIndex=o,this.render=l,this.renderInstances=c,this.renderMultiDraw=h}function pf(n){const e={geometries:0,textures:0},t={frame:0,calls:0,triangles:0,points:0,lines:0};function i(r,a,o){switch(t.calls++,a){case n.TRIANGLES:t.triangles+=o*(r/3);break;case n.LINES:t.lines+=o*(r/2);break;case n.LINE_STRIP:t.lines+=o*(r-1);break;case n.LINE_LOOP:t.lines+=o*r;break;case n.POINTS:t.points+=o*r;break;default:je("WebGLInfo: Unknown draw mode:",a);break}}function s(){t.calls=0,t.triangles=0,t.points=0,t.lines=0}return{memory:e,render:t,programs:null,autoReset:!0,reset:s,update:i}}function mf(n,e,t){const i=new WeakMap,s=new ht;function r(a,o,l){const c=a.morphTargetInfluences,h=o.morphAttributes.position||o.morphAttributes.normal||o.morphAttributes.color,d=h!==void 0?h.length:0;let u=i.get(o);if(u===void 0||u.count!==d){let T=function(){D.dispose(),i.delete(o),o.removeEventListener("dispose",T)};u!==void 0&&u.texture.dispose();const m=o.morphAttributes.position!==void 0,_=o.morphAttributes.normal!==void 0,x=o.morphAttributes.color!==void 0,p=o.morphAttributes.position||[],f=o.morphAttributes.normal||[],M=o.morphAttributes.color||[];let y=0;m===!0&&(y=1),_===!0&&(y=2),x===!0&&(y=3);let E=o.attributes.position.count*y,L=1;E>e.maxTextureSize&&(L=Math.ceil(E/e.maxTextureSize),E=e.maxTextureSize);const w=new Float32Array(E*L*4*d),D=new Ao(w,E,L,d);D.type=ai,D.needsUpdate=!0;const b=y*4;for(let U=0;U<d;U++){const C=p[U],z=f[U],X=M[U],j=E*L*4*U;for(let N=0;N<C.count;N++){const V=N*b;m===!0&&(s.fromBufferAttribute(C,N),w[j+V+0]=s.x,w[j+V+1]=s.y,w[j+V+2]=s.z,w[j+V+3]=0),_===!0&&(s.fromBufferAttribute(z,N),w[j+V+4]=s.x,w[j+V+5]=s.y,w[j+V+6]=s.z,w[j+V+7]=0),x===!0&&(s.fromBufferAttribute(X,N),w[j+V+8]=s.x,w[j+V+9]=s.y,w[j+V+10]=s.z,w[j+V+11]=X.itemSize===4?s.w:1)}}u={count:d,texture:D,size:new Ne(E,L)},i.set(o,u),o.addEventListener("dispose",T)}if(a.isInstancedMesh===!0&&a.morphTexture!==null)l.getUniforms().setValue(n,"morphTexture",a.morphTexture,t);else{let m=0;for(let x=0;x<c.length;x++)m+=c[x];const _=o.morphTargetsRelative?1:1-m;l.getUniforms().setValue(n,"morphTargetBaseInfluence",_),l.getUniforms().setValue(n,"morphTargetInfluences",c)}l.getUniforms().setValue(n,"morphTargetsTexture",u.texture,t),l.getUniforms().setValue(n,"morphTargetsTextureSize",u.size)}return{update:r}}function gf(n,e,t,i,s){let r=new WeakMap;function a(c){const h=s.render.frame,d=c.geometry,u=e.get(c,d);if(r.get(u)!==h&&(e.update(u),r.set(u,h)),c.isInstancedMesh&&(c.hasEventListener("dispose",l)===!1&&c.addEventListener("dispose",l),r.get(c)!==h&&(t.update(c.instanceMatrix,n.ARRAY_BUFFER),c.instanceColor!==null&&t.update(c.instanceColor,n.ARRAY_BUFFER),r.set(c,h))),c.isSkinnedMesh){const m=c.skeleton;r.get(m)!==h&&(m.update(),r.set(m,h))}return u}function o(){r=new WeakMap}function l(c){const h=c.target;h.removeEventListener("dispose",l),i.releaseStatesOfObject(h),t.remove(h.instanceMatrix),h.instanceColor!==null&&t.remove(h.instanceColor)}return{update:a,dispose:o}}const _f={[to]:"LINEAR_TONE_MAPPING",[io]:"REINHARD_TONE_MAPPING",[no]:"CINEON_TONE_MAPPING",[so]:"ACES_FILMIC_TONE_MAPPING",[ao]:"AGX_TONE_MAPPING",[oo]:"NEUTRAL_TONE_MAPPING",[ro]:"CUSTOM_TONE_MAPPING"};function vf(n,e,t,i,s){const r=new li(e,t,{type:n,depthBuffer:i,stencilBuffer:s,depthTexture:i?new En(e,t):void 0}),a=new li(e,t,{type:vi,depthBuffer:!1,stencilBuffer:!1}),o=new _t;o.setAttribute("position",new Ft([-1,3,0,-1,-1,0,3,-1,0],3)),o.setAttribute("uv",new Ft([0,2,0,0,2,0],2));const l=new Oh({uniforms:{tDiffuse:{value:null}},vertexShader:`
			precision highp float;

			uniform mat4 modelViewMatrix;
			uniform mat4 projectionMatrix;

			attribute vec3 position;
			attribute vec2 uv;

			varying vec2 vUv;

			void main() {
				vUv = uv;
				gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );
			}`,fragmentShader:`
			precision highp float;

			uniform sampler2D tDiffuse;

			varying vec2 vUv;

			#include <tonemapping_pars_fragment>
			#include <colorspace_pars_fragment>

			void main() {
				gl_FragColor = texture2D( tDiffuse, vUv );

				#ifdef LINEAR_TONE_MAPPING
					gl_FragColor.rgb = LinearToneMapping( gl_FragColor.rgb );
				#elif defined( REINHARD_TONE_MAPPING )
					gl_FragColor.rgb = ReinhardToneMapping( gl_FragColor.rgb );
				#elif defined( CINEON_TONE_MAPPING )
					gl_FragColor.rgb = CineonToneMapping( gl_FragColor.rgb );
				#elif defined( ACES_FILMIC_TONE_MAPPING )
					gl_FragColor.rgb = ACESFilmicToneMapping( gl_FragColor.rgb );
				#elif defined( AGX_TONE_MAPPING )
					gl_FragColor.rgb = AgXToneMapping( gl_FragColor.rgb );
				#elif defined( NEUTRAL_TONE_MAPPING )
					gl_FragColor.rgb = NeutralToneMapping( gl_FragColor.rgb );
				#elif defined( CUSTOM_TONE_MAPPING )
					gl_FragColor.rgb = CustomToneMapping( gl_FragColor.rgb );
				#endif

				#ifdef SRGB_TRANSFER
					gl_FragColor = sRGBTransferOETF( gl_FragColor );
				#endif
			}`,depthTest:!1,depthWrite:!1}),c=new Ht(o,l),h=new ks(-1,1,1,-1,0,1);let d=null,u=null,m=!1,_,x=null,p=[],f=!1;this.setSize=function(M,y){r.setSize(M,y),a.setSize(M,y);for(let E=0;E<p.length;E++){const L=p[E];L.setSize&&L.setSize(M,y)}},this.setEffects=function(M){p=M,f=p.length>0&&p[0].isRenderPass===!0;const y=r.width,E=r.height;for(let L=0;L<p.length;L++){const w=p[L];w.setSize&&w.setSize(y,E)}},this.begin=function(M,y){if(m||M.toneMapping===si&&p.length===0)return!1;if(x=y,y!==null){const E=y.width,L=y.height;(r.width!==E||r.height!==L)&&this.setSize(E,L)}return f===!1&&M.setRenderTarget(r),_=M.toneMapping,M.toneMapping=si,!0},this.hasRenderPass=function(){return f},this.end=function(M,y){M.toneMapping=_,m=!0;let E=r,L=a;for(let w=0;w<p.length;w++){const D=p[w];if(D.enabled!==!1&&(D.render(M,L,E,y),D.needsSwap!==!1)){const b=E;E=L,L=b}}if(d!==M.outputColorSpace||u!==M.toneMapping){d=M.outputColorSpace,u=M.toneMapping,l.defines={},Xe.getTransfer(d)===Qe&&(l.defines.SRGB_TRANSFER="");const w=_f[u];w&&(l.defines[w]=""),l.needsUpdate=!0}l.uniforms.tDiffuse.value=E.texture,M.setRenderTarget(x),M.render(c,h),x=null,m=!1},this.isCompositing=function(){return m},this.dispose=function(){r.depthTexture&&r.depthTexture.dispose(),r.dispose(),a.dispose(),o.dispose(),l.dispose()}}const xl=new Ut,Ia=new En(1,1),bl=new Ao,Sl=new ph,Ml=new $o,yl=[],El=[],wl=new Float32Array(16),Tl=new Float32Array(9),Al=new Float32Array(4);function Pn(n,e,t){const i=n[0];if(i<=0||i>0)return n;const s=e*t;let r=yl[s];if(r===void 0&&(r=new Float32Array(s),yl[s]=r),e!==0){i.toArray(r,0);for(let a=1,o=0;a!==e;++a)o+=t,n[a].toArray(r,o)}return r}function bt(n,e){if(n.length!==e.length)return!1;for(let t=0,i=n.length;t<i;t++)if(n[t]!==e[t])return!1;return!0}function St(n,e){for(let t=0,i=e.length;t<i;t++)n[t]=e[t]}function Hs(n,e){let t=El[e];t===void 0&&(t=new Int32Array(e),El[e]=t);for(let i=0;i!==e;++i)t[i]=n.allocateTextureUnit();return t}function xf(n,e){const t=this.cache;t[0]!==e&&(n.uniform1f(this.addr,e),t[0]=e)}function bf(n,e){const t=this.cache;if(e.x!==void 0)(t[0]!==e.x||t[1]!==e.y)&&(n.uniform2f(this.addr,e.x,e.y),t[0]=e.x,t[1]=e.y);else{if(bt(t,e))return;n.uniform2fv(this.addr,e),St(t,e)}}function Sf(n,e){const t=this.cache;if(e.x!==void 0)(t[0]!==e.x||t[1]!==e.y||t[2]!==e.z)&&(n.uniform3f(this.addr,e.x,e.y,e.z),t[0]=e.x,t[1]=e.y,t[2]=e.z);else if(e.r!==void 0)(t[0]!==e.r||t[1]!==e.g||t[2]!==e.b)&&(n.uniform3f(this.addr,e.r,e.g,e.b),t[0]=e.r,t[1]=e.g,t[2]=e.b);else{if(bt(t,e))return;n.uniform3fv(this.addr,e),St(t,e)}}function Mf(n,e){const t=this.cache;if(e.x!==void 0)(t[0]!==e.x||t[1]!==e.y||t[2]!==e.z||t[3]!==e.w)&&(n.uniform4f(this.addr,e.x,e.y,e.z,e.w),t[0]=e.x,t[1]=e.y,t[2]=e.z,t[3]=e.w);else{if(bt(t,e))return;n.uniform4fv(this.addr,e),St(t,e)}}function yf(n,e){const t=this.cache,i=e.elements;if(i===void 0){if(bt(t,e))return;n.uniformMatrix2fv(this.addr,!1,e),St(t,e)}else{if(bt(t,i))return;Al.set(i),n.uniformMatrix2fv(this.addr,!1,Al),St(t,i)}}function Ef(n,e){const t=this.cache,i=e.elements;if(i===void 0){if(bt(t,e))return;n.uniformMatrix3fv(this.addr,!1,e),St(t,e)}else{if(bt(t,i))return;Tl.set(i),n.uniformMatrix3fv(this.addr,!1,Tl),St(t,i)}}function wf(n,e){const t=this.cache,i=e.elements;if(i===void 0){if(bt(t,e))return;n.uniformMatrix4fv(this.addr,!1,e),St(t,e)}else{if(bt(t,i))return;wl.set(i),n.uniformMatrix4fv(this.addr,!1,wl),St(t,i)}}function Tf(n,e){const t=this.cache;t[0]!==e&&(n.uniform1i(this.addr,e),t[0]=e)}function Af(n,e){const t=this.cache;if(e.x!==void 0)(t[0]!==e.x||t[1]!==e.y)&&(n.uniform2i(this.addr,e.x,e.y),t[0]=e.x,t[1]=e.y);else{if(bt(t,e))return;n.uniform2iv(this.addr,e),St(t,e)}}function Cf(n,e){const t=this.cache;if(e.x!==void 0)(t[0]!==e.x||t[1]!==e.y||t[2]!==e.z)&&(n.uniform3i(this.addr,e.x,e.y,e.z),t[0]=e.x,t[1]=e.y,t[2]=e.z);else{if(bt(t,e))return;n.uniform3iv(this.addr,e),St(t,e)}}function Rf(n,e){const t=this.cache;if(e.x!==void 0)(t[0]!==e.x||t[1]!==e.y||t[2]!==e.z||t[3]!==e.w)&&(n.uniform4i(this.addr,e.x,e.y,e.z,e.w),t[0]=e.x,t[1]=e.y,t[2]=e.z,t[3]=e.w);else{if(bt(t,e))return;n.uniform4iv(this.addr,e),St(t,e)}}function Pf(n,e){const t=this.cache;t[0]!==e&&(n.uniform1ui(this.addr,e),t[0]=e)}function Lf(n,e){const t=this.cache;if(e.x!==void 0)(t[0]!==e.x||t[1]!==e.y)&&(n.uniform2ui(this.addr,e.x,e.y),t[0]=e.x,t[1]=e.y);else{if(bt(t,e))return;n.uniform2uiv(this.addr,e),St(t,e)}}function Df(n,e){const t=this.cache;if(e.x!==void 0)(t[0]!==e.x||t[1]!==e.y||t[2]!==e.z)&&(n.uniform3ui(this.addr,e.x,e.y,e.z),t[0]=e.x,t[1]=e.y,t[2]=e.z);else{if(bt(t,e))return;n.uniform3uiv(this.addr,e),St(t,e)}}function If(n,e){const t=this.cache;if(e.x!==void 0)(t[0]!==e.x||t[1]!==e.y||t[2]!==e.z||t[3]!==e.w)&&(n.uniform4ui(this.addr,e.x,e.y,e.z,e.w),t[0]=e.x,t[1]=e.y,t[2]=e.z,t[3]=e.w);else{if(bt(t,e))return;n.uniform4uiv(this.addr,e),St(t,e)}}function Nf(n,e,t){const i=this.cache,s=t.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s);let r;this.type===n.SAMPLER_2D_SHADOW?(Ia.compareFunction=t.isReversedDepthBuffer()?$r:Yr,r=Ia):r=xl,t.setTexture2D(e||r,s)}function Uf(n,e,t){const i=this.cache,s=t.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s),t.setTexture3D(e||Sl,s)}function Ff(n,e,t){const i=this.cache,s=t.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s),t.setTextureCube(e||Ml,s)}function Of(n,e,t){const i=this.cache,s=t.allocateTextureUnit();i[0]!==s&&(n.uniform1i(this.addr,s),i[0]=s),t.setTexture2DArray(e||bl,s)}function Bf(n){switch(n){case 5126:return xf;case 35664:return bf;case 35665:return Sf;case 35666:return Mf;case 35674:return yf;case 35675:return Ef;case 35676:return wf;case 5124:case 35670:return Tf;case 35667:case 35671:return Af;case 35668:case 35672:return Cf;case 35669:case 35673:return Rf;case 5125:return Pf;case 36294:return Lf;case 36295:return Df;case 36296:return If;case 35678:case 36198:case 36298:case 36306:case 35682:return Nf;case 35679:case 36299:case 36307:return Uf;case 35680:case 36300:case 36308:case 36293:return Ff;case 36289:case 36303:case 36311:case 36292:return Of}}function zf(n,e){n.uniform1fv(this.addr,e)}function kf(n,e){const t=Pn(e,this.size,2);n.uniform2fv(this.addr,t)}function Vf(n,e){const t=Pn(e,this.size,3);n.uniform3fv(this.addr,t)}function Gf(n,e){const t=Pn(e,this.size,4);n.uniform4fv(this.addr,t)}function Hf(n,e){const t=Pn(e,this.size,4);n.uniformMatrix2fv(this.addr,!1,t)}function Wf(n,e){const t=Pn(e,this.size,9);n.uniformMatrix3fv(this.addr,!1,t)}function Xf(n,e){const t=Pn(e,this.size,16);n.uniformMatrix4fv(this.addr,!1,t)}function jf(n,e){n.uniform1iv(this.addr,e)}function qf(n,e){n.uniform2iv(this.addr,e)}function Yf(n,e){n.uniform3iv(this.addr,e)}function $f(n,e){n.uniform4iv(this.addr,e)}function Zf(n,e){n.uniform1uiv(this.addr,e)}function Kf(n,e){n.uniform2uiv(this.addr,e)}function Jf(n,e){n.uniform3uiv(this.addr,e)}function Qf(n,e){n.uniform4uiv(this.addr,e)}function ep(n,e,t){const i=this.cache,s=e.length,r=Hs(t,s);bt(i,r)||(n.uniform1iv(this.addr,r),St(i,r));let a;this.type===n.SAMPLER_2D_SHADOW?a=Ia:a=xl;for(let o=0;o!==s;++o)t.setTexture2D(e[o]||a,r[o])}function tp(n,e,t){const i=this.cache,s=e.length,r=Hs(t,s);bt(i,r)||(n.uniform1iv(this.addr,r),St(i,r));for(let a=0;a!==s;++a)t.setTexture3D(e[a]||Sl,r[a])}function ip(n,e,t){const i=this.cache,s=e.length,r=Hs(t,s);bt(i,r)||(n.uniform1iv(this.addr,r),St(i,r));for(let a=0;a!==s;++a)t.setTextureCube(e[a]||Ml,r[a])}function np(n,e,t){const i=this.cache,s=e.length,r=Hs(t,s);bt(i,r)||(n.uniform1iv(this.addr,r),St(i,r));for(let a=0;a!==s;++a)t.setTexture2DArray(e[a]||bl,r[a])}function sp(n){switch(n){case 5126:return zf;case 35664:return kf;case 35665:return Vf;case 35666:return Gf;case 35674:return Hf;case 35675:return Wf;case 35676:return Xf;case 5124:case 35670:return jf;case 35667:case 35671:return qf;case 35668:case 35672:return Yf;case 35669:case 35673:return $f;case 5125:return Zf;case 36294:return Kf;case 36295:return Jf;case 36296:return Qf;case 35678:case 36198:case 36298:case 36306:case 35682:return ep;case 35679:case 36299:case 36307:return tp;case 35680:case 36300:case 36308:case 36293:return ip;case 36289:case 36303:case 36311:case 36292:return np}}class rp{constructor(e,t,i){this.id=e,this.addr=i,this.cache=[],this.type=t.type,this.setValue=Bf(t.type)}}class ap{constructor(e,t,i){this.id=e,this.addr=i,this.cache=[],this.type=t.type,this.size=t.size,this.setValue=sp(t.type)}}class op{constructor(e){this.id=e,this.seq=[],this.map={}}setValue(e,t,i){const s=this.seq;for(let r=0,a=s.length;r!==a;++r){const o=s[r];o.setValue(e,t[o.id],i)}}}const Na=/(\w+)(\])?(\[|\.)?/g;function Cl(n,e){n.seq.push(e),n.map[e.id]=e}function lp(n,e,t){const i=n.name,s=i.length;for(Na.lastIndex=0;;){const r=Na.exec(i),a=Na.lastIndex;let o=r[1];const l=r[2]==="]",c=r[3];if(l&&(o=o|0),c===void 0||c==="["&&a+2===s){Cl(t,c===void 0?new rp(o,n,e):new ap(o,n,e));break}else{let d=t.map[o];d===void 0&&(d=new op(o),Cl(t,d)),t=d}}}class Ws{constructor(e,t){this.seq=[],this.map={};const i=e.getProgramParameter(t,e.ACTIVE_UNIFORMS);for(let a=0;a<i;++a){const o=e.getActiveUniform(t,a),l=e.getUniformLocation(t,o.name);lp(o,l,this)}const s=[],r=[];for(const a of this.seq)a.type===e.SAMPLER_2D_SHADOW||a.type===e.SAMPLER_CUBE_SHADOW||a.type===e.SAMPLER_2D_ARRAY_SHADOW?s.push(a):r.push(a);s.length>0&&(this.seq=s.concat(r))}setValue(e,t,i,s){const r=this.map[t];r!==void 0&&r.setValue(e,i,s)}setOptional(e,t,i){const s=t[i];s!==void 0&&this.setValue(e,i,s)}static upload(e,t,i,s){for(let r=0,a=t.length;r!==a;++r){const o=t[r],l=i[o.id];l.needsUpdate!==!1&&o.setValue(e,l.value,s)}}static seqWithValue(e,t){const i=[];for(let s=0,r=e.length;s!==r;++s){const a=e[s];a.id in t&&i.push(a)}return i}}function Rl(n,e,t){const i=n.createShader(e);return n.shaderSource(i,t),n.compileShader(i),i}const cp=37297;let hp=0;function up(n,e){const t=n.split(`
`),i=[],s=Math.max(e-6,0),r=Math.min(e+6,t.length);for(let a=s;a<r;a++){const o=a+1;i.push(`${o===e?">":" "} ${o}: ${t[a]}`)}return i.join(`
`)}const Pl=new Ue;function dp(n){Xe._getMatrix(Pl,Xe.workingColorSpace,n);const e=`mat3( ${Pl.elements.map(t=>t.toFixed(4))} )`;switch(Xe.getTransfer(n)){case cs:return[e,"LinearTransferOETF"];case Qe:return[e,"sRGBTransferOETF"];default:return Ce("WebGLProgram: Unsupported color space: ",n),[e,"LinearTransferOETF"]}}function Ll(n,e,t){const i=n.getShaderParameter(e,n.COMPILE_STATUS),r=(n.getShaderInfoLog(e)||"").trim();if(i&&r==="")return"";const a=/ERROR: 0:(\d+)/.exec(r);if(a){const o=parseInt(a[1]);return t.toUpperCase()+`

`+r+`

`+up(n.getShaderSource(e),o)}else return r}function fp(n,e){const t=dp(e);return[`vec4 ${n}( vec4 value ) {`,`	return ${t[1]}( vec4( value.rgb * ${t[0]}, value.a ) );`,"}"].join(`
`)}const pp={[to]:"Linear",[io]:"Reinhard",[no]:"Cineon",[so]:"ACESFilmic",[ao]:"AgX",[oo]:"Neutral",[ro]:"Custom"};function mp(n,e){const t=pp[e];return t===void 0?(Ce("WebGLProgram: Unsupported toneMapping:",e),"vec3 "+n+"( vec3 color ) { return LinearToneMapping( color ); }"):"vec3 "+n+"( vec3 color ) { return "+t+"ToneMapping( color ); }"}const Xs=new A;function gp(){Xe.getLuminanceCoefficients(Xs);const n=Xs.x.toFixed(4),e=Xs.y.toFixed(4),t=Xs.z.toFixed(4);return["float luminance( const in vec3 rgb ) {",`	const vec3 weights = vec3( ${n}, ${e}, ${t} );`,"	return dot( weights, rgb );","}"].join(`
`)}function _p(n){return[n.extensionClipCullDistance?"#extension GL_ANGLE_clip_cull_distance : require":"",n.extensionMultiDraw?"#extension GL_ANGLE_multi_draw : require":""].filter(Zn).join(`
`)}function vp(n){const e=[];for(const t in n){const i=n[t];i!==!1&&e.push("#define "+t+" "+i)}return e.join(`
`)}function xp(n,e){const t={},i=n.getProgramParameter(e,n.ACTIVE_ATTRIBUTES);for(let s=0;s<i;s++){const r=n.getActiveAttrib(e,s),a=r.name;let o=1;r.type===n.FLOAT_MAT2&&(o=2),r.type===n.FLOAT_MAT3&&(o=3),r.type===n.FLOAT_MAT4&&(o=4),t[a]={type:r.type,location:n.getAttribLocation(e,a),locationSize:o}}return t}function Zn(n){return n!==""}function Dl(n,e){const t=e.numSpotLightShadows+e.numSpotLightMaps-e.numSpotLightShadowsWithMaps;return n.replace(/NUM_DIR_LIGHTS/g,e.numDirLights).replace(/NUM_SPOT_LIGHTS/g,e.numSpotLights).replace(/NUM_SPOT_LIGHT_MAPS/g,e.numSpotLightMaps).replace(/NUM_SPOT_LIGHT_COORDS/g,t).replace(/NUM_RECT_AREA_LIGHTS/g,e.numRectAreaLights).replace(/NUM_POINT_LIGHTS/g,e.numPointLights).replace(/NUM_HEMI_LIGHTS/g,e.numHemiLights).replace(/NUM_DIR_LIGHT_SHADOWS/g,e.numDirLightShadows).replace(/NUM_SPOT_LIGHT_SHADOWS_WITH_MAPS/g,e.numSpotLightShadowsWithMaps).replace(/NUM_SPOT_LIGHT_SHADOWS/g,e.numSpotLightShadows).replace(/NUM_POINT_LIGHT_SHADOWS/g,e.numPointLightShadows)}function Il(n,e){return n.replace(/NUM_CLIPPING_PLANES/g,e.numClippingPlanes).replace(/UNION_CLIPPING_PLANES/g,e.numClippingPlanes-e.numClipIntersection)}const bp=/^[ \t]*#include +<([\w\d./]+)>/gm;function Ua(n){return n.replace(bp,Mp)}const Sp=new Map;function Mp(n,e){let t=ze[e];if(t===void 0){const i=Sp.get(e);if(i!==void 0)t=ze[i],Ce('WebGLRenderer: Shader chunk "%s" has been deprecated. Use "%s" instead.',e,i);else throw new Error("Can not resolve #include <"+e+">")}return Ua(t)}const yp=/#pragma unroll_loop_start\s+for\s*\(\s*int\s+i\s*=\s*(\d+)\s*;\s*i\s*<\s*(\d+)\s*;\s*i\s*\+\+\s*\)\s*{([\s\S]+?)}\s+#pragma unroll_loop_end/g;function Nl(n){return n.replace(yp,Ep)}function Ep(n,e,t,i){let s="";for(let r=parseInt(e);r<parseInt(t);r++)s+=i.replace(/\[\s*i\s*\]/g,"[ "+r+" ]").replace(/UNROLLED_LOOP_INDEX/g,r);return s}function Ul(n){let e=`precision ${n.precision} float;
	precision ${n.precision} int;
	precision ${n.precision} sampler2D;
	precision ${n.precision} samplerCube;
	precision ${n.precision} sampler3D;
	precision ${n.precision} sampler2DArray;
	precision ${n.precision} sampler2DShadow;
	precision ${n.precision} samplerCubeShadow;
	precision ${n.precision} sampler2DArrayShadow;
	precision ${n.precision} isampler2D;
	precision ${n.precision} isampler3D;
	precision ${n.precision} isamplerCube;
	precision ${n.precision} isampler2DArray;
	precision ${n.precision} usampler2D;
	precision ${n.precision} usampler3D;
	precision ${n.precision} usamplerCube;
	precision ${n.precision} usampler2DArray;
	`;return n.precision==="highp"?e+=`
#define HIGH_PRECISION`:n.precision==="mediump"?e+=`
#define MEDIUM_PRECISION`:n.precision==="lowp"&&(e+=`
#define LOW_PRECISION`),e}const wp={[Qn]:"SHADOWMAP_TYPE_PCF",[Un]:"SHADOWMAP_TYPE_VSM"};function Tp(n){return wp[n.shadowMapType]||"SHADOWMAP_TYPE_BASIC"}const Ap={[Xi]:"ENVMAP_TYPE_CUBE",[on]:"ENVMAP_TYPE_CUBE",[es]:"ENVMAP_TYPE_CUBE_UV"};function Cp(n){return n.envMap===!1?"ENVMAP_TYPE_CUBE":Ap[n.envMapMode]||"ENVMAP_TYPE_CUBE"}const Rp={[on]:"ENVMAP_MODE_REFRACTION"};function Pp(n){return n.envMap===!1?"ENVMAP_MODE_REFLECTION":Rp[n.envMapMode]||"ENVMAP_MODE_REFLECTION"}const Lp={[eo]:"ENVMAP_BLENDING_MULTIPLY",[Dc]:"ENVMAP_BLENDING_MIX",[Ic]:"ENVMAP_BLENDING_ADD"};function Dp(n){return n.envMap===!1?"ENVMAP_BLENDING_NONE":Lp[n.combine]||"ENVMAP_BLENDING_NONE"}function Ip(n){const e=n.envMapCubeUVHeight;if(e===null)return null;const t=Math.log2(e)-2,i=1/e;return{texelWidth:1/(3*Math.max(Math.pow(2,t),112)),texelHeight:i,maxMip:t}}function Np(n,e,t,i){const s=n.getContext(),r=t.defines;let a=t.vertexShader,o=t.fragmentShader;const l=Tp(t),c=Cp(t),h=Pp(t),d=Dp(t),u=Ip(t),m=_p(t),_=vp(r),x=s.createProgram();let p,f,M=t.glslVersion?"#version "+t.glslVersion+`
`:"";t.isRawShaderMaterial?(p=["#define SHADER_TYPE "+t.shaderType,"#define SHADER_NAME "+t.shaderName,_].filter(Zn).join(`
`),p.length>0&&(p+=`
`),f=["#define SHADER_TYPE "+t.shaderType,"#define SHADER_NAME "+t.shaderName,_].filter(Zn).join(`
`),f.length>0&&(f+=`
`)):(p=[Ul(t),"#define SHADER_TYPE "+t.shaderType,"#define SHADER_NAME "+t.shaderName,_,t.extensionClipCullDistance?"#define USE_CLIP_DISTANCE":"",t.batching?"#define USE_BATCHING":"",t.batchingColor?"#define USE_BATCHING_COLOR":"",t.instancing?"#define USE_INSTANCING":"",t.instancingColor?"#define USE_INSTANCING_COLOR":"",t.instancingMorph?"#define USE_INSTANCING_MORPH":"",t.useFog&&t.fog?"#define USE_FOG":"",t.useFog&&t.fogExp2?"#define FOG_EXP2":"",t.map?"#define USE_MAP":"",t.envMap?"#define USE_ENVMAP":"",t.envMap?"#define "+h:"",t.lightMap?"#define USE_LIGHTMAP":"",t.aoMap?"#define USE_AOMAP":"",t.bumpMap?"#define USE_BUMPMAP":"",t.normalMap?"#define USE_NORMALMAP":"",t.normalMapObjectSpace?"#define USE_NORMALMAP_OBJECTSPACE":"",t.normalMapTangentSpace?"#define USE_NORMALMAP_TANGENTSPACE":"",t.displacementMap?"#define USE_DISPLACEMENTMAP":"",t.emissiveMap?"#define USE_EMISSIVEMAP":"",t.anisotropy?"#define USE_ANISOTROPY":"",t.anisotropyMap?"#define USE_ANISOTROPYMAP":"",t.clearcoatMap?"#define USE_CLEARCOATMAP":"",t.clearcoatRoughnessMap?"#define USE_CLEARCOAT_ROUGHNESSMAP":"",t.clearcoatNormalMap?"#define USE_CLEARCOAT_NORMALMAP":"",t.iridescenceMap?"#define USE_IRIDESCENCEMAP":"",t.iridescenceThicknessMap?"#define USE_IRIDESCENCE_THICKNESSMAP":"",t.specularMap?"#define USE_SPECULARMAP":"",t.specularColorMap?"#define USE_SPECULAR_COLORMAP":"",t.specularIntensityMap?"#define USE_SPECULAR_INTENSITYMAP":"",t.roughnessMap?"#define USE_ROUGHNESSMAP":"",t.metalnessMap?"#define USE_METALNESSMAP":"",t.alphaMap?"#define USE_ALPHAMAP":"",t.alphaHash?"#define USE_ALPHAHASH":"",t.transmission?"#define USE_TRANSMISSION":"",t.transmissionMap?"#define USE_TRANSMISSIONMAP":"",t.thicknessMap?"#define USE_THICKNESSMAP":"",t.sheenColorMap?"#define USE_SHEEN_COLORMAP":"",t.sheenRoughnessMap?"#define USE_SHEEN_ROUGHNESSMAP":"",t.mapUv?"#define MAP_UV "+t.mapUv:"",t.alphaMapUv?"#define ALPHAMAP_UV "+t.alphaMapUv:"",t.lightMapUv?"#define LIGHTMAP_UV "+t.lightMapUv:"",t.aoMapUv?"#define AOMAP_UV "+t.aoMapUv:"",t.emissiveMapUv?"#define EMISSIVEMAP_UV "+t.emissiveMapUv:"",t.bumpMapUv?"#define BUMPMAP_UV "+t.bumpMapUv:"",t.normalMapUv?"#define NORMALMAP_UV "+t.normalMapUv:"",t.displacementMapUv?"#define DISPLACEMENTMAP_UV "+t.displacementMapUv:"",t.metalnessMapUv?"#define METALNESSMAP_UV "+t.metalnessMapUv:"",t.roughnessMapUv?"#define ROUGHNESSMAP_UV "+t.roughnessMapUv:"",t.anisotropyMapUv?"#define ANISOTROPYMAP_UV "+t.anisotropyMapUv:"",t.clearcoatMapUv?"#define CLEARCOATMAP_UV "+t.clearcoatMapUv:"",t.clearcoatNormalMapUv?"#define CLEARCOAT_NORMALMAP_UV "+t.clearcoatNormalMapUv:"",t.clearcoatRoughnessMapUv?"#define CLEARCOAT_ROUGHNESSMAP_UV "+t.clearcoatRoughnessMapUv:"",t.iridescenceMapUv?"#define IRIDESCENCEMAP_UV "+t.iridescenceMapUv:"",t.iridescenceThicknessMapUv?"#define IRIDESCENCE_THICKNESSMAP_UV "+t.iridescenceThicknessMapUv:"",t.sheenColorMapUv?"#define SHEEN_COLORMAP_UV "+t.sheenColorMapUv:"",t.sheenRoughnessMapUv?"#define SHEEN_ROUGHNESSMAP_UV "+t.sheenRoughnessMapUv:"",t.specularMapUv?"#define SPECULARMAP_UV "+t.specularMapUv:"",t.specularColorMapUv?"#define SPECULAR_COLORMAP_UV "+t.specularColorMapUv:"",t.specularIntensityMapUv?"#define SPECULAR_INTENSITYMAP_UV "+t.specularIntensityMapUv:"",t.transmissionMapUv?"#define TRANSMISSIONMAP_UV "+t.transmissionMapUv:"",t.thicknessMapUv?"#define THICKNESSMAP_UV "+t.thicknessMapUv:"",t.vertexTangents&&t.flatShading===!1?"#define USE_TANGENT":"",t.vertexNormals?"#define HAS_NORMAL":"",t.vertexColors?"#define USE_COLOR":"",t.vertexAlphas?"#define USE_COLOR_ALPHA":"",t.vertexUv1s?"#define USE_UV1":"",t.vertexUv2s?"#define USE_UV2":"",t.vertexUv3s?"#define USE_UV3":"",t.pointsUvs?"#define USE_POINTS_UV":"",t.flatShading?"#define FLAT_SHADED":"",t.skinning?"#define USE_SKINNING":"",t.morphTargets?"#define USE_MORPHTARGETS":"",t.morphNormals&&t.flatShading===!1?"#define USE_MORPHNORMALS":"",t.morphColors?"#define USE_MORPHCOLORS":"",t.morphTargetsCount>0?"#define MORPHTARGETS_TEXTURE_STRIDE "+t.morphTextureStride:"",t.morphTargetsCount>0?"#define MORPHTARGETS_COUNT "+t.morphTargetsCount:"",t.doubleSided?"#define DOUBLE_SIDED":"",t.flipSided?"#define FLIP_SIDED":"",t.shadowMapEnabled?"#define USE_SHADOWMAP":"",t.shadowMapEnabled?"#define "+l:"",t.sizeAttenuation?"#define USE_SIZEATTENUATION":"",t.numLightProbes>0?"#define USE_LIGHT_PROBES":"",t.logarithmicDepthBuffer?"#define USE_LOGARITHMIC_DEPTH_BUFFER":"",t.reversedDepthBuffer?"#define USE_REVERSED_DEPTH_BUFFER":"","uniform mat4 modelMatrix;","uniform mat4 modelViewMatrix;","uniform mat4 projectionMatrix;","uniform mat4 viewMatrix;","uniform mat3 normalMatrix;","uniform vec3 cameraPosition;","uniform bool isOrthographic;","#ifdef USE_INSTANCING","	attribute mat4 instanceMatrix;","#endif","#ifdef USE_INSTANCING_COLOR","	attribute vec3 instanceColor;","#endif","#ifdef USE_INSTANCING_MORPH","	uniform sampler2D morphTexture;","#endif","attribute vec3 position;","attribute vec3 normal;","attribute vec2 uv;","#ifdef USE_UV1","	attribute vec2 uv1;","#endif","#ifdef USE_UV2","	attribute vec2 uv2;","#endif","#ifdef USE_UV3","	attribute vec2 uv3;","#endif","#ifdef USE_TANGENT","	attribute vec4 tangent;","#endif","#if defined( USE_COLOR_ALPHA )","	attribute vec4 color;","#elif defined( USE_COLOR )","	attribute vec3 color;","#endif","#ifdef USE_SKINNING","	attribute vec4 skinIndex;","	attribute vec4 skinWeight;","#endif",`
`].filter(Zn).join(`
`),f=[Ul(t),"#define SHADER_TYPE "+t.shaderType,"#define SHADER_NAME "+t.shaderName,_,t.useFog&&t.fog?"#define USE_FOG":"",t.useFog&&t.fogExp2?"#define FOG_EXP2":"",t.alphaToCoverage?"#define ALPHA_TO_COVERAGE":"",t.map?"#define USE_MAP":"",t.matcap?"#define USE_MATCAP":"",t.envMap?"#define USE_ENVMAP":"",t.envMap?"#define "+c:"",t.envMap?"#define "+h:"",t.envMap?"#define "+d:"",u?"#define CUBEUV_TEXEL_WIDTH "+u.texelWidth:"",u?"#define CUBEUV_TEXEL_HEIGHT "+u.texelHeight:"",u?"#define CUBEUV_MAX_MIP "+u.maxMip+".0":"",t.lightMap?"#define USE_LIGHTMAP":"",t.aoMap?"#define USE_AOMAP":"",t.bumpMap?"#define USE_BUMPMAP":"",t.normalMap?"#define USE_NORMALMAP":"",t.normalMapObjectSpace?"#define USE_NORMALMAP_OBJECTSPACE":"",t.normalMapTangentSpace?"#define USE_NORMALMAP_TANGENTSPACE":"",t.packedNormalMap?"#define USE_PACKED_NORMALMAP":"",t.emissiveMap?"#define USE_EMISSIVEMAP":"",t.anisotropy?"#define USE_ANISOTROPY":"",t.anisotropyMap?"#define USE_ANISOTROPYMAP":"",t.clearcoat?"#define USE_CLEARCOAT":"",t.clearcoatMap?"#define USE_CLEARCOATMAP":"",t.clearcoatRoughnessMap?"#define USE_CLEARCOAT_ROUGHNESSMAP":"",t.clearcoatNormalMap?"#define USE_CLEARCOAT_NORMALMAP":"",t.dispersion?"#define USE_DISPERSION":"",t.iridescence?"#define USE_IRIDESCENCE":"",t.iridescenceMap?"#define USE_IRIDESCENCEMAP":"",t.iridescenceThicknessMap?"#define USE_IRIDESCENCE_THICKNESSMAP":"",t.specularMap?"#define USE_SPECULARMAP":"",t.specularColorMap?"#define USE_SPECULAR_COLORMAP":"",t.specularIntensityMap?"#define USE_SPECULAR_INTENSITYMAP":"",t.roughnessMap?"#define USE_ROUGHNESSMAP":"",t.metalnessMap?"#define USE_METALNESSMAP":"",t.alphaMap?"#define USE_ALPHAMAP":"",t.alphaTest?"#define USE_ALPHATEST":"",t.alphaHash?"#define USE_ALPHAHASH":"",t.sheen?"#define USE_SHEEN":"",t.sheenColorMap?"#define USE_SHEEN_COLORMAP":"",t.sheenRoughnessMap?"#define USE_SHEEN_ROUGHNESSMAP":"",t.transmission?"#define USE_TRANSMISSION":"",t.transmissionMap?"#define USE_TRANSMISSIONMAP":"",t.thicknessMap?"#define USE_THICKNESSMAP":"",t.vertexTangents&&t.flatShading===!1?"#define USE_TANGENT":"",t.vertexColors||t.instancingColor?"#define USE_COLOR":"",t.vertexAlphas||t.batchingColor?"#define USE_COLOR_ALPHA":"",t.vertexUv1s?"#define USE_UV1":"",t.vertexUv2s?"#define USE_UV2":"",t.vertexUv3s?"#define USE_UV3":"",t.pointsUvs?"#define USE_POINTS_UV":"",t.gradientMap?"#define USE_GRADIENTMAP":"",t.flatShading?"#define FLAT_SHADED":"",t.doubleSided?"#define DOUBLE_SIDED":"",t.flipSided?"#define FLIP_SIDED":"",t.shadowMapEnabled?"#define USE_SHADOWMAP":"",t.shadowMapEnabled?"#define "+l:"",t.premultipliedAlpha?"#define PREMULTIPLIED_ALPHA":"",t.numLightProbes>0?"#define USE_LIGHT_PROBES":"",t.numLightProbeGrids>0?"#define USE_LIGHT_PROBES_GRID":"",t.decodeVideoTexture?"#define DECODE_VIDEO_TEXTURE":"",t.decodeVideoTextureEmissive?"#define DECODE_VIDEO_TEXTURE_EMISSIVE":"",t.logarithmicDepthBuffer?"#define USE_LOGARITHMIC_DEPTH_BUFFER":"",t.reversedDepthBuffer?"#define USE_REVERSED_DEPTH_BUFFER":"","uniform mat4 viewMatrix;","uniform vec3 cameraPosition;","uniform bool isOrthographic;",t.toneMapping!==si?"#define TONE_MAPPING":"",t.toneMapping!==si?ze.tonemapping_pars_fragment:"",t.toneMapping!==si?mp("toneMapping",t.toneMapping):"",t.dithering?"#define DITHERING":"",t.opaque?"#define OPAQUE":"",ze.colorspace_pars_fragment,fp("linearToOutputTexel",t.outputColorSpace),gp(),t.useDepthPacking?"#define DEPTH_PACKING "+t.depthPacking:"",`
`].filter(Zn).join(`
`)),a=Ua(a),a=Dl(a,t),a=Il(a,t),o=Ua(o),o=Dl(o,t),o=Il(o,t),a=Nl(a),o=Nl(o),t.isRawShaderMaterial!==!0&&(M=`#version 300 es
`,p=[m,"#define attribute in","#define varying out","#define texture2D texture"].join(`
`)+`
`+p,f=["#define varying in",t.glslVersion===xo?"":"layout(location = 0) out highp vec4 pc_fragColor;",t.glslVersion===xo?"":"#define gl_FragColor pc_fragColor","#define gl_FragDepthEXT gl_FragDepth","#define texture2D texture","#define textureCube texture","#define texture2DProj textureProj","#define texture2DLodEXT textureLod","#define texture2DProjLodEXT textureProjLod","#define textureCubeLodEXT textureLod","#define texture2DGradEXT textureGrad","#define texture2DProjGradEXT textureProjGrad","#define textureCubeGradEXT textureGrad"].join(`
`)+`
`+f);const y=M+p+a,E=M+f+o,L=Rl(s,s.VERTEX_SHADER,y),w=Rl(s,s.FRAGMENT_SHADER,E);s.attachShader(x,L),s.attachShader(x,w),t.index0AttributeName!==void 0?s.bindAttribLocation(x,0,t.index0AttributeName):t.morphTargets===!0&&s.bindAttribLocation(x,0,"position"),s.linkProgram(x);function D(C){if(n.debug.checkShaderErrors){const z=s.getProgramInfoLog(x)||"",X=s.getShaderInfoLog(L)||"",j=s.getShaderInfoLog(w)||"",N=z.trim(),V=X.trim(),H=j.trim();let K=!0,ee=!0;if(s.getProgramParameter(x,s.LINK_STATUS)===!1)if(K=!1,typeof n.debug.onShaderError=="function")n.debug.onShaderError(s,x,L,w);else{const ce=Ll(s,L,"vertex"),be=Ll(s,w,"fragment");je("THREE.WebGLProgram: Shader Error "+s.getError()+" - VALIDATE_STATUS "+s.getProgramParameter(x,s.VALIDATE_STATUS)+`

Material Name: `+C.name+`
Material Type: `+C.type+`

Program Info Log: `+N+`
`+ce+`
`+be)}else N!==""?Ce("WebGLProgram: Program Info Log:",N):(V===""||H==="")&&(ee=!1);ee&&(C.diagnostics={runnable:K,programLog:N,vertexShader:{log:V,prefix:p},fragmentShader:{log:H,prefix:f}})}s.deleteShader(L),s.deleteShader(w),b=new Ws(s,x),T=xp(s,x)}let b;this.getUniforms=function(){return b===void 0&&D(this),b};let T;this.getAttributes=function(){return T===void 0&&D(this),T};let U=t.rendererExtensionParallelShaderCompile===!1;return this.isReady=function(){return U===!1&&(U=s.getProgramParameter(x,cp)),U},this.destroy=function(){i.releaseStatesOfProgram(this),s.deleteProgram(x),this.program=void 0},this.type=t.shaderType,this.name=t.shaderName,this.id=hp++,this.cacheKey=e,this.usedTimes=1,this.program=x,this.vertexShader=L,this.fragmentShader=w,this}let Up=0;class Fp{constructor(){this.shaderCache=new Map,this.materialCache=new Map}update(e){const t=e.vertexShader,i=e.fragmentShader,s=this._getShaderStage(t),r=this._getShaderStage(i),a=this._getShaderCacheForMaterial(e);return a.has(s)===!1&&(a.add(s),s.usedTimes++),a.has(r)===!1&&(a.add(r),r.usedTimes++),this}remove(e){const t=this.materialCache.get(e);for(const i of t)i.usedTimes--,i.usedTimes===0&&this.shaderCache.delete(i.code);return this.materialCache.delete(e),this}getVertexShaderID(e){return this._getShaderStage(e.vertexShader).id}getFragmentShaderID(e){return this._getShaderStage(e.fragmentShader).id}dispose(){this.shaderCache.clear(),this.materialCache.clear()}_getShaderCacheForMaterial(e){const t=this.materialCache;let i=t.get(e);return i===void 0&&(i=new Set,t.set(e,i)),i}_getShaderStage(e){const t=this.shaderCache;let i=t.get(e);return i===void 0&&(i=new Op(e),t.set(e,i)),i}}class Op{constructor(e){this.id=Up++,this.code=e,this.usedTimes=0}}function Bp(n){return n===Yi||n===as||n===os}function zp(n,e,t,i,s,r){const a=new sa,o=new Fp,l=new Set,c=[],h=new Map,d=i.logarithmicDepthBuffer;let u=i.precision;const m={MeshDepthMaterial:"depth",MeshDistanceMaterial:"distance",MeshNormalMaterial:"normal",MeshBasicMaterial:"basic",MeshLambertMaterial:"lambert",MeshPhongMaterial:"phong",MeshToonMaterial:"toon",MeshStandardMaterial:"physical",MeshPhysicalMaterial:"physical",MeshMatcapMaterial:"matcap",LineBasicMaterial:"basic",LineDashedMaterial:"dashed",PointsMaterial:"points",ShadowMaterial:"shadow",SpriteMaterial:"sprite"};function _(b){return l.add(b),b===0?"uv":`uv${b}`}function x(b,T,U,C,z,X){const j=C.fog,N=z.geometry,V=b.isMeshStandardMaterial||b.isMeshLambertMaterial||b.isMeshPhongMaterial?C.environment:null,H=b.isMeshStandardMaterial||b.isMeshLambertMaterial&&!b.envMap||b.isMeshPhongMaterial&&!b.envMap,K=e.get(b.envMap||V,H),ee=K&&K.mapping===es?K.image.height:null,ce=m[b.type];b.precision!==null&&(u=i.getMaxPrecision(b.precision),u!==b.precision&&Ce("WebGLProgram.getParameters:",b.precision,"not supported, using",u,"instead."));const be=N.morphAttributes.position||N.morphAttributes.normal||N.morphAttributes.color,Me=be!==void 0?be.length:0;let He=0;N.morphAttributes.position!==void 0&&(He=1),N.morphAttributes.normal!==void 0&&(He=2),N.morphAttributes.color!==void 0&&(He=3);let Ze,De,Z,ue;if(ce){const Fe=ui[ce];Ze=Fe.vertexShader,De=Fe.fragmentShader}else Ze=b.vertexShader,De=b.fragmentShader,o.update(b),Z=o.getVertexShaderID(b),ue=o.getFragmentShaderID(b);const te=n.getRenderTarget(),Te=n.state.buffers.depth.getReversed(),Le=z.isInstancedMesh===!0,Ae=z.isBatchedMesh===!0,tt=!!b.map,ie=!!b.matcap,Pe=!!K,Ke=!!b.aoMap,ke=!!b.lightMap,yt=!!b.bumpMap,ct=!!b.normalMap,Wt=!!b.displacementMap,P=!!b.emissiveMap,Et=!!b.metalnessMap,We=!!b.roughnessMap,at=b.anisotropy>0,de=b.clearcoat>0,ut=b.dispersion>0,S=b.iridescence>0,g=b.sheen>0,F=b.transmission>0,Y=at&&!!b.anisotropyMap,Q=de&&!!b.clearcoatMap,ne=de&&!!b.clearcoatNormalMap,le=de&&!!b.clearcoatRoughnessMap,W=S&&!!b.iridescenceMap,$=S&&!!b.iridescenceThicknessMap,me=g&&!!b.sheenColorMap,ve=g&&!!b.sheenRoughnessMap,ae=!!b.specularMap,se=!!b.specularColorMap,Ie=!!b.specularIntensityMap,Be=F&&!!b.transmissionMap,$e=F&&!!b.thicknessMap,R=!!b.gradientMap,re=!!b.alphaMap,q=b.alphaTest>0,ge=!!b.alphaHash,oe=!!b.extensions;let J=si;b.toneMapped&&(te===null||te.isXRRenderTarget===!0)&&(J=n.toneMapping);const ye={shaderID:ce,shaderType:b.type,shaderName:b.name,vertexShader:Ze,fragmentShader:De,defines:b.defines,customVertexShaderID:Z,customFragmentShaderID:ue,isRawShaderMaterial:b.isRawShaderMaterial===!0,glslVersion:b.glslVersion,precision:u,batching:Ae,batchingColor:Ae&&z._colorsTexture!==null,instancing:Le,instancingColor:Le&&z.instanceColor!==null,instancingMorph:Le&&z.morphTexture!==null,outputColorSpace:te===null?n.outputColorSpace:te.isXRRenderTarget===!0?te.texture.colorSpace:Xe.workingColorSpace,alphaToCoverage:!!b.alphaToCoverage,map:tt,matcap:ie,envMap:Pe,envMapMode:Pe&&K.mapping,envMapCubeUVHeight:ee,aoMap:Ke,lightMap:ke,bumpMap:yt,normalMap:ct,displacementMap:Wt,emissiveMap:P,normalMapObjectSpace:ct&&b.normalMapType===Fc,normalMapTangentSpace:ct&&b.normalMapType===qr,packedNormalMap:ct&&b.normalMapType===qr&&Bp(b.normalMap.format),metalnessMap:Et,roughnessMap:We,anisotropy:at,anisotropyMap:Y,clearcoat:de,clearcoatMap:Q,clearcoatNormalMap:ne,clearcoatRoughnessMap:le,dispersion:ut,iridescence:S,iridescenceMap:W,iridescenceThicknessMap:$,sheen:g,sheenColorMap:me,sheenRoughnessMap:ve,specularMap:ae,specularColorMap:se,specularIntensityMap:Ie,transmission:F,transmissionMap:Be,thicknessMap:$e,gradientMap:R,opaque:b.transparent===!1&&b.blending===rn&&b.alphaToCoverage===!1,alphaMap:re,alphaTest:q,alphaHash:ge,combine:b.combine,mapUv:tt&&_(b.map.channel),aoMapUv:Ke&&_(b.aoMap.channel),lightMapUv:ke&&_(b.lightMap.channel),bumpMapUv:yt&&_(b.bumpMap.channel),normalMapUv:ct&&_(b.normalMap.channel),displacementMapUv:Wt&&_(b.displacementMap.channel),emissiveMapUv:P&&_(b.emissiveMap.channel),metalnessMapUv:Et&&_(b.metalnessMap.channel),roughnessMapUv:We&&_(b.roughnessMap.channel),anisotropyMapUv:Y&&_(b.anisotropyMap.channel),clearcoatMapUv:Q&&_(b.clearcoatMap.channel),clearcoatNormalMapUv:ne&&_(b.clearcoatNormalMap.channel),clearcoatRoughnessMapUv:le&&_(b.clearcoatRoughnessMap.channel),iridescenceMapUv:W&&_(b.iridescenceMap.channel),iridescenceThicknessMapUv:$&&_(b.iridescenceThicknessMap.channel),sheenColorMapUv:me&&_(b.sheenColorMap.channel),sheenRoughnessMapUv:ve&&_(b.sheenRoughnessMap.channel),specularMapUv:ae&&_(b.specularMap.channel),specularColorMapUv:se&&_(b.specularColorMap.channel),specularIntensityMapUv:Ie&&_(b.specularIntensityMap.channel),transmissionMapUv:Be&&_(b.transmissionMap.channel),thicknessMapUv:$e&&_(b.thicknessMap.channel),alphaMapUv:re&&_(b.alphaMap.channel),vertexTangents:!!N.attributes.tangent&&(ct||at),vertexNormals:!!N.attributes.normal,vertexColors:b.vertexColors,vertexAlphas:b.vertexColors===!0&&!!N.attributes.color&&N.attributes.color.itemSize===4,pointsUvs:z.isPoints===!0&&!!N.attributes.uv&&(tt||re),fog:!!j,useFog:b.fog===!0,fogExp2:!!j&&j.isFogExp2,flatShading:b.wireframe===!1&&(b.flatShading===!0||N.attributes.normal===void 0&&ct===!1&&(b.isMeshLambertMaterial||b.isMeshPhongMaterial||b.isMeshStandardMaterial||b.isMeshPhysicalMaterial)),sizeAttenuation:b.sizeAttenuation===!0,logarithmicDepthBuffer:d,reversedDepthBuffer:Te,skinning:z.isSkinnedMesh===!0,morphTargets:N.morphAttributes.position!==void 0,morphNormals:N.morphAttributes.normal!==void 0,morphColors:N.morphAttributes.color!==void 0,morphTargetsCount:Me,morphTextureStride:He,numDirLights:T.directional.length,numPointLights:T.point.length,numSpotLights:T.spot.length,numSpotLightMaps:T.spotLightMap.length,numRectAreaLights:T.rectArea.length,numHemiLights:T.hemi.length,numDirLightShadows:T.directionalShadowMap.length,numPointLightShadows:T.pointShadowMap.length,numSpotLightShadows:T.spotShadowMap.length,numSpotLightShadowsWithMaps:T.numSpotLightShadowsWithMaps,numLightProbes:T.numLightProbes,numLightProbeGrids:X.length,numClippingPlanes:r.numPlanes,numClipIntersection:r.numIntersection,dithering:b.dithering,shadowMapEnabled:n.shadowMap.enabled&&U.length>0,shadowMapType:n.shadowMap.type,toneMapping:J,decodeVideoTexture:tt&&b.map.isVideoTexture===!0&&Xe.getTransfer(b.map.colorSpace)===Qe,decodeVideoTextureEmissive:P&&b.emissiveMap.isVideoTexture===!0&&Xe.getTransfer(b.emissiveMap.colorSpace)===Qe,premultipliedAlpha:b.premultipliedAlpha,doubleSided:b.side===It,flipSided:b.side===vt,useDepthPacking:b.depthPacking>=0,depthPacking:b.depthPacking||0,index0AttributeName:b.index0AttributeName,extensionClipCullDistance:oe&&b.extensions.clipCullDistance===!0&&t.has("WEBGL_clip_cull_distance"),extensionMultiDraw:(oe&&b.extensions.multiDraw===!0||Ae)&&t.has("WEBGL_multi_draw"),rendererExtensionParallelShaderCompile:t.has("KHR_parallel_shader_compile"),customProgramCacheKey:b.customProgramCacheKey()};return ye.vertexUv1s=l.has(1),ye.vertexUv2s=l.has(2),ye.vertexUv3s=l.has(3),l.clear(),ye}function p(b){const T=[];if(b.shaderID?T.push(b.shaderID):(T.push(b.customVertexShaderID),T.push(b.customFragmentShaderID)),b.defines!==void 0)for(const U in b.defines)T.push(U),T.push(b.defines[U]);return b.isRawShaderMaterial===!1&&(f(T,b),M(T,b),T.push(n.outputColorSpace)),T.push(b.customProgramCacheKey),T.join()}function f(b,T){b.push(T.precision),b.push(T.outputColorSpace),b.push(T.envMapMode),b.push(T.envMapCubeUVHeight),b.push(T.mapUv),b.push(T.alphaMapUv),b.push(T.lightMapUv),b.push(T.aoMapUv),b.push(T.bumpMapUv),b.push(T.normalMapUv),b.push(T.displacementMapUv),b.push(T.emissiveMapUv),b.push(T.metalnessMapUv),b.push(T.roughnessMapUv),b.push(T.anisotropyMapUv),b.push(T.clearcoatMapUv),b.push(T.clearcoatNormalMapUv),b.push(T.clearcoatRoughnessMapUv),b.push(T.iridescenceMapUv),b.push(T.iridescenceThicknessMapUv),b.push(T.sheenColorMapUv),b.push(T.sheenRoughnessMapUv),b.push(T.specularMapUv),b.push(T.specularColorMapUv),b.push(T.specularIntensityMapUv),b.push(T.transmissionMapUv),b.push(T.thicknessMapUv),b.push(T.combine),b.push(T.fogExp2),b.push(T.sizeAttenuation),b.push(T.morphTargetsCount),b.push(T.morphAttributeCount),b.push(T.numDirLights),b.push(T.numPointLights),b.push(T.numSpotLights),b.push(T.numSpotLightMaps),b.push(T.numHemiLights),b.push(T.numRectAreaLights),b.push(T.numDirLightShadows),b.push(T.numPointLightShadows),b.push(T.numSpotLightShadows),b.push(T.numSpotLightShadowsWithMaps),b.push(T.numLightProbes),b.push(T.shadowMapType),b.push(T.toneMapping),b.push(T.numClippingPlanes),b.push(T.numClipIntersection),b.push(T.depthPacking)}function M(b,T){a.disableAll(),T.instancing&&a.enable(0),T.instancingColor&&a.enable(1),T.instancingMorph&&a.enable(2),T.matcap&&a.enable(3),T.envMap&&a.enable(4),T.normalMapObjectSpace&&a.enable(5),T.normalMapTangentSpace&&a.enable(6),T.clearcoat&&a.enable(7),T.iridescence&&a.enable(8),T.alphaTest&&a.enable(9),T.vertexColors&&a.enable(10),T.vertexAlphas&&a.enable(11),T.vertexUv1s&&a.enable(12),T.vertexUv2s&&a.enable(13),T.vertexUv3s&&a.enable(14),T.vertexTangents&&a.enable(15),T.anisotropy&&a.enable(16),T.alphaHash&&a.enable(17),T.batching&&a.enable(18),T.dispersion&&a.enable(19),T.batchingColor&&a.enable(20),T.gradientMap&&a.enable(21),T.packedNormalMap&&a.enable(22),T.vertexNormals&&a.enable(23),b.push(a.mask),a.disableAll(),T.fog&&a.enable(0),T.useFog&&a.enable(1),T.flatShading&&a.enable(2),T.logarithmicDepthBuffer&&a.enable(3),T.reversedDepthBuffer&&a.enable(4),T.skinning&&a.enable(5),T.morphTargets&&a.enable(6),T.morphNormals&&a.enable(7),T.morphColors&&a.enable(8),T.premultipliedAlpha&&a.enable(9),T.shadowMapEnabled&&a.enable(10),T.doubleSided&&a.enable(11),T.flipSided&&a.enable(12),T.useDepthPacking&&a.enable(13),T.dithering&&a.enable(14),T.transmission&&a.enable(15),T.sheen&&a.enable(16),T.opaque&&a.enable(17),T.pointsUvs&&a.enable(18),T.decodeVideoTexture&&a.enable(19),T.decodeVideoTextureEmissive&&a.enable(20),T.alphaToCoverage&&a.enable(21),T.numLightProbeGrids>0&&a.enable(22),b.push(a.mask)}function y(b){const T=m[b.type];let U;if(T){const C=ui[T];U=Nh.clone(C.uniforms)}else U=b.uniforms;return U}function E(b,T){let U=h.get(T);return U!==void 0?++U.usedTimes:(U=new Np(n,T,b,s),c.push(U),h.set(T,U)),U}function L(b){if(--b.usedTimes===0){const T=c.indexOf(b);c[T]=c[c.length-1],c.pop(),h.delete(b.cacheKey),b.destroy()}}function w(b){o.remove(b)}function D(){o.dispose()}return{getParameters:x,getProgramCacheKey:p,getUniforms:y,acquireProgram:E,releaseProgram:L,releaseShaderCache:w,programs:c,dispose:D}}function kp(){let n=new WeakMap;function e(a){return n.has(a)}function t(a){let o=n.get(a);return o===void 0&&(o={},n.set(a,o)),o}function i(a){n.delete(a)}function s(a,o,l){n.get(a)[o]=l}function r(){n=new WeakMap}return{has:e,get:t,remove:i,update:s,dispose:r}}function Vp(n,e){return n.groupOrder!==e.groupOrder?n.groupOrder-e.groupOrder:n.renderOrder!==e.renderOrder?n.renderOrder-e.renderOrder:n.material.id!==e.material.id?n.material.id-e.material.id:n.materialVariant!==e.materialVariant?n.materialVariant-e.materialVariant:n.z!==e.z?n.z-e.z:n.id-e.id}function Fl(n,e){return n.groupOrder!==e.groupOrder?n.groupOrder-e.groupOrder:n.renderOrder!==e.renderOrder?n.renderOrder-e.renderOrder:n.z!==e.z?e.z-n.z:n.id-e.id}function Ol(){const n=[];let e=0;const t=[],i=[],s=[];function r(){e=0,t.length=0,i.length=0,s.length=0}function a(u){let m=0;return u.isInstancedMesh&&(m+=2),u.isSkinnedMesh&&(m+=1),m}function o(u,m,_,x,p,f){let M=n[e];return M===void 0?(M={id:u.id,object:u,geometry:m,material:_,materialVariant:a(u),groupOrder:x,renderOrder:u.renderOrder,z:p,group:f},n[e]=M):(M.id=u.id,M.object=u,M.geometry=m,M.material=_,M.materialVariant=a(u),M.groupOrder=x,M.renderOrder=u.renderOrder,M.z=p,M.group=f),e++,M}function l(u,m,_,x,p,f){const M=o(u,m,_,x,p,f);_.transmission>0?i.push(M):_.transparent===!0?s.push(M):t.push(M)}function c(u,m,_,x,p,f){const M=o(u,m,_,x,p,f);_.transmission>0?i.unshift(M):_.transparent===!0?s.unshift(M):t.unshift(M)}function h(u,m){t.length>1&&t.sort(u||Vp),i.length>1&&i.sort(m||Fl),s.length>1&&s.sort(m||Fl)}function d(){for(let u=e,m=n.length;u<m;u++){const _=n[u];if(_.id===null)break;_.id=null,_.object=null,_.geometry=null,_.material=null,_.group=null}}return{opaque:t,transmissive:i,transparent:s,init:r,push:l,unshift:c,finish:d,sort:h}}function Gp(){let n=new WeakMap;function e(i,s){const r=n.get(i);let a;return r===void 0?(a=new Ol,n.set(i,[a])):s>=r.length?(a=new Ol,r.push(a)):a=r[s],a}function t(){n=new WeakMap}return{get:e,dispose:t}}function Hp(){const n={};return{get:function(e){if(n[e.id]!==void 0)return n[e.id];let t;switch(e.type){case"DirectionalLight":t={direction:new A,color:new Re};break;case"SpotLight":t={position:new A,direction:new A,color:new Re,distance:0,coneCos:0,penumbraCos:0,decay:0};break;case"PointLight":t={position:new A,color:new Re,distance:0,decay:0};break;case"HemisphereLight":t={direction:new A,skyColor:new Re,groundColor:new Re};break;case"RectAreaLight":t={color:new Re,position:new A,halfWidth:new A,halfHeight:new A};break}return n[e.id]=t,t}}}function Wp(){const n={};return{get:function(e){if(n[e.id]!==void 0)return n[e.id];let t;switch(e.type){case"DirectionalLight":t={shadowIntensity:1,shadowBias:0,shadowNormalBias:0,shadowRadius:1,shadowMapSize:new Ne};break;case"SpotLight":t={shadowIntensity:1,shadowBias:0,shadowNormalBias:0,shadowRadius:1,shadowMapSize:new Ne};break;case"PointLight":t={shadowIntensity:1,shadowBias:0,shadowNormalBias:0,shadowRadius:1,shadowMapSize:new Ne,shadowCameraNear:1,shadowCameraFar:1e3};break}return n[e.id]=t,t}}}let Xp=0;function jp(n,e){return(e.castShadow?2:0)-(n.castShadow?2:0)+(e.map?1:0)-(n.map?1:0)}function qp(n){const e=new Hp,t=Wp(),i={version:0,hash:{directionalLength:-1,pointLength:-1,spotLength:-1,rectAreaLength:-1,hemiLength:-1,numDirectionalShadows:-1,numPointShadows:-1,numSpotShadows:-1,numSpotMaps:-1,numLightProbes:-1},ambient:[0,0,0],probe:[],directional:[],directionalShadow:[],directionalShadowMap:[],directionalShadowMatrix:[],spot:[],spotLightMap:[],spotShadow:[],spotShadowMap:[],spotLightMatrix:[],rectArea:[],rectAreaLTC1:null,rectAreaLTC2:null,point:[],pointShadow:[],pointShadowMap:[],pointShadowMatrix:[],hemi:[],numSpotLightShadowsWithMaps:0,numLightProbes:0};for(let c=0;c<9;c++)i.probe.push(new A);const s=new A,r=new lt,a=new lt;function o(c){let h=0,d=0,u=0;for(let T=0;T<9;T++)i.probe[T].set(0,0,0);let m=0,_=0,x=0,p=0,f=0,M=0,y=0,E=0,L=0,w=0,D=0;c.sort(jp);for(let T=0,U=c.length;T<U;T++){const C=c[T],z=C.color,X=C.intensity,j=C.distance;let N=null;if(C.shadow&&C.shadow.map&&(C.shadow.map.texture.format===Yi?N=C.shadow.map.texture:N=C.shadow.map.depthTexture||C.shadow.map.texture),C.isAmbientLight)h+=z.r*X,d+=z.g*X,u+=z.b*X;else if(C.isLightProbe){for(let V=0;V<9;V++)i.probe[V].addScaledVector(C.sh.coefficients[V],X);D++}else if(C.isDirectionalLight){const V=e.get(C);if(V.color.copy(C.color).multiplyScalar(C.intensity),C.castShadow){const H=C.shadow,K=t.get(C);K.shadowIntensity=H.intensity,K.shadowBias=H.bias,K.shadowNormalBias=H.normalBias,K.shadowRadius=H.radius,K.shadowMapSize=H.mapSize,i.directionalShadow[m]=K,i.directionalShadowMap[m]=N,i.directionalShadowMatrix[m]=C.shadow.matrix,M++}i.directional[m]=V,m++}else if(C.isSpotLight){const V=e.get(C);V.position.setFromMatrixPosition(C.matrixWorld),V.color.copy(z).multiplyScalar(X),V.distance=j,V.coneCos=Math.cos(C.angle),V.penumbraCos=Math.cos(C.angle*(1-C.penumbra)),V.decay=C.decay,i.spot[x]=V;const H=C.shadow;if(C.map&&(i.spotLightMap[L]=C.map,L++,H.updateMatrices(C),C.castShadow&&w++),i.spotLightMatrix[x]=H.matrix,C.castShadow){const K=t.get(C);K.shadowIntensity=H.intensity,K.shadowBias=H.bias,K.shadowNormalBias=H.normalBias,K.shadowRadius=H.radius,K.shadowMapSize=H.mapSize,i.spotShadow[x]=K,i.spotShadowMap[x]=N,E++}x++}else if(C.isRectAreaLight){const V=e.get(C);V.color.copy(z).multiplyScalar(X),V.halfWidth.set(C.width*.5,0,0),V.halfHeight.set(0,C.height*.5,0),i.rectArea[p]=V,p++}else if(C.isPointLight){const V=e.get(C);if(V.color.copy(C.color).multiplyScalar(C.intensity),V.distance=C.distance,V.decay=C.decay,C.castShadow){const H=C.shadow,K=t.get(C);K.shadowIntensity=H.intensity,K.shadowBias=H.bias,K.shadowNormalBias=H.normalBias,K.shadowRadius=H.radius,K.shadowMapSize=H.mapSize,K.shadowCameraNear=H.camera.near,K.shadowCameraFar=H.camera.far,i.pointShadow[_]=K,i.pointShadowMap[_]=N,i.pointShadowMatrix[_]=C.shadow.matrix,y++}i.point[_]=V,_++}else if(C.isHemisphereLight){const V=e.get(C);V.skyColor.copy(C.color).multiplyScalar(X),V.groundColor.copy(C.groundColor).multiplyScalar(X),i.hemi[f]=V,f++}}p>0&&(n.has("OES_texture_float_linear")===!0?(i.rectAreaLTC1=he.LTC_FLOAT_1,i.rectAreaLTC2=he.LTC_FLOAT_2):(i.rectAreaLTC1=he.LTC_HALF_1,i.rectAreaLTC2=he.LTC_HALF_2)),i.ambient[0]=h,i.ambient[1]=d,i.ambient[2]=u;const b=i.hash;(b.directionalLength!==m||b.pointLength!==_||b.spotLength!==x||b.rectAreaLength!==p||b.hemiLength!==f||b.numDirectionalShadows!==M||b.numPointShadows!==y||b.numSpotShadows!==E||b.numSpotMaps!==L||b.numLightProbes!==D)&&(i.directional.length=m,i.spot.length=x,i.rectArea.length=p,i.point.length=_,i.hemi.length=f,i.directionalShadow.length=M,i.directionalShadowMap.length=M,i.pointShadow.length=y,i.pointShadowMap.length=y,i.spotShadow.length=E,i.spotShadowMap.length=E,i.directionalShadowMatrix.length=M,i.pointShadowMatrix.length=y,i.spotLightMatrix.length=E+L-w,i.spotLightMap.length=L,i.numSpotLightShadowsWithMaps=w,i.numLightProbes=D,b.directionalLength=m,b.pointLength=_,b.spotLength=x,b.rectAreaLength=p,b.hemiLength=f,b.numDirectionalShadows=M,b.numPointShadows=y,b.numSpotShadows=E,b.numSpotMaps=L,b.numLightProbes=D,i.version=Xp++)}function l(c,h){let d=0,u=0,m=0,_=0,x=0;const p=h.matrixWorldInverse;for(let f=0,M=c.length;f<M;f++){const y=c[f];if(y.isDirectionalLight){const E=i.directional[d];E.direction.setFromMatrixPosition(y.matrixWorld),s.setFromMatrixPosition(y.target.matrixWorld),E.direction.sub(s),E.direction.transformDirection(p),d++}else if(y.isSpotLight){const E=i.spot[m];E.position.setFromMatrixPosition(y.matrixWorld),E.position.applyMatrix4(p),E.direction.setFromMatrixPosition(y.matrixWorld),s.setFromMatrixPosition(y.target.matrixWorld),E.direction.sub(s),E.direction.transformDirection(p),m++}else if(y.isRectAreaLight){const E=i.rectArea[_];E.position.setFromMatrixPosition(y.matrixWorld),E.position.applyMatrix4(p),a.identity(),r.copy(y.matrixWorld),r.premultiply(p),a.extractRotation(r),E.halfWidth.set(y.width*.5,0,0),E.halfHeight.set(0,y.height*.5,0),E.halfWidth.applyMatrix4(a),E.halfHeight.applyMatrix4(a),_++}else if(y.isPointLight){const E=i.point[u];E.position.setFromMatrixPosition(y.matrixWorld),E.position.applyMatrix4(p),u++}else if(y.isHemisphereLight){const E=i.hemi[x];E.direction.setFromMatrixPosition(y.matrixWorld),E.direction.transformDirection(p),x++}}}return{setup:o,setupView:l,state:i}}function Bl(n){const e=new qp(n),t=[],i=[],s=[];function r(u){d.camera=u,t.length=0,i.length=0,s.length=0}function a(u){t.push(u)}function o(u){i.push(u)}function l(u){s.push(u)}function c(){e.setup(t)}function h(u){e.setupView(t,u)}const d={lightsArray:t,shadowsArray:i,lightProbeGridArray:s,camera:null,lights:e,transmissionRenderTarget:{},textureUnits:0};return{init:r,state:d,setupLights:c,setupLightsView:h,pushLight:a,pushShadow:o,pushLightProbeGrid:l}}function Yp(n){let e=new WeakMap;function t(s,r=0){const a=e.get(s);let o;return a===void 0?(o=new Bl(n),e.set(s,[o])):r>=a.length?(o=new Bl(n),a.push(o)):o=a[r],o}function i(){e=new WeakMap}return{get:t,dispose:i}}const $p=`void main() {
	gl_Position = vec4( position, 1.0 );
}`,Zp=`uniform sampler2D shadow_pass;
uniform vec2 resolution;
uniform float radius;
void main() {
	const float samples = float( VSM_SAMPLES );
	float mean = 0.0;
	float squared_mean = 0.0;
	float uvStride = samples <= 1.0 ? 0.0 : 2.0 / ( samples - 1.0 );
	float uvStart = samples <= 1.0 ? 0.0 : - 1.0;
	for ( float i = 0.0; i < samples; i ++ ) {
		float uvOffset = uvStart + i * uvStride;
		#ifdef HORIZONTAL_PASS
			vec2 distribution = texture2D( shadow_pass, ( gl_FragCoord.xy + vec2( uvOffset, 0.0 ) * radius ) / resolution ).rg;
			mean += distribution.x;
			squared_mean += distribution.y * distribution.y + distribution.x * distribution.x;
		#else
			float depth = texture2D( shadow_pass, ( gl_FragCoord.xy + vec2( 0.0, uvOffset ) * radius ) / resolution ).r;
			mean += depth;
			squared_mean += depth * depth;
		#endif
	}
	mean = mean / samples;
	squared_mean = squared_mean / samples;
	float std_dev = sqrt( max( 0.0, squared_mean - mean * mean ) );
	gl_FragColor = vec4( mean, std_dev, 0.0, 1.0 );
}`,Kp=[new A(1,0,0),new A(-1,0,0),new A(0,1,0),new A(0,-1,0),new A(0,0,1),new A(0,0,-1)],Jp=[new A(0,-1,0),new A(0,-1,0),new A(0,0,1),new A(0,0,-1),new A(0,-1,0),new A(0,-1,0)],zl=new lt,Kn=new A,Fa=new A;function Qp(n,e,t){let i=new ya;const s=new Ne,r=new Ne,a=new ht,o=new zh,l=new kh,c={},h=t.maxTextureSize,d={[ni]:vt,[vt]:ni,[It]:It},u=new ci({defines:{VSM_SAMPLES:8},uniforms:{shadow_pass:{value:null},resolution:{value:new Ne},radius:{value:4}},vertexShader:$p,fragmentShader:Zp}),m=u.clone();m.defines.HORIZONTAL_PASS=1;const _=new _t;_.setAttribute("position",new Lt(new Float32Array([-1,-1,.5,3,-1,.5,-1,3,.5]),3));const x=new Ht(_,u),p=this;this.enabled=!1,this.autoUpdate=!0,this.needsUpdate=!1,this.type=Qn;let f=this.type;this.render=function(w,D,b){if(p.enabled===!1||p.autoUpdate===!1&&p.needsUpdate===!1||w.length===0)return;this.type===fc&&(Ce("WebGLShadowMap: PCFSoftShadowMap has been deprecated. Using PCFShadowMap instead."),this.type=Qn);const T=n.getRenderTarget(),U=n.getActiveCubeFace(),C=n.getActiveMipmapLevel(),z=n.state;z.setBlending(gi),z.buffers.depth.getReversed()===!0?z.buffers.color.setClear(0,0,0,0):z.buffers.color.setClear(1,1,1,1),z.buffers.depth.setTest(!0),z.setScissorTest(!1);const X=f!==this.type;X&&D.traverse(function(j){j.material&&(Array.isArray(j.material)?j.material.forEach(N=>N.needsUpdate=!0):j.material.needsUpdate=!0)});for(let j=0,N=w.length;j<N;j++){const V=w[j],H=V.shadow;if(H===void 0){Ce("WebGLShadowMap:",V,"has no shadow.");continue}if(H.autoUpdate===!1&&H.needsUpdate===!1)continue;s.copy(H.mapSize);const K=H.getFrameExtents();s.multiply(K),r.copy(H.mapSize),(s.x>h||s.y>h)&&(s.x>h&&(r.x=Math.floor(h/K.x),s.x=r.x*K.x,H.mapSize.x=r.x),s.y>h&&(r.y=Math.floor(h/K.y),s.y=r.y*K.y,H.mapSize.y=r.y));const ee=n.state.buffers.depth.getReversed();if(H.camera._reversedDepth=ee,H.map===null||X===!0){if(H.map!==null&&(H.map.depthTexture!==null&&(H.map.depthTexture.dispose(),H.map.depthTexture=null),H.map.dispose()),this.type===Un){if(V.isPointLight){Ce("WebGLShadowMap: VSM shadow maps are not supported for PointLights. Use PCF or BasicShadowMap instead.");continue}H.map=new li(s.x,s.y,{format:Yi,type:vi,minFilter:Ct,magFilter:Ct,generateMipmaps:!1}),H.map.texture.name=V.name+".shadowMap",H.map.depthTexture=new En(s.x,s.y,ai),H.map.depthTexture.name=V.name+".shadowMapDepth",H.map.depthTexture.format=xi,H.map.depthTexture.compareFunction=null,H.map.depthTexture.minFilter=Tt,H.map.depthTexture.magFilter=Tt}else V.isPointLight?(H.map=new vl(s.x),H.map.depthTexture=new Dh(s.x,ri)):(H.map=new li(s.x,s.y),H.map.depthTexture=new En(s.x,s.y,ri)),H.map.depthTexture.name=V.name+".shadowMap",H.map.depthTexture.format=xi,this.type===Qn?(H.map.depthTexture.compareFunction=ee?$r:Yr,H.map.depthTexture.minFilter=Ct,H.map.depthTexture.magFilter=Ct):(H.map.depthTexture.compareFunction=null,H.map.depthTexture.minFilter=Tt,H.map.depthTexture.magFilter=Tt);H.camera.updateProjectionMatrix()}const ce=H.map.isWebGLCubeRenderTarget?6:1;for(let be=0;be<ce;be++){if(H.map.isWebGLCubeRenderTarget)n.setRenderTarget(H.map,be),n.clear();else{be===0&&(n.setRenderTarget(H.map),n.clear());const Me=H.getViewport(be);a.set(r.x*Me.x,r.y*Me.y,r.x*Me.z,r.y*Me.w),z.viewport(a)}if(V.isPointLight){const Me=H.camera,He=H.matrix,Ze=V.distance||Me.far;Ze!==Me.far&&(Me.far=Ze,Me.updateProjectionMatrix()),Kn.setFromMatrixPosition(V.matrixWorld),Me.position.copy(Kn),Fa.copy(Me.position),Fa.add(Kp[be]),Me.up.copy(Jp[be]),Me.lookAt(Fa),Me.updateMatrixWorld(),He.makeTranslation(-Kn.x,-Kn.y,-Kn.z),zl.multiplyMatrices(Me.projectionMatrix,Me.matrixWorldInverse),H._frustum.setFromProjectionMatrix(zl,Me.coordinateSystem,Me.reversedDepth)}else H.updateMatrices(V);i=H.getFrustum(),E(D,b,H.camera,V,this.type)}H.isPointLightShadow!==!0&&this.type===Un&&M(H,b),H.needsUpdate=!1}f=this.type,p.needsUpdate=!1,n.setRenderTarget(T,U,C)};function M(w,D){const b=e.update(x);u.defines.VSM_SAMPLES!==w.blurSamples&&(u.defines.VSM_SAMPLES=w.blurSamples,m.defines.VSM_SAMPLES=w.blurSamples,u.needsUpdate=!0,m.needsUpdate=!0),w.mapPass===null&&(w.mapPass=new li(s.x,s.y,{format:Yi,type:vi})),u.uniforms.shadow_pass.value=w.map.depthTexture,u.uniforms.resolution.value=w.mapSize,u.uniforms.radius.value=w.radius,n.setRenderTarget(w.mapPass),n.clear(),n.renderBufferDirect(D,null,b,u,x,null),m.uniforms.shadow_pass.value=w.mapPass.texture,m.uniforms.resolution.value=w.mapSize,m.uniforms.radius.value=w.radius,n.setRenderTarget(w.map),n.clear(),n.renderBufferDirect(D,null,b,m,x,null)}function y(w,D,b,T){let U=null;const C=b.isPointLight===!0?w.customDistanceMaterial:w.customDepthMaterial;if(C!==void 0)U=C;else if(U=b.isPointLight===!0?l:o,n.localClippingEnabled&&D.clipShadows===!0&&Array.isArray(D.clippingPlanes)&&D.clippingPlanes.length!==0||D.displacementMap&&D.displacementScale!==0||D.alphaMap&&D.alphaTest>0||D.map&&D.alphaTest>0||D.alphaToCoverage===!0){const z=U.uuid,X=D.uuid;let j=c[z];j===void 0&&(j={},c[z]=j);let N=j[X];N===void 0&&(N=U.clone(),j[X]=N,D.addEventListener("dispose",L)),U=N}if(U.visible=D.visible,U.wireframe=D.wireframe,T===Un?U.side=D.shadowSide!==null?D.shadowSide:D.side:U.side=D.shadowSide!==null?D.shadowSide:d[D.side],U.alphaMap=D.alphaMap,U.alphaTest=D.alphaToCoverage===!0?.5:D.alphaTest,U.map=D.map,U.clipShadows=D.clipShadows,U.clippingPlanes=D.clippingPlanes,U.clipIntersection=D.clipIntersection,U.displacementMap=D.displacementMap,U.displacementScale=D.displacementScale,U.displacementBias=D.displacementBias,U.wireframeLinewidth=D.wireframeLinewidth,U.linewidth=D.linewidth,b.isPointLight===!0&&U.isMeshDistanceMaterial===!0){const z=n.properties.get(U);z.light=b}return U}function E(w,D,b,T,U){if(w.visible===!1)return;if(w.layers.test(D.layers)&&(w.isMesh||w.isLine||w.isPoints)&&(w.castShadow||w.receiveShadow&&U===Un)&&(!w.frustumCulled||i.intersectsObject(w))){w.modelViewMatrix.multiplyMatrices(b.matrixWorldInverse,w.matrixWorld);const X=e.update(w),j=w.material;if(Array.isArray(j)){const N=X.groups;for(let V=0,H=N.length;V<H;V++){const K=N[V],ee=j[K.materialIndex];if(ee&&ee.visible){const ce=y(w,ee,T,U);w.onBeforeShadow(n,w,D,b,X,ce,K),n.renderBufferDirect(b,null,X,ce,w,K),w.onAfterShadow(n,w,D,b,X,ce,K)}}}else if(j.visible){const N=y(w,j,T,U);w.onBeforeShadow(n,w,D,b,X,N,null),n.renderBufferDirect(b,null,X,N,w,null),w.onAfterShadow(n,w,D,b,X,N,null)}}const z=w.children;for(let X=0,j=z.length;X<j;X++)E(z[X],D,b,T,U)}function L(w){w.target.removeEventListener("dispose",L);for(const b in c){const T=c[b],U=w.target.uuid;U in T&&(T[U].dispose(),delete T[U])}}}function em(n,e){function t(){let R=!1;const re=new ht;let q=null;const ge=new ht(0,0,0,0);return{setMask:function(oe){q!==oe&&!R&&(n.colorMask(oe,oe,oe,oe),q=oe)},setLocked:function(oe){R=oe},setClear:function(oe,J,ye,Fe,ft){ft===!0&&(oe*=Fe,J*=Fe,ye*=Fe),re.set(oe,J,ye,Fe),ge.equals(re)===!1&&(n.clearColor(oe,J,ye,Fe),ge.copy(re))},reset:function(){R=!1,q=null,ge.set(-1,0,0,0)}}}function i(){let R=!1,re=!1,q=null,ge=null,oe=null;return{setReversed:function(J){if(re!==J){const ye=e.get("EXT_clip_control");J?ye.clipControlEXT(ye.LOWER_LEFT_EXT,ye.ZERO_TO_ONE_EXT):ye.clipControlEXT(ye.LOWER_LEFT_EXT,ye.NEGATIVE_ONE_TO_ONE_EXT),re=J;const Fe=oe;oe=null,this.setClear(Fe)}},getReversed:function(){return re},setTest:function(J){J?te(n.DEPTH_TEST):Te(n.DEPTH_TEST)},setMask:function(J){q!==J&&!R&&(n.depthMask(J),q=J)},setFunc:function(J){if(re&&(J=jc[J]),ge!==J){switch(J){case Qs:n.depthFunc(n.NEVER);break;case er:n.depthFunc(n.ALWAYS);break;case tr:n.depthFunc(n.LESS);break;case an:n.depthFunc(n.LEQUAL);break;case ir:n.depthFunc(n.EQUAL);break;case nr:n.depthFunc(n.GEQUAL);break;case sr:n.depthFunc(n.GREATER);break;case rr:n.depthFunc(n.NOTEQUAL);break;default:n.depthFunc(n.LEQUAL)}ge=J}},setLocked:function(J){R=J},setClear:function(J){oe!==J&&(oe=J,re&&(J=1-J),n.clearDepth(J))},reset:function(){R=!1,q=null,ge=null,oe=null,re=!1}}}function s(){let R=!1,re=null,q=null,ge=null,oe=null,J=null,ye=null,Fe=null,ft=null;return{setTest:function(it){R||(it?te(n.STENCIL_TEST):Te(n.STENCIL_TEST))},setMask:function(it){re!==it&&!R&&(n.stencilMask(it),re=it)},setFunc:function(it,Ai,pi){(q!==it||ge!==Ai||oe!==pi)&&(n.stencilFunc(it,Ai,pi),q=it,ge=Ai,oe=pi)},setOp:function(it,Ai,pi){(J!==it||ye!==Ai||Fe!==pi)&&(n.stencilOp(it,Ai,pi),J=it,ye=Ai,Fe=pi)},setLocked:function(it){R=it},setClear:function(it){ft!==it&&(n.clearStencil(it),ft=it)},reset:function(){R=!1,re=null,q=null,ge=null,oe=null,J=null,ye=null,Fe=null,ft=null}}}const r=new t,a=new i,o=new s,l=new WeakMap,c=new WeakMap;let h={},d={},u={},m=new WeakMap,_=[],x=null,p=!1,f=null,M=null,y=null,E=null,L=null,w=null,D=null,b=new Re(0,0,0),T=0,U=!1,C=null,z=null,X=null,j=null,N=null;const V=n.getParameter(n.MAX_COMBINED_TEXTURE_IMAGE_UNITS);let H=!1,K=0;const ee=n.getParameter(n.VERSION);ee.indexOf("WebGL")!==-1?(K=parseFloat(/^WebGL (\d)/.exec(ee)[1]),H=K>=1):ee.indexOf("OpenGL ES")!==-1&&(K=parseFloat(/^OpenGL ES (\d)/.exec(ee)[1]),H=K>=2);let ce=null,be={};const Me=n.getParameter(n.SCISSOR_BOX),He=n.getParameter(n.VIEWPORT),Ze=new ht().fromArray(Me),De=new ht().fromArray(He);function Z(R,re,q,ge){const oe=new Uint8Array(4),J=n.createTexture();n.bindTexture(R,J),n.texParameteri(R,n.TEXTURE_MIN_FILTER,n.NEAREST),n.texParameteri(R,n.TEXTURE_MAG_FILTER,n.NEAREST);for(let ye=0;ye<q;ye++)R===n.TEXTURE_3D||R===n.TEXTURE_2D_ARRAY?n.texImage3D(re,0,n.RGBA,1,1,ge,0,n.RGBA,n.UNSIGNED_BYTE,oe):n.texImage2D(re+ye,0,n.RGBA,1,1,0,n.RGBA,n.UNSIGNED_BYTE,oe);return J}const ue={};ue[n.TEXTURE_2D]=Z(n.TEXTURE_2D,n.TEXTURE_2D,1),ue[n.TEXTURE_CUBE_MAP]=Z(n.TEXTURE_CUBE_MAP,n.TEXTURE_CUBE_MAP_POSITIVE_X,6),ue[n.TEXTURE_2D_ARRAY]=Z(n.TEXTURE_2D_ARRAY,n.TEXTURE_2D_ARRAY,1,1),ue[n.TEXTURE_3D]=Z(n.TEXTURE_3D,n.TEXTURE_3D,1,1),r.setClear(0,0,0,1),a.setClear(1),o.setClear(0),te(n.DEPTH_TEST),a.setFunc(an),yt(!1),ct(Za),te(n.CULL_FACE),Ke(gi);function te(R){h[R]!==!0&&(n.enable(R),h[R]=!0)}function Te(R){h[R]!==!1&&(n.disable(R),h[R]=!1)}function Le(R,re){return u[R]!==re?(n.bindFramebuffer(R,re),u[R]=re,R===n.DRAW_FRAMEBUFFER&&(u[n.FRAMEBUFFER]=re),R===n.FRAMEBUFFER&&(u[n.DRAW_FRAMEBUFFER]=re),!0):!1}function Ae(R,re){let q=_,ge=!1;if(R){q=m.get(re),q===void 0&&(q=[],m.set(re,q));const oe=R.textures;if(q.length!==oe.length||q[0]!==n.COLOR_ATTACHMENT0){for(let J=0,ye=oe.length;J<ye;J++)q[J]=n.COLOR_ATTACHMENT0+J;q.length=oe.length,ge=!0}}else q[0]!==n.BACK&&(q[0]=n.BACK,ge=!0);ge&&n.drawBuffers(q)}function tt(R){return x!==R?(n.useProgram(R),x=R,!0):!1}const ie={[Wi]:n.FUNC_ADD,[mc]:n.FUNC_SUBTRACT,[gc]:n.FUNC_REVERSE_SUBTRACT};ie[_c]=n.MIN,ie[vc]=n.MAX;const Pe={[xc]:n.ZERO,[bc]:n.ONE,[Sc]:n.SRC_COLOR,[Ks]:n.SRC_ALPHA,[Ac]:n.SRC_ALPHA_SATURATE,[wc]:n.DST_COLOR,[yc]:n.DST_ALPHA,[Mc]:n.ONE_MINUS_SRC_COLOR,[Js]:n.ONE_MINUS_SRC_ALPHA,[Tc]:n.ONE_MINUS_DST_COLOR,[Ec]:n.ONE_MINUS_DST_ALPHA,[Cc]:n.CONSTANT_COLOR,[Rc]:n.ONE_MINUS_CONSTANT_COLOR,[Pc]:n.CONSTANT_ALPHA,[Lc]:n.ONE_MINUS_CONSTANT_ALPHA};function Ke(R,re,q,ge,oe,J,ye,Fe,ft,it){if(R===gi){p===!0&&(Te(n.BLEND),p=!1);return}if(p===!1&&(te(n.BLEND),p=!0),R!==pc){if(R!==f||it!==U){if((M!==Wi||L!==Wi)&&(n.blendEquation(n.FUNC_ADD),M=Wi,L=Wi),it)switch(R){case rn:n.blendFuncSeparate(n.ONE,n.ONE_MINUS_SRC_ALPHA,n.ONE,n.ONE_MINUS_SRC_ALPHA);break;case Ka:n.blendFunc(n.ONE,n.ONE);break;case Ja:n.blendFuncSeparate(n.ZERO,n.ONE_MINUS_SRC_COLOR,n.ZERO,n.ONE);break;case Qa:n.blendFuncSeparate(n.DST_COLOR,n.ONE_MINUS_SRC_ALPHA,n.ZERO,n.ONE);break;default:je("WebGLState: Invalid blending: ",R);break}else switch(R){case rn:n.blendFuncSeparate(n.SRC_ALPHA,n.ONE_MINUS_SRC_ALPHA,n.ONE,n.ONE_MINUS_SRC_ALPHA);break;case Ka:n.blendFuncSeparate(n.SRC_ALPHA,n.ONE,n.ONE,n.ONE);break;case Ja:je("WebGLState: SubtractiveBlending requires material.premultipliedAlpha = true");break;case Qa:je("WebGLState: MultiplyBlending requires material.premultipliedAlpha = true");break;default:je("WebGLState: Invalid blending: ",R);break}y=null,E=null,w=null,D=null,b.set(0,0,0),T=0,f=R,U=it}return}oe=oe||re,J=J||q,ye=ye||ge,(re!==M||oe!==L)&&(n.blendEquationSeparate(ie[re],ie[oe]),M=re,L=oe),(q!==y||ge!==E||J!==w||ye!==D)&&(n.blendFuncSeparate(Pe[q],Pe[ge],Pe[J],Pe[ye]),y=q,E=ge,w=J,D=ye),(Fe.equals(b)===!1||ft!==T)&&(n.blendColor(Fe.r,Fe.g,Fe.b,ft),b.copy(Fe),T=ft),f=R,U=!1}function ke(R,re){R.side===It?Te(n.CULL_FACE):te(n.CULL_FACE);let q=R.side===vt;re&&(q=!q),yt(q),R.blending===rn&&R.transparent===!1?Ke(gi):Ke(R.blending,R.blendEquation,R.blendSrc,R.blendDst,R.blendEquationAlpha,R.blendSrcAlpha,R.blendDstAlpha,R.blendColor,R.blendAlpha,R.premultipliedAlpha),a.setFunc(R.depthFunc),a.setTest(R.depthTest),a.setMask(R.depthWrite),r.setMask(R.colorWrite);const ge=R.stencilWrite;o.setTest(ge),ge&&(o.setMask(R.stencilWriteMask),o.setFunc(R.stencilFunc,R.stencilRef,R.stencilFuncMask),o.setOp(R.stencilFail,R.stencilZFail,R.stencilZPass)),P(R.polygonOffset,R.polygonOffsetFactor,R.polygonOffsetUnits),R.alphaToCoverage===!0?te(n.SAMPLE_ALPHA_TO_COVERAGE):Te(n.SAMPLE_ALPHA_TO_COVERAGE)}function yt(R){C!==R&&(R?n.frontFace(n.CW):n.frontFace(n.CCW),C=R)}function ct(R){R!==uc?(te(n.CULL_FACE),R!==z&&(R===Za?n.cullFace(n.BACK):R===dc?n.cullFace(n.FRONT):n.cullFace(n.FRONT_AND_BACK))):Te(n.CULL_FACE),z=R}function Wt(R){R!==X&&(H&&n.lineWidth(R),X=R)}function P(R,re,q){R?(te(n.POLYGON_OFFSET_FILL),(j!==re||N!==q)&&(j=re,N=q,a.getReversed()&&(re=-re),n.polygonOffset(re,q))):Te(n.POLYGON_OFFSET_FILL)}function Et(R){R?te(n.SCISSOR_TEST):Te(n.SCISSOR_TEST)}function We(R){R===void 0&&(R=n.TEXTURE0+V-1),ce!==R&&(n.activeTexture(R),ce=R)}function at(R,re,q){q===void 0&&(ce===null?q=n.TEXTURE0+V-1:q=ce);let ge=be[q];ge===void 0&&(ge={type:void 0,texture:void 0},be[q]=ge),(ge.type!==R||ge.texture!==re)&&(ce!==q&&(n.activeTexture(q),ce=q),n.bindTexture(R,re||ue[R]),ge.type=R,ge.texture=re)}function de(){const R=be[ce];R!==void 0&&R.type!==void 0&&(n.bindTexture(R.type,null),R.type=void 0,R.texture=void 0)}function ut(){try{n.compressedTexImage2D(...arguments)}catch(R){je("WebGLState:",R)}}function S(){try{n.compressedTexImage3D(...arguments)}catch(R){je("WebGLState:",R)}}function g(){try{n.texSubImage2D(...arguments)}catch(R){je("WebGLState:",R)}}function F(){try{n.texSubImage3D(...arguments)}catch(R){je("WebGLState:",R)}}function Y(){try{n.compressedTexSubImage2D(...arguments)}catch(R){je("WebGLState:",R)}}function Q(){try{n.compressedTexSubImage3D(...arguments)}catch(R){je("WebGLState:",R)}}function ne(){try{n.texStorage2D(...arguments)}catch(R){je("WebGLState:",R)}}function le(){try{n.texStorage3D(...arguments)}catch(R){je("WebGLState:",R)}}function W(){try{n.texImage2D(...arguments)}catch(R){je("WebGLState:",R)}}function $(){try{n.texImage3D(...arguments)}catch(R){je("WebGLState:",R)}}function me(R){return d[R]!==void 0?d[R]:n.getParameter(R)}function ve(R,re){d[R]!==re&&(n.pixelStorei(R,re),d[R]=re)}function ae(R){Ze.equals(R)===!1&&(n.scissor(R.x,R.y,R.z,R.w),Ze.copy(R))}function se(R){De.equals(R)===!1&&(n.viewport(R.x,R.y,R.z,R.w),De.copy(R))}function Ie(R,re){let q=c.get(re);q===void 0&&(q=new WeakMap,c.set(re,q));let ge=q.get(R);ge===void 0&&(ge=n.getUniformBlockIndex(re,R.name),q.set(R,ge))}function Be(R,re){const ge=c.get(re).get(R);l.get(re)!==ge&&(n.uniformBlockBinding(re,ge,R.__bindingPointIndex),l.set(re,ge))}function $e(){n.disable(n.BLEND),n.disable(n.CULL_FACE),n.disable(n.DEPTH_TEST),n.disable(n.POLYGON_OFFSET_FILL),n.disable(n.SCISSOR_TEST),n.disable(n.STENCIL_TEST),n.disable(n.SAMPLE_ALPHA_TO_COVERAGE),n.blendEquation(n.FUNC_ADD),n.blendFunc(n.ONE,n.ZERO),n.blendFuncSeparate(n.ONE,n.ZERO,n.ONE,n.ZERO),n.blendColor(0,0,0,0),n.colorMask(!0,!0,!0,!0),n.clearColor(0,0,0,0),n.depthMask(!0),n.depthFunc(n.LESS),a.setReversed(!1),n.clearDepth(1),n.stencilMask(4294967295),n.stencilFunc(n.ALWAYS,0,4294967295),n.stencilOp(n.KEEP,n.KEEP,n.KEEP),n.clearStencil(0),n.cullFace(n.BACK),n.frontFace(n.CCW),n.polygonOffset(0,0),n.activeTexture(n.TEXTURE0),n.bindFramebuffer(n.FRAMEBUFFER,null),n.bindFramebuffer(n.DRAW_FRAMEBUFFER,null),n.bindFramebuffer(n.READ_FRAMEBUFFER,null),n.useProgram(null),n.lineWidth(1),n.scissor(0,0,n.canvas.width,n.canvas.height),n.viewport(0,0,n.canvas.width,n.canvas.height),n.pixelStorei(n.PACK_ALIGNMENT,4),n.pixelStorei(n.UNPACK_ALIGNMENT,4),n.pixelStorei(n.UNPACK_FLIP_Y_WEBGL,!1),n.pixelStorei(n.UNPACK_PREMULTIPLY_ALPHA_WEBGL,!1),n.pixelStorei(n.UNPACK_COLORSPACE_CONVERSION_WEBGL,n.BROWSER_DEFAULT_WEBGL),n.pixelStorei(n.PACK_ROW_LENGTH,0),n.pixelStorei(n.PACK_SKIP_PIXELS,0),n.pixelStorei(n.PACK_SKIP_ROWS,0),n.pixelStorei(n.UNPACK_ROW_LENGTH,0),n.pixelStorei(n.UNPACK_IMAGE_HEIGHT,0),n.pixelStorei(n.UNPACK_SKIP_PIXELS,0),n.pixelStorei(n.UNPACK_SKIP_ROWS,0),n.pixelStorei(n.UNPACK_SKIP_IMAGES,0),h={},d={},ce=null,be={},u={},m=new WeakMap,_=[],x=null,p=!1,f=null,M=null,y=null,E=null,L=null,w=null,D=null,b=new Re(0,0,0),T=0,U=!1,C=null,z=null,X=null,j=null,N=null,Ze.set(0,0,n.canvas.width,n.canvas.height),De.set(0,0,n.canvas.width,n.canvas.height),r.reset(),a.reset(),o.reset()}return{buffers:{color:r,depth:a,stencil:o},enable:te,disable:Te,bindFramebuffer:Le,drawBuffers:Ae,useProgram:tt,setBlending:Ke,setMaterial:ke,setFlipSided:yt,setCullFace:ct,setLineWidth:Wt,setPolygonOffset:P,setScissorTest:Et,activeTexture:We,bindTexture:at,unbindTexture:de,compressedTexImage2D:ut,compressedTexImage3D:S,texImage2D:W,texImage3D:$,pixelStorei:ve,getParameter:me,updateUBOMapping:Ie,uniformBlockBinding:Be,texStorage2D:ne,texStorage3D:le,texSubImage2D:g,texSubImage3D:F,compressedTexSubImage2D:Y,compressedTexSubImage3D:Q,scissor:ae,viewport:se,reset:$e}}function tm(n,e,t,i,s,r,a){const o=e.has("WEBGL_multisampled_render_to_texture")?e.get("WEBGL_multisampled_render_to_texture"):null,l=typeof navigator>"u"?!1:/OculusBrowser/g.test(navigator.userAgent),c=new Ne,h=new WeakMap,d=new Set;let u;const m=new WeakMap;let _=!1;try{_=typeof OffscreenCanvas<"u"&&new OffscreenCanvas(1,1).getContext("2d")!==null}catch{}function x(S,g){return _?new OffscreenCanvas(S,g):us("canvas")}function p(S,g,F){let Y=1;const Q=ut(S);if((Q.width>F||Q.height>F)&&(Y=F/Math.max(Q.width,Q.height)),Y<1)if(typeof HTMLImageElement<"u"&&S instanceof HTMLImageElement||typeof HTMLCanvasElement<"u"&&S instanceof HTMLCanvasElement||typeof ImageBitmap<"u"&&S instanceof ImageBitmap||typeof VideoFrame<"u"&&S instanceof VideoFrame){const ne=Math.floor(Y*Q.width),le=Math.floor(Y*Q.height);u===void 0&&(u=x(ne,le));const W=g?x(ne,le):u;return W.width=ne,W.height=le,W.getContext("2d").drawImage(S,0,0,ne,le),Ce("WebGLRenderer: Texture has been resized from ("+Q.width+"x"+Q.height+") to ("+ne+"x"+le+")."),W}else return"data"in S&&Ce("WebGLRenderer: Image in DataTexture is too big ("+Q.width+"x"+Q.height+")."),S;return S}function f(S){return S.generateMipmaps}function M(S){n.generateMipmap(S)}function y(S){return S.isWebGLCubeRenderTarget?n.TEXTURE_CUBE_MAP:S.isWebGL3DRenderTarget?n.TEXTURE_3D:S.isWebGLArrayRenderTarget||S.isCompressedArrayTexture?n.TEXTURE_2D_ARRAY:n.TEXTURE_2D}function E(S,g,F,Y,Q,ne=!1){if(S!==null){if(n[S]!==void 0)return n[S];Ce("WebGLRenderer: Attempt to use non-existing WebGL internal format '"+S+"'")}let le;Y&&(le=e.get("EXT_texture_norm16"),le||Ce("WebGLRenderer: Unable to use normalized textures without EXT_texture_norm16 extension"));let W=g;if(g===n.RED&&(F===n.FLOAT&&(W=n.R32F),F===n.HALF_FLOAT&&(W=n.R16F),F===n.UNSIGNED_BYTE&&(W=n.R8),F===n.UNSIGNED_SHORT&&le&&(W=le.R16_EXT),F===n.SHORT&&le&&(W=le.R16_SNORM_EXT)),g===n.RED_INTEGER&&(F===n.UNSIGNED_BYTE&&(W=n.R8UI),F===n.UNSIGNED_SHORT&&(W=n.R16UI),F===n.UNSIGNED_INT&&(W=n.R32UI),F===n.BYTE&&(W=n.R8I),F===n.SHORT&&(W=n.R16I),F===n.INT&&(W=n.R32I)),g===n.RG&&(F===n.FLOAT&&(W=n.RG32F),F===n.HALF_FLOAT&&(W=n.RG16F),F===n.UNSIGNED_BYTE&&(W=n.RG8),F===n.UNSIGNED_SHORT&&le&&(W=le.RG16_EXT),F===n.SHORT&&le&&(W=le.RG16_SNORM_EXT)),g===n.RG_INTEGER&&(F===n.UNSIGNED_BYTE&&(W=n.RG8UI),F===n.UNSIGNED_SHORT&&(W=n.RG16UI),F===n.UNSIGNED_INT&&(W=n.RG32UI),F===n.BYTE&&(W=n.RG8I),F===n.SHORT&&(W=n.RG16I),F===n.INT&&(W=n.RG32I)),g===n.RGB_INTEGER&&(F===n.UNSIGNED_BYTE&&(W=n.RGB8UI),F===n.UNSIGNED_SHORT&&(W=n.RGB16UI),F===n.UNSIGNED_INT&&(W=n.RGB32UI),F===n.BYTE&&(W=n.RGB8I),F===n.SHORT&&(W=n.RGB16I),F===n.INT&&(W=n.RGB32I)),g===n.RGBA_INTEGER&&(F===n.UNSIGNED_BYTE&&(W=n.RGBA8UI),F===n.UNSIGNED_SHORT&&(W=n.RGBA16UI),F===n.UNSIGNED_INT&&(W=n.RGBA32UI),F===n.BYTE&&(W=n.RGBA8I),F===n.SHORT&&(W=n.RGBA16I),F===n.INT&&(W=n.RGBA32I)),g===n.RGB&&(F===n.UNSIGNED_SHORT&&le&&(W=le.RGB16_EXT),F===n.SHORT&&le&&(W=le.RGB16_SNORM_EXT),F===n.UNSIGNED_INT_5_9_9_9_REV&&(W=n.RGB9_E5),F===n.UNSIGNED_INT_10F_11F_11F_REV&&(W=n.R11F_G11F_B10F)),g===n.RGBA){const $=ne?cs:Xe.getTransfer(Q);F===n.FLOAT&&(W=n.RGBA32F),F===n.HALF_FLOAT&&(W=n.RGBA16F),F===n.UNSIGNED_BYTE&&(W=$===Qe?n.SRGB8_ALPHA8:n.RGBA8),F===n.UNSIGNED_SHORT&&le&&(W=le.RGBA16_EXT),F===n.SHORT&&le&&(W=le.RGBA16_SNORM_EXT),F===n.UNSIGNED_SHORT_4_4_4_4&&(W=n.RGBA4),F===n.UNSIGNED_SHORT_5_5_5_1&&(W=n.RGB5_A1)}return(W===n.R16F||W===n.R32F||W===n.RG16F||W===n.RG32F||W===n.RGBA16F||W===n.RGBA32F)&&e.get("EXT_color_buffer_float"),W}function L(S,g){let F;return S?g===null||g===ri||g===On?F=n.DEPTH24_STENCIL8:g===ai?F=n.DEPTH32F_STENCIL8:g===Fn&&(F=n.DEPTH24_STENCIL8,Ce("DepthTexture: 16 bit depth attachment is not supported with stencil. Using 24-bit attachment.")):g===null||g===ri||g===On?F=n.DEPTH_COMPONENT24:g===ai?F=n.DEPTH_COMPONENT32F:g===Fn&&(F=n.DEPTH_COMPONENT16),F}function w(S,g){return f(S)===!0||S.isFramebufferTexture&&S.minFilter!==Tt&&S.minFilter!==Ct?Math.log2(Math.max(g.width,g.height))+1:S.mipmaps!==void 0&&S.mipmaps.length>0?S.mipmaps.length:S.isCompressedTexture&&Array.isArray(S.image)?g.mipmaps.length:1}function D(S){const g=S.target;g.removeEventListener("dispose",D),T(g),g.isVideoTexture&&h.delete(g),g.isHTMLTexture&&d.delete(g)}function b(S){const g=S.target;g.removeEventListener("dispose",b),C(g)}function T(S){const g=i.get(S);if(g.__webglInit===void 0)return;const F=S.source,Y=m.get(F);if(Y){const Q=Y[g.__cacheKey];Q.usedTimes--,Q.usedTimes===0&&U(S),Object.keys(Y).length===0&&m.delete(F)}i.remove(S)}function U(S){const g=i.get(S);n.deleteTexture(g.__webglTexture);const F=S.source,Y=m.get(F);delete Y[g.__cacheKey],a.memory.textures--}function C(S){const g=i.get(S);if(S.depthTexture&&(S.depthTexture.dispose(),i.remove(S.depthTexture)),S.isWebGLCubeRenderTarget)for(let Y=0;Y<6;Y++){if(Array.isArray(g.__webglFramebuffer[Y]))for(let Q=0;Q<g.__webglFramebuffer[Y].length;Q++)n.deleteFramebuffer(g.__webglFramebuffer[Y][Q]);else n.deleteFramebuffer(g.__webglFramebuffer[Y]);g.__webglDepthbuffer&&n.deleteRenderbuffer(g.__webglDepthbuffer[Y])}else{if(Array.isArray(g.__webglFramebuffer))for(let Y=0;Y<g.__webglFramebuffer.length;Y++)n.deleteFramebuffer(g.__webglFramebuffer[Y]);else n.deleteFramebuffer(g.__webglFramebuffer);if(g.__webglDepthbuffer&&n.deleteRenderbuffer(g.__webglDepthbuffer),g.__webglMultisampledFramebuffer&&n.deleteFramebuffer(g.__webglMultisampledFramebuffer),g.__webglColorRenderbuffer)for(let Y=0;Y<g.__webglColorRenderbuffer.length;Y++)g.__webglColorRenderbuffer[Y]&&n.deleteRenderbuffer(g.__webglColorRenderbuffer[Y]);g.__webglDepthRenderbuffer&&n.deleteRenderbuffer(g.__webglDepthRenderbuffer)}const F=S.textures;for(let Y=0,Q=F.length;Y<Q;Y++){const ne=i.get(F[Y]);ne.__webglTexture&&(n.deleteTexture(ne.__webglTexture),a.memory.textures--),i.remove(F[Y])}i.remove(S)}let z=0;function X(){z=0}function j(){return z}function N(S){z=S}function V(){const S=z;return S>=s.maxTextures&&Ce("WebGLTextures: Trying to use "+S+" texture units while this GPU supports only "+s.maxTextures),z+=1,S}function H(S){const g=[];return g.push(S.wrapS),g.push(S.wrapT),g.push(S.wrapR||0),g.push(S.magFilter),g.push(S.minFilter),g.push(S.anisotropy),g.push(S.internalFormat),g.push(S.format),g.push(S.type),g.push(S.generateMipmaps),g.push(S.premultiplyAlpha),g.push(S.flipY),g.push(S.unpackAlignment),g.push(S.colorSpace),g.join()}function K(S,g){const F=i.get(S);if(S.isVideoTexture&&at(S),S.isRenderTargetTexture===!1&&S.isExternalTexture!==!0&&S.version>0&&F.__version!==S.version){const Y=S.image;if(Y===null)Ce("WebGLRenderer: Texture marked for update but no image data found.");else if(Y.complete===!1)Ce("WebGLRenderer: Texture marked for update but image is incomplete");else{Te(F,S,g);return}}else S.isExternalTexture&&(F.__webglTexture=S.sourceTexture?S.sourceTexture:null);t.bindTexture(n.TEXTURE_2D,F.__webglTexture,n.TEXTURE0+g)}function ee(S,g){const F=i.get(S);if(S.isRenderTargetTexture===!1&&S.version>0&&F.__version!==S.version){Te(F,S,g);return}else S.isExternalTexture&&(F.__webglTexture=S.sourceTexture?S.sourceTexture:null);t.bindTexture(n.TEXTURE_2D_ARRAY,F.__webglTexture,n.TEXTURE0+g)}function ce(S,g){const F=i.get(S);if(S.isRenderTargetTexture===!1&&S.version>0&&F.__version!==S.version){Te(F,S,g);return}t.bindTexture(n.TEXTURE_3D,F.__webglTexture,n.TEXTURE0+g)}function be(S,g){const F=i.get(S);if(S.isCubeDepthTexture!==!0&&S.version>0&&F.__version!==S.version){Le(F,S,g);return}t.bindTexture(n.TEXTURE_CUBE_MAP,F.__webglTexture,n.TEXTURE0+g)}const Me={[lr]:n.REPEAT,[_i]:n.CLAMP_TO_EDGE,[cr]:n.MIRRORED_REPEAT},He={[Tt]:n.NEAREST,[Nc]:n.NEAREST_MIPMAP_NEAREST,[ts]:n.NEAREST_MIPMAP_LINEAR,[Ct]:n.LINEAR,[hr]:n.LINEAR_MIPMAP_NEAREST,[ji]:n.LINEAR_MIPMAP_LINEAR},Ze={[Oc]:n.NEVER,[Gc]:n.ALWAYS,[Bc]:n.LESS,[Yr]:n.LEQUAL,[zc]:n.EQUAL,[$r]:n.GEQUAL,[kc]:n.GREATER,[Vc]:n.NOTEQUAL};function De(S,g){if(g.type===ai&&e.has("OES_texture_float_linear")===!1&&(g.magFilter===Ct||g.magFilter===hr||g.magFilter===ts||g.magFilter===ji||g.minFilter===Ct||g.minFilter===hr||g.minFilter===ts||g.minFilter===ji)&&Ce("WebGLRenderer: Unable to use linear filtering with floating point textures. OES_texture_float_linear not supported on this device."),n.texParameteri(S,n.TEXTURE_WRAP_S,Me[g.wrapS]),n.texParameteri(S,n.TEXTURE_WRAP_T,Me[g.wrapT]),(S===n.TEXTURE_3D||S===n.TEXTURE_2D_ARRAY)&&n.texParameteri(S,n.TEXTURE_WRAP_R,Me[g.wrapR]),n.texParameteri(S,n.TEXTURE_MAG_FILTER,He[g.magFilter]),n.texParameteri(S,n.TEXTURE_MIN_FILTER,He[g.minFilter]),g.compareFunction&&(n.texParameteri(S,n.TEXTURE_COMPARE_MODE,n.COMPARE_REF_TO_TEXTURE),n.texParameteri(S,n.TEXTURE_COMPARE_FUNC,Ze[g.compareFunction])),e.has("EXT_texture_filter_anisotropic")===!0){if(g.magFilter===Tt||g.minFilter!==ts&&g.minFilter!==ji||g.type===ai&&e.has("OES_texture_float_linear")===!1)return;if(g.anisotropy>1||i.get(g).__currentAnisotropy){const F=e.get("EXT_texture_filter_anisotropic");n.texParameterf(S,F.TEXTURE_MAX_ANISOTROPY_EXT,Math.min(g.anisotropy,s.getMaxAnisotropy())),i.get(g).__currentAnisotropy=g.anisotropy}}}function Z(S,g){let F=!1;S.__webglInit===void 0&&(S.__webglInit=!0,g.addEventListener("dispose",D));const Y=g.source;let Q=m.get(Y);Q===void 0&&(Q={},m.set(Y,Q));const ne=H(g);if(ne!==S.__cacheKey){Q[ne]===void 0&&(Q[ne]={texture:n.createTexture(),usedTimes:0},a.memory.textures++,F=!0),Q[ne].usedTimes++;const le=Q[S.__cacheKey];le!==void 0&&(Q[S.__cacheKey].usedTimes--,le.usedTimes===0&&U(g)),S.__cacheKey=ne,S.__webglTexture=Q[ne].texture}return F}function ue(S,g,F){return Math.floor(Math.floor(S/F)/g)}function te(S,g,F,Y){const ne=S.updateRanges;if(ne.length===0)t.texSubImage2D(n.TEXTURE_2D,0,0,0,g.width,g.height,F,Y,g.data);else{ne.sort((ve,ae)=>ve.start-ae.start);let le=0;for(let ve=1;ve<ne.length;ve++){const ae=ne[le],se=ne[ve],Ie=ae.start+ae.count,Be=ue(se.start,g.width,4),$e=ue(ae.start,g.width,4);se.start<=Ie+1&&Be===$e&&ue(se.start+se.count-1,g.width,4)===Be?ae.count=Math.max(ae.count,se.start+se.count-ae.start):(++le,ne[le]=se)}ne.length=le+1;const W=t.getParameter(n.UNPACK_ROW_LENGTH),$=t.getParameter(n.UNPACK_SKIP_PIXELS),me=t.getParameter(n.UNPACK_SKIP_ROWS);t.pixelStorei(n.UNPACK_ROW_LENGTH,g.width);for(let ve=0,ae=ne.length;ve<ae;ve++){const se=ne[ve],Ie=Math.floor(se.start/4),Be=Math.ceil(se.count/4),$e=Ie%g.width,R=Math.floor(Ie/g.width),re=Be,q=1;t.pixelStorei(n.UNPACK_SKIP_PIXELS,$e),t.pixelStorei(n.UNPACK_SKIP_ROWS,R),t.texSubImage2D(n.TEXTURE_2D,0,$e,R,re,q,F,Y,g.data)}S.clearUpdateRanges(),t.pixelStorei(n.UNPACK_ROW_LENGTH,W),t.pixelStorei(n.UNPACK_SKIP_PIXELS,$),t.pixelStorei(n.UNPACK_SKIP_ROWS,me)}}function Te(S,g,F){let Y=n.TEXTURE_2D;(g.isDataArrayTexture||g.isCompressedArrayTexture)&&(Y=n.TEXTURE_2D_ARRAY),g.isData3DTexture&&(Y=n.TEXTURE_3D);const Q=Z(S,g),ne=g.source;t.bindTexture(Y,S.__webglTexture,n.TEXTURE0+F);const le=i.get(ne);if(ne.version!==le.__version||Q===!0){if(t.activeTexture(n.TEXTURE0+F),(typeof ImageBitmap<"u"&&g.image instanceof ImageBitmap)===!1){const q=Xe.getPrimaries(Xe.workingColorSpace),ge=g.colorSpace===Ci?null:Xe.getPrimaries(g.colorSpace),oe=g.colorSpace===Ci||q===ge?n.NONE:n.BROWSER_DEFAULT_WEBGL;t.pixelStorei(n.UNPACK_FLIP_Y_WEBGL,g.flipY),t.pixelStorei(n.UNPACK_PREMULTIPLY_ALPHA_WEBGL,g.premultiplyAlpha),t.pixelStorei(n.UNPACK_COLORSPACE_CONVERSION_WEBGL,oe)}t.pixelStorei(n.UNPACK_ALIGNMENT,g.unpackAlignment);let $=p(g.image,!1,s.maxTextureSize);$=de(g,$);const me=r.convert(g.format,g.colorSpace),ve=r.convert(g.type);let ae=E(g.internalFormat,me,ve,g.normalized,g.colorSpace,g.isVideoTexture);De(Y,g);let se;const Ie=g.mipmaps,Be=g.isVideoTexture!==!0,$e=le.__version===void 0||Q===!0,R=ne.dataReady,re=w(g,$);if(g.isDepthTexture)ae=L(g.format===qi,g.type),$e&&(Be?t.texStorage2D(n.TEXTURE_2D,1,ae,$.width,$.height):t.texImage2D(n.TEXTURE_2D,0,ae,$.width,$.height,0,me,ve,null));else if(g.isDataTexture)if(Ie.length>0){Be&&$e&&t.texStorage2D(n.TEXTURE_2D,re,ae,Ie[0].width,Ie[0].height);for(let q=0,ge=Ie.length;q<ge;q++)se=Ie[q],Be?R&&t.texSubImage2D(n.TEXTURE_2D,q,0,0,se.width,se.height,me,ve,se.data):t.texImage2D(n.TEXTURE_2D,q,ae,se.width,se.height,0,me,ve,se.data);g.generateMipmaps=!1}else Be?($e&&t.texStorage2D(n.TEXTURE_2D,re,ae,$.width,$.height),R&&te(g,$,me,ve)):t.texImage2D(n.TEXTURE_2D,0,ae,$.width,$.height,0,me,ve,$.data);else if(g.isCompressedTexture)if(g.isCompressedArrayTexture){Be&&$e&&t.texStorage3D(n.TEXTURE_2D_ARRAY,re,ae,Ie[0].width,Ie[0].height,$.depth);for(let q=0,ge=Ie.length;q<ge;q++)if(se=Ie[q],g.format!==Qt)if(me!==null)if(Be){if(R)if(g.layerUpdates.size>0){const oe=cl(se.width,se.height,g.format,g.type);for(const J of g.layerUpdates){const ye=se.data.subarray(J*oe/se.data.BYTES_PER_ELEMENT,(J+1)*oe/se.data.BYTES_PER_ELEMENT);t.compressedTexSubImage3D(n.TEXTURE_2D_ARRAY,q,0,0,J,se.width,se.height,1,me,ye)}g.clearLayerUpdates()}else t.compressedTexSubImage3D(n.TEXTURE_2D_ARRAY,q,0,0,0,se.width,se.height,$.depth,me,se.data)}else t.compressedTexImage3D(n.TEXTURE_2D_ARRAY,q,ae,se.width,se.height,$.depth,0,se.data,0,0);else Ce("WebGLRenderer: Attempt to load unsupported compressed texture format in .uploadTexture()");else Be?R&&t.texSubImage3D(n.TEXTURE_2D_ARRAY,q,0,0,0,se.width,se.height,$.depth,me,ve,se.data):t.texImage3D(n.TEXTURE_2D_ARRAY,q,ae,se.width,se.height,$.depth,0,me,ve,se.data)}else{Be&&$e&&t.texStorage2D(n.TEXTURE_2D,re,ae,Ie[0].width,Ie[0].height);for(let q=0,ge=Ie.length;q<ge;q++)se=Ie[q],g.format!==Qt?me!==null?Be?R&&t.compressedTexSubImage2D(n.TEXTURE_2D,q,0,0,se.width,se.height,me,se.data):t.compressedTexImage2D(n.TEXTURE_2D,q,ae,se.width,se.height,0,se.data):Ce("WebGLRenderer: Attempt to load unsupported compressed texture format in .uploadTexture()"):Be?R&&t.texSubImage2D(n.TEXTURE_2D,q,0,0,se.width,se.height,me,ve,se.data):t.texImage2D(n.TEXTURE_2D,q,ae,se.width,se.height,0,me,ve,se.data)}else if(g.isDataArrayTexture)if(Be){if($e&&t.texStorage3D(n.TEXTURE_2D_ARRAY,re,ae,$.width,$.height,$.depth),R)if(g.layerUpdates.size>0){const q=cl($.width,$.height,g.format,g.type);for(const ge of g.layerUpdates){const oe=$.data.subarray(ge*q/$.data.BYTES_PER_ELEMENT,(ge+1)*q/$.data.BYTES_PER_ELEMENT);t.texSubImage3D(n.TEXTURE_2D_ARRAY,0,0,0,ge,$.width,$.height,1,me,ve,oe)}g.clearLayerUpdates()}else t.texSubImage3D(n.TEXTURE_2D_ARRAY,0,0,0,0,$.width,$.height,$.depth,me,ve,$.data)}else t.texImage3D(n.TEXTURE_2D_ARRAY,0,ae,$.width,$.height,$.depth,0,me,ve,$.data);else if(g.isData3DTexture)Be?($e&&t.texStorage3D(n.TEXTURE_3D,re,ae,$.width,$.height,$.depth),R&&t.texSubImage3D(n.TEXTURE_3D,0,0,0,0,$.width,$.height,$.depth,me,ve,$.data)):t.texImage3D(n.TEXTURE_3D,0,ae,$.width,$.height,$.depth,0,me,ve,$.data);else if(g.isFramebufferTexture){if($e)if(Be)t.texStorage2D(n.TEXTURE_2D,re,ae,$.width,$.height);else{let q=$.width,ge=$.height;for(let oe=0;oe<re;oe++)t.texImage2D(n.TEXTURE_2D,oe,ae,q,ge,0,me,ve,null),q>>=1,ge>>=1}}else if(g.isHTMLTexture){if("texElementImage2D"in n){const q=n.canvas;if(q.hasAttribute("layoutsubtree")||q.setAttribute("layoutsubtree","true"),$.parentNode!==q){q.appendChild($),d.add(g),q.onpaint=Fe=>{const ft=Fe.changedElements;for(const it of d)ft.includes(it.image)&&(it.needsUpdate=!0)},q.requestPaint();return}const ge=0,oe=n.RGBA,J=n.RGBA,ye=n.UNSIGNED_BYTE;n.texElementImage2D(n.TEXTURE_2D,ge,oe,J,ye,$),n.texParameteri(n.TEXTURE_2D,n.TEXTURE_MIN_FILTER,n.LINEAR),n.texParameteri(n.TEXTURE_2D,n.TEXTURE_WRAP_S,n.CLAMP_TO_EDGE),n.texParameteri(n.TEXTURE_2D,n.TEXTURE_WRAP_T,n.CLAMP_TO_EDGE)}}else if(Ie.length>0){if(Be&&$e){const q=ut(Ie[0]);t.texStorage2D(n.TEXTURE_2D,re,ae,q.width,q.height)}for(let q=0,ge=Ie.length;q<ge;q++)se=Ie[q],Be?R&&t.texSubImage2D(n.TEXTURE_2D,q,0,0,me,ve,se):t.texImage2D(n.TEXTURE_2D,q,ae,me,ve,se);g.generateMipmaps=!1}else if(Be){if($e){const q=ut($);t.texStorage2D(n.TEXTURE_2D,re,ae,q.width,q.height)}R&&t.texSubImage2D(n.TEXTURE_2D,0,0,0,me,ve,$)}else t.texImage2D(n.TEXTURE_2D,0,ae,me,ve,$);f(g)&&M(Y),le.__version=ne.version,g.onUpdate&&g.onUpdate(g)}S.__version=g.version}function Le(S,g,F){if(g.image.length!==6)return;const Y=Z(S,g),Q=g.source;t.bindTexture(n.TEXTURE_CUBE_MAP,S.__webglTexture,n.TEXTURE0+F);const ne=i.get(Q);if(Q.version!==ne.__version||Y===!0){t.activeTexture(n.TEXTURE0+F);const le=Xe.getPrimaries(Xe.workingColorSpace),W=g.colorSpace===Ci?null:Xe.getPrimaries(g.colorSpace),$=g.colorSpace===Ci||le===W?n.NONE:n.BROWSER_DEFAULT_WEBGL;t.pixelStorei(n.UNPACK_FLIP_Y_WEBGL,g.flipY),t.pixelStorei(n.UNPACK_PREMULTIPLY_ALPHA_WEBGL,g.premultiplyAlpha),t.pixelStorei(n.UNPACK_ALIGNMENT,g.unpackAlignment),t.pixelStorei(n.UNPACK_COLORSPACE_CONVERSION_WEBGL,$);const me=g.isCompressedTexture||g.image[0].isCompressedTexture,ve=g.image[0]&&g.image[0].isDataTexture,ae=[];for(let J=0;J<6;J++)!me&&!ve?ae[J]=p(g.image[J],!0,s.maxCubemapSize):ae[J]=ve?g.image[J].image:g.image[J],ae[J]=de(g,ae[J]);const se=ae[0],Ie=r.convert(g.format,g.colorSpace),Be=r.convert(g.type),$e=E(g.internalFormat,Ie,Be,g.normalized,g.colorSpace),R=g.isVideoTexture!==!0,re=ne.__version===void 0||Y===!0,q=Q.dataReady;let ge=w(g,se);De(n.TEXTURE_CUBE_MAP,g);let oe;if(me){R&&re&&t.texStorage2D(n.TEXTURE_CUBE_MAP,ge,$e,se.width,se.height);for(let J=0;J<6;J++){oe=ae[J].mipmaps;for(let ye=0;ye<oe.length;ye++){const Fe=oe[ye];g.format!==Qt?Ie!==null?R?q&&t.compressedTexSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+J,ye,0,0,Fe.width,Fe.height,Ie,Fe.data):t.compressedTexImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+J,ye,$e,Fe.width,Fe.height,0,Fe.data):Ce("WebGLRenderer: Attempt to load unsupported compressed texture format in .setTextureCube()"):R?q&&t.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+J,ye,0,0,Fe.width,Fe.height,Ie,Be,Fe.data):t.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+J,ye,$e,Fe.width,Fe.height,0,Ie,Be,Fe.data)}}}else{if(oe=g.mipmaps,R&&re){oe.length>0&&ge++;const J=ut(ae[0]);t.texStorage2D(n.TEXTURE_CUBE_MAP,ge,$e,J.width,J.height)}for(let J=0;J<6;J++)if(ve){R?q&&t.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+J,0,0,0,ae[J].width,ae[J].height,Ie,Be,ae[J].data):t.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+J,0,$e,ae[J].width,ae[J].height,0,Ie,Be,ae[J].data);for(let ye=0;ye<oe.length;ye++){const ft=oe[ye].image[J].image;R?q&&t.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+J,ye+1,0,0,ft.width,ft.height,Ie,Be,ft.data):t.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+J,ye+1,$e,ft.width,ft.height,0,Ie,Be,ft.data)}}else{R?q&&t.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+J,0,0,0,Ie,Be,ae[J]):t.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+J,0,$e,Ie,Be,ae[J]);for(let ye=0;ye<oe.length;ye++){const Fe=oe[ye];R?q&&t.texSubImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+J,ye+1,0,0,Ie,Be,Fe.image[J]):t.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+J,ye+1,$e,Ie,Be,Fe.image[J])}}}f(g)&&M(n.TEXTURE_CUBE_MAP),ne.__version=Q.version,g.onUpdate&&g.onUpdate(g)}S.__version=g.version}function Ae(S,g,F,Y,Q,ne){const le=r.convert(F.format,F.colorSpace),W=r.convert(F.type),$=E(F.internalFormat,le,W,F.normalized,F.colorSpace),me=i.get(g),ve=i.get(F);if(ve.__renderTarget=g,!me.__hasExternalTextures){const ae=Math.max(1,g.width>>ne),se=Math.max(1,g.height>>ne);Q===n.TEXTURE_3D||Q===n.TEXTURE_2D_ARRAY?t.texImage3D(Q,ne,$,ae,se,g.depth,0,le,W,null):t.texImage2D(Q,ne,$,ae,se,0,le,W,null)}t.bindFramebuffer(n.FRAMEBUFFER,S),We(g)?o.framebufferTexture2DMultisampleEXT(n.FRAMEBUFFER,Y,Q,ve.__webglTexture,0,Et(g)):(Q===n.TEXTURE_2D||Q>=n.TEXTURE_CUBE_MAP_POSITIVE_X&&Q<=n.TEXTURE_CUBE_MAP_NEGATIVE_Z)&&n.framebufferTexture2D(n.FRAMEBUFFER,Y,Q,ve.__webglTexture,ne),t.bindFramebuffer(n.FRAMEBUFFER,null)}function tt(S,g,F){if(n.bindRenderbuffer(n.RENDERBUFFER,S),g.depthBuffer){const Y=g.depthTexture,Q=Y&&Y.isDepthTexture?Y.type:null,ne=L(g.stencilBuffer,Q),le=g.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT;We(g)?o.renderbufferStorageMultisampleEXT(n.RENDERBUFFER,Et(g),ne,g.width,g.height):F?n.renderbufferStorageMultisample(n.RENDERBUFFER,Et(g),ne,g.width,g.height):n.renderbufferStorage(n.RENDERBUFFER,ne,g.width,g.height),n.framebufferRenderbuffer(n.FRAMEBUFFER,le,n.RENDERBUFFER,S)}else{const Y=g.textures;for(let Q=0;Q<Y.length;Q++){const ne=Y[Q],le=r.convert(ne.format,ne.colorSpace),W=r.convert(ne.type),$=E(ne.internalFormat,le,W,ne.normalized,ne.colorSpace);We(g)?o.renderbufferStorageMultisampleEXT(n.RENDERBUFFER,Et(g),$,g.width,g.height):F?n.renderbufferStorageMultisample(n.RENDERBUFFER,Et(g),$,g.width,g.height):n.renderbufferStorage(n.RENDERBUFFER,$,g.width,g.height)}}n.bindRenderbuffer(n.RENDERBUFFER,null)}function ie(S,g,F){const Y=g.isWebGLCubeRenderTarget===!0;if(t.bindFramebuffer(n.FRAMEBUFFER,S),!(g.depthTexture&&g.depthTexture.isDepthTexture))throw new Error("renderTarget.depthTexture must be an instance of THREE.DepthTexture");const Q=i.get(g.depthTexture);if(Q.__renderTarget=g,(!Q.__webglTexture||g.depthTexture.image.width!==g.width||g.depthTexture.image.height!==g.height)&&(g.depthTexture.image.width=g.width,g.depthTexture.image.height=g.height,g.depthTexture.needsUpdate=!0),Y){if(Q.__webglInit===void 0&&(Q.__webglInit=!0,g.depthTexture.addEventListener("dispose",D)),Q.__webglTexture===void 0){Q.__webglTexture=n.createTexture(),t.bindTexture(n.TEXTURE_CUBE_MAP,Q.__webglTexture),De(n.TEXTURE_CUBE_MAP,g.depthTexture);const me=r.convert(g.depthTexture.format),ve=r.convert(g.depthTexture.type);let ae;g.depthTexture.format===xi?ae=n.DEPTH_COMPONENT24:g.depthTexture.format===qi&&(ae=n.DEPTH24_STENCIL8);for(let se=0;se<6;se++)n.texImage2D(n.TEXTURE_CUBE_MAP_POSITIVE_X+se,0,ae,g.width,g.height,0,me,ve,null)}}else K(g.depthTexture,0);const ne=Q.__webglTexture,le=Et(g),W=Y?n.TEXTURE_CUBE_MAP_POSITIVE_X+F:n.TEXTURE_2D,$=g.depthTexture.format===qi?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT;if(g.depthTexture.format===xi)We(g)?o.framebufferTexture2DMultisampleEXT(n.FRAMEBUFFER,$,W,ne,0,le):n.framebufferTexture2D(n.FRAMEBUFFER,$,W,ne,0);else if(g.depthTexture.format===qi)We(g)?o.framebufferTexture2DMultisampleEXT(n.FRAMEBUFFER,$,W,ne,0,le):n.framebufferTexture2D(n.FRAMEBUFFER,$,W,ne,0);else throw new Error("Unknown depthTexture format")}function Pe(S){const g=i.get(S),F=S.isWebGLCubeRenderTarget===!0;if(g.__boundDepthTexture!==S.depthTexture){const Y=S.depthTexture;if(g.__depthDisposeCallback&&g.__depthDisposeCallback(),Y){const Q=()=>{delete g.__boundDepthTexture,delete g.__depthDisposeCallback,Y.removeEventListener("dispose",Q)};Y.addEventListener("dispose",Q),g.__depthDisposeCallback=Q}g.__boundDepthTexture=Y}if(S.depthTexture&&!g.__autoAllocateDepthBuffer)if(F)for(let Y=0;Y<6;Y++)ie(g.__webglFramebuffer[Y],S,Y);else{const Y=S.texture.mipmaps;Y&&Y.length>0?ie(g.__webglFramebuffer[0],S,0):ie(g.__webglFramebuffer,S,0)}else if(F){g.__webglDepthbuffer=[];for(let Y=0;Y<6;Y++)if(t.bindFramebuffer(n.FRAMEBUFFER,g.__webglFramebuffer[Y]),g.__webglDepthbuffer[Y]===void 0)g.__webglDepthbuffer[Y]=n.createRenderbuffer(),tt(g.__webglDepthbuffer[Y],S,!1);else{const Q=S.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT,ne=g.__webglDepthbuffer[Y];n.bindRenderbuffer(n.RENDERBUFFER,ne),n.framebufferRenderbuffer(n.FRAMEBUFFER,Q,n.RENDERBUFFER,ne)}}else{const Y=S.texture.mipmaps;if(Y&&Y.length>0?t.bindFramebuffer(n.FRAMEBUFFER,g.__webglFramebuffer[0]):t.bindFramebuffer(n.FRAMEBUFFER,g.__webglFramebuffer),g.__webglDepthbuffer===void 0)g.__webglDepthbuffer=n.createRenderbuffer(),tt(g.__webglDepthbuffer,S,!1);else{const Q=S.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT,ne=g.__webglDepthbuffer;n.bindRenderbuffer(n.RENDERBUFFER,ne),n.framebufferRenderbuffer(n.FRAMEBUFFER,Q,n.RENDERBUFFER,ne)}}t.bindFramebuffer(n.FRAMEBUFFER,null)}function Ke(S,g,F){const Y=i.get(S);g!==void 0&&Ae(Y.__webglFramebuffer,S,S.texture,n.COLOR_ATTACHMENT0,n.TEXTURE_2D,0),F!==void 0&&Pe(S)}function ke(S){const g=S.texture,F=i.get(S),Y=i.get(g);S.addEventListener("dispose",b);const Q=S.textures,ne=S.isWebGLCubeRenderTarget===!0,le=Q.length>1;if(le||(Y.__webglTexture===void 0&&(Y.__webglTexture=n.createTexture()),Y.__version=g.version,a.memory.textures++),ne){F.__webglFramebuffer=[];for(let W=0;W<6;W++)if(g.mipmaps&&g.mipmaps.length>0){F.__webglFramebuffer[W]=[];for(let $=0;$<g.mipmaps.length;$++)F.__webglFramebuffer[W][$]=n.createFramebuffer()}else F.__webglFramebuffer[W]=n.createFramebuffer()}else{if(g.mipmaps&&g.mipmaps.length>0){F.__webglFramebuffer=[];for(let W=0;W<g.mipmaps.length;W++)F.__webglFramebuffer[W]=n.createFramebuffer()}else F.__webglFramebuffer=n.createFramebuffer();if(le)for(let W=0,$=Q.length;W<$;W++){const me=i.get(Q[W]);me.__webglTexture===void 0&&(me.__webglTexture=n.createTexture(),a.memory.textures++)}if(S.samples>0&&We(S)===!1){F.__webglMultisampledFramebuffer=n.createFramebuffer(),F.__webglColorRenderbuffer=[],t.bindFramebuffer(n.FRAMEBUFFER,F.__webglMultisampledFramebuffer);for(let W=0;W<Q.length;W++){const $=Q[W];F.__webglColorRenderbuffer[W]=n.createRenderbuffer(),n.bindRenderbuffer(n.RENDERBUFFER,F.__webglColorRenderbuffer[W]);const me=r.convert($.format,$.colorSpace),ve=r.convert($.type),ae=E($.internalFormat,me,ve,$.normalized,$.colorSpace,S.isXRRenderTarget===!0),se=Et(S);n.renderbufferStorageMultisample(n.RENDERBUFFER,se,ae,S.width,S.height),n.framebufferRenderbuffer(n.FRAMEBUFFER,n.COLOR_ATTACHMENT0+W,n.RENDERBUFFER,F.__webglColorRenderbuffer[W])}n.bindRenderbuffer(n.RENDERBUFFER,null),S.depthBuffer&&(F.__webglDepthRenderbuffer=n.createRenderbuffer(),tt(F.__webglDepthRenderbuffer,S,!0)),t.bindFramebuffer(n.FRAMEBUFFER,null)}}if(ne){t.bindTexture(n.TEXTURE_CUBE_MAP,Y.__webglTexture),De(n.TEXTURE_CUBE_MAP,g);for(let W=0;W<6;W++)if(g.mipmaps&&g.mipmaps.length>0)for(let $=0;$<g.mipmaps.length;$++)Ae(F.__webglFramebuffer[W][$],S,g,n.COLOR_ATTACHMENT0,n.TEXTURE_CUBE_MAP_POSITIVE_X+W,$);else Ae(F.__webglFramebuffer[W],S,g,n.COLOR_ATTACHMENT0,n.TEXTURE_CUBE_MAP_POSITIVE_X+W,0);f(g)&&M(n.TEXTURE_CUBE_MAP),t.unbindTexture()}else if(le){for(let W=0,$=Q.length;W<$;W++){const me=Q[W],ve=i.get(me);let ae=n.TEXTURE_2D;(S.isWebGL3DRenderTarget||S.isWebGLArrayRenderTarget)&&(ae=S.isWebGL3DRenderTarget?n.TEXTURE_3D:n.TEXTURE_2D_ARRAY),t.bindTexture(ae,ve.__webglTexture),De(ae,me),Ae(F.__webglFramebuffer,S,me,n.COLOR_ATTACHMENT0+W,ae,0),f(me)&&M(ae)}t.unbindTexture()}else{let W=n.TEXTURE_2D;if((S.isWebGL3DRenderTarget||S.isWebGLArrayRenderTarget)&&(W=S.isWebGL3DRenderTarget?n.TEXTURE_3D:n.TEXTURE_2D_ARRAY),t.bindTexture(W,Y.__webglTexture),De(W,g),g.mipmaps&&g.mipmaps.length>0)for(let $=0;$<g.mipmaps.length;$++)Ae(F.__webglFramebuffer[$],S,g,n.COLOR_ATTACHMENT0,W,$);else Ae(F.__webglFramebuffer,S,g,n.COLOR_ATTACHMENT0,W,0);f(g)&&M(W),t.unbindTexture()}S.depthBuffer&&Pe(S)}function yt(S){const g=S.textures;for(let F=0,Y=g.length;F<Y;F++){const Q=g[F];if(f(Q)){const ne=y(S),le=i.get(Q).__webglTexture;t.bindTexture(ne,le),M(ne),t.unbindTexture()}}}const ct=[],Wt=[];function P(S){if(S.samples>0){if(We(S)===!1){const g=S.textures,F=S.width,Y=S.height;let Q=n.COLOR_BUFFER_BIT;const ne=S.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT,le=i.get(S),W=g.length>1;if(W)for(let me=0;me<g.length;me++)t.bindFramebuffer(n.FRAMEBUFFER,le.__webglMultisampledFramebuffer),n.framebufferRenderbuffer(n.FRAMEBUFFER,n.COLOR_ATTACHMENT0+me,n.RENDERBUFFER,null),t.bindFramebuffer(n.FRAMEBUFFER,le.__webglFramebuffer),n.framebufferTexture2D(n.DRAW_FRAMEBUFFER,n.COLOR_ATTACHMENT0+me,n.TEXTURE_2D,null,0);t.bindFramebuffer(n.READ_FRAMEBUFFER,le.__webglMultisampledFramebuffer);const $=S.texture.mipmaps;$&&$.length>0?t.bindFramebuffer(n.DRAW_FRAMEBUFFER,le.__webglFramebuffer[0]):t.bindFramebuffer(n.DRAW_FRAMEBUFFER,le.__webglFramebuffer);for(let me=0;me<g.length;me++){if(S.resolveDepthBuffer&&(S.depthBuffer&&(Q|=n.DEPTH_BUFFER_BIT),S.stencilBuffer&&S.resolveStencilBuffer&&(Q|=n.STENCIL_BUFFER_BIT)),W){n.framebufferRenderbuffer(n.READ_FRAMEBUFFER,n.COLOR_ATTACHMENT0,n.RENDERBUFFER,le.__webglColorRenderbuffer[me]);const ve=i.get(g[me]).__webglTexture;n.framebufferTexture2D(n.DRAW_FRAMEBUFFER,n.COLOR_ATTACHMENT0,n.TEXTURE_2D,ve,0)}n.blitFramebuffer(0,0,F,Y,0,0,F,Y,Q,n.NEAREST),l===!0&&(ct.length=0,Wt.length=0,ct.push(n.COLOR_ATTACHMENT0+me),S.depthBuffer&&S.resolveDepthBuffer===!1&&(ct.push(ne),Wt.push(ne),n.invalidateFramebuffer(n.DRAW_FRAMEBUFFER,Wt)),n.invalidateFramebuffer(n.READ_FRAMEBUFFER,ct))}if(t.bindFramebuffer(n.READ_FRAMEBUFFER,null),t.bindFramebuffer(n.DRAW_FRAMEBUFFER,null),W)for(let me=0;me<g.length;me++){t.bindFramebuffer(n.FRAMEBUFFER,le.__webglMultisampledFramebuffer),n.framebufferRenderbuffer(n.FRAMEBUFFER,n.COLOR_ATTACHMENT0+me,n.RENDERBUFFER,le.__webglColorRenderbuffer[me]);const ve=i.get(g[me]).__webglTexture;t.bindFramebuffer(n.FRAMEBUFFER,le.__webglFramebuffer),n.framebufferTexture2D(n.DRAW_FRAMEBUFFER,n.COLOR_ATTACHMENT0+me,n.TEXTURE_2D,ve,0)}t.bindFramebuffer(n.DRAW_FRAMEBUFFER,le.__webglMultisampledFramebuffer)}else if(S.depthBuffer&&S.resolveDepthBuffer===!1&&l){const g=S.stencilBuffer?n.DEPTH_STENCIL_ATTACHMENT:n.DEPTH_ATTACHMENT;n.invalidateFramebuffer(n.DRAW_FRAMEBUFFER,[g])}}}function Et(S){return Math.min(s.maxSamples,S.samples)}function We(S){const g=i.get(S);return S.samples>0&&e.has("WEBGL_multisampled_render_to_texture")===!0&&g.__useRenderToTexture!==!1}function at(S){const g=a.render.frame;h.get(S)!==g&&(h.set(S,g),S.update())}function de(S,g){const F=S.colorSpace,Y=S.format,Q=S.type;return S.isCompressedTexture===!0||S.isVideoTexture===!0||F!==ls&&F!==Ci&&(Xe.getTransfer(F)===Qe?(Y!==Qt||Q!==kt)&&Ce("WebGLTextures: sRGB encoded textures have to use RGBAFormat and UnsignedByteType."):je("WebGLTextures: Unsupported texture color space:",F)),g}function ut(S){return typeof HTMLImageElement<"u"&&S instanceof HTMLImageElement?(c.width=S.naturalWidth||S.width,c.height=S.naturalHeight||S.height):typeof VideoFrame<"u"&&S instanceof VideoFrame?(c.width=S.displayWidth,c.height=S.displayHeight):(c.width=S.width,c.height=S.height),c}this.allocateTextureUnit=V,this.resetTextureUnits=X,this.getTextureUnits=j,this.setTextureUnits=N,this.setTexture2D=K,this.setTexture2DArray=ee,this.setTexture3D=ce,this.setTextureCube=be,this.rebindTextures=Ke,this.setupRenderTarget=ke,this.updateRenderTargetMipmap=yt,this.updateMultisampleRenderTarget=P,this.setupDepthRenderbuffer=Pe,this.setupFrameBufferTexture=Ae,this.useMultisampledRTT=We,this.isReversedDepthBuffer=function(){return t.buffers.depth.getReversed()}}function im(n,e){function t(i,s=Ci){let r;const a=Xe.getTransfer(s);if(i===kt)return n.UNSIGNED_BYTE;if(i===dr)return n.UNSIGNED_SHORT_4_4_4_4;if(i===fr)return n.UNSIGNED_SHORT_5_5_5_1;if(i===uo)return n.UNSIGNED_INT_5_9_9_9_REV;if(i===fo)return n.UNSIGNED_INT_10F_11F_11F_REV;if(i===co)return n.BYTE;if(i===ho)return n.SHORT;if(i===Fn)return n.UNSIGNED_SHORT;if(i===ur)return n.INT;if(i===ri)return n.UNSIGNED_INT;if(i===ai)return n.FLOAT;if(i===vi)return n.HALF_FLOAT;if(i===po)return n.ALPHA;if(i===mo)return n.RGB;if(i===Qt)return n.RGBA;if(i===xi)return n.DEPTH_COMPONENT;if(i===qi)return n.DEPTH_STENCIL;if(i===go)return n.RED;if(i===pr)return n.RED_INTEGER;if(i===Yi)return n.RG;if(i===mr)return n.RG_INTEGER;if(i===gr)return n.RGBA_INTEGER;if(i===is||i===ns||i===ss||i===rs)if(a===Qe)if(r=e.get("WEBGL_compressed_texture_s3tc_srgb"),r!==null){if(i===is)return r.COMPRESSED_SRGB_S3TC_DXT1_EXT;if(i===ns)return r.COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT;if(i===ss)return r.COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT;if(i===rs)return r.COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT}else return null;else if(r=e.get("WEBGL_compressed_texture_s3tc"),r!==null){if(i===is)return r.COMPRESSED_RGB_S3TC_DXT1_EXT;if(i===ns)return r.COMPRESSED_RGBA_S3TC_DXT1_EXT;if(i===ss)return r.COMPRESSED_RGBA_S3TC_DXT3_EXT;if(i===rs)return r.COMPRESSED_RGBA_S3TC_DXT5_EXT}else return null;if(i===_r||i===vr||i===xr||i===br)if(r=e.get("WEBGL_compressed_texture_pvrtc"),r!==null){if(i===_r)return r.COMPRESSED_RGB_PVRTC_4BPPV1_IMG;if(i===vr)return r.COMPRESSED_RGB_PVRTC_2BPPV1_IMG;if(i===xr)return r.COMPRESSED_RGBA_PVRTC_4BPPV1_IMG;if(i===br)return r.COMPRESSED_RGBA_PVRTC_2BPPV1_IMG}else return null;if(i===Sr||i===Mr||i===yr||i===Er||i===wr||i===as||i===Tr)if(r=e.get("WEBGL_compressed_texture_etc"),r!==null){if(i===Sr||i===Mr)return a===Qe?r.COMPRESSED_SRGB8_ETC2:r.COMPRESSED_RGB8_ETC2;if(i===yr)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ETC2_EAC:r.COMPRESSED_RGBA8_ETC2_EAC;if(i===Er)return r.COMPRESSED_R11_EAC;if(i===wr)return r.COMPRESSED_SIGNED_R11_EAC;if(i===as)return r.COMPRESSED_RG11_EAC;if(i===Tr)return r.COMPRESSED_SIGNED_RG11_EAC}else return null;if(i===Ar||i===Cr||i===Rr||i===Pr||i===Lr||i===Dr||i===Ir||i===Nr||i===Ur||i===Fr||i===Or||i===Br||i===zr||i===kr)if(r=e.get("WEBGL_compressed_texture_astc"),r!==null){if(i===Ar)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR:r.COMPRESSED_RGBA_ASTC_4x4_KHR;if(i===Cr)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR:r.COMPRESSED_RGBA_ASTC_5x4_KHR;if(i===Rr)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR:r.COMPRESSED_RGBA_ASTC_5x5_KHR;if(i===Pr)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR:r.COMPRESSED_RGBA_ASTC_6x5_KHR;if(i===Lr)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR:r.COMPRESSED_RGBA_ASTC_6x6_KHR;if(i===Dr)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR:r.COMPRESSED_RGBA_ASTC_8x5_KHR;if(i===Ir)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR:r.COMPRESSED_RGBA_ASTC_8x6_KHR;if(i===Nr)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR:r.COMPRESSED_RGBA_ASTC_8x8_KHR;if(i===Ur)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR:r.COMPRESSED_RGBA_ASTC_10x5_KHR;if(i===Fr)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR:r.COMPRESSED_RGBA_ASTC_10x6_KHR;if(i===Or)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR:r.COMPRESSED_RGBA_ASTC_10x8_KHR;if(i===Br)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR:r.COMPRESSED_RGBA_ASTC_10x10_KHR;if(i===zr)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR:r.COMPRESSED_RGBA_ASTC_12x10_KHR;if(i===kr)return a===Qe?r.COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR:r.COMPRESSED_RGBA_ASTC_12x12_KHR}else return null;if(i===Vr||i===Gr||i===Hr)if(r=e.get("EXT_texture_compression_bptc"),r!==null){if(i===Vr)return a===Qe?r.COMPRESSED_SRGB_ALPHA_BPTC_UNORM_EXT:r.COMPRESSED_RGBA_BPTC_UNORM_EXT;if(i===Gr)return r.COMPRESSED_RGB_BPTC_SIGNED_FLOAT_EXT;if(i===Hr)return r.COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_EXT}else return null;if(i===Wr||i===Xr||i===os||i===jr)if(r=e.get("EXT_texture_compression_rgtc"),r!==null){if(i===Wr)return r.COMPRESSED_RED_RGTC1_EXT;if(i===Xr)return r.COMPRESSED_SIGNED_RED_RGTC1_EXT;if(i===os)return r.COMPRESSED_RED_GREEN_RGTC2_EXT;if(i===jr)return r.COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT}else return null;return i===On?n.UNSIGNED_INT_24_8:n[i]!==void 0?n[i]:null}return{convert:t}}const nm=`
void main() {

	gl_Position = vec4( position, 1.0 );

}`,sm=`
uniform sampler2DArray depthColor;
uniform float depthWidth;
uniform float depthHeight;

void main() {

	vec2 coord = vec2( gl_FragCoord.x / depthWidth, gl_FragCoord.y / depthHeight );

	if ( coord.x >= 1.0 ) {

		gl_FragDepth = texture( depthColor, vec3( coord.x - 1.0, coord.y, 1 ) ).r;

	} else {

		gl_FragDepth = texture( depthColor, vec3( coord.x, coord.y, 0 ) ).r;

	}

}`;class rm{constructor(){this.texture=null,this.mesh=null,this.depthNear=0,this.depthFar=0}init(e,t){if(this.texture===null){const i=new Zo(e.texture);(e.depthNear!==t.depthNear||e.depthFar!==t.depthFar)&&(this.depthNear=e.depthNear,this.depthFar=e.depthFar),this.texture=i}}getMesh(e){if(this.texture!==null&&this.mesh===null){const t=e.cameras[0].viewport,i=new ci({vertexShader:nm,fragmentShader:sm,uniforms:{depthColor:{value:this.texture},depthWidth:{value:t.z},depthHeight:{value:t.w}}});this.mesh=new Ht(new wn(20,20),i)}return this.mesh}reset(){this.texture=null,this.mesh=null}getDepthTexture(){return this.texture}}class am extends Ri{constructor(e,t){super();const i=this;let s=null,r=1,a=null,o="local-floor",l=1,c=null,h=null,d=null,u=null,m=null,_=null;const x=typeof XRWebGLBinding<"u",p=new rm,f={},M=t.getContextAttributes();let y=null,E=null;const L=[],w=[],D=new Ne;let b=null;const T=new $t;T.viewport=new ht;const U=new $t;U.viewport=new ht;const C=[T,U],z=new jh;let X=null,j=null;this.cameraAutoUpdate=!0,this.enabled=!1,this.isPresenting=!1,this.getController=function(Z){let ue=L[Z];return ue===void 0&&(ue=new aa,L[Z]=ue),ue.getTargetRaySpace()},this.getControllerGrip=function(Z){let ue=L[Z];return ue===void 0&&(ue=new aa,L[Z]=ue),ue.getGripSpace()},this.getHand=function(Z){let ue=L[Z];return ue===void 0&&(ue=new aa,L[Z]=ue),ue.getHandSpace()};function N(Z){const ue=w.indexOf(Z.inputSource);if(ue===-1)return;const te=L[ue];te!==void 0&&(te.update(Z.inputSource,Z.frame,c||a),te.dispatchEvent({type:Z.type,data:Z.inputSource}))}function V(){s.removeEventListener("select",N),s.removeEventListener("selectstart",N),s.removeEventListener("selectend",N),s.removeEventListener("squeeze",N),s.removeEventListener("squeezestart",N),s.removeEventListener("squeezeend",N),s.removeEventListener("end",V),s.removeEventListener("inputsourceschange",H);for(let Z=0;Z<L.length;Z++){const ue=w[Z];ue!==null&&(w[Z]=null,L[Z].disconnect(ue))}X=null,j=null,p.reset();for(const Z in f)delete f[Z];e.setRenderTarget(y),m=null,u=null,d=null,s=null,E=null,De.stop(),i.isPresenting=!1,e.setPixelRatio(b),e.setSize(D.width,D.height,!1),i.dispatchEvent({type:"sessionend"})}this.setFramebufferScaleFactor=function(Z){r=Z,i.isPresenting===!0&&Ce("WebXRManager: Cannot change framebuffer scale while presenting.")},this.setReferenceSpaceType=function(Z){o=Z,i.isPresenting===!0&&Ce("WebXRManager: Cannot change reference space type while presenting.")},this.getReferenceSpace=function(){return c||a},this.setReferenceSpace=function(Z){c=Z},this.getBaseLayer=function(){return u!==null?u:m},this.getBinding=function(){return d===null&&x&&(d=new XRWebGLBinding(s,t)),d},this.getFrame=function(){return _},this.getSession=function(){return s},this.setSession=async function(Z){if(s=Z,s!==null){if(y=e.getRenderTarget(),s.addEventListener("select",N),s.addEventListener("selectstart",N),s.addEventListener("selectend",N),s.addEventListener("squeeze",N),s.addEventListener("squeezestart",N),s.addEventListener("squeezeend",N),s.addEventListener("end",V),s.addEventListener("inputsourceschange",H),M.xrCompatible!==!0&&await t.makeXRCompatible(),b=e.getPixelRatio(),e.getSize(D),x&&"createProjectionLayer"in XRWebGLBinding.prototype){let te=null,Te=null,Le=null;M.depth&&(Le=M.stencil?t.DEPTH24_STENCIL8:t.DEPTH_COMPONENT24,te=M.stencil?qi:xi,Te=M.stencil?On:ri);const Ae={colorFormat:t.RGBA8,depthFormat:Le,scaleFactor:r};d=this.getBinding(),u=d.createProjectionLayer(Ae),s.updateRenderState({layers:[u]}),e.setPixelRatio(1),e.setSize(u.textureWidth,u.textureHeight,!1),E=new li(u.textureWidth,u.textureHeight,{format:Qt,type:kt,depthTexture:new En(u.textureWidth,u.textureHeight,Te,void 0,void 0,void 0,void 0,void 0,void 0,te),stencilBuffer:M.stencil,colorSpace:e.outputColorSpace,samples:M.antialias?4:0,resolveDepthBuffer:u.ignoreDepthValues===!1,resolveStencilBuffer:u.ignoreDepthValues===!1})}else{const te={antialias:M.antialias,alpha:!0,depth:M.depth,stencil:M.stencil,framebufferScaleFactor:r};m=new XRWebGLLayer(s,t,te),s.updateRenderState({baseLayer:m}),e.setPixelRatio(1),e.setSize(m.framebufferWidth,m.framebufferHeight,!1),E=new li(m.framebufferWidth,m.framebufferHeight,{format:Qt,type:kt,colorSpace:e.outputColorSpace,stencilBuffer:M.stencil,resolveDepthBuffer:m.ignoreDepthValues===!1,resolveStencilBuffer:m.ignoreDepthValues===!1})}E.isXRRenderTarget=!0,this.setFoveation(l),c=null,a=await s.requestReferenceSpace(o),De.setContext(s),De.start(),i.isPresenting=!0,i.dispatchEvent({type:"sessionstart"})}},this.getEnvironmentBlendMode=function(){if(s!==null)return s.environmentBlendMode},this.getDepthTexture=function(){return p.getDepthTexture()};function H(Z){for(let ue=0;ue<Z.removed.length;ue++){const te=Z.removed[ue],Te=w.indexOf(te);Te>=0&&(w[Te]=null,L[Te].disconnect(te))}for(let ue=0;ue<Z.added.length;ue++){const te=Z.added[ue];let Te=w.indexOf(te);if(Te===-1){for(let Ae=0;Ae<L.length;Ae++)if(Ae>=w.length){w.push(te),Te=Ae;break}else if(w[Ae]===null){w[Ae]=te,Te=Ae;break}if(Te===-1)break}const Le=L[Te];Le&&Le.connect(te)}}const K=new A,ee=new A;function ce(Z,ue,te){K.setFromMatrixPosition(ue.matrixWorld),ee.setFromMatrixPosition(te.matrixWorld);const Te=K.distanceTo(ee),Le=ue.projectionMatrix.elements,Ae=te.projectionMatrix.elements,tt=Le[14]/(Le[10]-1),ie=Le[14]/(Le[10]+1),Pe=(Le[9]+1)/Le[5],Ke=(Le[9]-1)/Le[5],ke=(Le[8]-1)/Le[0],yt=(Ae[8]+1)/Ae[0],ct=tt*ke,Wt=tt*yt,P=Te/(-ke+yt),Et=P*-ke;if(ue.matrixWorld.decompose(Z.position,Z.quaternion,Z.scale),Z.translateX(Et),Z.translateZ(P),Z.matrixWorld.compose(Z.position,Z.quaternion,Z.scale),Z.matrixWorldInverse.copy(Z.matrixWorld).invert(),Le[10]===-1)Z.projectionMatrix.copy(ue.projectionMatrix),Z.projectionMatrixInverse.copy(ue.projectionMatrixInverse);else{const We=tt+P,at=ie+P,de=ct-Et,ut=Wt+(Te-Et),S=Pe*ie/at*We,g=Ke*ie/at*We;Z.projectionMatrix.makePerspective(de,ut,S,g,We,at),Z.projectionMatrixInverse.copy(Z.projectionMatrix).invert()}}function be(Z,ue){ue===null?Z.matrixWorld.copy(Z.matrix):Z.matrixWorld.multiplyMatrices(ue.matrixWorld,Z.matrix),Z.matrixWorldInverse.copy(Z.matrixWorld).invert()}this.updateCamera=function(Z){if(s===null)return;let ue=Z.near,te=Z.far;p.texture!==null&&(p.depthNear>0&&(ue=p.depthNear),p.depthFar>0&&(te=p.depthFar)),z.near=U.near=T.near=ue,z.far=U.far=T.far=te,(X!==z.near||j!==z.far)&&(s.updateRenderState({depthNear:z.near,depthFar:z.far}),X=z.near,j=z.far),z.layers.mask=Z.layers.mask|6,T.layers.mask=z.layers.mask&-5,U.layers.mask=z.layers.mask&-3;const Te=Z.parent,Le=z.cameras;be(z,Te);for(let Ae=0;Ae<Le.length;Ae++)be(Le[Ae],Te);Le.length===2?ce(z,T,U):z.projectionMatrix.copy(T.projectionMatrix),Me(Z,z,Te)};function Me(Z,ue,te){te===null?Z.matrix.copy(ue.matrixWorld):(Z.matrix.copy(te.matrixWorld),Z.matrix.invert(),Z.matrix.multiply(ue.matrixWorld)),Z.matrix.decompose(Z.position,Z.quaternion,Z.scale),Z.updateMatrixWorld(!0),Z.projectionMatrix.copy(ue.projectionMatrix),Z.projectionMatrixInverse.copy(ue.projectionMatrixInverse),Z.isPerspectiveCamera&&(Z.fov=kn*2*Math.atan(1/Z.projectionMatrix.elements[5]),Z.zoom=1)}this.getCamera=function(){return z},this.getFoveation=function(){if(!(u===null&&m===null))return l},this.setFoveation=function(Z){l=Z,u!==null&&(u.fixedFoveation=Z),m!==null&&m.fixedFoveation!==void 0&&(m.fixedFoveation=Z)},this.hasDepthSensing=function(){return p.texture!==null},this.getDepthSensingMesh=function(){return p.getMesh(z)},this.getCameraTexture=function(Z){return f[Z]};let He=null;function Ze(Z,ue){if(h=ue.getViewerPose(c||a),_=ue,h!==null){const te=h.views;m!==null&&(e.setRenderTargetFramebuffer(E,m.framebuffer),e.setRenderTarget(E));let Te=!1;te.length!==z.cameras.length&&(z.cameras.length=0,Te=!0);for(let ie=0;ie<te.length;ie++){const Pe=te[ie];let Ke=null;if(m!==null)Ke=m.getViewport(Pe);else{const yt=d.getViewSubImage(u,Pe);Ke=yt.viewport,ie===0&&(e.setRenderTargetTextures(E,yt.colorTexture,yt.depthStencilTexture),e.setRenderTarget(E))}let ke=C[ie];ke===void 0&&(ke=new $t,ke.layers.enable(ie),ke.viewport=new ht,C[ie]=ke),ke.matrix.fromArray(Pe.transform.matrix),ke.matrix.decompose(ke.position,ke.quaternion,ke.scale),ke.projectionMatrix.fromArray(Pe.projectionMatrix),ke.projectionMatrixInverse.copy(ke.projectionMatrix).invert(),ke.viewport.set(Ke.x,Ke.y,Ke.width,Ke.height),ie===0&&(z.matrix.copy(ke.matrix),z.matrix.decompose(z.position,z.quaternion,z.scale)),Te===!0&&z.cameras.push(ke)}const Le=s.enabledFeatures;if(Le&&Le.includes("depth-sensing")&&s.depthUsage=="gpu-optimized"&&x){d=i.getBinding();const ie=d.getDepthInformation(te[0]);ie&&ie.isValid&&ie.texture&&p.init(ie,s.renderState)}if(Le&&Le.includes("camera-access")&&x){e.state.unbindTexture(),d=i.getBinding();for(let ie=0;ie<te.length;ie++){const Pe=te[ie].camera;if(Pe){let Ke=f[Pe];Ke||(Ke=new Zo,f[Pe]=Ke);const ke=d.getCameraImage(Pe);Ke.sourceTexture=ke}}}}for(let te=0;te<L.length;te++){const Te=w[te],Le=L[te];Te!==null&&Le!==void 0&&Le.update(Te,ue,c||a)}He&&He(Z,ue),ue.detectedPlanes&&i.dispatchEvent({type:"planesdetected",data:ue}),_=null}const De=new hl;De.setAnimationLoop(Ze),this.setAnimationLoop=function(Z){He=Z},this.dispose=function(){}}}const om=new lt,kl=new Ue;kl.set(-1,0,0,0,1,0,0,0,1);function lm(n,e){function t(p,f){p.matrixAutoUpdate===!0&&p.updateMatrix(),f.value.copy(p.matrix)}function i(p,f){f.color.getRGB(p.fogColor.value,Jo(n)),f.isFog?(p.fogNear.value=f.near,p.fogFar.value=f.far):f.isFogExp2&&(p.fogDensity.value=f.density)}function s(p,f,M,y,E){f.isNodeMaterial?f.uniformsNeedUpdate=!1:f.isMeshBasicMaterial?r(p,f):f.isMeshLambertMaterial?(r(p,f),f.envMap&&(p.envMapIntensity.value=f.envMapIntensity)):f.isMeshToonMaterial?(r(p,f),d(p,f)):f.isMeshPhongMaterial?(r(p,f),h(p,f),f.envMap&&(p.envMapIntensity.value=f.envMapIntensity)):f.isMeshStandardMaterial?(r(p,f),u(p,f),f.isMeshPhysicalMaterial&&m(p,f,E)):f.isMeshMatcapMaterial?(r(p,f),_(p,f)):f.isMeshDepthMaterial?r(p,f):f.isMeshDistanceMaterial?(r(p,f),x(p,f)):f.isMeshNormalMaterial?r(p,f):f.isLineBasicMaterial?(a(p,f),f.isLineDashedMaterial&&o(p,f)):f.isPointsMaterial?l(p,f,M,y):f.isSpriteMaterial?c(p,f):f.isShadowMaterial?(p.color.value.copy(f.color),p.opacity.value=f.opacity):f.isShaderMaterial&&(f.uniformsNeedUpdate=!1)}function r(p,f){p.opacity.value=f.opacity,f.color&&p.diffuse.value.copy(f.color),f.emissive&&p.emissive.value.copy(f.emissive).multiplyScalar(f.emissiveIntensity),f.map&&(p.map.value=f.map,t(f.map,p.mapTransform)),f.alphaMap&&(p.alphaMap.value=f.alphaMap,t(f.alphaMap,p.alphaMapTransform)),f.bumpMap&&(p.bumpMap.value=f.bumpMap,t(f.bumpMap,p.bumpMapTransform),p.bumpScale.value=f.bumpScale,f.side===vt&&(p.bumpScale.value*=-1)),f.normalMap&&(p.normalMap.value=f.normalMap,t(f.normalMap,p.normalMapTransform),p.normalScale.value.copy(f.normalScale),f.side===vt&&p.normalScale.value.negate()),f.displacementMap&&(p.displacementMap.value=f.displacementMap,t(f.displacementMap,p.displacementMapTransform),p.displacementScale.value=f.displacementScale,p.displacementBias.value=f.displacementBias),f.emissiveMap&&(p.emissiveMap.value=f.emissiveMap,t(f.emissiveMap,p.emissiveMapTransform)),f.specularMap&&(p.specularMap.value=f.specularMap,t(f.specularMap,p.specularMapTransform)),f.alphaTest>0&&(p.alphaTest.value=f.alphaTest);const M=e.get(f),y=M.envMap,E=M.envMapRotation;y&&(p.envMap.value=y,p.envMapRotation.value.setFromMatrix4(om.makeRotationFromEuler(E)).transpose(),y.isCubeTexture&&y.isRenderTargetTexture===!1&&p.envMapRotation.value.premultiply(kl),p.reflectivity.value=f.reflectivity,p.ior.value=f.ior,p.refractionRatio.value=f.refractionRatio),f.lightMap&&(p.lightMap.value=f.lightMap,p.lightMapIntensity.value=f.lightMapIntensity,t(f.lightMap,p.lightMapTransform)),f.aoMap&&(p.aoMap.value=f.aoMap,p.aoMapIntensity.value=f.aoMapIntensity,t(f.aoMap,p.aoMapTransform))}function a(p,f){p.diffuse.value.copy(f.color),p.opacity.value=f.opacity,f.map&&(p.map.value=f.map,t(f.map,p.mapTransform))}function o(p,f){p.dashSize.value=f.dashSize,p.totalSize.value=f.dashSize+f.gapSize,p.scale.value=f.scale}function l(p,f,M,y){p.diffuse.value.copy(f.color),p.opacity.value=f.opacity,p.size.value=f.size*M,p.scale.value=y*.5,f.map&&(p.map.value=f.map,t(f.map,p.uvTransform)),f.alphaMap&&(p.alphaMap.value=f.alphaMap,t(f.alphaMap,p.alphaMapTransform)),f.alphaTest>0&&(p.alphaTest.value=f.alphaTest)}function c(p,f){p.diffuse.value.copy(f.color),p.opacity.value=f.opacity,p.rotation.value=f.rotation,f.map&&(p.map.value=f.map,t(f.map,p.mapTransform)),f.alphaMap&&(p.alphaMap.value=f.alphaMap,t(f.alphaMap,p.alphaMapTransform)),f.alphaTest>0&&(p.alphaTest.value=f.alphaTest)}function h(p,f){p.specular.value.copy(f.specular),p.shininess.value=Math.max(f.shininess,1e-4)}function d(p,f){f.gradientMap&&(p.gradientMap.value=f.gradientMap)}function u(p,f){p.metalness.value=f.metalness,f.metalnessMap&&(p.metalnessMap.value=f.metalnessMap,t(f.metalnessMap,p.metalnessMapTransform)),p.roughness.value=f.roughness,f.roughnessMap&&(p.roughnessMap.value=f.roughnessMap,t(f.roughnessMap,p.roughnessMapTransform)),f.envMap&&(p.envMapIntensity.value=f.envMapIntensity)}function m(p,f,M){p.ior.value=f.ior,f.sheen>0&&(p.sheenColor.value.copy(f.sheenColor).multiplyScalar(f.sheen),p.sheenRoughness.value=f.sheenRoughness,f.sheenColorMap&&(p.sheenColorMap.value=f.sheenColorMap,t(f.sheenColorMap,p.sheenColorMapTransform)),f.sheenRoughnessMap&&(p.sheenRoughnessMap.value=f.sheenRoughnessMap,t(f.sheenRoughnessMap,p.sheenRoughnessMapTransform))),f.clearcoat>0&&(p.clearcoat.value=f.clearcoat,p.clearcoatRoughness.value=f.clearcoatRoughness,f.clearcoatMap&&(p.clearcoatMap.value=f.clearcoatMap,t(f.clearcoatMap,p.clearcoatMapTransform)),f.clearcoatRoughnessMap&&(p.clearcoatRoughnessMap.value=f.clearcoatRoughnessMap,t(f.clearcoatRoughnessMap,p.clearcoatRoughnessMapTransform)),f.clearcoatNormalMap&&(p.clearcoatNormalMap.value=f.clearcoatNormalMap,t(f.clearcoatNormalMap,p.clearcoatNormalMapTransform),p.clearcoatNormalScale.value.copy(f.clearcoatNormalScale),f.side===vt&&p.clearcoatNormalScale.value.negate())),f.dispersion>0&&(p.dispersion.value=f.dispersion),f.iridescence>0&&(p.iridescence.value=f.iridescence,p.iridescenceIOR.value=f.iridescenceIOR,p.iridescenceThicknessMinimum.value=f.iridescenceThicknessRange[0],p.iridescenceThicknessMaximum.value=f.iridescenceThicknessRange[1],f.iridescenceMap&&(p.iridescenceMap.value=f.iridescenceMap,t(f.iridescenceMap,p.iridescenceMapTransform)),f.iridescenceThicknessMap&&(p.iridescenceThicknessMap.value=f.iridescenceThicknessMap,t(f.iridescenceThicknessMap,p.iridescenceThicknessMapTransform))),f.transmission>0&&(p.transmission.value=f.transmission,p.transmissionSamplerMap.value=M.texture,p.transmissionSamplerSize.value.set(M.width,M.height),f.transmissionMap&&(p.transmissionMap.value=f.transmissionMap,t(f.transmissionMap,p.transmissionMapTransform)),p.thickness.value=f.thickness,f.thicknessMap&&(p.thicknessMap.value=f.thicknessMap,t(f.thicknessMap,p.thicknessMapTransform)),p.attenuationDistance.value=f.attenuationDistance,p.attenuationColor.value.copy(f.attenuationColor)),f.anisotropy>0&&(p.anisotropyVector.value.set(f.anisotropy*Math.cos(f.anisotropyRotation),f.anisotropy*Math.sin(f.anisotropyRotation)),f.anisotropyMap&&(p.anisotropyMap.value=f.anisotropyMap,t(f.anisotropyMap,p.anisotropyMapTransform))),p.specularIntensity.value=f.specularIntensity,p.specularColor.value.copy(f.specularColor),f.specularColorMap&&(p.specularColorMap.value=f.specularColorMap,t(f.specularColorMap,p.specularColorMapTransform)),f.specularIntensityMap&&(p.specularIntensityMap.value=f.specularIntensityMap,t(f.specularIntensityMap,p.specularIntensityMapTransform))}function _(p,f){f.matcap&&(p.matcap.value=f.matcap)}function x(p,f){const M=e.get(f).light;p.referencePosition.value.setFromMatrixPosition(M.matrixWorld),p.nearDistance.value=M.shadow.camera.near,p.farDistance.value=M.shadow.camera.far}return{refreshFogUniforms:i,refreshMaterialUniforms:s}}function cm(n,e,t,i){let s={},r={},a=[];const o=n.getParameter(n.MAX_UNIFORM_BUFFER_BINDINGS);function l(M,y){const E=y.program;i.uniformBlockBinding(M,E)}function c(M,y){let E=s[M.id];E===void 0&&(_(M),E=h(M),s[M.id]=E,M.addEventListener("dispose",p));const L=y.program;i.updateUBOMapping(M,L);const w=e.render.frame;r[M.id]!==w&&(u(M),r[M.id]=w)}function h(M){const y=d();M.__bindingPointIndex=y;const E=n.createBuffer(),L=M.__size,w=M.usage;return n.bindBuffer(n.UNIFORM_BUFFER,E),n.bufferData(n.UNIFORM_BUFFER,L,w),n.bindBuffer(n.UNIFORM_BUFFER,null),n.bindBufferBase(n.UNIFORM_BUFFER,y,E),E}function d(){for(let M=0;M<o;M++)if(a.indexOf(M)===-1)return a.push(M),M;return je("WebGLRenderer: Maximum number of simultaneously usable uniforms groups reached."),0}function u(M){const y=s[M.id],E=M.uniforms,L=M.__cache;n.bindBuffer(n.UNIFORM_BUFFER,y);for(let w=0,D=E.length;w<D;w++){const b=Array.isArray(E[w])?E[w]:[E[w]];for(let T=0,U=b.length;T<U;T++){const C=b[T];if(m(C,w,T,L)===!0){const z=C.__offset,X=Array.isArray(C.value)?C.value:[C.value];let j=0;for(let N=0;N<X.length;N++){const V=X[N],H=x(V);typeof V=="number"||typeof V=="boolean"?(C.__data[0]=V,n.bufferSubData(n.UNIFORM_BUFFER,z+j,C.__data)):V.isMatrix3?(C.__data[0]=V.elements[0],C.__data[1]=V.elements[1],C.__data[2]=V.elements[2],C.__data[3]=0,C.__data[4]=V.elements[3],C.__data[5]=V.elements[4],C.__data[6]=V.elements[5],C.__data[7]=0,C.__data[8]=V.elements[6],C.__data[9]=V.elements[7],C.__data[10]=V.elements[8],C.__data[11]=0):ArrayBuffer.isView(V)?C.__data.set(new V.constructor(V.buffer,V.byteOffset,C.__data.length)):(V.toArray(C.__data,j),j+=H.storage/Float32Array.BYTES_PER_ELEMENT)}n.bufferSubData(n.UNIFORM_BUFFER,z,C.__data)}}}n.bindBuffer(n.UNIFORM_BUFFER,null)}function m(M,y,E,L){const w=M.value,D=y+"_"+E;if(L[D]===void 0)return typeof w=="number"||typeof w=="boolean"?L[D]=w:ArrayBuffer.isView(w)?L[D]=w.slice():L[D]=w.clone(),!0;{const b=L[D];if(typeof w=="number"||typeof w=="boolean"){if(b!==w)return L[D]=w,!0}else{if(ArrayBuffer.isView(w))return!0;if(b.equals(w)===!1)return b.copy(w),!0}}return!1}function _(M){const y=M.uniforms;let E=0;const L=16;for(let D=0,b=y.length;D<b;D++){const T=Array.isArray(y[D])?y[D]:[y[D]];for(let U=0,C=T.length;U<C;U++){const z=T[U],X=Array.isArray(z.value)?z.value:[z.value];for(let j=0,N=X.length;j<N;j++){const V=X[j],H=x(V),K=E%L,ee=K%H.boundary,ce=K+ee;E+=ee,ce!==0&&L-ce<H.storage&&(E+=L-ce),z.__data=new Float32Array(H.storage/Float32Array.BYTES_PER_ELEMENT),z.__offset=E,E+=H.storage}}}const w=E%L;return w>0&&(E+=L-w),M.__size=E,M.__cache={},this}function x(M){const y={boundary:0,storage:0};return typeof M=="number"||typeof M=="boolean"?(y.boundary=4,y.storage=4):M.isVector2?(y.boundary=8,y.storage=8):M.isVector3||M.isColor?(y.boundary=16,y.storage=12):M.isVector4?(y.boundary=16,y.storage=16):M.isMatrix3?(y.boundary=48,y.storage=48):M.isMatrix4?(y.boundary=64,y.storage=64):M.isTexture?Ce("WebGLRenderer: Texture samplers can not be part of an uniforms group."):ArrayBuffer.isView(M)?(y.boundary=16,y.storage=M.byteLength):Ce("WebGLRenderer: Unsupported uniform value type.",M),y}function p(M){const y=M.target;y.removeEventListener("dispose",p);const E=a.indexOf(y.__bindingPointIndex);a.splice(E,1),n.deleteBuffer(s[y.id]),delete s[y.id],delete r[y.id]}function f(){for(const M in s)n.deleteBuffer(s[M]);a=[],s={},r={}}return{bind:l,update:c,dispose:f}}const hm=new Uint16Array([12469,15057,12620,14925,13266,14620,13807,14376,14323,13990,14545,13625,14713,13328,14840,12882,14931,12528,14996,12233,15039,11829,15066,11525,15080,11295,15085,10976,15082,10705,15073,10495,13880,14564,13898,14542,13977,14430,14158,14124,14393,13732,14556,13410,14702,12996,14814,12596,14891,12291,14937,11834,14957,11489,14958,11194,14943,10803,14921,10506,14893,10278,14858,9960,14484,14039,14487,14025,14499,13941,14524,13740,14574,13468,14654,13106,14743,12678,14818,12344,14867,11893,14889,11509,14893,11180,14881,10751,14852,10428,14812,10128,14765,9754,14712,9466,14764,13480,14764,13475,14766,13440,14766,13347,14769,13070,14786,12713,14816,12387,14844,11957,14860,11549,14868,11215,14855,10751,14825,10403,14782,10044,14729,9651,14666,9352,14599,9029,14967,12835,14966,12831,14963,12804,14954,12723,14936,12564,14917,12347,14900,11958,14886,11569,14878,11247,14859,10765,14828,10401,14784,10011,14727,9600,14660,9289,14586,8893,14508,8533,15111,12234,15110,12234,15104,12216,15092,12156,15067,12010,15028,11776,14981,11500,14942,11205,14902,10752,14861,10393,14812,9991,14752,9570,14682,9252,14603,8808,14519,8445,14431,8145,15209,11449,15208,11451,15202,11451,15190,11438,15163,11384,15117,11274,15055,10979,14994,10648,14932,10343,14871,9936,14803,9532,14729,9218,14645,8742,14556,8381,14461,8020,14365,7603,15273,10603,15272,10607,15267,10619,15256,10631,15231,10614,15182,10535,15118,10389,15042,10167,14963,9787,14883,9447,14800,9115,14710,8665,14615,8318,14514,7911,14411,7507,14279,7198,15314,9675,15313,9683,15309,9712,15298,9759,15277,9797,15229,9773,15166,9668,15084,9487,14995,9274,14898,8910,14800,8539,14697,8234,14590,7790,14479,7409,14367,7067,14178,6621,15337,8619,15337,8631,15333,8677,15325,8769,15305,8871,15264,8940,15202,8909,15119,8775,15022,8565,14916,8328,14804,8009,14688,7614,14569,7287,14448,6888,14321,6483,14088,6171,15350,7402,15350,7419,15347,7480,15340,7613,15322,7804,15287,7973,15229,8057,15148,8012,15046,7846,14933,7611,14810,7357,14682,7069,14552,6656,14421,6316,14251,5948,14007,5528,15356,5942,15356,5977,15353,6119,15348,6294,15332,6551,15302,6824,15249,7044,15171,7122,15070,7050,14949,6861,14818,6611,14679,6349,14538,6067,14398,5651,14189,5311,13935,4958,15359,4123,15359,4153,15356,4296,15353,4646,15338,5160,15311,5508,15263,5829,15188,6042,15088,6094,14966,6001,14826,5796,14678,5543,14527,5287,14377,4985,14133,4586,13869,4257,15360,1563,15360,1642,15358,2076,15354,2636,15341,3350,15317,4019,15273,4429,15203,4732,15105,4911,14981,4932,14836,4818,14679,4621,14517,4386,14359,4156,14083,3795,13808,3437,15360,122,15360,137,15358,285,15355,636,15344,1274,15322,2177,15281,2765,15215,3223,15120,3451,14995,3569,14846,3567,14681,3466,14511,3305,14344,3121,14037,2800,13753,2467,15360,0,15360,1,15359,21,15355,89,15346,253,15325,479,15287,796,15225,1148,15133,1492,15008,1749,14856,1882,14685,1886,14506,1783,14324,1608,13996,1398,13702,1183]);let di=null;function um(){return di===null&&(di=new Ch(hm,16,16,Yi,vi),di.name="DFG_LUT",di.minFilter=Ct,di.magFilter=Ct,di.wrapS=_i,di.wrapT=_i,di.generateMipmaps=!1,di.needsUpdate=!0),di}class dm{constructor(e={}){const{canvas:t=Wc(),context:i=null,depth:s=!0,stencil:r=!1,alpha:a=!1,antialias:o=!1,premultipliedAlpha:l=!0,preserveDrawingBuffer:c=!1,powerPreference:h="default",failIfMajorPerformanceCaveat:d=!1,reversedDepthBuffer:u=!1,outputBufferType:m=kt}=e;this.isWebGLRenderer=!0;let _;if(i!==null){if(typeof WebGLRenderingContext<"u"&&i instanceof WebGLRenderingContext)throw new Error("THREE.WebGLRenderer: WebGL 1 is not supported since r163.");_=i.getContextAttributes().alpha}else _=a;const x=m,p=new Set([gr,mr,pr]),f=new Set([kt,ri,Fn,On,dr,fr]),M=new Uint32Array(4),y=new Int32Array(4),E=new A;let L=null,w=null;const D=[],b=[];let T=null;this.domElement=t,this.debug={checkShaderErrors:!0,onShaderError:null},this.autoClear=!0,this.autoClearColor=!0,this.autoClearDepth=!0,this.autoClearStencil=!0,this.sortObjects=!0,this.clippingPlanes=[],this.localClippingEnabled=!1,this.toneMapping=si,this.toneMappingExposure=1,this.transmissionResolutionScale=1;const U=this;let C=!1,z=null;this._outputColorSpace=jt;let X=0,j=0,N=null,V=-1,H=null;const K=new ht,ee=new ht;let ce=null;const be=new Re(0);let Me=0,He=t.width,Ze=t.height,De=1,Z=null,ue=null;const te=new ht(0,0,He,Ze),Te=new ht(0,0,He,Ze);let Le=!1;const Ae=new ya;let tt=!1,ie=!1;const Pe=new lt,Ke=new A,ke=new ht,yt={background:null,fog:null,environment:null,overrideMaterial:null,isScene:!0};let ct=!1;function Wt(){return N===null?De:1}let P=i;function Et(v,I){return t.getContext(v,I)}try{const v={alpha:!0,depth:s,stencil:r,antialias:o,premultipliedAlpha:l,preserveDrawingBuffer:c,powerPreference:h,failIfMajorPerformanceCaveat:d};if("setAttribute"in t&&t.setAttribute("data-engine",`three.js r${Zs}`),t.addEventListener("webglcontextlost",J,!1),t.addEventListener("webglcontextrestored",ye,!1),t.addEventListener("webglcontextcreationerror",Fe,!1),P===null){const I="webgl2";if(P=Et(I,v),P===null)throw Et(I)?new Error("Error creating WebGL context with your selected attributes."):new Error("Error creating WebGL context.")}}catch(v){throw je("WebGLRenderer: "+v.message),v}let We,at,de,ut,S,g,F,Y,Q,ne,le,W,$,me,ve,ae,se,Ie,Be,$e,R,re,q;function ge(){We=new uf(P),We.init(),R=new im(P,We),at=new nf(P,We,e,R),de=new em(P,We),at.reversedDepthBuffer&&u&&de.buffers.depth.setReversed(!0),ut=new pf(P),S=new kp,g=new tm(P,We,de,S,at,R,ut),F=new hf(U),Y=new Kh(P),re=new ef(P,Y),Q=new df(P,Y,ut,re),ne=new gf(P,Q,Y,re,ut),Ie=new mf(P,at,g),ve=new sf(S),le=new zp(U,F,We,at,re,ve),W=new lm(U,S),$=new Gp,me=new Yp(We),se=new Qd(U,F,de,ne,_,l),ae=new Qp(U,ne,at),q=new cm(P,ut,at,de),Be=new tf(P,We,ut),$e=new ff(P,We,ut),ut.programs=le.programs,U.capabilities=at,U.extensions=We,U.properties=S,U.renderLists=$,U.shadowMap=ae,U.state=de,U.info=ut}ge(),x!==kt&&(T=new vf(x,t.width,t.height,s,r));const oe=new am(U,P);this.xr=oe,this.getContext=function(){return P},this.getContextAttributes=function(){return P.getContextAttributes()},this.forceContextLoss=function(){const v=We.get("WEBGL_lose_context");v&&v.loseContext()},this.forceContextRestore=function(){const v=We.get("WEBGL_lose_context");v&&v.restoreContext()},this.getPixelRatio=function(){return De},this.setPixelRatio=function(v){v!==void 0&&(De=v,this.setSize(He,Ze,!1))},this.getSize=function(v){return v.set(He,Ze)},this.setSize=function(v,I,G=!0){if(oe.isPresenting){Ce("WebGLRenderer: Can't change size while VR device is presenting.");return}He=v,Ze=I,t.width=Math.floor(v*De),t.height=Math.floor(I*De),G===!0&&(t.style.width=v+"px",t.style.height=I+"px"),T!==null&&T.setSize(t.width,t.height),this.setViewport(0,0,v,I)},this.getDrawingBufferSize=function(v){return v.set(He*De,Ze*De).floor()},this.setDrawingBufferSize=function(v,I,G){He=v,Ze=I,De=G,t.width=Math.floor(v*G),t.height=Math.floor(I*G),this.setViewport(0,0,v,I)},this.setEffects=function(v){if(x===kt){je("THREE.WebGLRenderer: setEffects() requires outputBufferType set to HalfFloatType or FloatType.");return}if(v){for(let I=0;I<v.length;I++)if(v[I].isOutputPass===!0){Ce("THREE.WebGLRenderer: OutputPass is not needed in setEffects(). Tone mapping and color space conversion are applied automatically.");break}}T.setEffects(v||[])},this.getCurrentViewport=function(v){return v.copy(K)},this.getViewport=function(v){return v.copy(te)},this.setViewport=function(v,I,G,B){v.isVector4?te.set(v.x,v.y,v.z,v.w):te.set(v,I,G,B),de.viewport(K.copy(te).multiplyScalar(De).round())},this.getScissor=function(v){return v.copy(Te)},this.setScissor=function(v,I,G,B){v.isVector4?Te.set(v.x,v.y,v.z,v.w):Te.set(v,I,G,B),de.scissor(ee.copy(Te).multiplyScalar(De).round())},this.getScissorTest=function(){return Le},this.setScissorTest=function(v){de.setScissorTest(Le=v)},this.setOpaqueSort=function(v){Z=v},this.setTransparentSort=function(v){ue=v},this.getClearColor=function(v){return v.copy(se.getClearColor())},this.setClearColor=function(){se.setClearColor(...arguments)},this.getClearAlpha=function(){return se.getClearAlpha()},this.setClearAlpha=function(){se.setClearAlpha(...arguments)},this.clear=function(v=!0,I=!0,G=!0){let B=0;if(v){let k=!1;if(N!==null){const pe=N.texture.format;k=p.has(pe)}if(k){const pe=N.texture.type,xe=f.has(pe),fe=se.getClearColor(),Se=se.getClearAlpha(),Ee=fe.r,Oe=fe.g,Ve=fe.b;xe?(M[0]=Ee,M[1]=Oe,M[2]=Ve,M[3]=Se,P.clearBufferuiv(P.COLOR,0,M)):(y[0]=Ee,y[1]=Oe,y[2]=Ve,y[3]=Se,P.clearBufferiv(P.COLOR,0,y))}else B|=P.COLOR_BUFFER_BIT}I&&(B|=P.DEPTH_BUFFER_BIT,this.state.buffers.depth.setMask(!0)),G&&(B|=P.STENCIL_BUFFER_BIT,this.state.buffers.stencil.setMask(4294967295)),B!==0&&P.clear(B)},this.clearColor=function(){this.clear(!0,!1,!1)},this.clearDepth=function(){this.clear(!1,!0,!1)},this.clearStencil=function(){this.clear(!1,!1,!0)},this.setNodesHandler=function(v){v.setRenderer(this),z=v},this.dispose=function(){t.removeEventListener("webglcontextlost",J,!1),t.removeEventListener("webglcontextrestored",ye,!1),t.removeEventListener("webglcontextcreationerror",Fe,!1),se.dispose(),$.dispose(),me.dispose(),S.dispose(),F.dispose(),ne.dispose(),re.dispose(),q.dispose(),le.dispose(),oe.dispose(),oe.removeEventListener("sessionstart",ic),oe.removeEventListener("sessionend",nc),tn.stop()};function J(v){v.preventDefault(),So("WebGLRenderer: Context Lost."),C=!0}function ye(){So("WebGLRenderer: Context Restored."),C=!1;const v=ut.autoReset,I=ae.enabled,G=ae.autoUpdate,B=ae.needsUpdate,k=ae.type;ge(),ut.autoReset=v,ae.enabled=I,ae.autoUpdate=G,ae.needsUpdate=B,ae.type=k}function Fe(v){je("WebGLRenderer: A WebGL context could not be created. Reason: ",v.statusMessage)}function ft(v){const I=v.target;I.removeEventListener("dispose",ft),it(I)}function it(v){Ai(v),S.remove(v)}function Ai(v){const I=S.get(v).programs;I!==void 0&&(I.forEach(function(G){le.releaseProgram(G)}),v.isShaderMaterial&&le.releaseShaderCache(v))}this.renderBufferDirect=function(v,I,G,B,k,pe){I===null&&(I=yt);const xe=k.isMesh&&k.matrixWorld.determinant()<0,fe=Qg(v,I,G,B,k);de.setMaterial(B,xe);let Se=G.index,Ee=1;if(B.wireframe===!0){if(Se=Q.getWireframeAttribute(G),Se===void 0)return;Ee=2}const Oe=G.drawRange,Ve=G.attributes.position;let we=Oe.start*Ee,nt=(Oe.start+Oe.count)*Ee;pe!==null&&(we=Math.max(we,pe.start*Ee),nt=Math.min(nt,(pe.start+pe.count)*Ee)),Se!==null?(we=Math.max(we,0),nt=Math.min(nt,Se.count)):Ve!=null&&(we=Math.max(we,0),nt=Math.min(nt,Ve.count));const pt=nt-we;if(pt<0||pt===1/0)return;re.setup(k,B,fe,G,Se);let dt,st=Be;if(Se!==null&&(dt=Y.get(Se),st=$e,st.setIndex(dt)),k.isMesh)B.wireframe===!0?(de.setLineWidth(B.wireframeLinewidth*Wt()),st.setMode(P.LINES)):st.setMode(P.TRIANGLES);else if(k.isLine){let Dt=B.linewidth;Dt===void 0&&(Dt=1),de.setLineWidth(Dt*Wt()),k.isLineSegments?st.setMode(P.LINES):k.isLineLoop?st.setMode(P.LINE_LOOP):st.setMode(P.LINE_STRIP)}else k.isPoints?st.setMode(P.POINTS):k.isSprite&&st.setMode(P.TRIANGLES);if(k.isBatchedMesh)if(We.get("WEBGL_multi_draw"))st.renderMultiDraw(k._multiDrawStarts,k._multiDrawCounts,k._multiDrawCount);else{const Dt=k._multiDrawStarts,_e=k._multiDrawCounts,Xt=k._multiDrawCount,Ye=Se?Y.get(Se).bytesPerElement:1,Jt=S.get(B).currentProgram.getUniforms();for(let mi=0;mi<Xt;mi++)Jt.setValue(P,"_gl_DrawID",mi),st.render(Dt[mi]/Ye,_e[mi])}else if(k.isInstancedMesh)st.renderInstances(we,pt,k.count);else if(G.isInstancedBufferGeometry){const Dt=G._maxInstanceCount!==void 0?G._maxInstanceCount:1/0,_e=Math.min(G.instanceCount,Dt);st.renderInstances(we,pt,_e)}else st.render(we,pt)};function pi(v,I,G){v.transparent===!0&&v.side===It&&v.forceSinglePass===!1?(v.side=vt,v.needsUpdate=!0,$s(v,I,G),v.side=ni,v.needsUpdate=!0,$s(v,I,G),v.side=It):$s(v,I,G)}this.compile=function(v,I,G=null){G===null&&(G=v),w=me.get(G),w.init(I),b.push(w),G.traverseVisible(function(k){k.isLight&&k.layers.test(I.layers)&&(w.pushLight(k),k.castShadow&&w.pushShadow(k))}),v!==G&&v.traverseVisible(function(k){k.isLight&&k.layers.test(I.layers)&&(w.pushLight(k),k.castShadow&&w.pushShadow(k))}),w.setupLights();const B=new Set;return v.traverse(function(k){if(!(k.isMesh||k.isPoints||k.isLine||k.isSprite))return;const pe=k.material;if(pe)if(Array.isArray(pe))for(let xe=0;xe<pe.length;xe++){const fe=pe[xe];pi(fe,G,k),B.add(fe)}else pi(pe,G,k),B.add(pe)}),w=b.pop(),B},this.compileAsync=function(v,I,G=null){const B=this.compile(v,I,G);return new Promise(k=>{function pe(){if(B.forEach(function(xe){S.get(xe).currentProgram.isReady()&&B.delete(xe)}),B.size===0){k(v);return}setTimeout(pe,10)}We.get("KHR_parallel_shader_compile")!==null?pe():setTimeout(pe,10)})};let Ya=null;function Kg(v){Ya&&Ya(v)}function ic(){tn.stop()}function nc(){tn.start()}const tn=new hl;tn.setAnimationLoop(Kg),typeof self<"u"&&tn.setContext(self),this.setAnimationLoop=function(v){Ya=v,oe.setAnimationLoop(v),v===null?tn.stop():tn.start()},oe.addEventListener("sessionstart",ic),oe.addEventListener("sessionend",nc),this.render=function(v,I){if(I!==void 0&&I.isCamera!==!0){je("WebGLRenderer.render: camera is not an instance of THREE.Camera.");return}if(C===!0)return;z!==null&&z.renderStart(v,I);const G=oe.enabled===!0&&oe.isPresenting===!0,B=T!==null&&(N===null||G)&&T.begin(U,N);if(v.matrixWorldAutoUpdate===!0&&v.updateMatrixWorld(),I.parent===null&&I.matrixWorldAutoUpdate===!0&&I.updateMatrixWorld(),oe.enabled===!0&&oe.isPresenting===!0&&(T===null||T.isCompositing()===!1)&&(oe.cameraAutoUpdate===!0&&oe.updateCamera(I),I=oe.getCamera()),v.isScene===!0&&v.onBeforeRender(U,v,I,N),w=me.get(v,b.length),w.init(I),w.state.textureUnits=g.getTextureUnits(),b.push(w),Pe.multiplyMatrices(I.projectionMatrix,I.matrixWorldInverse),Ae.setFromProjectionMatrix(Pe,oi,I.reversedDepth),ie=this.localClippingEnabled,tt=ve.init(this.clippingPlanes,ie),L=$.get(v,D.length),L.init(),D.push(L),oe.enabled===!0&&oe.isPresenting===!0){const xe=U.xr.getDepthSensingMesh();xe!==null&&$a(xe,I,-1/0,U.sortObjects)}$a(v,I,0,U.sortObjects),L.finish(),U.sortObjects===!0&&L.sort(Z,ue),ct=oe.enabled===!1||oe.isPresenting===!1||oe.hasDepthSensing()===!1,ct&&se.addToRenderList(L,v),this.info.render.frame++,tt===!0&&ve.beginShadows();const k=w.state.shadowsArray;if(ae.render(k,v,I),tt===!0&&ve.endShadows(),this.info.autoReset===!0&&this.info.reset(),(B&&T.hasRenderPass())===!1){const xe=L.opaque,fe=L.transmissive;if(w.setupLights(),I.isArrayCamera){const Se=I.cameras;if(fe.length>0)for(let Ee=0,Oe=Se.length;Ee<Oe;Ee++){const Ve=Se[Ee];rc(xe,fe,v,Ve)}ct&&se.render(v);for(let Ee=0,Oe=Se.length;Ee<Oe;Ee++){const Ve=Se[Ee];sc(L,v,Ve,Ve.viewport)}}else fe.length>0&&rc(xe,fe,v,I),ct&&se.render(v),sc(L,v,I)}N!==null&&j===0&&(g.updateMultisampleRenderTarget(N),g.updateRenderTargetMipmap(N)),B&&T.end(U),v.isScene===!0&&v.onAfterRender(U,v,I),re.resetDefaultState(),V=-1,H=null,b.pop(),b.length>0?(w=b[b.length-1],g.setTextureUnits(w.state.textureUnits),tt===!0&&ve.setGlobalState(U.clippingPlanes,w.state.camera)):w=null,D.pop(),D.length>0?L=D[D.length-1]:L=null,z!==null&&z.renderEnd()};function $a(v,I,G,B){if(v.visible===!1)return;if(v.layers.test(I.layers)){if(v.isGroup)G=v.renderOrder;else if(v.isLOD)v.autoUpdate===!0&&v.update(I);else if(v.isLightProbeGrid)w.pushLightProbeGrid(v);else if(v.isLight)w.pushLight(v),v.castShadow&&w.pushShadow(v);else if(v.isSprite){if(!v.frustumCulled||Ae.intersectsSprite(v)){B&&ke.setFromMatrixPosition(v.matrixWorld).applyMatrix4(Pe);const xe=ne.update(v),fe=v.material;fe.visible&&L.push(v,xe,fe,G,ke.z,null)}}else if((v.isMesh||v.isLine||v.isPoints)&&(!v.frustumCulled||Ae.intersectsObject(v))){const xe=ne.update(v),fe=v.material;if(B&&(v.boundingSphere!==void 0?(v.boundingSphere===null&&v.computeBoundingSphere(),ke.copy(v.boundingSphere.center)):(xe.boundingSphere===null&&xe.computeBoundingSphere(),ke.copy(xe.boundingSphere.center)),ke.applyMatrix4(v.matrixWorld).applyMatrix4(Pe)),Array.isArray(fe)){const Se=xe.groups;for(let Ee=0,Oe=Se.length;Ee<Oe;Ee++){const Ve=Se[Ee],we=fe[Ve.materialIndex];we&&we.visible&&L.push(v,xe,we,G,ke.z,Ve)}}else fe.visible&&L.push(v,xe,fe,G,ke.z,null)}}const pe=v.children;for(let xe=0,fe=pe.length;xe<fe;xe++)$a(pe[xe],I,G,B)}function sc(v,I,G,B){const{opaque:k,transmissive:pe,transparent:xe}=v;w.setupLightsView(G),tt===!0&&ve.setGlobalState(U.clippingPlanes,G),B&&de.viewport(K.copy(B)),k.length>0&&Ys(k,I,G),pe.length>0&&Ys(pe,I,G),xe.length>0&&Ys(xe,I,G),de.buffers.depth.setTest(!0),de.buffers.depth.setMask(!0),de.buffers.color.setMask(!0),de.setPolygonOffset(!1)}function rc(v,I,G,B){if((G.isScene===!0?G.overrideMaterial:null)!==null)return;if(w.state.transmissionRenderTarget[B.id]===void 0){const we=We.has("EXT_color_buffer_half_float")||We.has("EXT_color_buffer_float");w.state.transmissionRenderTarget[B.id]=new li(1,1,{generateMipmaps:!0,type:we?vi:kt,minFilter:ji,samples:Math.max(4,at.samples),stencilBuffer:r,resolveDepthBuffer:!1,resolveStencilBuffer:!1,colorSpace:Xe.workingColorSpace})}const pe=w.state.transmissionRenderTarget[B.id],xe=B.viewport||K;pe.setSize(xe.z*U.transmissionResolutionScale,xe.w*U.transmissionResolutionScale);const fe=U.getRenderTarget(),Se=U.getActiveCubeFace(),Ee=U.getActiveMipmapLevel();U.setRenderTarget(pe),U.getClearColor(be),Me=U.getClearAlpha(),Me<1&&U.setClearColor(16777215,.5),U.clear(),ct&&se.render(G);const Oe=U.toneMapping;U.toneMapping=si;const Ve=B.viewport;if(B.viewport!==void 0&&(B.viewport=void 0),w.setupLightsView(B),tt===!0&&ve.setGlobalState(U.clippingPlanes,B),Ys(v,G,B),g.updateMultisampleRenderTarget(pe),g.updateRenderTargetMipmap(pe),We.has("WEBGL_multisampled_render_to_texture")===!1){let we=!1;for(let nt=0,pt=I.length;nt<pt;nt++){const dt=I[nt],{object:st,geometry:Dt,material:_e,group:Xt}=dt;if(_e.side===It&&st.layers.test(B.layers)){const Ye=_e.side;_e.side=vt,_e.needsUpdate=!0,ac(st,G,B,Dt,_e,Xt),_e.side=Ye,_e.needsUpdate=!0,we=!0}}we===!0&&(g.updateMultisampleRenderTarget(pe),g.updateRenderTargetMipmap(pe))}U.setRenderTarget(fe,Se,Ee),U.setClearColor(be,Me),Ve!==void 0&&(B.viewport=Ve),U.toneMapping=Oe}function Ys(v,I,G){const B=I.isScene===!0?I.overrideMaterial:null;for(let k=0,pe=v.length;k<pe;k++){const xe=v[k],{object:fe,geometry:Se,group:Ee}=xe;let Oe=xe.material;Oe.allowOverride===!0&&B!==null&&(Oe=B),fe.layers.test(G.layers)&&ac(fe,I,G,Se,Oe,Ee)}}function ac(v,I,G,B,k,pe){v.onBeforeRender(U,I,G,B,k,pe),v.modelViewMatrix.multiplyMatrices(G.matrixWorldInverse,v.matrixWorld),v.normalMatrix.getNormalMatrix(v.modelViewMatrix),k.onBeforeRender(U,I,G,B,v,pe),k.transparent===!0&&k.side===It&&k.forceSinglePass===!1?(k.side=vt,k.needsUpdate=!0,U.renderBufferDirect(G,I,B,k,v,pe),k.side=ni,k.needsUpdate=!0,U.renderBufferDirect(G,I,B,k,v,pe),k.side=It):U.renderBufferDirect(G,I,B,k,v,pe),v.onAfterRender(U,I,G,B,k,pe)}function $s(v,I,G){I.isScene!==!0&&(I=yt);const B=S.get(v),k=w.state.lights,pe=w.state.shadowsArray,xe=k.state.version,fe=le.getParameters(v,k.state,pe,I,G,w.state.lightProbeGridArray),Se=le.getProgramCacheKey(fe);let Ee=B.programs;B.environment=v.isMeshStandardMaterial||v.isMeshLambertMaterial||v.isMeshPhongMaterial?I.environment:null,B.fog=I.fog;const Oe=v.isMeshStandardMaterial||v.isMeshLambertMaterial&&!v.envMap||v.isMeshPhongMaterial&&!v.envMap;B.envMap=F.get(v.envMap||B.environment,Oe),B.envMapRotation=B.environment!==null&&v.envMap===null?I.environmentRotation:v.envMapRotation,Ee===void 0&&(v.addEventListener("dispose",ft),Ee=new Map,B.programs=Ee);let Ve=Ee.get(Se);if(Ve!==void 0){if(B.currentProgram===Ve&&B.lightsStateVersion===xe)return lc(v,fe),Ve}else fe.uniforms=le.getUniforms(v),z!==null&&v.isNodeMaterial&&z.build(v,G,fe),v.onBeforeCompile(fe,U),Ve=le.acquireProgram(fe,Se),Ee.set(Se,Ve),B.uniforms=fe.uniforms;const we=B.uniforms;return(!v.isShaderMaterial&&!v.isRawShaderMaterial||v.clipping===!0)&&(we.clippingPlanes=ve.uniform),lc(v,fe),B.needsLights=t_(v),B.lightsStateVersion=xe,B.needsLights&&(we.ambientLightColor.value=k.state.ambient,we.lightProbe.value=k.state.probe,we.directionalLights.value=k.state.directional,we.directionalLightShadows.value=k.state.directionalShadow,we.spotLights.value=k.state.spot,we.spotLightShadows.value=k.state.spotShadow,we.rectAreaLights.value=k.state.rectArea,we.ltc_1.value=k.state.rectAreaLTC1,we.ltc_2.value=k.state.rectAreaLTC2,we.pointLights.value=k.state.point,we.pointLightShadows.value=k.state.pointShadow,we.hemisphereLights.value=k.state.hemi,we.directionalShadowMatrix.value=k.state.directionalShadowMatrix,we.spotLightMatrix.value=k.state.spotLightMatrix,we.spotLightMap.value=k.state.spotLightMap,we.pointShadowMatrix.value=k.state.pointShadowMatrix),B.lightProbeGrid=w.state.lightProbeGridArray.length>0,B.currentProgram=Ve,B.uniformsList=null,Ve}function oc(v){if(v.uniformsList===null){const I=v.currentProgram.getUniforms();v.uniformsList=Ws.seqWithValue(I.seq,v.uniforms)}return v.uniformsList}function lc(v,I){const G=S.get(v);G.outputColorSpace=I.outputColorSpace,G.batching=I.batching,G.batchingColor=I.batchingColor,G.instancing=I.instancing,G.instancingColor=I.instancingColor,G.instancingMorph=I.instancingMorph,G.skinning=I.skinning,G.morphTargets=I.morphTargets,G.morphNormals=I.morphNormals,G.morphColors=I.morphColors,G.morphTargetsCount=I.morphTargetsCount,G.numClippingPlanes=I.numClippingPlanes,G.numIntersection=I.numClipIntersection,G.vertexAlphas=I.vertexAlphas,G.vertexTangents=I.vertexTangents,G.toneMapping=I.toneMapping}function Jg(v,I){if(v.length===0)return null;if(v.length===1)return v[0].texture!==null?v[0]:null;E.setFromMatrixPosition(I.matrixWorld);for(let G=0,B=v.length;G<B;G++){const k=v[G];if(k.texture!==null&&k.boundingBox.containsPoint(E))return k}return null}function Qg(v,I,G,B,k){I.isScene!==!0&&(I=yt),g.resetTextureUnits();const pe=I.fog,xe=B.isMeshStandardMaterial||B.isMeshLambertMaterial||B.isMeshPhongMaterial?I.environment:null,fe=N===null?U.outputColorSpace:N.isXRRenderTarget===!0?N.texture.colorSpace:Xe.workingColorSpace,Se=B.isMeshStandardMaterial||B.isMeshLambertMaterial&&!B.envMap||B.isMeshPhongMaterial&&!B.envMap,Ee=F.get(B.envMap||xe,Se),Oe=B.vertexColors===!0&&!!G.attributes.color&&G.attributes.color.itemSize===4,Ve=!!G.attributes.tangent&&(!!B.normalMap||B.anisotropy>0),we=!!G.morphAttributes.position,nt=!!G.morphAttributes.normal,pt=!!G.morphAttributes.color;let dt=si;B.toneMapped&&(N===null||N.isXRRenderTarget===!0)&&(dt=U.toneMapping);const st=G.morphAttributes.position||G.morphAttributes.normal||G.morphAttributes.color,Dt=st!==void 0?st.length:0,_e=S.get(B),Xt=w.state.lights;if(tt===!0&&(ie===!0||v!==H)){const ot=v===H&&B.id===V;ve.setState(B,v,ot)}let Ye=!1;B.version===_e.__version?(_e.needsLights&&_e.lightsStateVersion!==Xt.state.version||_e.outputColorSpace!==fe||k.isBatchedMesh&&_e.batching===!1||!k.isBatchedMesh&&_e.batching===!0||k.isBatchedMesh&&_e.batchingColor===!0&&k.colorTexture===null||k.isBatchedMesh&&_e.batchingColor===!1&&k.colorTexture!==null||k.isInstancedMesh&&_e.instancing===!1||!k.isInstancedMesh&&_e.instancing===!0||k.isSkinnedMesh&&_e.skinning===!1||!k.isSkinnedMesh&&_e.skinning===!0||k.isInstancedMesh&&_e.instancingColor===!0&&k.instanceColor===null||k.isInstancedMesh&&_e.instancingColor===!1&&k.instanceColor!==null||k.isInstancedMesh&&_e.instancingMorph===!0&&k.morphTexture===null||k.isInstancedMesh&&_e.instancingMorph===!1&&k.morphTexture!==null||_e.envMap!==Ee||B.fog===!0&&_e.fog!==pe||_e.numClippingPlanes!==void 0&&(_e.numClippingPlanes!==ve.numPlanes||_e.numIntersection!==ve.numIntersection)||_e.vertexAlphas!==Oe||_e.vertexTangents!==Ve||_e.morphTargets!==we||_e.morphNormals!==nt||_e.morphColors!==pt||_e.toneMapping!==dt||_e.morphTargetsCount!==Dt||!!_e.lightProbeGrid!=w.state.lightProbeGridArray.length>0)&&(Ye=!0):(Ye=!0,_e.__version=B.version);let Jt=_e.currentProgram;Ye===!0&&(Jt=$s(B,I,k),z&&B.isNodeMaterial&&z.onUpdateProgram(B,Jt,_e));let mi=!1,Vi=!1,In=!1;const rt=Jt.getUniforms(),mt=_e.uniforms;if(de.useProgram(Jt.program)&&(mi=!0,Vi=!0,In=!0),B.id!==V&&(V=B.id,Vi=!0),_e.needsLights){const ot=Jg(w.state.lightProbeGridArray,k);_e.lightProbeGrid!==ot&&(_e.lightProbeGrid=ot,Vi=!0)}if(mi||H!==v){de.buffers.depth.getReversed()&&v.reversedDepth!==!0&&(v._reversedDepth=!0,v.updateProjectionMatrix()),rt.setValue(P,"projectionMatrix",v.projectionMatrix),rt.setValue(P,"viewMatrix",v.matrixWorldInverse);const Hi=rt.map.cameraPosition;Hi!==void 0&&Hi.setValue(P,Ke.setFromMatrixPosition(v.matrixWorld)),at.logarithmicDepthBuffer&&rt.setValue(P,"logDepthBufFC",2/(Math.log(v.far+1)/Math.LN2)),(B.isMeshPhongMaterial||B.isMeshToonMaterial||B.isMeshLambertMaterial||B.isMeshBasicMaterial||B.isMeshStandardMaterial||B.isShaderMaterial)&&rt.setValue(P,"isOrthographic",v.isOrthographicCamera===!0),H!==v&&(H=v,Vi=!0,In=!0)}if(_e.needsLights&&(Xt.state.directionalShadowMap.length>0&&rt.setValue(P,"directionalShadowMap",Xt.state.directionalShadowMap,g),Xt.state.spotShadowMap.length>0&&rt.setValue(P,"spotShadowMap",Xt.state.spotShadowMap,g),Xt.state.pointShadowMap.length>0&&rt.setValue(P,"pointShadowMap",Xt.state.pointShadowMap,g)),k.isSkinnedMesh){rt.setOptional(P,k,"bindMatrix"),rt.setOptional(P,k,"bindMatrixInverse");const ot=k.skeleton;ot&&(ot.boneTexture===null&&ot.computeBoneTexture(),rt.setValue(P,"boneTexture",ot.boneTexture,g))}k.isBatchedMesh&&(rt.setOptional(P,k,"batchingTexture"),rt.setValue(P,"batchingTexture",k._matricesTexture,g),rt.setOptional(P,k,"batchingIdTexture"),rt.setValue(P,"batchingIdTexture",k._indirectTexture,g),rt.setOptional(P,k,"batchingColorTexture"),k._colorsTexture!==null&&rt.setValue(P,"batchingColorTexture",k._colorsTexture,g));const Gi=G.morphAttributes;if((Gi.position!==void 0||Gi.normal!==void 0||Gi.color!==void 0)&&Ie.update(k,G,Jt),(Vi||_e.receiveShadow!==k.receiveShadow)&&(_e.receiveShadow=k.receiveShadow,rt.setValue(P,"receiveShadow",k.receiveShadow)),(B.isMeshStandardMaterial||B.isMeshLambertMaterial||B.isMeshPhongMaterial)&&B.envMap===null&&I.environment!==null&&(mt.envMapIntensity.value=I.environmentIntensity),mt.dfgLUT!==void 0&&(mt.dfgLUT.value=um()),Vi){if(rt.setValue(P,"toneMappingExposure",U.toneMappingExposure),_e.needsLights&&e_(mt,In),pe&&B.fog===!0&&W.refreshFogUniforms(mt,pe),W.refreshMaterialUniforms(mt,B,De,Ze,w.state.transmissionRenderTarget[v.id]),_e.needsLights&&_e.lightProbeGrid){const ot=_e.lightProbeGrid;mt.probesSH.value=ot.texture,mt.probesMin.value.copy(ot.boundingBox.min),mt.probesMax.value.copy(ot.boundingBox.max),mt.probesResolution.value.copy(ot.resolution)}Ws.upload(P,oc(_e),mt,g)}if(B.isShaderMaterial&&B.uniformsNeedUpdate===!0&&(Ws.upload(P,oc(_e),mt,g),B.uniformsNeedUpdate=!1),B.isSpriteMaterial&&rt.setValue(P,"center",k.center),rt.setValue(P,"modelViewMatrix",k.modelViewMatrix),rt.setValue(P,"normalMatrix",k.normalMatrix),rt.setValue(P,"modelMatrix",k.matrixWorld),B.uniformsGroups!==void 0){const ot=B.uniformsGroups;for(let Hi=0,Nn=ot.length;Hi<Nn;Hi++){const cc=ot[Hi];q.update(cc,Jt),q.bind(cc,Jt)}}return Jt}function e_(v,I){v.ambientLightColor.needsUpdate=I,v.lightProbe.needsUpdate=I,v.directionalLights.needsUpdate=I,v.directionalLightShadows.needsUpdate=I,v.pointLights.needsUpdate=I,v.pointLightShadows.needsUpdate=I,v.spotLights.needsUpdate=I,v.spotLightShadows.needsUpdate=I,v.rectAreaLights.needsUpdate=I,v.hemisphereLights.needsUpdate=I}function t_(v){return v.isMeshLambertMaterial||v.isMeshToonMaterial||v.isMeshPhongMaterial||v.isMeshStandardMaterial||v.isShadowMaterial||v.isShaderMaterial&&v.lights===!0}this.getActiveCubeFace=function(){return X},this.getActiveMipmapLevel=function(){return j},this.getRenderTarget=function(){return N},this.setRenderTargetTextures=function(v,I,G){const B=S.get(v);B.__autoAllocateDepthBuffer=v.resolveDepthBuffer===!1,B.__autoAllocateDepthBuffer===!1&&(B.__useRenderToTexture=!1),S.get(v.texture).__webglTexture=I,S.get(v.depthTexture).__webglTexture=B.__autoAllocateDepthBuffer?void 0:G,B.__hasExternalTextures=!0},this.setRenderTargetFramebuffer=function(v,I){const G=S.get(v);G.__webglFramebuffer=I,G.__useDefaultFramebuffer=I===void 0};const i_=P.createFramebuffer();this.setRenderTarget=function(v,I=0,G=0){N=v,X=I,j=G;let B=null,k=!1,pe=!1;if(v){const fe=S.get(v);if(fe.__useDefaultFramebuffer!==void 0){de.bindFramebuffer(P.FRAMEBUFFER,fe.__webglFramebuffer),K.copy(v.viewport),ee.copy(v.scissor),ce=v.scissorTest,de.viewport(K),de.scissor(ee),de.setScissorTest(ce),V=-1;return}else if(fe.__webglFramebuffer===void 0)g.setupRenderTarget(v);else if(fe.__hasExternalTextures)g.rebindTextures(v,S.get(v.texture).__webglTexture,S.get(v.depthTexture).__webglTexture);else if(v.depthBuffer){const Oe=v.depthTexture;if(fe.__boundDepthTexture!==Oe){if(Oe!==null&&S.has(Oe)&&(v.width!==Oe.image.width||v.height!==Oe.image.height))throw new Error("WebGLRenderTarget: Attached DepthTexture is initialized to the incorrect size.");g.setupDepthRenderbuffer(v)}}const Se=v.texture;(Se.isData3DTexture||Se.isDataArrayTexture||Se.isCompressedArrayTexture)&&(pe=!0);const Ee=S.get(v).__webglFramebuffer;v.isWebGLCubeRenderTarget?(Array.isArray(Ee[I])?B=Ee[I][G]:B=Ee[I],k=!0):v.samples>0&&g.useMultisampledRTT(v)===!1?B=S.get(v).__webglMultisampledFramebuffer:Array.isArray(Ee)?B=Ee[G]:B=Ee,K.copy(v.viewport),ee.copy(v.scissor),ce=v.scissorTest}else K.copy(te).multiplyScalar(De).floor(),ee.copy(Te).multiplyScalar(De).floor(),ce=Le;if(G!==0&&(B=i_),de.bindFramebuffer(P.FRAMEBUFFER,B)&&de.drawBuffers(v,B),de.viewport(K),de.scissor(ee),de.setScissorTest(ce),k){const fe=S.get(v.texture);P.framebufferTexture2D(P.FRAMEBUFFER,P.COLOR_ATTACHMENT0,P.TEXTURE_CUBE_MAP_POSITIVE_X+I,fe.__webglTexture,G)}else if(pe){const fe=I;for(let Se=0;Se<v.textures.length;Se++){const Ee=S.get(v.textures[Se]);P.framebufferTextureLayer(P.FRAMEBUFFER,P.COLOR_ATTACHMENT0+Se,Ee.__webglTexture,G,fe)}}else if(v!==null&&G!==0){const fe=S.get(v.texture);P.framebufferTexture2D(P.FRAMEBUFFER,P.COLOR_ATTACHMENT0,P.TEXTURE_2D,fe.__webglTexture,G)}V=-1},this.readRenderTargetPixels=function(v,I,G,B,k,pe,xe,fe=0){if(!(v&&v.isWebGLRenderTarget)){je("WebGLRenderer.readRenderTargetPixels: renderTarget is not THREE.WebGLRenderTarget.");return}let Se=S.get(v).__webglFramebuffer;if(v.isWebGLCubeRenderTarget&&xe!==void 0&&(Se=Se[xe]),Se){de.bindFramebuffer(P.FRAMEBUFFER,Se);try{const Ee=v.textures[fe],Oe=Ee.format,Ve=Ee.type;if(v.textures.length>1&&P.readBuffer(P.COLOR_ATTACHMENT0+fe),!at.textureFormatReadable(Oe)){je("WebGLRenderer.readRenderTargetPixels: renderTarget is not in RGBA or implementation defined format.");return}if(!at.textureTypeReadable(Ve)){je("WebGLRenderer.readRenderTargetPixels: renderTarget is not in UnsignedByteType or implementation defined type.");return}I>=0&&I<=v.width-B&&G>=0&&G<=v.height-k&&P.readPixels(I,G,B,k,R.convert(Oe),R.convert(Ve),pe)}finally{const Ee=N!==null?S.get(N).__webglFramebuffer:null;de.bindFramebuffer(P.FRAMEBUFFER,Ee)}}},this.readRenderTargetPixelsAsync=async function(v,I,G,B,k,pe,xe,fe=0){if(!(v&&v.isWebGLRenderTarget))throw new Error("THREE.WebGLRenderer.readRenderTargetPixels: renderTarget is not THREE.WebGLRenderTarget.");let Se=S.get(v).__webglFramebuffer;if(v.isWebGLCubeRenderTarget&&xe!==void 0&&(Se=Se[xe]),Se)if(I>=0&&I<=v.width-B&&G>=0&&G<=v.height-k){de.bindFramebuffer(P.FRAMEBUFFER,Se);const Ee=v.textures[fe],Oe=Ee.format,Ve=Ee.type;if(v.textures.length>1&&P.readBuffer(P.COLOR_ATTACHMENT0+fe),!at.textureFormatReadable(Oe))throw new Error("THREE.WebGLRenderer.readRenderTargetPixelsAsync: renderTarget is not in RGBA or implementation defined format.");if(!at.textureTypeReadable(Ve))throw new Error("THREE.WebGLRenderer.readRenderTargetPixelsAsync: renderTarget is not in UnsignedByteType or implementation defined type.");const we=P.createBuffer();P.bindBuffer(P.PIXEL_PACK_BUFFER,we),P.bufferData(P.PIXEL_PACK_BUFFER,pe.byteLength,P.STREAM_READ),P.readPixels(I,G,B,k,R.convert(Oe),R.convert(Ve),0);const nt=N!==null?S.get(N).__webglFramebuffer:null;de.bindFramebuffer(P.FRAMEBUFFER,nt);const pt=P.fenceSync(P.SYNC_GPU_COMMANDS_COMPLETE,0);return P.flush(),await Xc(P,pt,4),P.bindBuffer(P.PIXEL_PACK_BUFFER,we),P.getBufferSubData(P.PIXEL_PACK_BUFFER,0,pe),P.deleteBuffer(we),P.deleteSync(pt),pe}else throw new Error("THREE.WebGLRenderer.readRenderTargetPixelsAsync: requested read bounds are out of range.")},this.copyFramebufferToTexture=function(v,I=null,G=0){const B=Math.pow(2,-G),k=Math.floor(v.image.width*B),pe=Math.floor(v.image.height*B),xe=I!==null?I.x:0,fe=I!==null?I.y:0;g.setTexture2D(v,0),P.copyTexSubImage2D(P.TEXTURE_2D,G,0,0,xe,fe,k,pe),de.unbindTexture()};const n_=P.createFramebuffer(),s_=P.createFramebuffer();this.copyTextureToTexture=function(v,I,G=null,B=null,k=0,pe=0){let xe,fe,Se,Ee,Oe,Ve,we,nt,pt;const dt=v.isCompressedTexture?v.mipmaps[pe]:v.image;if(G!==null)xe=G.max.x-G.min.x,fe=G.max.y-G.min.y,Se=G.isBox3?G.max.z-G.min.z:1,Ee=G.min.x,Oe=G.min.y,Ve=G.isBox3?G.min.z:0;else{const mt=Math.pow(2,-k);xe=Math.floor(dt.width*mt),fe=Math.floor(dt.height*mt),v.isDataArrayTexture?Se=dt.depth:v.isData3DTexture?Se=Math.floor(dt.depth*mt):Se=1,Ee=0,Oe=0,Ve=0}B!==null?(we=B.x,nt=B.y,pt=B.z):(we=0,nt=0,pt=0);const st=R.convert(I.format),Dt=R.convert(I.type);let _e;I.isData3DTexture?(g.setTexture3D(I,0),_e=P.TEXTURE_3D):I.isDataArrayTexture||I.isCompressedArrayTexture?(g.setTexture2DArray(I,0),_e=P.TEXTURE_2D_ARRAY):(g.setTexture2D(I,0),_e=P.TEXTURE_2D),de.activeTexture(P.TEXTURE0),de.pixelStorei(P.UNPACK_FLIP_Y_WEBGL,I.flipY),de.pixelStorei(P.UNPACK_PREMULTIPLY_ALPHA_WEBGL,I.premultiplyAlpha),de.pixelStorei(P.UNPACK_ALIGNMENT,I.unpackAlignment);const Xt=de.getParameter(P.UNPACK_ROW_LENGTH),Ye=de.getParameter(P.UNPACK_IMAGE_HEIGHT),Jt=de.getParameter(P.UNPACK_SKIP_PIXELS),mi=de.getParameter(P.UNPACK_SKIP_ROWS),Vi=de.getParameter(P.UNPACK_SKIP_IMAGES);de.pixelStorei(P.UNPACK_ROW_LENGTH,dt.width),de.pixelStorei(P.UNPACK_IMAGE_HEIGHT,dt.height),de.pixelStorei(P.UNPACK_SKIP_PIXELS,Ee),de.pixelStorei(P.UNPACK_SKIP_ROWS,Oe),de.pixelStorei(P.UNPACK_SKIP_IMAGES,Ve);const In=v.isDataArrayTexture||v.isData3DTexture,rt=I.isDataArrayTexture||I.isData3DTexture;if(v.isDepthTexture){const mt=S.get(v),Gi=S.get(I),ot=S.get(mt.__renderTarget),Hi=S.get(Gi.__renderTarget);de.bindFramebuffer(P.READ_FRAMEBUFFER,ot.__webglFramebuffer),de.bindFramebuffer(P.DRAW_FRAMEBUFFER,Hi.__webglFramebuffer);for(let Nn=0;Nn<Se;Nn++)In&&(P.framebufferTextureLayer(P.READ_FRAMEBUFFER,P.COLOR_ATTACHMENT0,S.get(v).__webglTexture,k,Ve+Nn),P.framebufferTextureLayer(P.DRAW_FRAMEBUFFER,P.COLOR_ATTACHMENT0,S.get(I).__webglTexture,pe,pt+Nn)),P.blitFramebuffer(Ee,Oe,xe,fe,we,nt,xe,fe,P.DEPTH_BUFFER_BIT,P.NEAREST);de.bindFramebuffer(P.READ_FRAMEBUFFER,null),de.bindFramebuffer(P.DRAW_FRAMEBUFFER,null)}else if(k!==0||v.isRenderTargetTexture||S.has(v)){const mt=S.get(v),Gi=S.get(I);de.bindFramebuffer(P.READ_FRAMEBUFFER,n_),de.bindFramebuffer(P.DRAW_FRAMEBUFFER,s_);for(let ot=0;ot<Se;ot++)In?P.framebufferTextureLayer(P.READ_FRAMEBUFFER,P.COLOR_ATTACHMENT0,mt.__webglTexture,k,Ve+ot):P.framebufferTexture2D(P.READ_FRAMEBUFFER,P.COLOR_ATTACHMENT0,P.TEXTURE_2D,mt.__webglTexture,k),rt?P.framebufferTextureLayer(P.DRAW_FRAMEBUFFER,P.COLOR_ATTACHMENT0,Gi.__webglTexture,pe,pt+ot):P.framebufferTexture2D(P.DRAW_FRAMEBUFFER,P.COLOR_ATTACHMENT0,P.TEXTURE_2D,Gi.__webglTexture,pe),k!==0?P.blitFramebuffer(Ee,Oe,xe,fe,we,nt,xe,fe,P.COLOR_BUFFER_BIT,P.NEAREST):rt?P.copyTexSubImage3D(_e,pe,we,nt,pt+ot,Ee,Oe,xe,fe):P.copyTexSubImage2D(_e,pe,we,nt,Ee,Oe,xe,fe);de.bindFramebuffer(P.READ_FRAMEBUFFER,null),de.bindFramebuffer(P.DRAW_FRAMEBUFFER,null)}else rt?v.isDataTexture||v.isData3DTexture?P.texSubImage3D(_e,pe,we,nt,pt,xe,fe,Se,st,Dt,dt.data):I.isCompressedArrayTexture?P.compressedTexSubImage3D(_e,pe,we,nt,pt,xe,fe,Se,st,dt.data):P.texSubImage3D(_e,pe,we,nt,pt,xe,fe,Se,st,Dt,dt):v.isDataTexture?P.texSubImage2D(P.TEXTURE_2D,pe,we,nt,xe,fe,st,Dt,dt.data):v.isCompressedTexture?P.compressedTexSubImage2D(P.TEXTURE_2D,pe,we,nt,dt.width,dt.height,st,dt.data):P.texSubImage2D(P.TEXTURE_2D,pe,we,nt,xe,fe,st,Dt,dt);de.pixelStorei(P.UNPACK_ROW_LENGTH,Xt),de.pixelStorei(P.UNPACK_IMAGE_HEIGHT,Ye),de.pixelStorei(P.UNPACK_SKIP_PIXELS,Jt),de.pixelStorei(P.UNPACK_SKIP_ROWS,mi),de.pixelStorei(P.UNPACK_SKIP_IMAGES,Vi),pe===0&&I.generateMipmaps&&P.generateMipmap(_e),de.unbindTexture()},this.initRenderTarget=function(v){S.get(v).__webglFramebuffer===void 0&&g.setupRenderTarget(v)},this.initTexture=function(v){v.isCubeTexture?g.setTextureCube(v,0):v.isData3DTexture?g.setTexture3D(v,0):v.isDataArrayTexture||v.isCompressedArrayTexture?g.setTexture2DArray(v,0):g.setTexture2D(v,0),de.unbindTexture()},this.resetState=function(){X=0,j=0,N=null,de.reset(),re.reset()},typeof __THREE_DEVTOOLS__<"u"&&__THREE_DEVTOOLS__.dispatchEvent(new CustomEvent("observe",{detail:this}))}get coordinateSystem(){return oi}get outputColorSpace(){return this._outputColorSpace}set outputColorSpace(e){this._outputColorSpace=e;const t=this.getContext();t.drawingBufferColorSpace=Xe._getDrawingBufferColorSpace(e),t.unpackColorSpace=Xe._getUnpackColorSpace()}}function fm(n,e,t,i,s=null){const r=n[t];if(!r||!e.has(t))return[];const a=r.zone_name;if(!a)return[t];const o=[...e].filter(h=>n[h]?.zone_name===a),l=o.length===i.size&&o.every(h=>i.has(h)),c=a===s&&i.size>0&&[...i].every(h=>n[h]?.zone_name===a);return l||c?[t]:o}function pm(n){if(n.category==="shading")return"shading";if(n.category==="daylighting")return"daylighting";const e=String(n.surface_type??"").toLowerCase();return e==="floor"?"floor":e==="wall"?"wall":e==="roof"||e==="ceiling"?"roof":e==="window"||e==="glassdoor"?"window":e==="door"?"door":"wall"}function Vl(n,e){if(e==null)return!0;const t=Array.isArray(e)?e:[e],i=String(n?.name??"").toLowerCase();return t.some(s=>i.includes(String(s).trim().toLowerCase()))}function mm(n,e,t){if(!n)return["Missing object"];const i=t.filters??{},s=[],r={zone:"zone_name",space:"space_name",surface:"name"};for(const[o,l]of Object.entries(r))i[o]&&!i[o].includes(String(n[l]??"").toLowerCase())&&s.push(o[0].toUpperCase()+o.slice(1)+" filter");Vl(n,i.name)||s.push("Name filter");const a=pm(n);return(i.type&&!i.type.includes(a)||t.componentVisibility?.[a]===!1)&&s.push("Type filter ("+(a==="roof"?"roof / ceiling":a)+")"),t.hidden?.has(e)&&s.push("Hidden manually"),t.isolated&&!t.isolated.has(e)&&s.push("Isolation"),t.diagnosticOnly&&!t.diagnosticObjectSet?.has(e)&&s.push("Diagnostics filter"),s}const Gl={type:"change"},Oa={type:"start"},Hl={type:"end"},js=new jn,Wl=new Ti,gm=Math.cos(70*Jr.DEG2RAD),Mt=new A,Bt=2*Math.PI,et={NONE:-1,ROTATE:0,DOLLY:1,PAN:2,TOUCH_ROTATE:3,TOUCH_PAN:4,TOUCH_DOLLY_PAN:5,TOUCH_DOLLY_ROTATE:6},Ba=1e-6;class _m extends $h{constructor(e,t=null){super(e,t),this.state=et.NONE,this.target=new A,this.cursor=new A,this.minDistance=0,this.maxDistance=1/0,this.minZoom=0,this.maxZoom=1/0,this.minTargetRadius=0,this.maxTargetRadius=1/0,this.minPolarAngle=0,this.maxPolarAngle=Math.PI,this.minAzimuthAngle=-1/0,this.maxAzimuthAngle=1/0,this.enableDamping=!1,this.dampingFactor=.05,this.enableZoom=!0,this.zoomSpeed=1,this.enableRotate=!0,this.rotateSpeed=1,this.keyRotateSpeed=1,this.enablePan=!0,this.panSpeed=1,this.screenSpacePanning=!0,this.keyPanSpeed=7,this.zoomToCursor=!1,this.autoRotate=!1,this.autoRotateSpeed=2,this.keys={LEFT:"ArrowLeft",UP:"ArrowUp",RIGHT:"ArrowRight",BOTTOM:"ArrowDown"},this.mouseButtons={LEFT:nn.ROTATE,MIDDLE:nn.DOLLY,RIGHT:nn.PAN},this.touches={ONE:sn.ROTATE,TWO:sn.DOLLY_PAN},this.target0=this.target.clone(),this.position0=this.object.position.clone(),this.zoom0=this.object.zoom,this._cursorStyle="auto",this._domElementKeyEvents=null,this._lastPosition=new A,this._lastQuaternion=new Pi,this._lastTargetPosition=new A,this._quat=new Pi().setFromUnitVectors(e.up,new A(0,1,0)),this._quatInverse=this._quat.clone().invert(),this._spherical=new ol,this._sphericalDelta=new ol,this._scale=1,this._panOffset=new A,this._rotateStart=new Ne,this._rotateEnd=new Ne,this._rotateDelta=new Ne,this._panStart=new Ne,this._panEnd=new Ne,this._panDelta=new Ne,this._dollyStart=new Ne,this._dollyEnd=new Ne,this._dollyDelta=new Ne,this._dollyDirection=new A,this._mouse=new Ne,this._performCursorZoom=!1,this._pointers=[],this._pointerPositions={},this._controlActive=!1,this._onPointerMove=xm.bind(this),this._onPointerDown=vm.bind(this),this._onPointerUp=bm.bind(this),this._onContextMenu=Am.bind(this),this._onMouseWheel=ym.bind(this),this._onKeyDown=Em.bind(this),this._onTouchStart=wm.bind(this),this._onTouchMove=Tm.bind(this),this._onMouseDown=Sm.bind(this),this._onMouseMove=Mm.bind(this),this._interceptControlDown=Cm.bind(this),this._interceptControlUp=Rm.bind(this),this.domElement!==null&&this.connect(this.domElement),this.update()}set cursorStyle(e){this._cursorStyle=e,e==="grab"?this.domElement.style.cursor="grab":this.domElement.style.cursor="auto"}get cursorStyle(){return this._cursorStyle}connect(e){super.connect(e),this.domElement.addEventListener("pointerdown",this._onPointerDown),this.domElement.addEventListener("pointercancel",this._onPointerUp),this.domElement.addEventListener("contextmenu",this._onContextMenu),this.domElement.addEventListener("wheel",this._onMouseWheel,{passive:!1}),this.domElement.getRootNode().addEventListener("keydown",this._interceptControlDown,{passive:!0,capture:!0}),this.domElement.style.touchAction="none"}disconnect(){this.domElement.removeEventListener("pointerdown",this._onPointerDown),this.domElement.ownerDocument.removeEventListener("pointermove",this._onPointerMove),this.domElement.ownerDocument.removeEventListener("pointerup",this._onPointerUp),this.domElement.removeEventListener("pointercancel",this._onPointerUp),this.domElement.removeEventListener("wheel",this._onMouseWheel),this.domElement.removeEventListener("contextmenu",this._onContextMenu),this.stopListenToKeyEvents(),this.domElement.getRootNode().removeEventListener("keydown",this._interceptControlDown,{capture:!0}),this.domElement.style.touchAction=""}dispose(){this.disconnect()}getPolarAngle(){return this._spherical.phi}getAzimuthalAngle(){return this._spherical.theta}getDistance(){return this.object.position.distanceTo(this.target)}listenToKeyEvents(e){e.addEventListener("keydown",this._onKeyDown),this._domElementKeyEvents=e}stopListenToKeyEvents(){this._domElementKeyEvents!==null&&(this._domElementKeyEvents.removeEventListener("keydown",this._onKeyDown),this._domElementKeyEvents=null)}saveState(){this.target0.copy(this.target),this.position0.copy(this.object.position),this.zoom0=this.object.zoom}reset(){this.target.copy(this.target0),this.object.position.copy(this.position0),this.object.zoom=this.zoom0,this.object.updateProjectionMatrix(),this.dispatchEvent(Gl),this.update(),this.state=et.NONE}pan(e,t){this._pan(e,t),this.update()}dollyIn(e){this._dollyIn(e),this.update()}dollyOut(e){this._dollyOut(e),this.update()}rotateLeft(e){this._rotateLeft(e),this.update()}rotateUp(e){this._rotateUp(e),this.update()}update(e=null){const t=this.object.position;Mt.copy(t).sub(this.target),Mt.applyQuaternion(this._quat),this._spherical.setFromVector3(Mt),this.autoRotate&&this.state===et.NONE&&this._rotateLeft(this._getAutoRotationAngle(e)),this.enableDamping?(this._spherical.theta+=this._sphericalDelta.theta*this.dampingFactor,this._spherical.phi+=this._sphericalDelta.phi*this.dampingFactor):(this._spherical.theta+=this._sphericalDelta.theta,this._spherical.phi+=this._sphericalDelta.phi);let i=this.minAzimuthAngle,s=this.maxAzimuthAngle;isFinite(i)&&isFinite(s)&&(i<-Math.PI?i+=Bt:i>Math.PI&&(i-=Bt),s<-Math.PI?s+=Bt:s>Math.PI&&(s-=Bt),i<=s?this._spherical.theta=Math.max(i,Math.min(s,this._spherical.theta)):this._spherical.theta=this._spherical.theta>(i+s)/2?Math.max(i,this._spherical.theta):Math.min(s,this._spherical.theta)),this._spherical.phi=Math.max(this.minPolarAngle,Math.min(this.maxPolarAngle,this._spherical.phi)),this._spherical.makeSafe(),this.enableDamping===!0?this.target.addScaledVector(this._panOffset,this.dampingFactor):this.target.add(this._panOffset),this.target.sub(this.cursor),this.target.clampLength(this.minTargetRadius,this.maxTargetRadius),this.target.add(this.cursor);let r=!1;if(this.zoomToCursor&&this._performCursorZoom||this.object.isOrthographicCamera)this._spherical.radius=this._clampDistance(this._spherical.radius);else{const a=this._spherical.radius;this._spherical.radius=this._clampDistance(this._spherical.radius*this._scale),r=a!=this._spherical.radius}if(Mt.setFromSpherical(this._spherical),Mt.applyQuaternion(this._quatInverse),t.copy(this.target).add(Mt),this.object.lookAt(this.target),this.enableDamping===!0?(this._sphericalDelta.theta*=1-this.dampingFactor,this._sphericalDelta.phi*=1-this.dampingFactor,this._panOffset.multiplyScalar(1-this.dampingFactor)):(this._sphericalDelta.set(0,0,0),this._panOffset.set(0,0,0)),this.zoomToCursor&&this._performCursorZoom){let a=null;if(this.object.isPerspectiveCamera){const o=Mt.length();a=this._clampDistance(o*this._scale);const l=o-a;this.object.position.addScaledVector(this._dollyDirection,l),this.object.updateMatrixWorld(),r=!!l}else if(this.object.isOrthographicCamera){const o=new A(this._mouse.x,this._mouse.y,0);o.unproject(this.object);const l=this.object.zoom;this.object.zoom=Math.max(this.minZoom,Math.min(this.maxZoom,this.object.zoom/this._scale)),this.object.updateProjectionMatrix(),r=l!==this.object.zoom;const c=new A(this._mouse.x,this._mouse.y,0);c.unproject(this.object),this.object.position.sub(c).add(o),this.object.updateMatrixWorld(),a=Mt.length()}else console.warn("WARNING: OrbitControls.js encountered an unknown camera type - zoom to cursor disabled."),this.zoomToCursor=!1;a!==null&&(this.screenSpacePanning?this.target.set(0,0,-1).transformDirection(this.object.matrix).multiplyScalar(a).add(this.object.position):(js.origin.copy(this.object.position),js.direction.set(0,0,-1).transformDirection(this.object.matrix),Math.abs(this.object.up.dot(js.direction))<gm?this.object.lookAt(this.target):(Wl.setFromNormalAndCoplanarPoint(this.object.up,this.target),js.intersectPlane(Wl,this.target))))}else if(this.object.isOrthographicCamera){const a=this.object.zoom;this.object.zoom=Math.max(this.minZoom,Math.min(this.maxZoom,this.object.zoom/this._scale)),a!==this.object.zoom&&(this.object.updateProjectionMatrix(),r=!0)}return this._scale=1,this._performCursorZoom=!1,r||this._lastPosition.distanceToSquared(this.object.position)>Ba||8*(1-this._lastQuaternion.dot(this.object.quaternion))>Ba||this._lastTargetPosition.distanceToSquared(this.target)>Ba?(this.dispatchEvent(Gl),this._lastPosition.copy(this.object.position),this._lastQuaternion.copy(this.object.quaternion),this._lastTargetPosition.copy(this.target),!0):!1}_getAutoRotationAngle(e){return e!==null?Bt/60*this.autoRotateSpeed*e:Bt/60/60*this.autoRotateSpeed}_getZoomScale(e){const t=Math.abs(e*.01);return Math.pow(.95,this.zoomSpeed*t)}_rotateLeft(e){this._sphericalDelta.theta-=e}_rotateUp(e){this._sphericalDelta.phi-=e}_panLeft(e,t){Mt.setFromMatrixColumn(t,0),Mt.multiplyScalar(-e),this._panOffset.add(Mt)}_panUp(e,t){this.screenSpacePanning===!0?Mt.setFromMatrixColumn(t,1):(Mt.setFromMatrixColumn(t,0),Mt.crossVectors(this.object.up,Mt)),Mt.multiplyScalar(e),this._panOffset.add(Mt)}_pan(e,t){const i=this.domElement;if(this.object.isPerspectiveCamera){const s=this.object.position;Mt.copy(s).sub(this.target);let r=Mt.length();r*=Math.tan(this.object.fov/2*Math.PI/180),this._panLeft(2*e*r/i.clientHeight,this.object.matrix),this._panUp(2*t*r/i.clientHeight,this.object.matrix)}else this.object.isOrthographicCamera?(this._panLeft(e*(this.object.right-this.object.left)/this.object.zoom/i.clientWidth,this.object.matrix),this._panUp(t*(this.object.top-this.object.bottom)/this.object.zoom/i.clientHeight,this.object.matrix)):(console.warn("WARNING: OrbitControls.js encountered an unknown camera type - pan disabled."),this.enablePan=!1)}_dollyOut(e){this.object.isPerspectiveCamera||this.object.isOrthographicCamera?this._scale/=e:(console.warn("WARNING: OrbitControls.js encountered an unknown camera type - dolly/zoom disabled."),this.enableZoom=!1)}_dollyIn(e){this.object.isPerspectiveCamera||this.object.isOrthographicCamera?this._scale*=e:(console.warn("WARNING: OrbitControls.js encountered an unknown camera type - dolly/zoom disabled."),this.enableZoom=!1)}_updateZoomParameters(e,t){if(!this.zoomToCursor)return;this._performCursorZoom=!0;const i=this.domElement.getBoundingClientRect(),s=e-i.left,r=t-i.top,a=i.width,o=i.height;this._mouse.x=s/a*2-1,this._mouse.y=-(r/o)*2+1,this._dollyDirection.set(this._mouse.x,this._mouse.y,1).unproject(this.object).sub(this.object.position).normalize()}_clampDistance(e){return Math.max(this.minDistance,Math.min(this.maxDistance,e))}_handleMouseDownRotate(e){this._rotateStart.set(e.clientX,e.clientY)}_handleMouseDownDolly(e){this._updateZoomParameters(e.clientX,e.clientX),this._dollyStart.set(e.clientX,e.clientY)}_handleMouseDownPan(e){this._panStart.set(e.clientX,e.clientY)}_handleMouseMoveRotate(e){this._rotateEnd.set(e.clientX,e.clientY),this._rotateDelta.subVectors(this._rotateEnd,this._rotateStart).multiplyScalar(this.rotateSpeed);const t=this.domElement;this._rotateLeft(Bt*this._rotateDelta.x/t.clientHeight),this._rotateUp(Bt*this._rotateDelta.y/t.clientHeight),this._rotateStart.copy(this._rotateEnd),this.update()}_handleMouseMoveDolly(e){this._dollyEnd.set(e.clientX,e.clientY),this._dollyDelta.subVectors(this._dollyEnd,this._dollyStart),this._dollyDelta.y>0?this._dollyOut(this._getZoomScale(this._dollyDelta.y)):this._dollyDelta.y<0&&this._dollyIn(this._getZoomScale(this._dollyDelta.y)),this._dollyStart.copy(this._dollyEnd),this.update()}_handleMouseMovePan(e){this._panEnd.set(e.clientX,e.clientY),this._panDelta.subVectors(this._panEnd,this._panStart).multiplyScalar(this.panSpeed),this._pan(this._panDelta.x,this._panDelta.y),this._panStart.copy(this._panEnd),this.update()}_handleMouseWheel(e){this._updateZoomParameters(e.clientX,e.clientY),e.deltaY<0?this._dollyIn(this._getZoomScale(e.deltaY)):e.deltaY>0&&this._dollyOut(this._getZoomScale(e.deltaY)),this.update()}_handleKeyDown(e){let t=!1;switch(e.code){case this.keys.UP:e.ctrlKey||e.metaKey||e.shiftKey?this.enableRotate&&this._rotateUp(Bt*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(0,this.keyPanSpeed),t=!0;break;case this.keys.BOTTOM:e.ctrlKey||e.metaKey||e.shiftKey?this.enableRotate&&this._rotateUp(-Bt*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(0,-this.keyPanSpeed),t=!0;break;case this.keys.LEFT:e.ctrlKey||e.metaKey||e.shiftKey?this.enableRotate&&this._rotateLeft(Bt*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(this.keyPanSpeed,0),t=!0;break;case this.keys.RIGHT:e.ctrlKey||e.metaKey||e.shiftKey?this.enableRotate&&this._rotateLeft(-Bt*this.keyRotateSpeed/this.domElement.clientHeight):this.enablePan&&this._pan(-this.keyPanSpeed,0),t=!0;break}t&&(e.preventDefault(),this.update())}_handleTouchStartRotate(e){if(this._pointers.length===1)this._rotateStart.set(e.pageX,e.pageY);else{const t=this._getSecondPointerPosition(e),i=.5*(e.pageX+t.x),s=.5*(e.pageY+t.y);this._rotateStart.set(i,s)}}_handleTouchStartPan(e){if(this._pointers.length===1)this._panStart.set(e.pageX,e.pageY);else{const t=this._getSecondPointerPosition(e),i=.5*(e.pageX+t.x),s=.5*(e.pageY+t.y);this._panStart.set(i,s)}}_handleTouchStartDolly(e){const t=this._getSecondPointerPosition(e),i=e.pageX-t.x,s=e.pageY-t.y,r=Math.sqrt(i*i+s*s);this._dollyStart.set(0,r)}_handleTouchStartDollyPan(e){this.enableZoom&&this._handleTouchStartDolly(e),this.enablePan&&this._handleTouchStartPan(e)}_handleTouchStartDollyRotate(e){this.enableZoom&&this._handleTouchStartDolly(e),this.enableRotate&&this._handleTouchStartRotate(e)}_handleTouchMoveRotate(e){if(this._pointers.length==1)this._rotateEnd.set(e.pageX,e.pageY);else{const i=this._getSecondPointerPosition(e),s=.5*(e.pageX+i.x),r=.5*(e.pageY+i.y);this._rotateEnd.set(s,r)}this._rotateDelta.subVectors(this._rotateEnd,this._rotateStart).multiplyScalar(this.rotateSpeed);const t=this.domElement;this._rotateLeft(Bt*this._rotateDelta.x/t.clientHeight),this._rotateUp(Bt*this._rotateDelta.y/t.clientHeight),this._rotateStart.copy(this._rotateEnd)}_handleTouchMovePan(e){if(this._pointers.length===1)this._panEnd.set(e.pageX,e.pageY);else{const t=this._getSecondPointerPosition(e),i=.5*(e.pageX+t.x),s=.5*(e.pageY+t.y);this._panEnd.set(i,s)}this._panDelta.subVectors(this._panEnd,this._panStart).multiplyScalar(this.panSpeed),this._pan(this._panDelta.x,this._panDelta.y),this._panStart.copy(this._panEnd)}_handleTouchMoveDolly(e){const t=this._getSecondPointerPosition(e),i=e.pageX-t.x,s=e.pageY-t.y,r=Math.sqrt(i*i+s*s);this._dollyEnd.set(0,r),this._dollyDelta.set(0,Math.pow(this._dollyEnd.y/this._dollyStart.y,this.zoomSpeed)),this._dollyOut(this._dollyDelta.y),this._dollyStart.copy(this._dollyEnd);const a=(e.pageX+t.x)*.5,o=(e.pageY+t.y)*.5;this._updateZoomParameters(a,o)}_handleTouchMoveDollyPan(e){this.enableZoom&&this._handleTouchMoveDolly(e),this.enablePan&&this._handleTouchMovePan(e)}_handleTouchMoveDollyRotate(e){this.enableZoom&&this._handleTouchMoveDolly(e),this.enableRotate&&this._handleTouchMoveRotate(e)}_addPointer(e){this._pointers.push(e.pointerId)}_removePointer(e){delete this._pointerPositions[e.pointerId];for(let t=0;t<this._pointers.length;t++)if(this._pointers[t]==e.pointerId){this._pointers.splice(t,1);return}}_isTrackingPointer(e){for(let t=0;t<this._pointers.length;t++)if(this._pointers[t]==e.pointerId)return!0;return!1}_trackPointer(e){let t=this._pointerPositions[e.pointerId];t===void 0&&(t=new Ne,this._pointerPositions[e.pointerId]=t),t.set(e.pageX,e.pageY)}_getSecondPointerPosition(e){const t=e.pointerId===this._pointers[0]?this._pointers[1]:this._pointers[0];return this._pointerPositions[t]}_customWheelEvent(e){const t=e.deltaMode,i={clientX:e.clientX,clientY:e.clientY,deltaY:e.deltaY};switch(t){case 1:i.deltaY*=16;break;case 2:i.deltaY*=100;break}return e.ctrlKey&&!this._controlActive&&(i.deltaY*=10),i}}function vm(n){this.enabled!==!1&&(this._pointers.length===0&&(this.domElement.setPointerCapture(n.pointerId),this.domElement.ownerDocument.addEventListener("pointermove",this._onPointerMove),this.domElement.ownerDocument.addEventListener("pointerup",this._onPointerUp)),!this._isTrackingPointer(n)&&(this._addPointer(n),n.pointerType==="touch"?this._onTouchStart(n):this._onMouseDown(n),this._cursorStyle==="grab"&&(this.domElement.style.cursor="grabbing")))}function xm(n){this.enabled!==!1&&(n.pointerType==="touch"?this._onTouchMove(n):this._onMouseMove(n))}function bm(n){switch(this._removePointer(n),this._pointers.length){case 0:this.domElement.releasePointerCapture(n.pointerId),this.domElement.ownerDocument.removeEventListener("pointermove",this._onPointerMove),this.domElement.ownerDocument.removeEventListener("pointerup",this._onPointerUp),this.dispatchEvent(Hl),this.state=et.NONE,this._cursorStyle==="grab"&&(this.domElement.style.cursor="grab");break;case 1:const e=this._pointers[0],t=this._pointerPositions[e];this._onTouchStart({pointerId:e,pageX:t.x,pageY:t.y});break}}function Sm(n){let e;switch(n.button){case 0:e=this.mouseButtons.LEFT;break;case 1:e=this.mouseButtons.MIDDLE;break;case 2:e=this.mouseButtons.RIGHT;break;default:e=-1}switch(e){case nn.DOLLY:if(this.enableZoom===!1)return;this._handleMouseDownDolly(n),this.state=et.DOLLY;break;case nn.ROTATE:if(n.ctrlKey||n.metaKey||n.shiftKey){if(this.enablePan===!1)return;this._handleMouseDownPan(n),this.state=et.PAN}else{if(this.enableRotate===!1)return;this._handleMouseDownRotate(n),this.state=et.ROTATE}break;case nn.PAN:if(n.ctrlKey||n.metaKey||n.shiftKey){if(this.enableRotate===!1)return;this._handleMouseDownRotate(n),this.state=et.ROTATE}else{if(this.enablePan===!1)return;this._handleMouseDownPan(n),this.state=et.PAN}break;default:this.state=et.NONE}this.state!==et.NONE&&this.dispatchEvent(Oa)}function Mm(n){switch(this.state){case et.ROTATE:if(this.enableRotate===!1)return;this._handleMouseMoveRotate(n);break;case et.DOLLY:if(this.enableZoom===!1)return;this._handleMouseMoveDolly(n);break;case et.PAN:if(this.enablePan===!1)return;this._handleMouseMovePan(n);break}}function ym(n){this.enabled===!1||this.enableZoom===!1||this.state!==et.NONE||(n.preventDefault(),this.dispatchEvent(Oa),this._handleMouseWheel(this._customWheelEvent(n)),this.dispatchEvent(Hl))}function Em(n){this.enabled!==!1&&this._handleKeyDown(n)}function wm(n){switch(this._trackPointer(n),this._pointers.length){case 1:switch(this.touches.ONE){case sn.ROTATE:if(this.enableRotate===!1)return;this._handleTouchStartRotate(n),this.state=et.TOUCH_ROTATE;break;case sn.PAN:if(this.enablePan===!1)return;this._handleTouchStartPan(n),this.state=et.TOUCH_PAN;break;default:this.state=et.NONE}break;case 2:switch(this.touches.TWO){case sn.DOLLY_PAN:if(this.enableZoom===!1&&this.enablePan===!1)return;this._handleTouchStartDollyPan(n),this.state=et.TOUCH_DOLLY_PAN;break;case sn.DOLLY_ROTATE:if(this.enableZoom===!1&&this.enableRotate===!1)return;this._handleTouchStartDollyRotate(n),this.state=et.TOUCH_DOLLY_ROTATE;break;default:this.state=et.NONE}break;default:this.state=et.NONE}this.state!==et.NONE&&this.dispatchEvent(Oa)}function Tm(n){switch(this._trackPointer(n),this.state){case et.TOUCH_ROTATE:if(this.enableRotate===!1)return;this._handleTouchMoveRotate(n),this.update();break;case et.TOUCH_PAN:if(this.enablePan===!1)return;this._handleTouchMovePan(n),this.update();break;case et.TOUCH_DOLLY_PAN:if(this.enableZoom===!1&&this.enablePan===!1)return;this._handleTouchMoveDollyPan(n),this.update();break;case et.TOUCH_DOLLY_ROTATE:if(this.enableZoom===!1&&this.enableRotate===!1)return;this._handleTouchMoveDollyRotate(n),this.update();break;default:this.state=et.NONE}}function Am(n){this.enabled!==!1&&n.preventDefault()}function Cm(n){n.key==="Control"&&(this._controlActive=!0,this.domElement.getRootNode().addEventListener("keyup",this._interceptControlUp,{passive:!0,capture:!0}))}function Rm(n){n.key==="Control"&&(this._controlActive=!1,this.domElement.getRootNode().removeEventListener("keyup",this._interceptControlUp,{passive:!0,capture:!0}))}function Xl(n){if(!n)return new ArrayBuffer(0);const e=window.atob(n),t=new Uint8Array(e.length);for(let i=0;i<e.length;i+=1)t[i]=e.charCodeAt(i);return t.buffer}function Jn(n){return new Float32Array(Xl(n))}function za(n){return new Int32Array(Xl(n))}const jl={Wall:"#c9a87d",Roof:"#b85f5f",Ceiling:"#d8b4a0",Floor:"#8a9a83",Window:"#55a9d6",GlassDoor:"#60b6dc",Door:"#8c6239",Shading:"#8f9298",Daylighting:"#e63946",Undefined:"#d5d7da"},Pm={Outdoors:"#4f87c5",Ground:"#8b6f47",Surface:"#e1a340",Adiabatic:"#b36bba",OtherSideCoefficients:"#65a96e",OtherSideConditionsModel:"#5e9a91"},Lm=["#440154","#3b528b","#21918c","#5ec962","#fde725"],Dm=["#3b4cc0","#8db0fe","#f7f7f7","#f4987a","#b40426"],Im=["#111827","#6b7280","#f9fafb"],Nm=["#fff7bc","#fec44f","#d95f0e","#7f2704"],Um=["#3b4cc0","#00a6ca","#7ad151","#fde725","#f8961e","#d00000"];function ql(n){return n==="diverging"?Dm:n==="grayscale"?Im:n==="sequential"?Nm:n==="rainbow"?Um:Lm}function ka(n){const e=String(n??"Undefined");let t=2166136261;for(let s=0;s<e.length;s+=1)t^=e.charCodeAt(s),t=Math.imul(t,16777619);const i=Math.abs(t)%360;return new Re(`hsl(${i}, 52%, 58%)`)}function Fm(n,e){const i=Math.max(0,Math.min(1,e))*(n.length-1),s=Math.floor(i),r=Math.min(n.length-1,s+1);return new Re(n[s]).lerp(new Re(n[r]),i-s)}function Om(n,e,t,i="#b8bdc5"){if(!Number.isFinite(n))return new Re(i);const[s,r]=e,a=r===s?.5:(n-s)/(r-s);return Fm(ql(t),a)}function Yl(n,e,t,i,s,r){return e==="result"?Om(t,i,s,r):e==="surface_type"?new Re(jl[n.surface_type]??jl.Undefined):e==="boundary"?new Re(Pm[n.outside_boundary_condition]??"#d5d7da"):e==="construction"?ka(n.construction_name):e==="zone"?ka(n.zone_name):e==="space"?ka(n.space_name):e==="normal"?new Re("#f4f5f7"):new Re("#d5d7da")}function Bm(n){return ql(n).join(", ")}/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const zm={xmlns:"http://www.w3.org/2000/svg",width:24,height:24,viewBox:"0 0 24 24",fill:"none",stroke:"currentColor","stroke-width":2,"stroke-linecap":"round","stroke-linejoin":"round"};/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const $l=([n,e,t])=>{const i=document.createElementNS("http://www.w3.org/2000/svg",n);return Object.keys(e).forEach(s=>{i.setAttribute(s,String(e[s]))}),t?.length&&t.forEach(s=>{const r=$l(s);i.appendChild(r)}),i},km=(n,e={})=>{const i={...zm,...e};return $l(["svg",i,n])};/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Vm=[["path",{d:"M21 8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16Z"}],["path",{d:"m3.3 7 8.7 5 8.7-5"}],["path",{d:"M12 22V12"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Gm=[["path",{d:"M2.97 12.92A2 2 0 0 0 2 14.63v3.24a2 2 0 0 0 .97 1.71l3 1.8a2 2 0 0 0 2.06 0L12 19v-5.5l-5-3-4.03 2.42Z"}],["path",{d:"m7 16.5-4.74-2.85"}],["path",{d:"m7 16.5 5-3"}],["path",{d:"M7 16.5v5.17"}],["path",{d:"M12 13.5V19l3.97 2.38a2 2 0 0 0 2.06 0l3-1.8a2 2 0 0 0 .97-1.71v-3.24a2 2 0 0 0-.97-1.71L17 10.5l-5 3Z"}],["path",{d:"m17 16.5-5-3"}],["path",{d:"m17 16.5 4.74-2.85"}],["path",{d:"M17 16.5v5.17"}],["path",{d:"M7.97 4.42A2 2 0 0 0 7 6.13v4.37l5 3 5-3V6.13a2 2 0 0 0-.97-1.71l-3-1.8a2 2 0 0 0-2.06 0l-3 1.8Z"}],["path",{d:"M12 8 7.26 5.15"}],["path",{d:"m12 8 4.74-2.85"}],["path",{d:"M12 13.5V8"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Hm=[["path",{d:"M13.997 4a2 2 0 0 1 1.76 1.05l.486.9A2 2 0 0 0 18.003 7H20a2 2 0 0 1 2 2v9a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V9a2 2 0 0 1 2-2h1.997a2 2 0 0 0 1.759-1.048l.489-.904A2 2 0 0 1 10.004 4z"}],["circle",{cx:"12",cy:"13",r:"3"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Wm=[["path",{d:"M12 16v5"}],["path",{d:"M16 14.639V21"}],["path",{d:"M20 10.656V21"}],["path",{d:"m22 3-8.646 8.646a.5.5 0 0 1-.708 0L9.354 8.354a.5.5 0 0 0-.707 0L2 15"}],["path",{d:"M4 18.463V21"}],["path",{d:"M8 14.656V21"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Xm=[["path",{d:"M20 6 9 17l-5-5"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const jm=[["path",{d:"m6 9 6 6 6-6"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const qm=[["path",{d:"m15 18-6-6 6-6"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Ym=[["path",{d:"m9 18 6-6-6-6"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const $m=[["path",{d:"m18 15-6-6-6 6"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Zm=[["circle",{cx:"12",cy:"12",r:"10"}],["path",{d:"M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3"}],["path",{d:"M12 17h.01"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Km=[["circle",{cx:"12",cy:"12",r:"10"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Jm=[["circle",{cx:"12",cy:"12",r:"10"}],["path",{d:"M12 6v6h4"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Qm=[["ellipse",{cx:"12",cy:"5",rx:"9",ry:"3"}],["path",{d:"M3 5V19A9 3 0 0 0 21 19V5"}],["path",{d:"M3 12A9 3 0 0 0 21 12"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const eg=[["path",{d:"m15 15 6 6"}],["path",{d:"m15 9 6-6"}],["path",{d:"M21 16v5h-5"}],["path",{d:"M21 8V3h-5"}],["path",{d:"M3 16v5h5"}],["path",{d:"m3 21 6-6"}],["path",{d:"M3 8V3h5"}],["path",{d:"M9 9 3 3"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const tg=[["path",{d:"M10.733 5.076a10.744 10.744 0 0 1 11.205 6.575 1 1 0 0 1 0 .696 10.747 10.747 0 0 1-1.444 2.49"}],["path",{d:"M14.084 14.158a3 3 0 0 1-4.242-4.242"}],["path",{d:"M17.479 17.499a10.75 10.75 0 0 1-15.417-5.151 1 1 0 0 1 0-.696 10.75 10.75 0 0 1 4.446-5.143"}],["path",{d:"m2 2 20 20"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const ig=[["path",{d:"M2.062 12.348a1 1 0 0 1 0-.696 10.75 10.75 0 0 1 19.876 0 1 1 0 0 1 0 .696 10.75 10.75 0 0 1-19.876 0"}],["circle",{cx:"12",cy:"12",r:"3"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const ng=[["circle",{cx:"12",cy:"12",r:"3"}],["path",{d:"M3 7V5a2 2 0 0 1 2-2h2"}],["path",{d:"M17 3h2a2 2 0 0 1 2 2v2"}],["path",{d:"M21 17v2a2 2 0 0 1-2 2h-2"}],["path",{d:"M7 21H5a2 2 0 0 1-2-2v-2"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const sg=[["path",{d:"M20 10a1 1 0 0 0 1-1V6a1 1 0 0 0-1-1h-2.5a1 1 0 0 1-.8-.4l-.9-1.2A1 1 0 0 0 15 3h-2a1 1 0 0 0-1 1v5a1 1 0 0 0 1 1Z"}],["path",{d:"M20 21a1 1 0 0 0 1-1v-3a1 1 0 0 0-1-1h-2.9a1 1 0 0 1-.88-.55l-.42-.85a1 1 0 0 0-.92-.6H13a1 1 0 0 0-1 1v5a1 1 0 0 0 1 1Z"}],["path",{d:"M3 5a2 2 0 0 0 2 2h3"}],["path",{d:"M3 3v13a2 2 0 0 0 2 2h3"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const rg=[["path",{d:"M12 3v18"}],["path",{d:"M3 12h18"}],["rect",{x:"3",y:"3",width:"18",height:"18",rx:"2"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const ag=[["rect",{width:"18",height:"18",x:"3",y:"3",rx:"2"}],["path",{d:"M3 9h18"}],["path",{d:"M3 15h18"}],["path",{d:"M9 3v18"}],["path",{d:"M15 3v18"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const og=[["path",{d:"M12.83 2.18a2 2 0 0 0-1.66 0L2.6 6.08a1 1 0 0 0 0 1.83l8.58 3.91a2 2 0 0 0 1.66 0l8.58-3.9a1 1 0 0 0 0-1.83z"}],["path",{d:"M2 12a1 1 0 0 0 .58.91l8.6 3.91a2 2 0 0 0 1.65 0l8.58-3.9A1 1 0 0 0 22 12"}],["path",{d:"M2 17a1 1 0 0 0 .58.91l8.6 3.91a2 2 0 0 0 1.65 0l8.58-3.9A1 1 0 0 0 22 17"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const lg=[["path",{d:"M2 5h20"}],["path",{d:"M6 12h12"}],["path",{d:"M9 19h6"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const cg=[["path",{d:"M4.037 4.688a.495.495 0 0 1 .651-.651l16 6.5a.5.5 0 0 1-.063.947l-6.124 1.58a2 2 0 0 0-1.438 1.435l-1.579 6.126a.5.5 0 0 1-.947.063z"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const hg=[["path",{d:"M5 3v16h16"}],["path",{d:"m5 19 6-6"}],["path",{d:"m2 6 3-3 3 3"}],["path",{d:"m18 16 3 3-3 3"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const ug=[["path",{d:"M20.341 6.484A10 10 0 0 1 10.266 21.85"}],["path",{d:"M3.659 17.516A10 10 0 0 1 13.74 2.152"}],["circle",{cx:"12",cy:"12",r:"3"}],["circle",{cx:"19",cy:"5",r:"2"}],["circle",{cx:"5",cy:"19",r:"2"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const dg=[["path",{d:"m14.622 17.897-10.68-2.913"}],["path",{d:"M18.376 2.622a1 1 0 1 1 3.002 3.002L17.36 9.643a.5.5 0 0 0 0 .707l.944.944a2.41 2.41 0 0 1 0 3.408l-.944.944a.5.5 0 0 1-.707 0L8.354 7.348a.5.5 0 0 1 0-.707l.944-.944a2.41 2.41 0 0 1 3.408 0l.944.944a.5.5 0 0 0 .707 0z"}],["path",{d:"M9 8c-1.804 2.71-3.97 3.46-6.583 3.948a.507.507 0 0 0-.302.819l7.32 8.883a1 1 0 0 0 1.185.204C12.735 20.405 16 16.792 16 15"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const fg=[["path",{d:"M12 22a1 1 0 0 1 0-20 10 9 0 0 1 10 9 5 5 0 0 1-5 5h-2.25a1.75 1.75 0 0 0-1.4 2.8l.3.4a1.75 1.75 0 0 1-1.4 2.8z"}],["circle",{cx:"13.5",cy:"6.5",r:".5",fill:"currentColor"}],["circle",{cx:"17.5",cy:"10.5",r:".5",fill:"currentColor"}],["circle",{cx:"6.5",cy:"12.5",r:".5",fill:"currentColor"}],["circle",{cx:"8.5",cy:"7.5",r:".5",fill:"currentColor"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const pg=[["rect",{width:"18",height:"18",x:"3",y:"3",rx:"2"}],["path",{d:"M15 3v18"}],["path",{d:"m8 9 3 3-3 3"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Zl=[["rect",{width:"18",height:"18",x:"3",y:"3",rx:"2"}],["path",{d:"M15 3v18"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const mg=[["rect",{width:"18",height:"18",x:"3",y:"3",rx:"2"}],["path",{d:"M3 9h18"}],["path",{d:"M9 21V9"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const gg=[["rect",{x:"14",y:"3",width:"5",height:"18",rx:"1"}],["rect",{x:"5",y:"3",width:"5",height:"18",rx:"1"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const _g=[["path",{d:"M5 5a2 2 0 0 1 3.008-1.728l11.997 6.998a2 2 0 0 1 .003 3.458l-12 7A2 2 0 0 1 5 19z"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const vg=[["path",{d:"M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8"}],["path",{d:"M3 3v5h5"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const xg=[["path",{d:"M21.3 15.3a2.4 2.4 0 0 1 0 3.4l-2.6 2.6a2.4 2.4 0 0 1-3.4 0L2.7 8.7a2.41 2.41 0 0 1 0-3.4l2.6-2.6a2.41 2.41 0 0 1 3.4 0Z"}],["path",{d:"m14.5 12.5 2-2"}],["path",{d:"m11.5 9.5 2-2"}],["path",{d:"m8.5 6.5 2-2"}],["path",{d:"m17.5 15.5 2-2"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const bg=[["path",{d:"M15.2 3a2 2 0 0 1 1.4.6l3.8 3.8a2 2 0 0 1 .6 1.4V19a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2z"}],["path",{d:"M17 21v-7a1 1 0 0 0-1-1H8a1 1 0 0 0-1 1v7"}],["path",{d:"M7 3v4a1 1 0 0 0 1 1h7"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Sg=[["path",{d:"M3 7V5a2 2 0 0 1 2-2h2"}],["path",{d:"M17 3h2a2 2 0 0 1 2 2v2"}],["path",{d:"M21 17v2a2 2 0 0 1-2 2h-2"}],["path",{d:"M7 21H5a2 2 0 0 1-2-2v-2"}],["circle",{cx:"12",cy:"12",r:"1"}],["path",{d:"M18.944 12.33a1 1 0 0 0 0-.66 7.5 7.5 0 0 0-13.888 0 1 1 0 0 0 0 .66 7.5 7.5 0 0 0 13.888 0"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Mg=[["path",{d:"M3 7V5a2 2 0 0 1 2-2h2"}],["path",{d:"M17 3h2a2 2 0 0 1 2 2v2"}],["path",{d:"M21 17v2a2 2 0 0 1-2 2h-2"}],["path",{d:"M7 21H5a2 2 0 0 1-2-2v-2"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const yg=[["path",{d:"m21 21-4.34-4.34"}],["circle",{cx:"11",cy:"11",r:"8"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Eg=[["path",{d:"M14 17H5"}],["path",{d:"M19 7h-9"}],["circle",{cx:"17",cy:"17",r:"3"}],["circle",{cx:"7",cy:"7",r:"3"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const wg=[["path",{d:"M10 5H3"}],["path",{d:"M12 19H3"}],["path",{d:"M14 3v4"}],["path",{d:"M16 17v4"}],["path",{d:"M21 12h-9"}],["path",{d:"M21 19h-5"}],["path",{d:"M21 5h-7"}],["path",{d:"M8 10v4"}],["path",{d:"M8 12H3"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Tg=[["path",{d:"M10 11v6"}],["path",{d:"M14 11v6"}],["path",{d:"M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6"}],["path",{d:"M3 6h18"}],["path",{d:"M8 6V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Ag=[["path",{d:"m21.73 18-8-14a2 2 0 0 0-3.48 0l-8 14A2 2 0 0 0 4 21h16a2 2 0 0 0 1.73-3"}],["path",{d:"M12 9v4"}],["path",{d:"M12 17h.01"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Cg=[["path",{d:"M9 14 4 9l5-5"}],["path",{d:"M4 9h10.5a5.5 5.5 0 0 1 5.5 5.5a5.5 5.5 0 0 1-5.5 5.5H11"}]];/**
 * @license lucide v1.59.0 - ISC
 *
 * This source code is licensed under the ISC license.
 * See the LICENSE file in the root directory of this source tree.
 */const Rg={object:Vm,snapshot:Hm,results:Wm,previous:qm,next:Ym,connection:Km,hide:tg,isolate:ng,ground:rg,wireframe:ag,layers:og,filters:lg,axes:hg,panel:Zl,close:pg,pause:gg,play:_g,reset:vg,measure:xg,save:bg,fit:Mg,xray:Sg,search:yg,display:Eg,properties:wg,delete:Tg,undo:Cg,clear:[["path",{d:"M18 6 6 18"}],["path",{d:"m6 6 12 12"}]],eye:ig,tree:sg,select:cg,components:Gm,colors:fg,appearance:dg,camera:ug,data:Qm,time:Jm,timeline:Zl,explode:eg,diagnostics:Ag,collapse:$m,expand:jm,controls:mg,help:Zm,check:Xm};function Zt(n){if(["focus_model","section","glass","timeline"].includes(n)){const e=document.createElementNS("http://www.w3.org/2000/svg","svg");e.setAttribute("viewBox","0 0 24 24"),e.setAttribute("width","18"),e.setAttribute("height","18"),e.setAttribute("fill","none"),e.setAttribute("stroke","currentColor"),e.setAttribute("stroke-width","1.7"),e.setAttribute("stroke-linecap","round"),e.setAttribute("stroke-linejoin","round"),e.setAttribute("aria-hidden","true"),e.setAttribute("focusable","false"),e.classList.add("eplusr-viewer__icon");const t={focus_model:["M3 8V3h5m8 0h5v5M3 16v5h5m8 0h5v-5","m12 7 5 3v5l-5 3-5-3v-5Zm-5 3 5 3 5-3m-5 3v5"],section:["m12 3 8 4v10l-8 4-8-4V7Zm-8 4 8 4 8-4m-8 4v10","M2 12h20"],glass:["M4 3h16v18H4Zm8 0v18M4 11h16","m7 6 2-2m6 14 2-2"],timeline:["M3 4h18v16H3Zm0 11h18M7 14v3m5-3v3m5-3v3"]};for(const i of t[n]){const s=document.createElementNS(e.namespaceURI,"path");s.setAttribute("d",i),e.append(s)}return e}return km(Rg[n],{width:16,height:16,"stroke-width":1.7,"aria-hidden":"true",focusable:"false",class:"eplusr-viewer__icon"})}function Kt(n,e,t,i=!1){const s=document.createElement("span");s.className="eplusr-viewer__button-label",s.textContent=t,n.replaceChildren(Zt(e),s),n.classList.add("eplusr-viewer__icon-button"),n.classList.toggle("eplusr-viewer__icon-only",i),n.setAttribute("aria-label",t),n.dataset.hint=t,n.removeAttribute("title")}class Pg{constructor(e,t){this.root=e,this.tip=document.createElement("div"),this.tip.id=t,this.tip.className="eplusr-viewer__command-hint",this.tip.setAttribute("role","tooltip"),this.tip.hidden=!0,e.append(this.tip),this.listeners={pointerover:i=>{const s=i.target.closest("[data-hint]");s&&s!==this.target&&this.schedule(s,350),this.tip.contains(i.target)&&window.clearTimeout(this.leaveTimer)},pointerout:i=>{this.target?.contains(i.relatedTarget)||this.tip.contains(i.relatedTarget)||this.target?.contains(document.activeElement)&&document.activeElement.matches(":focus-visible")||(this.leaveTimer=window.setTimeout(()=>this.hide(),120))},focusin:i=>{const s=i.target.closest("[data-hint]");s&&s.matches(":focus-visible")&&this.schedule(s,0)},focusout:()=>this.hide(),pointerdown:()=>this.hide(),scroll:()=>this.hide(),keydown:i=>{i.key!=="Escape"||this.tip.hidden||(i.preventDefault(),i.stopImmediatePropagation(),this.hide())}};for(const[i,s]of Object.entries(this.listeners))e.addEventListener(i,s,!0)}schedule(e,t){this.hide(),this.target=e,this.timer=window.setTimeout(()=>this.show(e),t)}show(e){if(!e.isConnected||!e.getClientRects().length||e!==this.target)return;this.tip.textContent=e.dataset.hint,this.tip.hidden=!1;const t=(e.getAttribute("aria-describedby")??"").split(/\s+/).filter(Boolean);t.includes(this.tip.id)||t.push(this.tip.id),e.setAttribute("aria-describedby",t.join(" "));const i=e.getBoundingClientRect(),s=this.root.getBoundingClientRect(),r=this.tip.offsetWidth,a=this.tip.offsetHeight;this.tip.style.left=Math.max(5,Math.min(s.width-r-5,i.left-s.left+(i.width-r)/2))+"px",this.tip.style.top=Math.max(5,Math.min(s.height-a-5,i.bottom-s.top+8+a<=s.height?i.bottom-s.top+8:i.top-s.top-a-8))+"px"}hide(){if(window.clearTimeout(this.timer),window.clearTimeout(this.leaveTimer),this.target){const e=(this.target.getAttribute("aria-describedby")??"").split(/\s+/).filter(t=>t&&t!==this.tip.id);e.length?this.target.setAttribute("aria-describedby",e.join(" ")):this.target.removeAttribute("aria-describedby")}this.target=null,this.tip.hidden=!0}dispose(){this.hide();for(const[e,t]of Object.entries(this.listeners))this.root.removeEventListener(e,t,!0);this.tip.remove()}}const Lg="data:image/svg+xml,%3csvg%20xmlns='http://www.w3.org/2000/svg'%20xmlns:xlink='http://www.w3.org/1999/xlink'%20preserveAspectRatio='xMidYMid'%20width='724'%20height='561'%20viewBox='0%200%20724%20561'%3e%3cdefs%3e%3clinearGradient%20id='gradientFill-1'%20x1='0'%20x2='1'%20y1='0'%20y2='1'%20gradientUnits='objectBoundingBox'%20spreadMethod='pad'%3e%3cstop%20offset='0'%20stop-color='rgb(203,206,208)'%20stop-opacity='1'/%3e%3cstop%20offset='1'%20stop-color='rgb(132,131,139)'%20stop-opacity='1'/%3e%3c/linearGradient%3e%3clinearGradient%20id='gradientFill-2'%20x1='0'%20x2='1'%20y1='0'%20y2='1'%20gradientUnits='objectBoundingBox'%20spreadMethod='pad'%3e%3cstop%20offset='0'%20stop-color='rgb(39,109,195)'%20stop-opacity='1'/%3e%3cstop%20offset='1'%20stop-color='rgb(22,92,170)'%20stop-opacity='1'/%3e%3c/linearGradient%3e%3c/defs%3e%3cpath%20d='M361.453,485.937%20C162.329,485.937%200.906,377.828%200.906,244.469%20C0.906,111.109%20162.329,3.000%20361.453,3.000%20C560.578,3.000%20722.000,111.109%20722.000,244.469%20C722.000,377.828%20560.578,485.937%20361.453,485.937%20ZM416.641,97.406%20C265.289,97.406%20142.594,171.314%20142.594,262.484%20C142.594,353.654%20265.289,427.562%20416.641,427.562%20C567.992,427.562%20679.687,377.033%20679.687,262.484%20C679.687,147.971%20567.992,97.406%20416.641,97.406%20Z'%20fill='url(%23gradientFill-1)'%20fill-rule='evenodd'/%3e%3cpath%20d='M550.000,377.000%20C550.000,377.000%20571.822,383.585%20584.500,390.000%20C588.899,392.226%20596.510,396.668%20602.000,402.500%20C607.378,408.212%20610.000,414.000%20610.000,414.000%20L696.000,559.000%20L557.000,559.062%20L492.000,437.000%20C492.000,437.000%20478.690,414.131%20470.500,407.500%20C463.668,401.969%20460.755,400.000%20454.000,400.000%20C449.298,400.000%20420.974,400.000%20420.974,400.000%20L421.000,558.974%20L298.000,559.026%20L298.000,152.938%20L545.000,152.938%20C545.000,152.938%20657.500,154.967%20657.500,262.000%20C657.500,369.033%20550.000,377.000%20550.000,377.000%20ZM496.500,241.024%20L422.037,240.976%20L422.000,310.026%20L496.500,310.002%20C496.500,310.002%20531.000,309.895%20531.000,274.877%20C531.000,239.155%20496.500,241.024%20496.500,241.024%20Z'%20fill='url(%23gradientFill-2)'%20fill-rule='evenodd'/%3e%3c/svg%3e";class Dg{constructor(){this.root=document.createElement("span"),this.root.className="eplusr-viewer__connection",this.root.setAttribute("role","status"),this.root.tabIndex=0;const e=document.createElement("img");e.src=Lg,e.alt="",e.setAttribute("aria-hidden","true"),e.width=24,e.height=19,this.text=document.createElement("span"),this.text.className="eplusr-viewer__sr-only",this.root.append(e,this.text),this.set("Static view · not connected to R","static")}set(e,t){this.root.dataset.state=t,this.root.dataset.hint=e,this.text.textContent=e}}function zt(n,e="",t=""){const i=document.createElement(n);return i.className=e,i.textContent=t,i}class Ig{constructor(e,t){this.viewer=e,this.workspace=t,this.nodes=[],this.expanded=new Set,this.root=zt("aside","eplusr-viewer__model-browser"),this.root.id=e.panel.id+"-model-browser",this.root.setAttribute("aria-label","Model browser");const i=zt("div","eplusr-viewer__pane-header"),s=zt("h2","","Model browser"),r=t.button("Close model browser","clear",()=>t.showBrowser(!1),!0);i.append(s,r);const a=zt("div","eplusr-viewer__tree-search");this.search=zt("input"),this.search.type="search",this.search.placeholder="Find in model tree",this.search.setAttribute("aria-label","Find in model tree");const o=t.button("Clear tree search","clear",()=>{this.search.value="",this.filter(),this.search.focus()},!0);a.append(this.search,o),this.status=zt("p","eplusr-viewer__tree-status"),this.status.setAttribute("role","status"),this.tree=zt("ul","eplusr-viewer__tree"),this.tree.setAttribute("role","tree"),this.tree.setAttribute("aria-label","Zones and surfaces"),this.tree.setAttribute("aria-multiselectable","true"),this.root.append(i,a,this.status,this.tree),this.search.addEventListener("input",()=>this.filter()),this.search.addEventListener("keydown",l=>{if(l.key!=="Enter")return;l.preventDefault(),l.stopPropagation();const c=this.visibleNodes().filter(d=>d.key.startsWith("object:")),h=c.find(d=>d.indices.some(u=>this.viewer.isObjectVisible(u)))??c[0];h&&this.focus(h)}),this.root.addEventListener("keydown",l=>{l.key==="Escape"&&!l.defaultPrevented&&(l.preventDefault(),t.showBrowser(!1)),l.stopPropagation()})}build(){this.nodes=[],this.tree.replaceChildren();const e=new Map;this.viewer.objects.forEach((t,i)=>{const s=t.zone_name||"Unassigned objects";e.has(s)||e.set(s,new Map);const r=t.space_name||"";e.get(s).has(r)||e.get(s).set(r,[]),e.get(s).get(r).push(i)});for(const[t,i]of e){const s=[...i.values()].flat(),r=this.add(t,"zone:"+t,s,null,this.tree,"Zone");for(const[a,o]of i){const l=a?this.add(a,"space:"+t+":"+a,o,r,r.children,"Space"):r;for(const c of o){const h=this.viewer.objects[c],d=h.name?.startsWith(t+"_")?h.name.slice(t.length+1):h.name;this.add(d||"Object "+(c+1),"object:"+(h.id??c),[c],l,l.children,h.surface_type||h.class_name||"Surface")}}}this.filter(),this.sync()}add(e,t,i,s,r,a){const o={key:t,indices:i,parent:s,label:e,kind:a,level:(s?.level??0)+1};o.li=zt("li"),o.li.setAttribute("role","none"),o.row=zt("div","eplusr-viewer__tree-row"),o.row.setAttribute("role","treeitem"),o.row.tabIndex=-1,o.row.setAttribute("aria-level",String(o.level)),o.sourceName=i.length===1?this.viewer.objects[i[0]].name:e,o.row.title=o.sourceName,o.row.setAttribute("aria-label",a+": "+o.row.title),o.twist=this.workspace.button("Expand "+e,"next",()=>this.expand(o,!this.expanded.has(t)),!0),o.twist.tabIndex=-1,o.twist.addEventListener("click",h=>h.stopPropagation());const l=zt("span","eplusr-viewer__tree-name",e.replaceAll("_"," "));l.prepend(Zt(t.startsWith("object:")?"object":"layers")),o.count=zt("span","eplusr-viewer__tree-count"),o.eye=this.workspace.button("Hide "+e,"eye",()=>this.visibility(o),!0),o.eye.tabIndex=-1,o.eye.addEventListener("click",h=>h.stopPropagation());const c=zt("span","eplusr-viewer__tree-label");return o.reason=zt("span","eplusr-viewer__tree-reason"),o.reason.id=this.viewer.panel.id+"-tree-reason-"+this.nodes.length,o.reason.hidden=!0,c.append(l,o.reason),o.row.append(o.twist,c,o.count,o.eye),o.children=zt("ul"),o.children.setAttribute("role","group"),o.li.append(o.row,o.children),r.append(o.li),o.row.addEventListener("click",h=>this.select(o,h)),o.row.addEventListener("focus",()=>this.focus(o,!1)),o.row.addEventListener("keydown",h=>this.keydown(o,h)),this.nodes.push(o),o}filter(){const e=this.search.value.trim().toLowerCase();this.tree.dataset.searching=String(!!e);for(const i of[...this.nodes].reverse()){const s=i.key.startsWith("object:")?this.viewer.objects[i.indices[0]]:null,r=[i.label,i.kind,i.sourceName,s?.construction_name,s?.class].join(" ").toLowerCase().includes(e);i.match=!e||r||this.ancestors(i).some(o=>o.label.toLowerCase().includes(e))||[...i.children.children].some(o=>!o.hidden),i.li.hidden=!i.match;const a=i.children.children.length>0;i.twist.hidden=!a,i.children.hidden=!a||!(e?i.match:this.expanded.has(i.key)),a&&i.row.setAttribute("aria-expanded",String(!i.children.hidden))}this.updateSearchSummary();const t=this.visibleNodes();this.nodes.forEach(i=>{i.row.tabIndex=i===t[0]?0:-1})}updateSearchSummary(){const e=this.search.value.trim(),t=this.nodes.filter(s=>s.key.startsWith("object:")&&s.match),i=t.filter(s=>this.viewer.isObjectVisible(s.indices[0])).length;this.status.textContent=e?t.length+" matching surfaces · "+i+" visible":this.viewer.objects.length+" surfaces · "+this.tree.children.length+" groups"}expand(e,t){e.children.children.length&&(t?this.expanded.add(e.key):this.expanded.delete(e.key),e.children.hidden=!t,e.row.setAttribute("aria-expanded",String(t)),e.twist.setAttribute("aria-label",(t?"Collapse ":"Expand ")+e.label),this.focus(e))}select(e,t){let i=e.indices.filter(a=>this.viewer.isObjectVisible(a));if(t.shiftKey&&this.anchor&&e.key.startsWith("object:")&&this.anchor.key.startsWith("object:")){const a=this.visibleNodes().filter(c=>c.key.startsWith("object:")),o=a.indexOf(this.anchor),l=a.indexOf(e);o>=0&&l>=0&&(i=a.slice(Math.min(o,l),Math.max(o,l)+1).flatMap(c=>c.indices).filter(c=>this.viewer.isObjectVisible(c)))}if(!i.length){const a=[...new Set(e.indices.flatMap(o=>this.viewer.visibilityReasons(o)))];this.status.textContent="Hidden · "+a.join(" · ")+". Adjust the matching visibility filter to locate.",this.focus(e);return}const s=t.ctrlKey||t.metaKey||t.shiftKey;s||this.viewer.selected.clear();const r=s&&!t.shiftKey&&i.every(a=>this.viewer.selected.has(a));i.forEach(a=>r?this.viewer.selected.delete(a):this.viewer.selected.add(a)),t.shiftKey||(this.anchor=e),this.viewer.updateColors(),s||this.viewer.fitView(new Set(i)),this.viewer.emitSelection(),this.focus(e)}visibility(e){const t=e.indices.some(i=>this.viewer.isObjectVisible(i));e.indices.forEach(i=>t?this.viewer.hidden.add(i):this.viewer.hidden.delete(i)),this.viewer.applyVisibilityChange(),!t&&!e.indices.some(i=>this.viewer.isObjectVisible(i))&&(this.status.textContent="Still hidden by a filter or isolation. Clear filters to reveal.")}visibleNodes(){return this.nodes.filter(e=>!e.li.hidden&&!this.ancestors(e).some(t=>t.li.hidden||t.children.hidden))}ancestors(e){const t=[];for(let i=e.parent;i;i=i.parent)t.push(i);return t}focus(e,t=!0){this.nodes.forEach(i=>{i.row.tabIndex=i===e?0:-1}),t&&(e.row.focus(),e.row.scrollIntoView({block:"nearest",inline:"nearest"}))}keydown(e,t){const i=this.visibleNodes(),s=i.indexOf(e);let r;if(t.key==="ArrowDown")r=i[Math.min(i.length-1,s+1)];else if(t.key==="ArrowUp")r=i[Math.max(0,s-1)];else if(t.key==="Home")r=i[0];else if(t.key==="End")r=i.at(-1);else if(t.key==="ArrowRight")e.children.hidden?this.expand(e,!0):r=i[s+1]?.parent===e?i[s+1]:e;else if(t.key==="ArrowLeft")e.children.children.length&&!e.children.hidden?this.expand(e,!1):r=e.parent;else if(t.key==="Enter"||t.key===" ")this.select(e,t);else if(t.key.toLowerCase()==="h")this.visibility(e);else return;t.preventDefault(),t.stopPropagation(),r&&this.focus(r)}sync(){for(const e of this.nodes){const t=e.indices.filter(s=>this.viewer.isObjectVisible(s)).length,i=e.indices.filter(s=>this.viewer.selected.has(s)).length;if(e.row.setAttribute("aria-selected",String(i>0&&i===t)),e.row.dataset.partial=String(i>0&&i<t),e.row.dataset.hidden=String(!t),e.count.textContent=e.indices.length>1?t+"/"+e.indices.length:"",e.reason.hidden=t>0,t)e.row.removeAttribute("aria-describedby"),e.row.title=e.sourceName;else{const s=[...new Set(e.indices.flatMap(r=>this.viewer.visibilityReasons(r)))];e.reason.textContent="Hidden · "+s.join(" · "),e.row.setAttribute("aria-describedby",e.reason.id),e.row.title=e.sourceName+" · "+e.reason.textContent}Kt(e.eye,t?"eye":"hide",(t?"Hide ":"Show ")+e.label,!0),e.eye.setAttribute("aria-pressed",String(!!t))}this.updateSearchSummary()}}function Je(n,e="",t=""){const i=document.createElement(n);return i.className=e,i.textContent=t,i}class Ng{constructor(e,t,i){this.viewer=e,this.hints=new Pg(e.root,e.panel.id+"-command-hint"),this.dialogs={},this.tabs={},this.pages={},this.timelineVisible=!1,this.root=Je("header","eplusr-viewer__ribbon"),this.header=Je("div","eplusr-viewer__ribbon-header");const s=Je("span","eplusr-viewer__brand","IDFView");s.prepend(Zt("object"));const r=Je("div","eplusr-viewer__ribbon-tabs");r.setAttribute("role","tablist"),r.setAttribute("aria-label","Viewer ribbon"),this.quick=Je("div","eplusr-viewer__quick-actions"),this.header.append(s,r,this.quick),this.content=Je("div","eplusr-viewer__ribbon-content");const a=["home","view","results"];a.forEach((x,p)=>{const f=Je("button","",x==="home"?"Model":x[0].toUpperCase()+x.slice(1));f.type="button",f.id=e.panel.id+"-ribbon-"+x,f.setAttribute("role","tab");const M=Je("div","eplusr-viewer__ribbon-page");M.id=f.id+"-commands",M.setAttribute("role","tabpanel"),M.setAttribute("aria-labelledby",f.id),f.setAttribute("aria-controls",M.id),f.addEventListener("click",()=>this.select(x)),f.addEventListener("keydown",y=>{const E=y.key==="ArrowRight"?(p+1)%a.length:y.key==="ArrowLeft"?(p+a.length-1)%a.length:y.key==="Home"?0:y.key==="End"?a.length-1:null;E!==null&&(y.preventDefault(),y.stopPropagation(),this.select(a[E]),this.tabs[a[E]].focus())}),this.tabs[x]=f,this.pages[x]=M,r.append(f),this.content.append(M)});const o=Je("div","eplusr-viewer__ribbon-band");this.previous=this.button("Previous commands","previous",()=>this.scroll(-1),!0),this.next=this.button("More commands","next",()=>this.scroll(1),!0),this.previous.classList.add("eplusr-viewer__ribbon-scroll","eplusr-viewer__ribbon-scroll--previous"),this.next.classList.add("eplusr-viewer__ribbon-scroll","eplusr-viewer__ribbon-scroll--next"),o.append(this.content,this.previous,this.next),this.content.addEventListener("scroll",()=>this.syncOverflow()),this.root.append(this.header,o),e.toolbar.replaceWith(this.root),e.toolbar=this.quick,e.viewport=Je("div","eplusr-viewer__viewport"),e.canvasHost.replaceWith(e.viewport),e.viewport.append(e.canvasHost),this.browser=new Ig(e,this),e.workspace.prepend(this.browser.root),this.taskPane=Je("aside","eplusr-viewer__task-pane"),this.taskPane.hidden=!0,this.taskPane.setAttribute("aria-label","Task panel");const l=Je("div","eplusr-viewer__pane-header");this.taskHeading=Je("h2"),this.taskHeading.id=e.panel.id+"-task-title",this.taskPane.setAttribute("aria-labelledby",this.taskHeading.id),this.closeTaskButton=this.button("Close task panel","clear",()=>this.closeEditors(),!0),l.append(this.taskHeading,this.closeTaskButton),this.taskPane.append(l),e.workspace.append(this.taskPane),this.taskPane.addEventListener("keydown",x=>{x.key==="Escape"&&!x.defaultPrevented&&(x.preventDefault(),this.closeEditors()),x.stopPropagation()}),this.restore=this.button("Show controls","controls",()=>this.modelOnly(!1),!0),this.restore.classList.add("eplusr-viewer__restore-controls"),this.restore.hidden=!0,e.canvasHost.append(this.restore),this.treeButton=this.button("Model tree","tree",()=>this.showBrowser(!this.browserVisible),!0),this.modelButton=this.button("Model only","focus_model",()=>this.modelOnly(!0),!0),this.collapseButton=this.button("Collapse ribbon","collapse",()=>this.collapse(!this.collapsed),!0),this.helpButton=this.button("Navigation help","help",()=>this.open("help"),!0),e.connectionStatus=new Dg,this.quick.append(this.treeButton,this.modelButton,this.collapseButton,this.helpButton,e.connectionStatus.root),this.dialog("objects","Visibility filters",[t.search]),this.dialog("selection","Selection mode",[e.selectionModeSelect.closest("label")]),this.dialog("components","Visible components",[t.components]),this.dialog("appearance","Model appearance",[this.field("Color by",e.renderSelect),e.windowStyleSelect.closest("label")]),this.dialog("camera","Camera",[this.field("View",e.viewSelect),this.field("Projection",e.projectionSelect)]),this.dialog("explode","Exploded view",[e.explodeSelect.closest("label"),e.explodeSlider]),this.dialog("display","Section plane",[t.clipping]);const c=Je("div","eplusr-viewer__navigation-help");c.append(Je("p","","Drag to orbit · Right-drag to pan · Scroll to zoom"),Je("p","","Double-click: select Zone, then surface · I: isolate selection"),Je("p","","Ctrl / ⌘: add to selection · Shift: select a range in the model tree"),Je("p","","F: fit selection (or visible model) · I: isolate · M: measure · Esc: exit the current tool"),Je("p","","Model tree: arrows navigate and expand · H toggles visibility")),this.dialog("help","Navigation help",[c]),this.dialog("saved","Saved views",[t.saved]),this.snapshotImage=Je("img","eplusr-viewer__snapshot-preview"),this.snapshotImage.alt="Snapshot of the current 3D view",this.snapshotStatus=Je("p","eplusr-viewer__status"),this.snapshotLink=Je("a","eplusr-viewer__snapshot-download","Download PNG"),this.dialog("snapshot","Snapshot",[this.snapshotImage,this.snapshotStatus,this.snapshotLink]),this.dialog("diagnostics","Geometry diagnostics",[t.diagnostics]),this.dialog("properties","Selection properties",[t.selection]),this.dialog("results","Result data",[e.queryPanel.section,e.resultEmpty]);const h=[e.aggregationSelect.closest("label"),e.timeJumpInput.closest("label")];this.dialog("time","Result time settings",h),t.results.querySelector(".eplusr-viewer__section-body").prepend(e.variableSelect.closest("label")),this.dialog("colors","Result color scale",[t.results]),e.panel.remove(),e.resultStatus.classList.add("eplusr-viewer__result-status"),e.canvasHost.append(e.resultStatus),e.root.append(e.scopeBar);const d=this.pages.home;this.group(d,"Navigate").append(e.fitModelButton,e.fitSelectionButton),this.group(d,"Objects").append(this.launcher("Filter objects","filters","objects"),this.launcher("Components","components","components")),this.group(d,"Selection").append(this.launcher("Selection mode","select","selection"),e.selectionActions),Kt(e.panelButton,"properties","Properties"),e.panelButton.setAttribute("aria-controls",this.taskPane.id=e.panel.id+"-task-pane"),this.group(d,"Inspect").append(e.panelButton,i.reset,this.launcher("Appearance","appearance","appearance"));const u=this.pages.view;this.group(u,"Camera").append(this.launcher("Camera","camera","camera"),this.launcher("Saved views","save","saved"),i.capture),this.group(u,"Display").append(e.wireButton,e.xrayButton,e.axisButton,e.groundButton),this.group(u,"Tools").append(e.measureButton,e.clearMeasureButton,this.launcher("Section plane","section","display"),this.launcher("Exploded view","explode","explode"),this.launcher("Diagnostics","diagnostics","diagnostics"));const m=this.pages.results;this.group(m,"Source").append(this.launcher("Select result data","data","results")),this.colorScaleButton=this.launcher("Color scale","colors","colors"),this.group(m,"Appearance").append(this.colorScaleButton,this.launcher("Glass / color by","glass","appearance")),this.timelineButton=this.button("Timeline","timeline",()=>this.showTimeline(!this.timelineVisible)),this.timeSettingsButton=this.launcher("Time settings","time","time"),this.group(m,"Time").append(this.timeSettingsButton,this.timelineButton),e.timelineVariableLabel=Je("span","eplusr-viewer__timeline-variable"),e.timeline.querySelector(".eplusr-viewer__timeline-heading").prepend(e.timelineVariableLabel),e.timeline.querySelector(".eplusr-viewer__timeline-heading").append(this.button("Hide timeline","clear",()=>this.showTimeline(!1),!0)),e.root.addEventListener("keydown",x=>{x.key!=="Escape"||x.defaultPrevented||(this.clean?(x.preventDefault(),x.stopImmediatePropagation(),this.modelOnly(!1)):this.activeTask&&!this.taskPane.contains(x.target)&&(x.preventDefault(),x.stopImmediatePropagation(),this.closeEditors()))},!0),this.root.querySelectorAll("button[aria-label]:not([role=tab])").forEach(x=>x.classList.add("eplusr-viewer__icon-only"));const _={"Fit model":"Fit all visible objects","Fit selection":"Fit the selected objects · F",Isolate:"Show only the selection · I",Measure:"Measure between two model points · M","Select result data":"Choose variables, environments and other result filters","Color scale":"Choose the result variable, palette and numerical range","Time settings":"Aggregate results or jump to a time",Timeline:"Show or hide the result playback strip","Glass / color by":"Choose transparent glass or quantitative window colors","Model only":"Focus on the 3D model; hide interface controls · Esc restores them","Show controls":"Restore the model tree, commands and task panel · Esc"};this.root.querySelectorAll("button[aria-label]").forEach(x=>{x.dataset.hint=_[x.getAttribute("aria-label")]??x.getAttribute("aria-label")}),this.restore.dataset.hint=_["Show controls"],this.select("home"),this.resizeLayout()}button(e,t,i,s=!1){const r=Je("button");return r.type="button",Kt(r,t,e,s),r.addEventListener("click",i),r}field(e,t){const i=Je("label","eplusr-viewer__field");return i.append(Je("span","",e),t),i}group(e,t){const i=Je("section","eplusr-viewer__ribbon-group");i.setAttribute("aria-label",t);const s=Je("div","eplusr-viewer__ribbon-commands");return i.append(s,Je("div","eplusr-viewer__ribbon-caption",t)),e.append(i),s}launcher(e,t,i){const s=this.button(e,t,()=>this.open(i));return s.dataset.task=i,s}dialog(e,t,i){const s=Je("div","eplusr-viewer__tab-body eplusr-viewer__task-body");s.id=this.viewer.panel.id+"-task-"+e,s.hidden=!0,i.forEach(r=>{r.tagName==="DETAILS"&&(r.open=!0,r.classList.add("eplusr-viewer__task-section")),s.append(r)}),this.taskPane.append(s),this.dialogs[e]={element:s,title:t}}open(e){this.dialogs[e]&&(this.clean&&this.modelOnly(!1),this.taskPane.contains(document.activeElement)||(this.returnFocus=document.activeElement),this.activeTask=e,Object.entries(this.dialogs).forEach(([t,i])=>{i.element.hidden=t!==e}),this.taskHeading.textContent=this.dialogs[e].title,this.taskPane.hidden=!1,this.drawers&&(this.browserVisible=!1),this.syncLayout(),this.viewer.resize(),this.closeTaskButton.focus(),this.viewer.root.dispatchEvent(new CustomEvent("eplusr-view")))}previewSnapshot(e,t,i,s){this.snapshotImage.src=e,this.snapshotLink.href=e,this.snapshotLink.download=t,this.snapshotStatus.textContent=i+" × "+s+" px · PNG",this.open("snapshot")}closeEditors(){this.activeTask=null,this.taskPane.hidden=!0,this.syncLayout(),this.viewer.resize(),(this.returnFocus?.isConnected&&this.returnFocus.getClientRects().length?this.returnFocus:this.tabs[this.active])?.focus(),this.viewer.root.dispatchEvent(new CustomEvent("eplusr-view"))}showBrowser(e){this.browserVisible=!!e,e&&this.drawers&&(this.activeTask=null,this.taskPane.hidden=!0),this.syncLayout(),this.viewer.resize(),e?this.browser.search.focus():this.treeButton.focus(),this.viewer.root.dispatchEvent(new CustomEvent("eplusr-view"))}resizeLayout(){const e=this.viewer.root.clientWidth<1100;this.drawers!==e&&(this.drawers=e,this.browserVisible=!e,e&&this.activeTask&&(this.browserVisible=!1)),this.syncLayout()}syncLayout(){const e=this.viewer.root;e.classList.toggle("eplusr-viewer--drawers",this.drawers),e.classList.toggle("eplusr-viewer--browser-open",!!this.browserVisible),e.classList.toggle("eplusr-viewer--task-open",!!this.activeTask),this.browser.root.hidden=!this.browserVisible,this.treeButton.setAttribute("aria-pressed",String(!!this.browserVisible)),this.treeButton.dataset.hint=this.browserVisible?"Hide model tree":"Show model tree",this.treeButton.setAttribute("aria-expanded",String(!!this.browserVisible)),this.treeButton.setAttribute("aria-controls",this.browser.root.id),this.viewer.panelButton.setAttribute("aria-expanded",String(this.activeTask==="properties")),this.root.querySelectorAll("[data-task]").forEach(t=>t.setAttribute("aria-pressed",String(t.dataset.task===this.activeTask)))}select(e){Object.keys(this.tabs).forEach(t=>{this.tabs[t].setAttribute("aria-selected",String(t===e)),this.tabs[t].tabIndex=t===e?0:-1,this.pages[t].hidden=t!==e}),this.active=e,this.content.scrollLeft=0,this.collapse(!1)}scroll(e){this.content.scrollBy({left:e*Math.max(160,this.content.clientWidth-100),behavior:"instant"}),this.syncOverflow()}syncOverflow(){const e=!this.collapsed&&this.content.scrollWidth>this.content.parentElement.clientWidth+1;this.previous.hidden=!e,this.next.hidden=!e,this.previous.disabled=this.content.scrollLeft<2,this.next.disabled=this.content.scrollLeft+this.content.clientWidth>=this.content.scrollWidth-2}collapse(e){this.collapsed=e,this.content.hidden=e,this.collapseButton.setAttribute("aria-expanded",String(!e)),Kt(this.collapseButton,e?"expand":"collapse",e?"Expand ribbon":"Collapse ribbon",!0),this.viewer.resize(),this.syncOverflow(),this.viewer.root.dispatchEvent(new CustomEvent("eplusr-view"))}showTimeline(e){const t=!e&&this.viewer.timeline.contains(document.activeElement);this.timelineVisible=!!e,e||this.viewer.pause(),this.syncResults(),this.viewer.resize(),t&&this.tabs.results.focus(),this.viewer.root.dispatchEvent(new CustomEvent("eplusr-view"))}syncResults(){const e=!!this.viewer.resultSet.length;this.viewer.timeline.hidden=!e||!this.timelineVisible||this.clean,[this.timelineButton,this.colorScaleButton,this.timeSettingsButton,this.viewer.variableSelect,this.viewer.paletteSelect,this.viewer.rangeModeSelect,this.viewer.aggregationSelect,this.viewer.timeJumpInput].filter(Boolean).forEach(t=>{t.disabled=!e}),this.timelineButton.setAttribute("aria-pressed",String(e&&this.timelineVisible))}modelOnly(e){e&&(this.viewer.pause(),this.viewer.toggleMeasureMode(!1)),this.clean=!!e,this.viewer.root.classList.toggle("eplusr-viewer--model-only",this.clean),this.root.hidden=this.clean,this.viewer.scopeBar.hidden=this.clean,this.restore.hidden=!this.clean,this.syncResults(),this.viewer.resize(),(this.clean?this.restore:this.modelButton).focus(),this.viewer.root.dispatchEvent(new CustomEvent("eplusr-view"))}}class Ug{constructor(e){this.viewer=e,this.element=document.createElement("div"),this.element.className="eplusr-viewer__navigation",this.element.setAttribute("role","group"),this.element.setAttribute("aria-label","View orientation and true north");const t=document.createElementNS("http://www.w3.org/2000/svg","svg");t.setAttribute("viewBox","0 0 100 100"),t.setAttribute("aria-hidden","true"),t.classList.add("eplusr-viewer__navigation-lines"),this.element.appendChild(t);const i=[["X","Right view (+X)","right",[1,0,0],"#a8534c"],["−X","Left view (−X)","left",[-1,0,0],"#a8534c"],["N","North / Back view (+Y)","back",[0,1,0],"#3c7657"],["−Y","Front view (−Y)","front",[0,-1,0],"#3c7657"],["Z","Top view (+Z)","top",[0,0,1],"#426d9c"],["−Z","Bottom view (−Z)","bottom",[0,0,-1],"#426d9c"]];this.axes=i.map(([s,r,a,o,l])=>{const c=document.createElementNS(t.namespaceURI,"line");c.setAttribute("x1","50"),c.setAttribute("y1","50"),c.setAttribute("stroke",l),t.appendChild(c);const h=document.createElement("button");return h.type="button",h.textContent=s,h.title=r,h.setAttribute("aria-label",r),h.style.setProperty("--axis-color",l),h.addEventListener("click",()=>{const d={right:"left",left:"right",back:"front",front:"back",top:"bottom",bottom:"top"};e.setView(e.viewSelect.value===a?d[a]:a)}),this.element.appendChild(h),{button:h,line:c,vector:new A(...o),view:a}}),this.iso=document.createElement("button"),this.iso.type="button",this.iso.textContent="ISO",this.iso.className="eplusr-viewer__navigation-iso",this.iso.title="Isometric view",this.iso.setAttribute("aria-label","Isometric view"),this.iso.addEventListener("click",()=>e.setView("iso")),this.caption=document.createElement("div"),this.caption.className="eplusr-viewer__navigation-caption",this.element.append(this.iso,this.caption),this.collapseButton=document.createElement("button"),this.collapseButton.type="button",this.collapseButton.className="eplusr-viewer__navigation-collapse",this.collapseButton.addEventListener("click",()=>this.setCollapsed(!this.collapsed)),this.element.appendChild(this.collapseButton),e.canvasHost.appendChild(this.element)}resize(e){e!==this.narrow&&(this.narrow=e,this.setCollapsed(e)),this.collapseButton.hidden=!e,this.element.classList.toggle("eplusr-viewer__navigation--collapsible",e)}setCollapsed(e){this.collapsed=e,this.element.classList.toggle("eplusr-viewer__navigation--collapsed",e),this.collapseButton.textContent=e?"Axes":"Less",this.collapseButton.setAttribute("aria-label",e?"Expand compass":"Collapse compass"),this.collapseButton.setAttribute("aria-expanded",String(!e))}update(){const e=this.viewer,t=e.camera.quaternion.clone().invert();this.axes.forEach(({button:i,line:s,vector:r,view:a})=>{const o=r.clone().applyQuaternion(t),l=50+o.x*34,c=50-o.y*34;i.style.left=`${l}%`,i.style.top=`${c*.86+10}px`,i.style.zIndex=o.z>=0?"3":"2",i.classList.toggle("eplusr-viewer__axis-back",o.z<-.1),i.hidden=Math.hypot(o.x,o.y)<.25&&o.z<0,i.setAttribute("aria-pressed",String(e.viewSelect.value===a)),s.setAttribute("x2",String(l)),s.setAttribute("y2",String(c)),s.style.opacity=i.hidden?"0":o.z<0?"0.35":"0.8"}),this.iso.setAttribute("aria-pressed",String(e.viewSelect.value==="iso")),this.caption.textContent=e.viewSelect.selectedOptions[0]?.textContent??"Custom"}dispose(){this.element.remove()}}function Ln(n){const e=String(n??"");return["C","degC","°C"].includes(e)?"°C":["F","degF","°F"].includes(e)?"°F":e.replace(/m\^?2\b/g,"m²").replace(/m\^?3\b/g,"m³")}function Va(n,e){const t=String(n?.times?.[e]??"");return t==="Value"?"Static values":`Time: ${t}`}class Fg{constructor(e,t,i){this.select=e,this.label=t,this.rows=[],this.root=document.createElement("div"),this.root.className="eplusr-viewer__multi",this.root.setAttribute("role","group"),this.root.setAttribute("aria-labelledby",i+"-label");const s=document.createElement("div");s.className="eplusr-viewer__multi-heading";const r=document.createElement("span");r.id=i+"-label",r.textContent=t,this.count=document.createElement("span"),this.count.className="eplusr-viewer__multi-count",this.clear=document.createElement("button"),this.clear.type="button",this.clear.className="eplusr-viewer__multi-clear",this.clear.setAttribute("aria-label","Clear "+t.toLowerCase()),this.clear.dataset.hint="Clear all selected "+t.toLowerCase(),this.clear.append(Zt("clear")),this.clear.addEventListener("click",()=>{for(const l of this.select.options)l.selected=!1;this.search.value="",this.renderOptions(),this.changed(),this.search.focus()}),s.append(r,this.count,this.clear),this.chips=document.createElement("div"),this.chips.className="eplusr-viewer__multi-chips",this.chips.setAttribute("role","group"),this.chips.setAttribute("aria-label","Selected "+t.toLowerCase());const a=document.createElement("div");a.className="eplusr-viewer__multi-search";const o=document.createElement("label");o.className="eplusr-viewer__sr-only",o.htmlFor=i+"-search",o.textContent="Search "+t.toLowerCase(),this.search=document.createElement("input"),this.search.id=i+"-search",this.search.type="search",this.search.name=e.name+"_search",this.search.placeholder="Search "+t.toLowerCase()+"…",this.search.autocomplete="off",this.search.spellcheck=!1,this.search.setAttribute("autocapitalize","none"),this.search.setAttribute("autocorrect","off"),this.search.setAttribute("aria-controls",i+"-options"),this.search.addEventListener("focus",()=>this.open()),this.search.addEventListener("click",()=>this.open()),this.search.addEventListener("input",l=>{l.stopPropagation(),this.renderOptions(),this.open()}),this.search.addEventListener("change",l=>l.stopPropagation()),this.toggle=document.createElement("button"),this.toggle.type="button",this.toggle.className="eplusr-viewer__multi-toggle",this.toggle.setAttribute("aria-label","Show options for "+t.toLowerCase()),this.toggle.setAttribute("aria-controls",i+"-options"),this.toggle.setAttribute("aria-expanded","false"),this.toggle.appendChild(Zt("next")),this.toggle.addEventListener("click",()=>{this.panel.hidden?(this.search.focus(),this.open()):this.close()}),a.append(o,Zt("search"),this.search,this.toggle),this.panel=document.createElement("div"),this.panel.id=i+"-options",this.panel.className="eplusr-viewer__multi-panel",this.panel.hidden=!0,this.options=document.createElement("div"),this.options.className="eplusr-viewer__multi-options",this.options.setAttribute("role","group"),this.options.setAttribute("aria-label",t+" options"),this.summary=document.createElement("p"),this.summary.className="eplusr-viewer__multi-summary",this.summary.setAttribute("role","status"),this.summary.setAttribute("aria-live","polite"),this.panel.append(this.options,this.summary),e.hidden=!0,this.root.append(s,this.chips,a,this.panel,e),this.root.addEventListener("keydown",l=>this.keydown(l))}configure(){this.search.value="",this.close(),this.renderOptions(),this.renderSelection()}setOptions(e,t,i=!1){const s=this.rows.find(o=>o.checkbox===document.activeElement)?.option.value,r=new Set(t.map(o=>String(o).toLowerCase())),a=JSON.stringify([...this.select.options].map(o=>o.value))!==JSON.stringify(e);if(a)this.select.replaceChildren(...e.map(o=>{const l=document.createElement("option");return l.value=l.textContent=l.title=o,l.selected=r.has(String(o).toLowerCase()),l})),this.renderOptions();else for(const o of this.select.options)o.selected=r.has(o.value.toLowerCase());i?this.configure():this.renderSelection(),a&&s!=null&&(this.rows.find(l=>l.option.value===s)?.checkbox??this.search).focus()}renderOptions(){const e=this.search.value.trim().toLocaleLowerCase().split(/\s+/).filter(Boolean),t=[...this.select.options].filter(i=>e.every(s=>i.text.toLocaleLowerCase().includes(s)));if(this.options.replaceChildren(),this.rows=t.map(i=>{const s=document.createElement("label");s.className="eplusr-viewer__multi-option",s.title=i.text;const r=document.createElement("input");r.type="checkbox",r.name=this.select.name+"_choice",r.checked=i.selected,r.value=i.value;const a=document.createElement("span");return a.textContent=i.text,r.addEventListener("change",o=>{o.stopPropagation(),i.selected=r.checked,this.changed()}),s.append(r,a),this.options.appendChild(s),{option:i,checkbox:r}}),!t.length){const i=document.createElement("p");i.className="eplusr-viewer__multi-empty",i.textContent="No matching options.",this.options.appendChild(i)}this.summary.textContent=`${t.length} of ${this.select.options.length} options`}renderSelection(){const e=[...this.select.selectedOptions];if(this.count.textContent=`${e.length} selected`,this.count.hidden=e.length<=1,this.clear.hidden=e.length===0,this.clear.disabled=e.length===0,this.chips.replaceChildren(),!e.length){const t=document.createElement("span");t.className="eplusr-viewer__multi-all",t.textContent="All values · no restriction",this.chips.appendChild(t)}e.forEach((t,i)=>{const s=document.createElement("button");s.type="button",s.className="eplusr-viewer__multi-chip",s.title=t.text,s.setAttribute("aria-label",`Remove ${t.text} from ${this.label.toLowerCase()}`);const r=document.createElement("span");r.textContent=t.text,s.append(r,Zt("clear")),s.addEventListener("click",()=>{t.selected=!1,this.changed();const a=this.chips.querySelectorAll("button");a.length?a[Math.min(i,a.length-1)].focus():this.search.focus()}),this.chips.appendChild(s)});for(const t of this.rows)t.checkbox.checked=t.option.selected}changed(){this.renderSelection(),this.select.dispatchEvent(new Event("change",{bubbles:!0}))}open(){this.panel.hidden&&(this.panel.hidden=!1,this.toggle.setAttribute("aria-expanded","true"),this.root.dispatchEvent(new CustomEvent("eplusr-multiselect-open",{bubbles:!0}))),this.ensureVisible()}ensureVisible(){const e=this.root.closest(".eplusr-viewer__tab-body");if(this.panel.hidden||!e?.clientHeight)return;this.options.style.maxHeight=Math.max(44,Math.min(192,e.clientHeight-80))+"px";const t=e.getBoundingClientRect(),i=this.panel.getBoundingClientRect().bottom;i>t.bottom-6&&(e.scrollTop+=i-t.bottom+6);const s=this.search.getBoundingClientRect().top;s<t.top+4&&(e.scrollTop-=t.top+4-s)}close(){this.panel.hidden=!0,this.toggle.setAttribute("aria-expanded","false")}keydown(e){if(e.stopPropagation(),e.isComposing)return;if(e.key==="Escape"){e.preventDefault(),this.search.focus(),this.close();return}if(e.target===this.search&&["Enter","ArrowDown"].includes(e.key)){e.preventDefault(),this.open(),this.rows[0]?.checkbox.focus();return}const t=this.rows.findIndex(s=>s.checkbox===e.target);if(t<0)return;const i={ArrowDown:t+1,ArrowUp:t-1,Home:0,End:this.rows.length-1}[e.key];i!==void 0&&(e.preventDefault(),i<0?this.search.focus():this.rows[Math.min(i,this.rows.length-1)].checkbox.focus())}}function Og(n,e){const t=["name","environment_name","reporting_frequency","key_value","day_type"],i=["variables","environments","frequencies","keys","day_types"],s=n.combinations,r={},a={},o=[];let l=Array.isArray(s)?s:null;return t.forEach((c,h)=>{const d=l?[...new Set(l.map(x=>x[c]).filter(x=>x!=null&&x!==""))].sort():[n[i[h]]??[]].flat(),u=new Set((e[c]??[]).map(x=>String(x).toLowerCase()));r[c]=d.filter(x=>u.has(String(x).toLowerCase())),a[c]=d;const m=new Set(r[c].map(x=>String(x).toLowerCase())),_=(e[c]??[]).filter(x=>!m.has(String(x).toLowerCase()));_.length&&o.push({field:c,values:_}),l&&m.size&&(l=l.filter(x=>m.has(String(x[c]).toLowerCase())))}),{selected:r,options:a,removed:o}}function Kl(n){return n==null?[]:Array.isArray(n)?n:[n]}function Jl(n,e,t,i){if(!n.trim())return null;const s=[];for(const r of n.trim().split(/[,\s]+/)){const a=r.match(/^(-?\d+)(?::(-?\d+))?$/);if(!a)throw new Error(`${e}: use integers or ranges such as 8:18.`);const o=Number(a[1]),l=Number(a[2]??a[1]);if(Math.min(o,l)<t||Math.max(o,l)>i||Math.abs(l-o)>1e4)throw new Error(`${e}: values must be between ${t} and ${i}.`);for(let c=o;o<=l?c<=l:c>=l;c+=o<=l?1:-1)s.push(c)}return[...new Set(s)]}class Bg{constructor(e){this.viewer=e,this.section=document.createElement("details"),this.section.className="eplusr-viewer__result-query",this.section.open=!0;const t=document.createElement("summary");t.textContent="Result data",this.form=document.createElement("form"),this.form.setAttribute("aria-label","EnergyPlus result query"),this.form.noValidate=!0,this.fields={},this.multiFields={},this.fieldset=document.createElement("fieldset");const i=document.createElement("legend");i.textContent="Select simulation results",i.className="eplusr-viewer__sr-only",this.fieldset.appendChild(i),this.field("name","Variables","select",this.fieldset),this.field("environment_name","Environments","select",this.fieldset),this.field("reporting_frequency","Reporting frequency","select",this.fieldset);const s=document.createElement("p");s.className="eplusr-viewer__status",s.textContent="Empty selection includes all values.";const r=document.createElement("details"),a=document.createElement("summary");a.textContent="Keys and time filters",r.appendChild(a),this.field("key_value","Result keys","select",r);const o=document.createElement("div");o.className="eplusr-viewer__query-grid";const l=[["month","Months",1,12],["day","Days",1,31],["hour","Hours",0,24],["minute","Minutes",0,60],["interval","Interval (min)",1,527040],["simulation_days","Simulation days",1,366]];this.numericFields=l,l.forEach(([h,d])=>this.field(h,d,"input",o).placeholder=h==="hour"?"e.g. 8:18":"All"),this.field("year","Year","input",o).placeholder="Source calendar",this.field("tz","Time zone","input",o).placeholder="UTC",r.appendChild(o),this.field("day_type","Day types","select",r),this.field("period","Dates / date-times (period)","input",r).placeholder="2017-07-21, 2017-07-22",this.fields.period.title="Comma-separated dates or date-times, matching report_data(period = ...). Each entry selects that date/time; two entries are not a continuous range.";const c=document.createElement("div");c.className="eplusr-viewer__button-row",this.apply=document.createElement("button"),this.apply.type="submit",this.apply.textContent="Apply",this.apply.setAttribute("aria-label","Apply result query"),this.reset=document.createElement("button"),this.reset.type="button",this.reset.textContent="Reset",this.reset.setAttribute("aria-label","Reset query"),this.reset.addEventListener("click",()=>{this.paired=!1,this.keysChanged=!0,Object.values(this.fields).forEach(h=>{h.tagName==="SELECT"?[...h.options].forEach(d=>{d.selected=!1}):h.value=""}),this.reconcile(!0),this.submit()}),c.append(this.apply,this.reset),this.status=document.createElement("p"),this.status.className="eplusr-viewer__status",this.status.id=`${e.container.id}-query-status`,this.status.setAttribute("role","status"),this.status.setAttribute("aria-live","polite"),this.form.setAttribute("aria-describedby",this.status.id),this.fieldset.append(s,r,c),this.form.append(this.fieldset,this.status),this.form.addEventListener("eplusr-multiselect-open",h=>{Object.values(this.multiFields).forEach(d=>{d.root!==h.target&&d.close()})}),this.form.addEventListener("submit",h=>{h.preventDefault(),this.submit()}),this.form.addEventListener("change",h=>{[this.fields.name,this.fields.key_value].includes(h.target)&&(this.keysChanged=!0),h.target.removeAttribute("aria-invalid");const d=this.reconcile();this.message((d.length?"Removed unavailable selections: "+d.map(u=>this.multiFields[u.field].label+" — "+u.values.join(", ")).join("; ")+". ":"")+"Changes not applied.","draft")}),this.section.append(t,this.form),e.tabBodies.results.prepend(this.section),this.configure(null)}field(e,t,i,s){if(i==="select"){const l=document.createElement("select");l.name=e,l.multiple=!0;const c=new Fg(l,t,this.viewer.container.id+"-query-"+e);return this.fields[e]=l,this.multiFields[e]=c,s.appendChild(c.root),l}const r=document.createElement("label");r.className="eplusr-viewer__field";const a=document.createElement("span");a.textContent=t;const o=document.createElement(i);return o.name=e,o.setAttribute("aria-label",t),r.append(a,o),s.appendChild(r),this.fields[e]=o,o}configure(e){if(this.config=e,this.section.hidden=!e,!e){this.applied=null;return}this.paired=!!e.paired_keys,this.keysChanged=!1,this.applied=e.filters??{},this.reconcile(!0,this.applied);for(const t of Object.keys(this.fields).filter(i=>!this.multiFields[i]))this.fields[t].value=Kl(this.applied[t]).join(", ");this.fieldset.disabled=!e.enabled,this.setPending(!1),this.message(e.enabled?"Applied result query.":"Connect to R to query other source results.",e.enabled?"applied":"offline")}reconcile(e=!1,t=null){if(!this.config)return[];const i=t??Object.fromEntries(Object.entries(this.multiFields).map(([r,a])=>[r,[...a.select.selectedOptions].map(o=>o.value)])),s=Og(this.config.catalog,Object.fromEntries(Object.keys(this.multiFields).map(r=>[r,Kl(i[r])])));for(const[r,a]of Object.entries(this.multiFields))a.setOptions(s.options[r],s.selected[r],e);return!e&&s.removed.some(r=>r.field==="key_value")&&(this.keysChanged=!0),this.resize(),s.removed}resize(){Object.values(this.multiFields).forEach(e=>e.ensureVisible())}read(){const e={};for(const[i,s]of Object.entries(this.fields))if(s.tagName==="SELECT"){const r=[...s.selectedOptions].map(a=>a.value);e[i]=r.length?r:null}for(const[i,s,r,a]of this.numericFields)try{e[i]=Jl(this.fields[i].value,s,r,a)}catch(o){throw this.fields[i].setAttribute("aria-invalid","true"),this.fields[i].focus(),o}try{const i=Jl(this.fields.year.value,"Year",1,9999);if(i?.length>1)throw new Error("Year: choose a single year.");e.year=i?.[0]??null}catch(i){throw this.fields.year.setAttribute("aria-invalid","true"),this.fields.year.focus(),i}e.tz=this.fields.tz.value.trim()||null;const t=this.fields.period.value.trim();return e.period=t?t.split(/\s*,\s*/).map(i=>i.replace("T"," ")):null,this.paired&&!this.keysChanged&&delete e.key_value,e}submit(){if(!(!this.config?.enabled||this.pending))try{const e=this.read();this.setPending(!0),this.message("Querying results…","pending"),this.viewer.root.dispatchEvent(new CustomEvent("eplusr-result-query",{detail:e}))}catch(e){this.fail(e.message)}}setPending(e){this.pending=e,this.form.setAttribute("aria-busy",String(e)),this.apply.disabled=e,this.reset.disabled=e}message(e,t){this.status.dataset.state=t;const i=document.createElement("span");i.textContent=e,i.className=t==="applied"?"eplusr-viewer__sr-only":"",this.status.replaceChildren(...t==="applied"?[Zt("check"),i]:[i]),t==="applied"?this.status.dataset.hint=e:delete this.status.dataset.hint}fail(e){this.setPending(!1),this.message(e,"error")}}function zg(n,e,t){const i=n.map(()=>[]);for(let s=0;s<t.length;s+=1){const r=s*6,a=[Array.from(e.slice(r,r+3)).join(","),Array.from(e.slice(r+3,r+6)).join(",")];i[t[s]].push(a.sort().join(":"))}return i.map(s=>s.length>=3?s.sort().join(";"):null)}function kg(n,e,t){const i=n.map(()=>null);for(let s=0;s<t.length;s+=1){const r=t[s];if(i[r])continue;const a=s*9,o=[0,1,2].map(d=>e[a+3+d]-e[a+d]),l=[0,1,2].map(d=>e[a+6+d]-e[a+d]),c=[o[1]*l[2]-o[2]*l[1],o[2]*l[0]-o[0]*l[2],o[0]*l[1]-o[1]*l[0]],h=Math.hypot(...c);h>1e-12&&(i[r]=c.map(d=>d/h))}return i}function Vg(n,e,t,i,s){const r=new Map(n.map((h,d)=>[h.name?.toLowerCase(),d])),a=zg(n,i,s),o=kg(n,e,t),l=n.map(()=>[]);n.forEach((h,d)=>{const u=r.get(h.building_surface_name?.toLowerCase());h.category==="subsurface"&&u!=null&&l[u].push(a[d])});const c=new Int32Array(n.length).fill(-1);for(const h of["surface","subsurface"])n.forEach((d,u)=>{if(d.category!==h||c[u]>=0)return;const m=r.get(d.outside_boundary_condition_object?.toLowerCase());if(m==null||m===u||n[m].category!==h)return;const _=n[m];if(_.outside_boundary_condition_object?.toLowerCase()===d.name?.toLowerCase()&&!(h==="surface"&&[d,_].some(x=>x.outside_boundary_condition?.toLowerCase()!=="surface"))){if(h==="subsurface"){const x=r.get(d.building_surface_name?.toLowerCase()),p=r.get(_.building_surface_name?.toLowerCase());if(x==null||p==null||c[x]!==p)return}!a[u]||a[u]!==a[m]||JSON.stringify(l[u].sort())!==JSON.stringify(l[m].sort())||!o[u]||!o[m]||o[u].reduce((x,p,f)=>x+p*o[m][f],0)>-.999999||(c[u]=m,c[m]=u)}});return c}function Gg(n,e,t,i,s=new Set){const r=new Uint8Array(e.length);e.forEach((o,l)=>{if(o<0||!t.has(l)||!t.has(o))return;const c=i[l],h=i[o];c&&h&&(c.x-h.x)**2+(c.y-h.y)**2+(c.z-h.z)**2<=1e-12&&(r[l]=1)});const a=[];return n.forEach((o,l)=>{const c=(r[o]?1:0)+(s.has(o)?2:0),h=a.at(-1);h?.materialIndex===c?h.count+=3:a.push({start:l*3,count:3,materialIndex:c})}),{active:r,groups:a}}const Hg="#8db9cd",Wg=.28,Ql=["#242f3a","#748292","#e6edf5"].map(n=>new Re(n));function Xg(n){return["window","glassdoor"].includes(String(n.surface_type??"").toLowerCase())}function Ga(n){return n.r*.2126+n.g*.7152+n.b*.0722}function jg(n,e,t){return t>=1?n:n.clone().convertLinearToSRGB().multiplyScalar(t).add(e.clone().convertLinearToSRGB().multiplyScalar(1-t)).convertSRGBToLinear()}function qg(n,e,t=1,i=null){const s=[Ga(jg(n,e,t))];i&&s.push(Ga(i));let r=Ql[0],a=-1;for(const o of Ql){const l=Ga(o),c=Math.min(...s.map(h=>(Math.max(h,l)+.05)/(Math.min(h,l)+.05)));c>a&&(r=o,a=c)}return r}const ec={Fit:"fit","Fit model":"fit","Fit selection":"isolate",Panel:"panel",Edges:"wireframe","X-ray":"xray",Axes:"axes",Ground:"ground",Measure:"measure","Show all":"reset",Snapshot:"snapshot",Isolate:"isolate",Hide:"hide",Clear:"clear","Clear selection":"clear","Clear filters":"filters","Exit isolation":"undo","Save current":"save",Delete:"delete",Previous:"previous",Next:"next",Play:"play"},fi=["floor","wall","roof","window","door","shading","daylighting"];function O(n,e,t){const i=document.createElement(n);return e&&(i.className=e),t!==void 0&&(i.textContent=t),n==="button"&&ec[t]&&Kt(i,ec[t],t,["Previous","Next","Play","Clear"].includes(t)),i}function qe(n,e=n){const t=document.createElement("option");return t.value=n,t.textContent=e,t}function Dn(n,e=[0,1]){let t=1/0,i=-1/0;for(const s of n)Number.isFinite(s)&&(t=Math.min(t,s),i=Math.max(i,s));return Number.isFinite(t)?[t,i]:e}function tc(n,e){const t=Array.from(n).filter(Number.isFinite).sort((a,o)=>a-o);if(!t.length)return Number.NaN;const i=Math.max(0,Math.min(1,e))*(t.length-1),s=Math.floor(i),r=Math.min(t.length-1,s+1);return t[s]+(t[r]-t[s])*(i-s)}function Yg(n,e){return e==="surface_type"?n.surface_type??"Undefined":e==="boundary"?n.outside_boundary_condition??"Undefined":e==="construction"?n.construction_name??"Undefined":e==="zone"?n.zone_name??"No zone":e==="space"?n.space_name??"No space":"Surface"}function At(n,e=5){return Number.isFinite(Number(n))?Number(n).toLocaleString(void 0,{maximumSignificantDigits:e}):"Missing"}function $g(n,e){const t=String(n??""),i=t.match(/(?:(\d{4})[-/])?(\d{1,2})[-/](\d{1,2})/);if(!i)return t;const s=i[1]?`${i[1]}-`:"",r=String(Number(i[2])).padStart(2,"0"),a=String(Number(i[3])).padStart(2,"0");return e==="month"?`${s}${r}`:`${s}${r}-${a}`}class Zg{constructor(e){this.container=e,this.container.innerHTML="",this.root=O("div","eplusr-viewer"),this.canvasHost=O("div","eplusr-viewer__canvas"),this.root.appendChild(this.canvasHost),this.container.appendChild(this.root),this.scene=new Mh,this.scene.background=new Re("#f3f5f7"),this.renderer=new dm({antialias:!0,alpha:!1,preserveDrawingBuffer:!0}),this.renderer.setPixelRatio(Math.min(window.devicePixelRatio||1,2)),this.renderer.localClippingEnabled=!0,this.renderer.domElement.setAttribute("role","img"),this.renderer.domElement.setAttribute("aria-label","Interactive 3D EnergyPlus model. Use the toolbar, mouse, or keyboard shortcuts to inspect the geometry."),this.canvasHost.appendChild(this.renderer.domElement),this.perspectiveCamera=new $t(38,1,.01,1e5),this.perspectiveCamera.up.set(0,0,1),this.orthographicCamera=new ks(-1,1,1,-1,-1e5,1e5),this.orthographicCamera.up.set(0,0,1),this.camera=this.perspectiveCamera,this.controls=new _m(this.camera,this.renderer.domElement),this.controls.enableDamping=!0,this.controls.dampingFactor=.12,this.controls.screenSpacePanning=!0,this.raycaster=new qh,this.pointer=new Ne,this.objects=[],this.positions=new Float32Array,this.faceObjects=new Int32Array,this.edgePositions=new Float32Array,this.edgeObjects=new Int32Array,this.pointPositions=new Float32Array,this.selected=new Set,this.hidden=new Set,this.isolated=null,this.isolationHistory=[],this.hovered=null,this.visibleObjects=new Set,this.componentVisibility=Object.fromEntries(fi.map(t=>[t,!0])),this.zoneFilter="",this.spaceFilter="",this.nameFilter="",this.remoteFilters={},this.selectionMode="surface",this.renderBy="surface_type",this.frame=0,this.resultSet=[],this.resultVariableIndex=0,this.results=null,this.resultAbortController=null,this.playbackTimer=null,this.playbackSpeed=1,this.xray=!1,this.windowStyle="glass",this.glazingObjects=new Set,this.windowParents=new Map,this.showWireframe=!0,this.showAxis=!0,this.showGround=!1,this.pointerDown=null,this.animationFrame=null,this.needsRender=!0,this.clipPlane=new Ti(new A(0,0,-1),0),this.clipEnabled=!1,this.clipAxis="z",this.clipCap=!1,this.explodeMode="none",this.explodeAmount=0,this.measureMode=!1,this.measurePoints=[],this.measurement=null,this.showDiagnostics=!1,this.diagnosticOnly=!1,this.diagnosticObjectSet=new Set,this.addLights(),this.buildUi(),this.bindEvents(),this.animate()}addLights(){this.scene.add(new Vh(16777215,8226710,2.2));const e=new Wh(16777215,2.5);e.position.set(-1,-1,2),this.scene.add(e)}buildUi(){this.toolbar=O("div","eplusr-viewer__toolbar"),this.toolbar.setAttribute("role","toolbar"),this.toolbar.setAttribute("aria-label","3D model viewer controls"),this.panel=O("aside","eplusr-viewer__panel"),this.panel.id=`${this.container.id||"eplusr-viewer"}-panel`,this.panel.setAttribute("aria-label","Model inspection panel"),this.tooltip=O("div","eplusr-viewer__tooltip"),this.legend=O("div","eplusr-viewer__legend"),this.legend.setAttribute("aria-label","Color legend"),this.workspace=O("div","eplusr-viewer__workspace"),this.scopeBar=O("div","eplusr-viewer__scope"),this.visibleCount=O("span","eplusr-viewer__visible-count"),this.visibleCount.setAttribute("role","status"),this.filterChips=O("div","eplusr-viewer__filter-chips"),this.filterChips.setAttribute("aria-label","Active visibility filters"),this.selectionCount=O("button","eplusr-viewer__selection-count"),this.selectionCount.type="button",this.selectionCount.addEventListener("click",()=>{this.setPanelTab("properties")});const e=O("button","eplusr-viewer__clear-filters","Clear filters");e.type="button",e.addEventListener("click",()=>this.clearFilters()),this.clearFiltersButton=e,this.scopeBar.append(this.visibleCount,this.filterChips,e,this.selectionCount),this.workspace.append(this.canvasHost,this.panel),this.canvasHost.append(this.tooltip,this.legend),this.timeline=O("div","eplusr-viewer__timeline"),this.timeline.hidden=!0,this.timeline.setAttribute("aria-label","Result time controls"),this.root.append(this.toolbar,this.scopeBar,this.workspace,this.timeline);const t=this.toolbarGroup("Rendering");this.renderSelect=O("select"),this.renderSelect.setAttribute("aria-label","Color surfaces by"),[["surface_type","Surface type"],["boundary","Boundary"],["construction","Construction"],["zone","Zone"],["space","Space"],["normal","Normals"]].forEach(([ie,Pe])=>this.renderSelect.appendChild(qe(ie,Pe))),this.renderSelect.addEventListener("change",()=>this.setRenderBy(this.renderSelect.value)),t.append(Zt("layers"),this.renderSelect);const i=this.toolbarGroup("Camera");this.viewSelect=O("select"),this.viewSelect.setAttribute("aria-label","Camera view"),[["iso","Isometric"],["top","Top"],["bottom","Bottom"],["front","Front"],["back","Back"],["left","Left"],["right","Right"]].forEach(([ie,Pe])=>this.viewSelect.appendChild(qe(ie,Pe)));const s=qe("custom","Custom");s.disabled=!0,this.viewSelect.appendChild(s),this.viewSelect.classList.add("eplusr-viewer__view-presets"),this.viewSelect.addEventListener("change",()=>this.setView(this.viewSelect.value)),i.append(Zt("object"),this.viewSelect),this.projectionSelect=O("select","eplusr-viewer__projection"),this.projectionSelect.setAttribute("aria-label","Projection"),this.projectionSelect.append(qe("perspective","Perspective"),qe("orthographic","Orthographic")),this.projectionSelect.addEventListener("change",()=>this.setProjection(this.projectionSelect.value==="orthographic")),this.fitModelButton=O("button","eplusr-viewer__fit-model","Fit model"),this.fitModelButton.type="button",this.fitModelButton.addEventListener("click",()=>this.fitView()),this.fitSelectionButton=O("button","eplusr-viewer__fit-selection","Fit selection"),this.fitSelectionButton.type="button",this.fitSelectionButton.disabled=!0,this.fitSelectionButton.addEventListener("click",()=>{this.selected.size&&this.fitView(this.selected)}),i.append(this.projectionSelect,this.fitModelButton,this.fitSelectionButton);const r=this.toolbarGroup("Display");this.wireButton=this.toggleButton("Edges",!0,ie=>{this.showWireframe=ie,this.updateHelpers()},r),this.xrayButton=this.toggleButton("X-ray",!1,ie=>{this.xray=ie,this.updateMaterials(),this.updateColors()},r),this.axisButton=this.toggleButton("Axes",!0,ie=>{this.showAxis=ie,this.updateHelpers()},r),this.groundButton=this.toggleButton("Ground",!1,ie=>{this.showGround=ie,this.updateHelpers()},r);const a=this.toolbarGroup("Actions");this.measureButton=this.toggleButton("Measure",!1,ie=>this.toggleMeasureMode(ie),a),this.measureButton.title="Measure the distance between two picked points";const o=O("button","","Show all");o.type="button",o.addEventListener("click",()=>this.showAll());const l=O("button","","Snapshot");l.type="button",l.addEventListener("click",()=>this.downloadSnapshot()),this.panelButton=O("button","","Panel"),this.panelButton.type="button",this.panelButton.setAttribute("aria-controls",this.panel.id),this.panelButton.setAttribute("aria-expanded","true"),this.panelButton.addEventListener("click",()=>this.togglePanel()),a.append(o,l,this.panelButton),this.buildTaskContainers();const c=this.panelSection("Visibility filters",!0),h=O("div","eplusr-viewer__search"),d=O("label","eplusr-viewer__field");d.appendChild(O("span","eplusr-viewer__field-title","Name contains")),this.nameFilterInput=O("input"),this.nameFilterInput.type="search",this.nameFilterInput.placeholder="Part of an object name",this.nameFilterInput.setAttribute("aria-label","Name contains"),this.nameFilterInput.setAttribute("aria-describedby",this.panel.id+"-name-filter-status"),this.searchList=O("datalist"),this.searchList.id=this.panel.id+"-name-filter-options",this.nameFilterInput.setAttribute("list",this.searchList.id),d.append(this.nameFilterInput,this.searchList),this.clearNameFilterButton=O("button"),this.clearNameFilterButton.type="button",Kt(this.clearNameFilterButton,"clear","Clear name filter",!0),this.clearNameFilterButton.addEventListener("click",()=>{this.setNameFilter(""),this.nameFilterInput.focus()}),h.append(d,this.clearNameFilterButton),this.nameFilterStatus=O("div","eplusr-viewer__status"),this.nameFilterStatus.id=this.panel.id+"-name-filter-status",this.nameFilterStatus.setAttribute("role","status"),this.nameFilterStatus.setAttribute("aria-live","polite"),c.body.append(h,this.nameFilterStatus),this.nameFilterInput.addEventListener("input",()=>this.setNameFilter(this.nameFilterInput.value)),this.nameFilterInput.addEventListener("keydown",ie=>{ie.key==="Enter"&&ie.preventDefault(),ie.key==="Escape"&&this.nameFilterInput.value&&(ie.preventDefault(),ie.stopPropagation(),this.setNameFilter(""))}),this.selectionModeSelect=this.field("Pick as","select",c.body),this.selectionModeSelect.append(qe("surface","Surfaces"),qe("space","Spaces"),qe("zone","Zones")),this.selectionModeSelect.addEventListener("change",()=>{this.selectionMode=this.selectionModeSelect.value,this.updateViewportFeedback(),this.root.dispatchEvent(new CustomEvent("eplusr-view"))}),this.zoneSelect=this.field("Zone","select",c.body),this.zoneSelect.addEventListener("change",()=>this.setZoneFilter(this.zoneSelect.value)),this.spaceSelect=this.field("Space","select",c.body),this.spaceSelect.addEventListener("change",()=>this.setSpaceFilter(this.spaceSelect.value)),this.panelSection("Display controls",!0,"","display").body.append(r,this.measureButton,l),c.body.append(o);const m=this.panelSection("Saved views",!1,"","display");this.savedViewSelect=this.field("Camera preset","select",m.body),this.savedViewSelect.addEventListener("change",()=>{this.savedViewSelect.value&&this.restoreCameraView(this.savedViewSelect.value)}),this.savedViewName=O("input"),this.savedViewName.placeholder="View name",this.savedViewName.setAttribute("aria-label","Saved view name");const _=O("div","eplusr-viewer__button-row"),x=O("button","","Save current");x.type="button",x.addEventListener("click",()=>this.saveCameraView(this.savedViewName.value));const p=O("button","","Delete");p.type="button",p.addEventListener("click",()=>this.deleteCameraView(this.savedViewSelect.value)),_.append(x,p),m.body.append(this.savedViewName,_),this.savedViewStatus=O("p","eplusr-viewer__status"),this.savedViewStatus.setAttribute("role","status"),m.body.append(this.savedViewStatus),this.populateSavedViews();const f=this.panelSection("Components",!0),M=O("div","eplusr-viewer__checks"),y={floor:"Floors",wall:"Walls",roof:"Roofs / ceilings",window:"Windows",door:"Doors",shading:"Shading",daylighting:"Daylighting points"};this.componentInputs={},fi.forEach(ie=>{const Pe=O("label"),Ke=O("input");Ke.type="checkbox",Ke.checked=!0,Ke.addEventListener("change",()=>{this.remoteFilters.type=null,fi.forEach(ke=>{this.componentVisibility[ke]=this.componentInputs[ke].checked}),this.applyVisibilityChange()}),Pe.append(Ke,document.createTextNode(y[ie])),M.appendChild(Pe),this.componentInputs[ie]=Ke}),f.body.appendChild(M);const E=this.panelSection("Surface appearance",!0,"","display");this.windowStyleSelect=this.field("Window appearance","select",E.body),[["glass","Transparent glass"],["opaque","Opaque colors"]].forEach(([ie,Pe])=>this.windowStyleSelect.appendChild(qe(ie,Pe))),this.windowStyleSelect.value=this.windowStyle,this.windowStyleSelect.addEventListener("change",()=>this.setWindowStyle(this.windowStyleSelect.value)),E.body.appendChild(O("p","eplusr-viewer__status","Glass is not color mapped. Choose Opaque colors to inspect window result colors."));const L=this.panelSection("Clipping and explode",!1,"","display");this.clipAxisSelect=this.field("Plane direction","select",L.body),[["z","Horizontal (Z)"],["x","X plane"],["y","Y plane"]].forEach(([ie,Pe])=>this.clipAxisSelect.appendChild(qe(ie,Pe))),this.clipAxisSelect.addEventListener("change",()=>this.setClipAxis(this.clipAxisSelect.value));const w=O("div","eplusr-viewer__field"),D=O("label");this.clipToggle=O("input"),this.clipToggle.type="checkbox",this.clipToggle.addEventListener("change",()=>{this.clipEnabled=this.clipToggle.checked,this.updateClipping()}),D.append(this.clipToggle,document.createTextNode(" Enable clipping plane")),this.clipSlider=O("input"),this.clipSlider.type="range",this.clipSlider.step="any",this.clipSlider.setAttribute("aria-label","Clipping plane position"),this.clipSlider.addEventListener("input",()=>this.updateClipping()),this.clipValue=O("output","eplusr-viewer__value");const b=O("label");this.clipCapToggle=O("input"),this.clipCapToggle.type="checkbox",this.clipCapToggle.addEventListener("change",()=>{this.clipCap=this.clipCapToggle.checked,this.updateClipCap()}),b.append(this.clipCapToggle,document.createTextNode(" Show section cap")),w.append(D,this.clipSlider,this.clipValue,b),L.body.appendChild(w),this.explodeSelect=this.field("Explode by","select",L.body),[["none","None"],["floor","Floors"],["zone","Zones"]].forEach(([ie,Pe])=>this.explodeSelect.appendChild(qe(ie,Pe))),this.explodeSelect.addEventListener("change",()=>this.setExplode(this.explodeSelect.value,Number(this.explodeSlider.value))),this.explodeSlider=O("input"),this.explodeSlider.type="range",this.explodeSlider.min="0",this.explodeSlider.max="2",this.explodeSlider.step="0.05",this.explodeSlider.value="0",this.explodeSlider.setAttribute("aria-label","Explode distance"),this.explodeSlider.addEventListener("input",()=>this.setExplode(this.explodeSelect.value,Number(this.explodeSlider.value))),L.body.appendChild(this.explodeSlider);const T=this.panelSection("Color mapping",!0,"eplusr-viewer__results","results");this.resultSection=T.details;const U=O("label","eplusr-viewer__field");U.appendChild(O("span","","Variable")),this.variableSelect=O("select"),this.variableSelect.setAttribute("aria-label","Result variable"),this.variableSelect.addEventListener("change",()=>{this.setResultVariable(Number(this.variableSelect.value)).catch(ie=>this.reportResultError(ie))}),U.appendChild(this.variableSelect);const C=O("label","eplusr-viewer__field");C.appendChild(O("span","","Color palette")),this.paletteSelect=O("select"),this.paletteSelect.setAttribute("aria-label","Color palette"),[["viridis","Viridis"],["diverging","Diverging"],["grayscale","Grayscale"],["sequential","Sequential"],["rainbow","Rainbow"]].forEach(([ie,Pe])=>this.paletteSelect.appendChild(qe(ie,Pe))),this.paletteSelect.addEventListener("change",()=>this.setPalette(this.paletteSelect.value)),C.appendChild(this.paletteSelect);const z=O("div","eplusr-viewer__field"),X=O("div","eplusr-viewer__range");this.rangeMinimum=O("input"),this.rangeMaximum=O("input"),[this.rangeMinimum,this.rangeMaximum].forEach(ie=>{ie.type="number",ie.step="any",ie.addEventListener("change",()=>this.setResultRange())}),this.rangeMinimum.setAttribute("aria-label","Minimum display value"),this.rangeMaximum.setAttribute("aria-label","Maximum display value");const j=O("label","eplusr-viewer__field");j.append(O("span","","Min"),this.rangeMinimum);const N=O("label","eplusr-viewer__field");N.append(O("span","","Max"),this.rangeMaximum),X.append(j,N),this.rangeModeSelect=this.field("Range policy","select",z),this.rangeModeSelect.append(qe("all","All frames · fixed"),qe("frame","Current frame"),qe("manual","Manual")),this.rangeModeSelect.addEventListener("change",()=>this.setRangeMode(this.rangeModeSelect.value)),z.appendChild(X);const V=O("div","eplusr-viewer__button-row"),H=O("button","","Reset range");H.type="button",H.addEventListener("click",()=>this.resetResultRange());const K=O("button","","Symmetric");K.type="button",K.addEventListener("click",()=>this.setSymmetricRange()),this.percentileSelect=O("select"),this.percentileSelect.setAttribute("aria-label","Percentile clipping"),[[1,"100%"],[.99,"99%"],[.95,"95%"],[.9,"90%"]].forEach(([ie,Pe])=>this.percentileSelect.appendChild(qe(ie,Pe))),this.percentileSelect.addEventListener("change",()=>this.setPercentileRange(Number(this.percentileSelect.value))),V.append(H,K);const ee=O("label","eplusr-viewer__field");ee.append(O("span","","Percentile coverage"),this.percentileSelect),this.rangeStatus=O("div","eplusr-viewer__status"),this.rangeStatus.setAttribute("role","status"),z.append(ee,this.rangeStatus),z.appendChild(V);const ce=O("label","eplusr-viewer__field eplusr-viewer__color-field");ce.appendChild(O("span","","Missing-value color")),this.missingColorInput=O("input"),this.missingColorInput.type="color",this.missingColorInput.value="#b8bdc5",this.missingColorInput.setAttribute("aria-label","Missing-value color"),this.missingColorInput.addEventListener("input",()=>this.setMissingColor(this.missingColorInput.value)),ce.appendChild(this.missingColorInput);const be=O("div","eplusr-viewer__playback");this.previousTimeButton=O("button","","Previous"),this.previousTimeButton.type="button",this.previousTimeButton.addEventListener("click",()=>this.stepTime(-1)),this.playButton=O("button","","Play"),this.playButton.type="button",this.playButton.addEventListener("click",()=>this.playbackTimer?this.pause():this.play()),this.nextTimeButton=O("button","","Next"),this.nextTimeButton.type="button",this.nextTimeButton.addEventListener("click",()=>this.stepTime(1)),this.speedSelect=O("select"),this.speedSelect.setAttribute("aria-label","Playback speed"),[[.5,"0.5×"],[1,"1×"],[2,"2×"],[5,"5×"],[10,"10×"]].forEach(([ie,Pe])=>this.speedSelect.appendChild(qe(ie,Pe))),this.speedSelect.value=String(this.playbackSpeed),this.speedSelect.addEventListener("change",()=>{this.playbackSpeed=Number(this.speedSelect.value),this.playbackTimer&&this.play(),this.savePreferences()}),be.append(this.previousTimeButton,this.playButton,this.nextTimeButton,this.speedSelect),this.aggregationSelect=O("select"),this.aggregationSelect.setAttribute("aria-label","Time aggregation"),[["frame","Original frames"],["day","Daily mean"],["month","Monthly mean"]].forEach(([ie,Pe])=>this.aggregationSelect.appendChild(qe(ie,Pe))),this.aggregationSelect.addEventListener("change",()=>{this.setAggregation(this.aggregationSelect.value).catch(ie=>this.reportResultError(ie))}),this.timeJumpInput=O("input"),this.timeJumpInput.type="search",this.timeJumpInput.placeholder="Jump to time",this.timeJumpInput.setAttribute("aria-label","Jump to result time"),this.timeJumpInput.addEventListener("keydown",ie=>{ie.key==="Enter"&&this.jumpToTime(this.timeJumpInput.value)}),this.timeField=O("div","eplusr-viewer__field eplusr-viewer__time"),this.timeLabel=O("span","","Time"),this.timeSlider=O("input"),this.timeSlider.type="range",this.timeSlider.min="0",this.timeSlider.step="1",this.timeSlider.setAttribute("aria-label","Result time frame"),this.timeSlider.addEventListener("input",()=>{this.setTime(Number(this.timeSlider.value)).catch(ie=>this.reportResultError(ie))}),this.frameCounter=O("span","eplusr-viewer__frame-count");const Me=O("div","eplusr-viewer__timeline-heading");Me.append(U,this.timeLabel,this.frameCounter),this.timeField.append(be,this.timeSlider,this.speedSelect),this.timeline.append(Me,this.timeField),this.resultStatus=O("div","eplusr-viewer__status"),this.resultStatus.setAttribute("role","status"),this.resultStatus.setAttribute("aria-live","polite");const He=O("label","eplusr-viewer__field");He.append(O("span","","Time aggregation"),this.aggregationSelect);const Ze=O("label","eplusr-viewer__field");Ze.append(O("span","","Jump to time"),this.timeJumpInput),T.body.append(C,z,ce,He,Ze),this.resultEmpty=O("p","eplusr-viewer__status","Attach existing results from R to color surfaces and browse time frames."),this.tabBodies.results.appendChild(this.resultEmpty),this.timeline.appendChild(this.resultStatus),this.queryPanel=new Bg(this);const De=this.panelSection("Geometry diagnostics",!1,"eplusr-viewer__diagnostics");this.diagnosticSection=De.details,this.diagnosticEmpty=O("p","eplusr-viewer__status","Geometry diagnostics are not attached to this widget."),De.body.append(this.diagnosticEmpty);const Z=O("label");this.diagnosticToggle=O("input"),this.diagnosticToggle.type="checkbox",this.diagnosticToggle.addEventListener("change",()=>{this.showDiagnostics=this.diagnosticToggle.checked,this.updateHelpers()}),Z.append(this.diagnosticToggle,document.createTextNode(" Show nearby vertex candidates"));const ue=O("label");this.diagnosticOnlyToggle=O("input"),this.diagnosticOnlyToggle.type="checkbox",this.diagnosticOnlyToggle.addEventListener("change",()=>{this.diagnosticOnly=this.diagnosticOnlyToggle.checked,this.applyVisibilityChange({fit:!0})}),ue.append(this.diagnosticOnlyToggle,document.createTextNode(" Only show affected objects")),this.diagnosticTypeSelect=O("select"),this.diagnosticTypeSelect.setAttribute("aria-label","Diagnostic issue type"),this.diagnosticTypeSelect.addEventListener("change",()=>this.renderDiagnostics()),this.diagnosticSummary=O("div","eplusr-viewer__diagnostic-summary"),this.diagnosticList=O("div","eplusr-viewer__diagnostic-list"),De.body.append(Z,ue,this.diagnosticTypeSelect,this.diagnosticSummary,this.diagnosticList);const te=this.panelSection("Selection",!0,"","properties");this.selectionActions=O("div","eplusr-viewer__button-row eplusr-viewer__selection-actions");const Te=O("button","","Isolate");Te.type="button",Te.addEventListener("click",()=>this.isolateSelection());const Le=O("button","","Hide");Le.type="button",Le.addEventListener("click",()=>this.hideSelection());const Ae=O("button","","Clear selection");Ae.type="button",Ae.addEventListener("click",()=>this.clearSelection());const tt=O("button","","Exit isolation");tt.type="button",tt.addEventListener("click",()=>this.exitIsolation()),this.selectionActionButtons={isolate:Te,hide:Le,clear:Ae,exitIsolation:tt},this.selectionActions.append(Te,Le,Ae,tt),this.isolationStatus=O("div","eplusr-viewer__isolation-status","Model"),this.isolationStatus.setAttribute("role","status"),this.isolationStatus.setAttribute("aria-live","polite"),this.selectionPanel=O("div","eplusr-viewer__selection","Click a surface to inspect its properties; double-click to select its Zone, then double-click again to select a surface."),this.selectionPanel.setAttribute("role","status"),this.selectionPanel.setAttribute("aria-live","polite"),this.measureStatus=O("div","eplusr-viewer__measure-status","Measurement is off."),te.body.append(this.isolationStatus,this.selectionActions,this.selectionPanel),this.buildViewportFeedback(),this.navigation=new Ug(this),this.ribbon=new Ng(this,{search:c.details,components:f.details,saved:m.details,clipping:L.details,diagnostics:De.details,selection:te.details,results:T.details},{reset:o,capture:l}),this.short&&this.ribbon.collapse(!0),window.ResizeObserver&&(this.layoutObserver=new ResizeObserver(()=>this.resize()),this.layoutObserver.observe(this.root),this.layoutObserver.observe(this.canvasHost),this.layoutObserver.observe(this.legend))}buildTaskContainers(){this.tabBodies=Object.fromEntries(["objects","display","results","properties"].map(e=>[e,O("div")]))}setPanelTab(e){this.ribbon?.open(e)}toolbarGroup(e){const t=O("div","eplusr-viewer__toolbar-group");return t.setAttribute("role","group"),t.setAttribute("aria-label",e),this.toolbar.appendChild(t),t}panelSection(e,t=!0,i="",s="objects"){const r=O("details",`eplusr-viewer__section ${i}`.trim());r.open=t,r.appendChild(O("summary","",e));const a=O("div","eplusr-viewer__section-body");return r.appendChild(a),this.tabBodies[s].appendChild(r),{details:r,body:a}}toggleButton(e,t,i,s=this.toolbar){const r=O("button","",e);return r.type="button",r.setAttribute("aria-pressed",String(t)),r.addEventListener("click",()=>{const a=r.getAttribute("aria-pressed")!=="true";r.setAttribute("aria-pressed",String(a)),i(a)}),s.appendChild(r),r}field(e,t,i=this.panel){const s=O("label","eplusr-viewer__field");s.appendChild(O("span","",e));const r=O(t);return r.setAttribute("aria-label",e),s.appendChild(r),i.appendChild(s),r}load(e){this.pause(),this.resultAbortController&&this.resultAbortController.abort(),this.northArrow&&(this.scene.remove(this.northArrow),this.northArrow=null),this.northLabel&&(this.scene.remove(this.northLabel),this.northLabel.material.map.dispose(),this.northLabel.material.dispose(),this.northLabel=null),this.payload=e,this.zoneFilter="",this.spaceFilter="",this.remoteFilters={},this.objects=e.objects??[],this.positions=Jn(e.geometry?.positions),this.faceObjects=za(e.geometry?.face_objects),this.edgePositions=Jn(e.geometry?.edge_positions),this.edgeObjects=za(e.geometry?.edge_objects),this.pointPositions=Jn(e.geometry?.point_positions),this.sharedBoundaryPairs=Vg(this.objects,this.positions,this.faceObjects,this.edgePositions,this.edgeObjects),this.glazingObjects=new Set(this.objects.flatMap((s,r)=>Xg(s)?[r]:[]));const t=new Map(this.objects.map((s,r)=>[s.name?.toLowerCase(),r]));this.windowParents=new Map([...this.glazingObjects].map(s=>[s,t.get(this.objects[s].building_surface_name?.toLowerCase())])),this.computeObjectMetrics(),this.setResults(e.results,!1),this.diagnostics={...e.diagnostics??{},pointPositions:Jn(e.diagnostics?.point_positions),objectIndices:za(e.diagnostics?.object_indices),issues:e.diagnostics?.issues??[]},this.diagnosticObjectSet=new Set(this.diagnostics.objectIndices??[]),this.showDiagnostics=!!(this.diagnostics.enabled&&this.diagnostics.count),this.diagnosticOnly=!1,this.renderBy=e.options?.render_by??"surface_type",this.xray=!!e.options?.x_ray,this.showWireframe=e.options?.wireframe!==!1,this.showAxis=e.options?.axis!==!1,this.showGround=!!e.options?.ground,this.scene.background=new Re(e.options?.background??"#f3f5f7"),this.selected.clear(),this.hidden.clear(),this.isolated=null,this.isolationHistory=[],this.measurePoints=[],this.measurement=null,this.renderSelect.value=this.renderBy,this.resultSet.length&&!Array.from(this.renderSelect.options).some(s=>s.value==="result")&&this.renderSelect.appendChild(qe("result","Results")),this.renderSelect.value=this.renderBy,this.wireButton.setAttribute("aria-pressed",String(this.showWireframe)),this.xrayButton.setAttribute("aria-pressed",String(this.xray)),this.axisButton.setAttribute("aria-pressed",String(this.showAxis)),this.groundButton.setAttribute("aria-pressed",String(this.showGround)),this.populateFilters(),this.loadPreferences(),this.rebuildGeometry(),this.configureResultControls(),this.configureClipRange();const i=e.options?.viewpoint??{};Number.isFinite(i.fov)&&(this.perspectiveCamera.fov=i.fov,this.perspectiveCamera.updateProjectionMatrix()),this.viewSelect.value=i.look_at??"iso",this.setView(this.viewSelect.value),this.fitView(),this.results?.mode==="provider"&&this.setTime(0).catch(s=>this.reportResultError(s)),this.updateSelectionPanel(),this.clearMeasurement(),this.toggleMeasureMode(!1),this.renderDiagnostics(),this.ribbon.browser.build(),this.resize()}setResults(e,t=!0){this.queryPanel.configure(e?.query??null),this.pause(),this.resultAbortController?.abort();const i=e?.variables??(e?[e]:[]);if(this.resultSet=i.map(s=>{const r=Jn(s.values),a=Array.isArray(s.times)?s.times:s.times==null?[]:[s.times];return{...s,times:a,values:r,sourceValues:r.slice(),sourceTimes:[...a],sourceRange:[...s.range??Dn(r)],rangeMode:"all",coverage:1,manualRange:null,loadedFrame:null,aggregation:"frame",frameGroups:a.map((o,l)=>[l]),cache:new Map,missingColor:"#b8bdc5"}}),this.resultVariableIndex=Math.max(0,Math.min(Number(e?.active??0),Math.max(0,this.resultSet.length-1))),this.results=this.resultSet[this.resultVariableIndex]??null,!!t&&(this.frame=0,this.resultSet.length&&!Array.from(this.renderSelect.options).some(s=>s.value==="result")&&this.renderSelect.appendChild(qe("result","Results")),this.renderBy=this.resultSet.length?"result":"surface_type",this.renderSelect.value=this.renderBy,this.configureResultControls(),this.updateColors(),this.results?.mode==="provider"))return this.setTime(0)}readState(){return{selected_ids:[...this.selected].map(e=>String(this.objects[e].id)),visible_ids:[...this.visibleObjects].map(e=>String(this.objects[e].id)),render_by:this.renderBy,selection_mode:this.selectionMode,zone:this.zoneFilter,space:this.spaceFilter,filters:this.effectiveFilters(),frame:this.frame,variable:this.resultVariableIndex,x_ray:this.xray,wireframe:this.showWireframe,window_style:this.windowStyle,axis:this.showAxis,ground:this.showGround,timeline_visible:!this.timeline.hidden,model_only:!!this.ribbon?.clean,ribbon_tab:this.ribbon?.active??null,task_panel:this.ribbon?.activeTask??null,model_browser_visible:!!this.ribbon?.browserVisible,tool:this.measureMode?"measure":"inspect",measurement:this.measurement?.distance??null,projection:this.camera===this.orthographicCamera?"orthographic":"perspective",view:this.viewSelect.value,result_query:this.queryPanel.applied??null,range_mode:this.results?.rangeMode??null,result_range:this.results?.range??null,camera:{position:this.camera.position.toArray(),target:this.controls.target.toArray()}}}selectionGroup(e){const t=this.selectionMode==="zone"?"zone_name":this.selectionMode==="space"?"space_name":null,i=t?this.objects[e]?.[t]:null;return!t||!i?[e]:this.objects.flatMap((s,r)=>s[t]===i&&this.isObjectVisible(r)?[r]:[])}computeObjectMetrics(){const e=this.objects.map(()=>new A),t=new Uint32Array(this.objects.length);for(let o=0;o<this.faceObjects.length;o+=1){const l=this.faceObjects[o],c=o*9;for(let h=0;h<3;h+=1)e[l].add(new A(this.positions[c+h*3],this.positions[c+h*3+1],this.positions[c+h*3+2])),t[l]+=1}this.objectCenters=e.map((o,l)=>t[l]?o.multiplyScalar(1/t[l]):new A),this.modelCenter=this.objectCenters.reduce((o,l,c)=>t[c]?o.add(l):o,new A);const i=t.reduce((o,l)=>o+(l?1:0),0);i&&this.modelCenter.multiplyScalar(1/i);const s=[...new Set(this.objectCenters.filter((o,l)=>t[l]).map(o=>Math.round(o.z*100)/100))].sort((o,l)=>o-l);this.floorOffsets=this.objectCenters.map(o=>s.indexOf(Math.round(o.z*100)/100)-(s.length-1)/2);const r=new Map,a=new Map;this.objects.forEach((o,l)=>{const c=o.zone_name;!c||!t[l]||(r.has(c)||r.set(c,new A),r.get(c).add(this.objectCenters[l]),a.set(c,(a.get(c)??0)+1))}),this.zoneDirections=new Map,r.forEach((o,l)=>{const h=o.multiplyScalar(1/a.get(l)).clone().sub(this.modelCenter);h.lengthSq()<1e-10&&h.set(1,0,0),h.z=0,this.zoneDirections.set(l,h.normalize())}),this.modelRadius=Math.max(1,this.visibleBoxFromSource().getSize(new A).length()/2)}visibleBoxFromSource(){const e=new Ni;for(let t=0;t<this.positions.length;t+=3)e.expandByPoint(new A(this.positions[t],this.positions[t+1],this.positions[t+2]));return e.isEmpty()&&e.expandByPoint(new A).expandByScalar(1),e}objectOffset(e){if(!this.explodeAmount||this.explodeMode==="none")return new A;const t=this.modelRadius*.16*this.explodeAmount;return this.explodeMode==="floor"?new A(0,0,(this.floorOffsets[e]??0)*t):this.explodeMode==="zone"?(this.zoneDirections.get(this.objects[e]?.zone_name)??new A).clone().multiplyScalar(t):new A}setExplode(e,t){this.clearMeasurement(),this.explodeMode=e,this.explodeAmount=Number.isFinite(t)?t:0,this.rebuildGeometry(),this.configureClipRange(),this.fitView()}populateFilters(){const e=i=>[...new Set(this.objects.map(s=>s[i]).filter(Boolean))].sort();this.zoneSelect.replaceChildren(qe("","All zones")),e("zone_name").forEach(i=>this.zoneSelect.appendChild(qe(i))),this.populateSpaceFilter();const t=[...new Set(this.objects.map(i=>i.name).filter(Boolean))].sort();this.searchList.replaceChildren(...t.slice(0,5e3).map(i=>qe(i))),this.updateNameFilterStatus()}populateSpaceFilter(){const e=[...new Set(this.objects.filter(t=>this.remoteFilters.zone?this.remoteFilters.zone.includes(String(t.zone_name??"").toLowerCase()):!this.zoneFilter||t.zone_name===this.zoneFilter).map(t=>t.space_name).filter(Boolean))].sort();this.spaceFilter&&!e.includes(this.spaceFilter)&&(this.spaceFilter=""),this.spaceSelect.replaceChildren(qe("","All spaces"),...e.map(t=>qe(t))),this.spaceSelect.value=this.spaceFilter}effectiveFilters(){return{...this.remoteFilters,name:this.remoteFilters.name??(this.nameFilter.trim()?[this.nameFilter.trim().toLowerCase()]:null),zone:this.remoteFilters.zone??(this.zoneFilter?[this.zoneFilter.toLowerCase()]:null),space:this.remoteFilters.space??(this.spaceFilter?[this.spaceFilter.toLowerCase()]:null),type:fi.every(e=>this.componentVisibility[e])?null:fi.filter(e=>this.componentVisibility[e])}}syncFilterUi(){if(!this.filterChips)return;const e=this.effectiveFilters(),t=(r,a)=>{const o={zone:"zone_name",space:"space_name",surface:"name"}[r];return a.map(l=>this.objects.find(c=>String(c[o]??"").toLowerCase()===l)?.[o]??l)};["zone","space"].forEach(r=>{const a=r==="zone"?this.zoneSelect:this.spaceSelect;a.querySelectorAll("[data-filter-summary]").forEach(l=>l.remove());const o=e[r];if(o?.length===1){const l=t(r,o)[0];[...a.options].some(c=>c.value===l)||a.appendChild(qe(l)),a.value=l}else if(o){const l=qe("__multiple__",o.length+" "+r+"s selected");l.disabled=!0,l.dataset.filterSummary="true",a.appendChild(l),a.value=l.value}else a.value=""}),fi.forEach(r=>{this.componentInputs[r].checked=this.componentVisibility[r]}),this.filterChips.replaceChildren();const i=(r,a)=>{const o=O("button","eplusr-viewer__filter-chip",r+" ×");o.type="button",o.title=r,o.setAttribute("aria-label","Remove filter: "+r),o.replaceChildren(O("span","",r),Zt("clear")),o.addEventListener("click",a),this.filterChips.appendChild(o)};["zone","space","surface"].forEach(r=>{const a=e[r];a&&i(r[0].toUpperCase()+r.slice(1)+": "+(t(r,a).join(", ")||"None"),()=>{this.remoteFilters[r]=null,r==="zone"&&(this.zoneFilter=""),r==="space"&&(this.spaceFilter=""),this.populateSpaceFilter(),this.applyVisibilityChange({fit:!0})})}),e.name&&i("Name contains: "+(this.nameFilter||e.name.join(", ")),()=>this.setNameFilter(""));const s=this.remoteFilters.name?.[0]??this.nameFilter;this.nameFilterInput.value!==s&&(this.nameFilterInput.value=s),this.clearNameFilterButton.hidden=!e.name,this.updateNameFilterStatus(),e.type&&i("Types: "+(e.type.join(", ")||"None"),()=>{this.remoteFilters.type=null,fi.forEach(r=>{this.componentVisibility[r]=!0}),this.applyVisibilityChange({fit:!0})}),this.hidden.size&&i(this.hidden.size+" hidden",()=>{this.hidden.clear(),this.applyVisibilityChange({fit:!0})}),this.isolated&&i("Isolated: "+this.isolated.size,()=>{this.isolated=null,this.isolationHistory=[],this.applyVisibilityChange({fit:!0})}),this.diagnosticOnly&&i("Affected objects",()=>{this.diagnosticOnly=!1,this.diagnosticOnlyToggle.checked=!1,this.applyVisibilityChange({fit:!0})}),this.visibleCount.replaceChildren(Zt("filters"),document.createTextNode(this.visibleObjects.size+" / "+this.objects.length+" visible")),Kt(this.selectionCount,"isolate",this.selected.size+" selected"),this.selectionCount.disabled=!this.selected.size,this.clearFiltersButton.hidden=!this.filterChips.children.length,this.ribbon?.browser.sync()}clearFilters(){this.remoteFilters={},this.nameFilter="",this.zoneFilter="",this.spaceFilter="",this.hidden.clear(),this.isolated=null,this.isolationHistory=[],this.diagnosticOnly=!1,this.diagnosticOnlyToggle.checked=!1,fi.forEach(e=>{this.componentVisibility[e]=!0}),this.populateSpaceFilter(),this.applyVisibilityChange({fit:!0})}setZoneFilter(e){this.remoteFilters.zone=null,this.remoteFilters.space=null,this.zoneFilter=e,this.populateSpaceFilter(),this.applyVisibilityChange({fit:!0})}setSpaceFilter(e){if(this.remoteFilters.space=null,this.spaceFilter=e,e){const t=[...new Set(this.objects.filter(i=>i.space_name===e).map(i=>i.zone_name).filter(Boolean))];t.length===1&&this.zoneFilter!==t[0]&&(this.remoteFilters.zone=null,this.zoneFilter=t[0],this.zoneSelect.value=t[0],this.populateSpaceFilter(),this.spaceSelect.value=e)}this.applyVisibilityChange({fit:!0})}applyVisibilityChange({fit:e=!1,preserveSelection:t=!1}={}){this.rebuildGeometry();const i=this.selected.size;t||(this.selected=new Set([...this.selected].filter(s=>this.isObjectVisible(s)))),e&&this.fitView(),this.updateColors(),this.selected.size!==i?this.emitSelection():this.updateSelectionPanel()}setNameFilter(e){this.nameFilter=e,this.remoteFilters.name=null,this.applyVisibilityChange(),this.root.dispatchEvent(new CustomEvent("eplusr-view"))}updateNameFilterStatus(){const e=this.effectiveFilters().name;if(this.nameFilterStatus.hidden=!e,!e)return;const t=this.objects.flatMap((r,a)=>Vl(r,e)?[a]:[]),i=t.filter(r=>this.isObjectVisible(r)),s=[...new Set(t.flatMap(r=>this.visibilityReasons(r)))];this.nameFilterStatus.textContent=t.length?i.length+" of "+t.length+" name matches visible"+(i.length<t.length?" · "+s.join(" · "):""):"No object names match.",this.nameFilterStatus.dataset.state=i.length?"matches":"excluded"}togglePanel(e=null){const t=this.ribbon.activeTask==="properties";e===!0||e===null&&t?this.ribbon.closeEditors():t||this.ribbon.open("properties")}configureResultControls(){this.rangeMinimum.removeAttribute("aria-invalid"),this.rangeMaximum.removeAttribute("aria-invalid"),this.rangeStatus.textContent="",this.resultSection.style.display=this.resultSet.length?"block":"none",this.ribbon?this.ribbon.syncResults():this.timeline.hidden=!0,this.resultEmpty.hidden=!!this.resultSet.length,this.variableSelect.replaceChildren(),this.resultSet.forEach((i,s)=>{this.variableSelect.appendChild(qe(s,(i.label??`Variable ${s+1}`)+(i.units?` (${Ln(i.units)})`:"")))}),this.variableSelect.value=String(this.resultVariableIndex),this.timelineVariableLabel&&(this.timelineVariableLabel.textContent=this.variableSelect.selectedOptions[0]?.textContent??"Results");const e=this.results?.times?.length??0;if(this.timeField.style.display="flex",this.timeSlider.disabled=e<2,this.frameCounter.textContent=e?`${this.frame+1} / ${e}`:"",this.timeSlider.max=String(Math.max(0,e-1)),this.timeSlider.value=String(Math.min(this.frame,Math.max(0,e-1))),e&&(this.timeLabel.textContent=Va(this.results,this.frame)),this.results){this.paletteSelect.value=this.results.palette??"viridis",this.missingColorInput.value=this.results.missingColor??"#b8bdc5",this.aggregationSelect.value=this.results.aggregation??"frame";const i=this.results.range??Dn(this.results.values);this.rangeMinimum.value=String(i[0]),this.rangeMaximum.value=String(i[1])}this.previousTimeButton.disabled=e<2,this.nextTimeButton.disabled=e<2,this.playButton.disabled=e<2,this.timeJumpInput.value=e?String(this.results.times[this.frame]):"",this.resultStatus.textContent=this.results?"":"No simulation results attached.",this.speedSelect.disabled=e<2,this.syncRangeControls();const t=Number(this.diagnostics?.count??0);this.diagnosticSection.style.display="block",this.diagnosticEmpty.hidden=!!this.diagnostics?.enabled,this.diagnosticToggle.disabled=!this.diagnostics?.enabled||t===0,this.diagnosticTypeSelect.disabled=!this.diagnostics?.enabled||t===0,this.diagnosticToggle.checked=this.showDiagnostics,this.diagnosticOnlyToggle.checked=this.diagnosticOnly,this.diagnosticOnlyToggle.disabled=t===0,this.renderDiagnostics()}configureClipRange(){const e=this.visibleBox(),t=Number.isFinite(e.min[this.clipAxis])?e.min[this.clipAxis]:0,i=Number.isFinite(e.max[this.clipAxis])?e.max[this.clipAxis]:1;this.clipSlider.min=String(t),this.clipSlider.max=String(i),this.clipSlider.value=String(i),this.clipValue.textContent=At(i),this.updateClipping()}diagnosticIssues(){if(Array.isArray(this.diagnostics?.issues)&&this.diagnostics.issues.length)return this.diagnostics.issues;const e=[],t=this.diagnostics?.objectIndices??new Int32Array,i=this.diagnostics?.pointPositions??new Float32Array;for(let s=0;s+1<t.length;s+=2){const r=s/2*3;e.push({type:"near_vertex",severity:"warning",label:"Nearby vertices may indicate an adjacency gap",object_indices:[t[s],t[s+1]],point:Array.from(i.slice(r,r+3))})}return e}renderDiagnostics(){if(!this.diagnosticTypeSelect)return;const e=this.diagnosticIssues(),t=this.diagnosticTypeSelect.value,i=[...new Set(e.map(o=>o.type))].sort(),s={near_vertex:"Adjacency gaps",nonplanar:"Non-planar surfaces",duplicate:"Duplicate surfaces",overlap:"Coplanar overlaps",reversed_normal:"Reversed normals",subsurface_outside:"Subsurfaces outside parents",self_intersection:"Self-intersections"};this.diagnosticTypeSelect.replaceChildren(qe("","All issue types"),...i.map(o=>qe(o,s[o]??o))),i.includes(t)&&(this.diagnosticTypeSelect.value=t);const r=this.diagnosticTypeSelect.value,a=r?e.filter(o=>o.type===r):e;this.diagnosticObjectSet=new Set(a.flatMap(o=>o.object_indices??[])),this.diagnosticSummary.hidden=!this.diagnostics?.enabled,this.diagnosticSummary.textContent=e.length?`${a.length} of ${e.length} issues shown (tolerance ${this.diagnostics.tolerance} m).`:"No geometry issues found.",this.diagnosticList.replaceChildren(),a.slice(0,200).forEach((o,l)=>{const c=O("button",`eplusr-viewer__diagnostic-item eplusr-viewer__diagnostic-item--${o.severity??"info"}`);c.type="button",c.append(O("span","eplusr-viewer__diagnostic-type",s[o.type]??o.type),O("span","",o.label??"Geometry issue")),c.addEventListener("click",()=>this.selectDiagnosticIssue(a[l])),this.diagnosticList.appendChild(c)}),a.length>200&&this.diagnosticList.appendChild(O("div","eplusr-viewer__status",`${a.length-200} additional issues are not listed.`)),this.diagnosticOnlyToggle.disabled=a.length===0,this.rebuildDiagnostics(),this.diagnosticOnly&&this.applyVisibilityChange({fit:!0})}selectDiagnosticIssue(e){const t=(e?.object_indices??[]).filter(i=>this.objects[i]);t.length&&(this.remoteFilters={},this.zoneFilter="",this.spaceFilter="",this.zoneSelect.value="",this.populateSpaceFilter(),this.hidden.clear(),this.isolated=null,this.isolationHistory=[],this.selected=new Set(t),this.rebuildGeometry(),this.fitView(t),this.emitSelection())}setClipAxis(e){this.clipAxis=e,this.configureClipRange()}visibilityReasons(e){return mm(this.objects[e],e,{filters:this.effectiveFilters(),componentVisibility:this.componentVisibility,hidden:this.hidden,isolated:this.isolated,diagnosticOnly:this.diagnosticOnly,diagnosticObjectSet:this.diagnosticObjectSet})}isObjectVisible(e){return this.visibilityReasons(e).length===0}rebuildGeometry(){const e=this.objects.map((r,a)=>this.objectOffset(a));this.currentOffsets=e,this.visibleFaceObjects=Array.from(this.faceObjects),this.visibleObjects=new Set,(!this.mesh||this.mesh.geometry.getAttribute("position").array.length!==this.positions.length)&&this.initializeGeometryObjects();const t=this.mesh.geometry.getAttribute("position"),i=t.array;for(let r=0;r<this.faceObjects.length;r+=1){const a=this.faceObjects[r],o=r*9;if(!this.isObjectVisible(a)){i.fill(0,o,o+9);continue}const l=e[a];for(let c=0;c<9;c+=1)i[o+c]=this.positions[o+c]+l.getComponent(c%3);this.visibleObjects.add(a)}t.needsUpdate=!0,this.mesh.geometry.computeVertexNormals();const s=this.visibleBox();this.mesh.geometry.boundingBox=s,this.mesh.geometry.boundingSphere=s.getBoundingSphere(new Sn),this.rebuildEdges(),this.rebuildSelectionOutline(),this.rebuildPoints(),this.rebuildDiagnostics(),this.updateColors(),this.updateMaterials(),this.updateHelpers(),this.updateClipping(),this.syncFilterUi(),this.requestRender()}updateGeometryGroups(){if(!this.mesh)return;this.glassIndices=this.windowStyle==="glass"&&this.renderBy!=="normal"?this.glazingObjects:new Set;const e=Gg(this.faceObjects,this.sharedBoundaryPairs??new Int32Array,this.visibleObjects,this.currentOffsets??[],this.glassIndices);this.activeSharedBoundaries=e.active,this.mesh.geometry.clearGroups();for(const t of e.groups)this.mesh.geometry.addGroup(t.start,t.count,t.materialIndex)}initializeGeometryObjects(){this.mesh&&(this.scene.remove(this.mesh,this.backMesh),this.mesh.geometry.dispose(),this.surfaceMaterial?.dispose(),this.resultMaterial?.dispose(),this.backMaterial.dispose(),this.pairedSurfaceMaterial?.dispose(),this.pairedResultMaterial?.dispose(),this.hiddenBackMaterial?.dispose(),this.glassMaterial?.dispose(),this.pairedGlassMaterial?.dispose());const e=new _t,t=new Lt(new Float32Array(this.positions.length),3);t.setUsage(hs);const i=new Lt(new Float32Array(this.positions.length),3);i.setUsage(hs),e.setAttribute("position",t),e.setAttribute("color",i),this.visibleVertexObjects=new Int32Array(this.faceObjects.length*3);for(let o=0;o<this.faceObjects.length;o+=1)this.visibleVertexObjects.fill(this.faceObjects[o],o*3,o*3+3);this.surfaceMaterial=new Bh({vertexColors:!0,side:It,polygonOffset:!0,polygonOffsetFactor:1,polygonOffsetUnits:1,roughness:.92,metalness:0,transparent:this.xray,opacity:this.xray?.34:1,depthWrite:!this.xray}),this.resultMaterial=new Bi({polygonOffset:!0,polygonOffsetFactor:1,polygonOffsetUnits:1,vertexColors:!0,side:It,transparent:this.xray,opacity:this.xray?.34:1,depthWrite:!this.xray}),this.pairedSurfaceMaterial=this.surfaceMaterial.clone(),this.pairedSurfaceMaterial.side=vt,this.pairedResultMaterial=this.resultMaterial.clone(),this.pairedResultMaterial.side=vt,this.glassMaterial=new Bi({color:Hg,side:It,transparent:!0,opacity:Wg,depthWrite:!1,polygonOffset:!0,polygonOffsetFactor:1,polygonOffsetUnits:1}),this.pairedGlassMaterial=this.glassMaterial.clone(),this.pairedGlassMaterial.side=vt,this.material=this.surfaceMaterial,this.pairedMaterial=this.pairedSurfaceMaterial,this.mesh=new Ht(e,[this.material,this.pairedMaterial,this.glassMaterial,this.pairedGlassMaterial]),this.mesh.name="EnergyPlus surfaces",this.backMaterial=new Bi({color:"#d34747",side:vt,polygonOffset:!0,polygonOffsetFactor:1,polygonOffsetUnits:1}),this.hiddenBackMaterial=new Bi({visible:!1}),this.backMesh=new Ht(e,[this.backMaterial,this.hiddenBackMaterial]),this.scene.add(this.mesh,this.backMesh),this.edges&&(this.scene.remove(this.edges),this.edges.geometry.dispose(),this.edges.material.dispose());const s=new _t,r=new Lt(new Float32Array(this.edgePositions.length),3);r.setUsage(hs),s.setAttribute("position",r);const a=new Lt(new Float32Array(this.edgePositions.length),3);a.setUsage(hs),s.setAttribute("color",a),this.edges=new Ns(s,new yn({vertexColors:!0,color:"#ffffff"})),this.scene.add(this.edges)}rebuildEdges(){const e=this.edges.geometry.getAttribute("position"),t=e.array;for(let i=0;i<this.edgeObjects.length;i+=1){const s=this.edgeObjects[i],r=i*6;if(!this.isObjectVisible(s)){t.fill(0,r,r+6);continue}const a=this.currentOffsets?.[s]??new A;for(let o=0;o<6;o+=1)t[r+o]=this.edgePositions[r+o]+a.getComponent(o%3)}e.needsUpdate=!0,this.requestRender()}rebuildSelectionOutline(){const e=[];for(let i=0;i<this.edgeObjects.length;i+=1){const s=this.edgeObjects[i];if(!this.selected.has(s)||!this.isObjectVisible(s))continue;const r=i*6,a=this.currentOffsets?.[s]??new A;for(let o=0;o<6;o+=1)e.push(this.edgePositions[r+o]+a.getComponent(o%3))}this.selectionOutline&&(this.scene.remove(this.selectionOutline),this.selectionOutline.geometry.dispose(),this.selectionOutline.material.dispose());const t=new _t;t.setAttribute("position",new Ft(e,3)),this.selectionOutline=new Ns(t,new yn({color:"#ff8a00",depthTest:!1,transparent:!0,opacity:1})),this.selectionOutline.renderOrder=20,this.scene.add(this.selectionOutline),this.requestRender()}rebuildPoints(){if(!this.points){const e=new _t;e.setAttribute("position",new Lt(this.pointPositions,3)),this.points=new Ta(e,new Us({color:"#e63946",size:7,sizeAttenuation:!1})),this.scene.add(this.points)}this.points.visible=this.componentVisibility.daylighting,this.requestRender()}rebuildDiagnostics(){this.diagnosticPoints&&(this.scene.remove(this.diagnosticPoints),this.diagnosticPoints.geometry.dispose(),this.diagnosticPoints.material.dispose());const e=this.diagnosticTypeSelect?.value??"",t=this.diagnosticIssues().filter(r=>!e||r.type===e).flatMap(r=>{if(!Array.isArray(r.point)||r.point.length!==3)return[];const a=r.object_indices?.[0],o=Number.isInteger(a)?this.currentOffsets?.[a]??this.objectOffset(a):new A;return r.point.map((l,c)=>l+o.getComponent(c))}),i=t.length?new Float32Array(t):this.diagnostics?.pointPositions??new Float32Array,s=new _t;s.setAttribute("position",new Lt(i,3)),this.diagnosticPoints=new Ta(s,new Us({color:"#dc2626",size:10,sizeAttenuation:!1,depthTest:!1})),this.diagnosticPoints.renderOrder=10,this.diagnosticPoints.visible=this.showDiagnostics,this.scene.add(this.diagnosticPoints)}updateColors(){if(!this.mesh)return;this.updateGeometryGroups();const e=this.mesh.geometry.getAttribute("color"),t=new Map;this.applyResultRange();const i=this.renderBy==="result"?this.resultMaterial:this.surfaceMaterial;if(i!==this.material){this.material=i,this.pairedMaterial=this.renderBy==="result"?this.pairedResultMaterial:this.pairedSurfaceMaterial,this.mesh.material=[this.material,this.pairedMaterial,this.glassMaterial,this.pairedGlassMaterial];for(const r of this.mesh.material)r.clippingPlanes=this.clipEnabled?[this.clipPlane]:[];this.updateMaterials()}const s=this.results?.range??Dn(this.results?.values??[]);for(let r=0;r<this.visibleVertexObjects.length;r+=1){const a=this.visibleVertexObjects[r],o=this.results?this.currentResultValue(a):Number.NaN;let l=Yl(this.objects[a],this.renderBy,o,s,this.results?.palette,this.results?.missingColor);this.renderBy!=="result"&&(this.selected.has(a)?l=new Re("#ff8a00"):this.hovered===a&&(l=new Re("#ffe066"))),t.set(a,l),e.setXYZ(r,l.r,l.g,l.b)}e.needsUpdate=!0,this.updateEdgeColors(t),this.backMesh.visible=this.renderBy==="normal";for(const[r,a]of[[this.material,this.renderBy==="normal"?ni:It],[this.pairedMaterial,this.renderBy==="normal"?ni:vt]])r.side!==a&&(r.side=a,r.needsUpdate=!0);this.updateLegend(),this.updateHoverOutline(),this.refreshTooltip(),this.requestRender()}updateEdgeColors(e){const t=this.edges?.geometry.getAttribute("color");if(!t)return;const i=this.scene.background,s=new Map;for(let r=0;r<this.edgeObjects.length;r+=1){const a=this.edgeObjects[r];if(!s.has(a)){const l=this.glassIndices.has(a),c=l?this.glassMaterial.color:e.get(a)??new Re("#b8bdc5"),h=l?e.get(this.windowParents.get(a)):null;s.set(a,qg(c,i,l?this.glassMaterial.opacity:this.xray?.34:1,h))}const o=s.get(a);t.setXYZ(r*2,o.r,o.g,o.b),t.setXYZ(r*2+1,o.r,o.g,o.b)}t.needsUpdate=!0}setWindowStyle(e){if(!["glass","opaque"].includes(e))throw new Error("Window appearance must be glass or opaque.");this.windowStyle=e,this.windowStyleSelect.value=e,this.updateColors(),this.updateClipping(),this.root.dispatchEvent(new CustomEvent("eplusr-view"))}updateMaterials(){if(this.material){for(const e of[this.material,this.pairedMaterial])e.transparent=this.xray,e.opacity=this.xray?.34:1,e.depthWrite=!this.xray,e.needsUpdate=!0;this.requestRender()}}updateHelpers(){if(this.mesh){if(!this.axisHelper){const e=Math.max(1,this.mesh.geometry.boundingSphere?.radius??1)*.5;this.axisHelper=new Yh(e),this.scene.add(this.axisHelper)}if(this.axisHelper.visible=this.showAxis,this.edges.visible=this.showWireframe,this.points.visible=this.componentVisibility.daylighting,this.diagnosticPoints&&(this.diagnosticPoints.visible=this.showDiagnostics),!this.ground){const e=this.mesh.geometry.boundingBox??new Ni,t=new A,i=new A;e.getSize(t),e.getCenter(i);const s=new wn(Math.max(t.x,1)*1.15,Math.max(t.y,1)*1.15);this.ground=new Ht(s,new Bi({color:"#e3e5e7",side:It})),this.ground.position.set(i.x,i.y,Math.min(0,e.min.z)),this.scene.add(this.ground)}this.ground.visible=this.showGround,this.requestRender()}}updateClipping(){if(!this.material)return;const e={x:new A(-1,0,0),y:new A(0,-1,0),z:new A(0,0,-1)};this.clipPlane.normal.copy(e[this.clipAxis]??e.z),this.clipPlane.constant=Number(this.clipSlider.value),this.clipValue.textContent=`${this.clipAxis.toUpperCase()}: ${At(this.clipPlane.constant)} m`;const t=this.clipEnabled?[this.clipPlane]:[];this.material.clippingPlanes=t,this.pairedMaterial.clippingPlanes=t,this.glassMaterial.clippingPlanes=t,this.pairedGlassMaterial.clippingPlanes=t,this.backMaterial.clippingPlanes=t,this.edges.material.clippingPlanes=t,this.selectionOutline&&(this.selectionOutline.material.clippingPlanes=t),this.hoverOutline&&(this.hoverOutline.material.clippingPlanes=t),this.material.needsUpdate=!0,this.pairedMaterial.needsUpdate=!0,this.glassMaterial.needsUpdate=!0,this.pairedGlassMaterial.needsUpdate=!0,this.updateClipCap(),this.requestRender()}updateClipCap(){if(this.clipCapMesh&&(this.scene.remove(this.clipCapMesh),this.clipCapMesh.geometry.dispose(),this.clipCapMesh.material.dispose(),this.clipCapMesh=null),!this.clipEnabled||!this.clipCap){this.requestRender();return}const e=this.visibleBox(),t=e.getSize(new A),i=this.clipAxis==="x"?[t.y,t.z]:this.clipAxis==="y"?[t.x,t.z]:[t.x,t.y],s=new wn(Math.max(1,i[0])*1.05,Math.max(1,i[1])*1.05),r=new Bi({color:"#f59e0b",transparent:!0,opacity:.16,side:It,depthWrite:!1});this.clipCapMesh=new Ht(s,r);const a=e.getCenter(new A);this.clipCapMesh.position.copy(a),this.clipCapMesh.position[this.clipAxis]=Number(this.clipSlider.value),this.clipAxis==="x"&&(this.clipCapMesh.rotation.y=Math.PI/2),this.clipAxis==="y"&&(this.clipCapMesh.rotation.x=Math.PI/2),this.clipCapMesh.renderOrder=12,this.scene.add(this.clipCapMesh),this.requestRender()}updateLegend(){if(this.legend.classList.toggle("eplusr-viewer__legend--result",this.renderBy==="result"),this.renderBy==="result"&&!this.results){this.legend.style.display="none";return}if(this.legend.style.removeProperty("display"),this.legend.classList.add("eplusr-viewer__legend--visible"),this.legend.innerHTML="",this.renderBy==="result"){const i=this.results.range??Dn(this.results.values);this.legend.appendChild(O("div","eplusr-viewer__legend-title",`${this.results.label??"Result"}${this.results.units?` (${Ln(this.results.units)})`:""}`)),this.legend.appendChild(O("div","eplusr-viewer__legend-time",String(this.results.times[this.frame]??"")));const s=O("div","eplusr-viewer__legend-gradient");s.style.background=`linear-gradient(90deg, ${Bm(this.results.palette)})`;const r=O("div","eplusr-viewer__legend-labels");[0,.25,.5,.75,1].forEach(c=>{const h=O("span","eplusr-viewer__legend-tick",At(i[0]+c*(i[1]-i[0]),4));h.title=String(i[0]+c*(i[1]-i[0])),r.appendChild(h)});const a={all:"All frames · fixed",frame:"Current frame",manual:"Manual range"};this.legend.append(s,r,O("div","eplusr-viewer__legend-policy",a[this.results.rangeMode]+(this.results.coverage<1?" · "+Math.round(this.results.coverage*100)+"%":"")));const o=O("div","eplusr-viewer__legend-item"),l=O("span","eplusr-viewer__legend-swatch");if(l.style.background=this.results.missingColor??"#b8bdc5",o.append(O("span","eplusr-viewer__legend-unit",Ln(this.results.units)),l,O("span","","Missing")),this.legend.appendChild(o),this.appendGlassLegend(),this.selected.size===1){const c=this.currentResultValue([...this.selected][0]);if(Number.isFinite(c)){const h=i[1]===i[0]?.5:Math.max(0,Math.min(1,(c-i[0])/(i[1]-i[0]))),d=O("div","eplusr-viewer__legend-marker",At(c));d.style.left=`${h*100}%`,this.legend.appendChild(d)}}else if(this.selected.size>1){const c=this.selectionResultStats([...this.selected]);c&&this.legend.appendChild(O("div","eplusr-viewer__legend-summary",`Selected: ${At(c.minimum)} – ${At(c.maximum)}, mean ${At(c.mean)}`))}return}const e={surface_type:"Surface type",boundary:"Boundary condition",construction:"Construction",zone:"Zone",space:"Space",normal:"Surface normal"};if(this.legend.appendChild(O("div","eplusr-viewer__legend-title",e[this.renderBy]??"Categories")),this.renderBy==="normal"){[["#f4f5f7","Front face"],["#d34747","Back face"]].forEach(([i,s])=>{const r=O("div","eplusr-viewer__legend-item"),a=O("span","eplusr-viewer__legend-swatch");a.style.background=i,r.append(a,O("span","",s)),this.legend.appendChild(r)});return}const t=new Map;this.visibleObjects.forEach(i=>{const s=this.objects[i];if(this.glassIndices.has(i))return;const r=Yg(s,this.renderBy);t.has(r)||t.set(r,Yl(s,this.renderBy,Number.NaN,[0,1],"viridis"))}),[...t.entries()].sort(([i],[s])=>i.localeCompare(s)).slice(0,20).forEach(([i,s])=>{const r=O("div","eplusr-viewer__legend-item"),a=O("span","eplusr-viewer__legend-swatch");a.style.background=`#${s.getHexString()}`,r.append(a,O("span","",i)),this.legend.appendChild(r)}),this.appendGlassLegend(),t.size>20&&this.legend.appendChild(O("div","eplusr-viewer__legend-summary",`…and ${t.size-20} more categories`))}appendGlassLegend(){if(!this.glassIndices||![...this.glassIndices].some(i=>this.visibleObjects.has(i)))return;const e=O("div","eplusr-viewer__legend-item"),t=O("span","eplusr-viewer__legend-swatch");t.style.background="rgba(141,185,205,0.28)",e.append(t,O("span","",this.renderBy==="result"?"Glass · not color mapped":"Transparent glass")),this.legend.appendChild(e)}setRenderBy(e){e==="result"&&!this.results||(this.renderBy=e,this.renderSelect.value=e,this.updateColors())}async setResultVariable(e){this.resultSet.length&&(this.pause(),this.resultVariableIndex=Math.max(0,Math.min(this.resultSet.length-1,Math.round(e))),this.results=this.resultSet[this.resultVariableIndex],this.frame=Math.min(this.frame,Math.max(0,this.results.times.length-1)),this.configureResultControls(),this.setRenderBy("result"),this.results.mode==="provider"?await this.setTime(this.frame):(this.updateColors(),this.updateSelectionPanel()))}setPalette(e){this.results&&(this.results.palette=e,this.updateColors())}setMissingColor(e){this.results&&(this.results.missingColor=e,this.updateColors())}setResultRange(e=Number(this.rangeMinimum.value),t=Number(this.rangeMaximum.value)){if(this.results){if(!Number.isFinite(e)||!Number.isFinite(t)||e>=t){this.rangeStatus.textContent="Min must be smaller than Max.",this.rangeMinimum.setAttribute("aria-invalid","true"),this.rangeMaximum.setAttribute("aria-invalid","true");return}this.results.manualRange=[e,t],this.results.rangeMode="manual",this.results.coverage=1,this.rangeStatus.textContent="",this.rangeMinimum.removeAttribute("aria-invalid"),this.rangeMaximum.removeAttribute("aria-invalid"),this.updateColors()}}setRangeMode(e){if(!(!this.results||!["all","frame","manual"].includes(e))){if(this.results.rangeMode=e,this.results.coverage=1,e==="manual"&&!this.results.manualRange){const[t,i]=this.results.range??this.results.sourceRange;this.results.manualRange=t===i?[t-.5,i+.5]:[t,i]}this.rangeStatus.textContent="",this.rangeMinimum.removeAttribute("aria-invalid"),this.rangeMaximum.removeAttribute("aria-invalid"),this.updateColors()}}rangeValues(){if(!this.results)return new Float32Array;if(this.results.mode==="provider")return this.results.loadedFrame===this.frame?this.results.values:new Float32Array;if(this.results.rangeMode==="all")return this.results.values;const e=this.frame*this.objects.length;return this.results.values.subarray(e,e+this.objects.length)}applyResultRange(){if(!this.results)return;const e=this.results;let t=e.rangeMode==="manual"?e.manualRange:e.rangeMode==="all"?e.sourceRange:Dn(this.rangeValues(),e.sourceRange);if(e.coverage<1&&e.rangeMode!=="manual"){const i=this.rangeValues(),s=(1-e.coverage)/2,r=tc(i,s),a=tc(i,1-s);Number.isFinite(r)&&Number.isFinite(a)&&(t=[r,a])}e.range=[...t??e.sourceRange],this.syncRangeControls()}syncRangeControls(){if(!this.results||!this.rangeModeSelect)return;const e=this.results;this.rangeModeSelect.value=e.rangeMode,this.rangeMinimum.hasAttribute("aria-invalid")||(this.rangeMinimum.value=String(e.range?.[0]??e.sourceRange[0]),this.rangeMaximum.value=String(e.range?.[1]??e.sourceRange[1])),this.percentileSelect.value=String(e.coverage),this.percentileSelect.disabled=e.rangeMode==="manual"||e.mode==="provider"&&e.rangeMode==="all",this.percentileSelect.title=e.mode==="provider"&&e.rangeMode==="all"?"Choose Current frame to use percentile coverage with streamed results.":"Coverage of finite result values"}resetResultRange(){this.setRangeMode("all")}setPercentileRange(e){!this.results||this.percentileSelect.disabled||(this.results.coverage=Math.max(0,Math.min(1,Number(e))),this.updateColors())}setSymmetricRange(){if(!this.results)return;const e=this.results.range??this.results.sourceRange,t=Math.max(Math.abs(e[0]),Math.abs(e[1]));t>0&&this.setResultRange(-t,t)}buildFrameGroups(e,t){if(t==="frame")return e.sourceTimes.map((s,r)=>({label:s,indices:[r]}));const i=new Map;return e.sourceTimes.forEach((s,r)=>{const a=$g(s,t);i.has(a)||i.set(a,[]),i.get(a).push(r)}),[...i.entries()].map(([s,r])=>({label:s,indices:r}))}aggregateEmbedResult(e,t){const i=Number(e.shape?.[1]??this.objects.length),s=new Float32Array(t.length*i);return s.fill(Number.NaN),t.forEach((r,a)=>{for(let o=0;o<i;o+=1){let l=0,c=0;r.indices.forEach(h=>{const d=e.sourceValues[h*i+o];Number.isFinite(d)&&(l+=d,c+=1)}),c&&(s[a*i+o]=l/c)}}),s}async setAggregation(e){if(!this.results)return;this.pause();const t=this.buildFrameGroups(this.results,e);this.results.aggregation=e,this.results.frameGroups=t.map(i=>i.indices),this.results.times=t.map(i=>i.label),this.results.mode==="embed"?this.results.values=e==="frame"?this.results.sourceValues.slice():this.aggregateEmbedResult(this.results,t):(this.results.values=new Float32Array(this.objects.length),this.results.values.fill(Number.NaN)),this.frame=0,this.configureResultControls(),this.results.mode==="provider"?await this.setTime(0):(this.updateColors(),this.updateSelectionPanel())}stepTime(e){this.results&&(this.pause(),this.setTime(Math.max(0,Math.min(this.results.times.length-1,this.frame+e))).catch(t=>this.reportResultError(t)))}jumpToTime(e){if(!this.results)return;const t=e.trim().toLowerCase(),i=this.results.times.findIndex(r=>String(r).toLowerCase()===t),s=i>=0?i:this.results.times.findIndex(r=>String(r).toLowerCase().includes(t));s>=0?(this.pause(),this.setTime(s).catch(r=>this.reportResultError(r)),this.resultStatus.textContent=`Moved to ${this.results.times[s]}.`):this.resultStatus.textContent=`No time frame matches “${e}”.`}async setTime(e){if(this.results){if(this.frame=Math.max(0,Math.min(this.results.times.length-1,Math.round(e))),this.timeSlider.value=String(this.frame),this.frameCounter.textContent=`${this.frame+1} / ${this.results.times.length}`,this.timeSlider.setAttribute("aria-valuetext",String(this.results.times[this.frame])),this.timeLabel.textContent=Va(this.results,this.frame),this.timeJumpInput.value=String(this.results.times[this.frame]),this.results.mode==="provider"){const t=this.frame,i=this.results,s=i.frameGroups?.[t]??[t],r=`${i.aggregation}:${s.join(",")}`;if(i.cache.has(r)){i.values=i.cache.get(r),i.loadedFrame=t,this.root.classList.remove("eplusr-viewer--error","eplusr-viewer--loading"),this.updateColors(),this.updateSelectionPanel(),this.resultStatus.textContent=`Loaded ${i.times[t]}.`,this.root.dispatchEvent(new CustomEvent("eplusr-frame"));return}this.resultAbortController&&this.resultAbortController.abort();const a=new AbortController;this.resultAbortController=a;const o=i.endpoint.includes("?")?"&":"?";this.root.classList.remove("eplusr-viewer--error"),this.root.classList.add("eplusr-viewer--loading"),this.resultStatus.textContent=`Loading ${i.times[t]}…`,this.updateColors(),this.updateSelectionPanel();try{const l=await fetch(`${i.endpoint}${o}variable=${encodeURIComponent(i.variable)}&frames=${encodeURIComponent(s.join(","))}`,{signal:a.signal});if(!l.ok)throw new Error(`Unable to load result frame ${t} (${l.status}).`);const c=new Float32Array(await l.arrayBuffer());if(c.length!==this.objects.length)throw new Error("The result endpoint returned an unexpected number of values.");if(t!==this.frame||i!==this.results)return;i.values=c,i.loadedFrame=t,i.cache.set(r,c),this.resultStatus.textContent=`Loaded ${i.times[t]}.`}catch(l){if(l.name==="AbortError")return;this.pause(),this.resultStatus.textContent=l.message,this.root.classList.add("eplusr-viewer--error");return}finally{this.resultAbortController===a&&(this.resultAbortController=null,this.root.classList.remove("eplusr-viewer--loading"))}}this.updateColors(),this.updateSelectionPanel(),this.root.dispatchEvent(new CustomEvent("eplusr-frame"))}}reportResultError(e){e?.name!=="AbortError"&&(this.pause(),this.root.classList.remove("eplusr-viewer--loading"),this.root.classList.add("eplusr-viewer--error"),this.resultStatus&&(this.resultStatus.textContent=e?.message??String(e)))}play(e=this.playbackSpeed){if(!this.results||this.results.times.length<2)return;this.pause(),this.ribbon?.clean&&this.ribbon.modelOnly(!1),this.ribbon?.showTimeline(!0),this.playbackSpeed=Number(e)||1,this.speedSelect.value=String(this.playbackSpeed),Kt(this.playButton,"pause","Pause",!0);const t=Math.max(50,800/this.playbackSpeed);this.playbackTimer=window.setInterval(()=>{this.setTime((this.frame+1)%this.results.times.length).catch(i=>{this.pause(),this.resultStatus.textContent=i.message})},t),this.savePreferences()}pause(){this.playbackTimer&&window.clearInterval(this.playbackTimer),this.playbackTimer=null,this.playButton&&Kt(this.playButton,"play","Play",!0)}savePreferences(){try{window.localStorage.setItem("eplusr-idfviewer-settings-v1",JSON.stringify({playbackSpeed:this.playbackSpeed}))}catch{}}loadPreferences(){try{const e=JSON.parse(window.localStorage.getItem("eplusr-idfviewer-settings-v1")??"{}");Number.isFinite(Number(e.playbackSpeed))&&(this.playbackSpeed=Number(e.playbackSpeed)),this.speedSelect.value=String(this.playbackSpeed)}catch{}}cameraViews(){try{return JSON.parse(window.localStorage.getItem("eplusr-idfviewer-camera-views-v1")??"{}")}catch{return{}}}populateSavedViews(){if(!this.savedViewSelect)return;const e=this.cameraViews(),t=this.savedViewSelect.value;this.savedViewSelect.replaceChildren(qe("","Choose a saved view"),...Object.keys(e).sort().map(i=>qe(i))),e[t]&&(this.savedViewSelect.value=t)}saveCameraView(e){const t=this.cameraViews(),i=e.trim()||`View ${Object.keys(t).length+1}`;t[i]={orthographic:this.camera===this.orthographicCamera,position:this.camera.position.toArray(),quaternion:this.camera.quaternion.toArray(),target:this.controls.target.toArray()};try{window.localStorage.setItem("eplusr-idfviewer-camera-views-v1",JSON.stringify(t)),this.populateSavedViews(),this.savedViewSelect.value=i,this.savedViewName.value="",this.savedViewStatus.textContent=`Saved “${i}”.`}catch{this.savedViewStatus.textContent="Camera presets cannot be saved in this browser. Your view and name are unchanged."}}restoreCameraView(e){const t=this.cameraViews()[e];t&&(this.setProjection(!!t.orthographic),this.camera.position.fromArray(t.position),this.camera.quaternion.fromArray(t.quaternion),this.controls.target.fromArray(t.target),this.controls.update(),this.requestRender())}deleteCameraView(e){if(!e)return;const t=this.cameraViews();delete t[e],this.savedViewStatus.textContent="";try{window.localStorage.setItem("eplusr-idfviewer-camera-views-v1",JSON.stringify(t))}catch{this.savedViewStatus.textContent="Camera presets cannot be updated in this browser."}this.populateSavedViews()}setProjection(e){const t=e?this.orthographicCamera:this.perspectiveCamera;if(t===this.camera)return;this.stopNavigationMotion();const i=this.camera,s=i.position.clone().sub(this.controls.target),r=Math.tan(Jr.degToRad(this.perspectiveCamera.fov/2));e?(this.orthoFitHeight=s.length()*r,t.zoom=1):s.setLength((this.orthoFitHeight??1)/i.zoom/r),t.position.copy(this.controls.target).add(s),t.quaternion.copy(i.quaternion),t.up.copy(i.up),this.camera=t,this.controls.object=t,this.projectionSelect.value=e?"orthographic":"perspective",this.resize(),this.controls.update(),this.requestRender()}stopNavigationMotion(){const e=this.controls.enableDamping;this.controls.enableDamping=!1,this.controls.update(),this.controls.enableDamping=e}syncNavigation(){this.presetQuaternion&&this.camera.quaternion.angleTo(this.presetQuaternion)>.01&&(this.viewSelect.value="custom"),this.projectionSelect.value=this.camera===this.orthographicCamera?"orthographic":"perspective",this.navigation?.update()}setView(e){if(e==="custom")return;this.stopNavigationMotion();const t=this.visibleBox(),i=t.getCenter(new A),s=t.getSize(new A),r=Math.max(s.x,s.y,s.z,1)*1.8,a={top:new A(0,-r*1e-6,r),bottom:new A(0,-r*1e-6,-r),front:new A(0,-r,0),back:new A(0,r,0),left:new A(-r,0,0),right:new A(r,0,0),iso:new A(r,-r,r)}[e]??new A(r,-r,r);this.camera.position.copy(i).add(a),this.camera.up.set(0,0,1),this.controls.target.copy(i),this.camera.lookAt(i),this.controls.update(),this.viewSelect.value=e,this.fitView(),this.presetQuaternion=this.camera.quaternion.clone(),this.viewSelect.value=e,this.navigation?.update(),this.requestRender()}visibleBox(e=null){const t=new Ni,i=e?new Set(e):null;for(let s=0;s<this.faceObjects.length;s+=1){const r=this.faceObjects[s];if(i?!i.has(r):!this.isObjectVisible(r))continue;const a=s*9,o=this.currentOffsets?.[r]??this.objectOffset(r);for(let l=0;l<3;l+=1)t.expandByPoint(new A(this.positions[a+l*3]+o.x,this.positions[a+l*3+1]+o.y,this.positions[a+l*3+2]+o.z))}return t.isEmpty()&&t.expandByPoint(new A).expandByScalar(1),t}fitView(e=null){this.stopNavigationMotion();const t=this.visibleBox(e),i=t.getCenter(new A),s=Math.max(.5,t.getSize(new A).length()/2),r=this.camera.position.clone().sub(this.controls.target).normalize();r.lengthSq()<.5&&r.set(1,-1,.8).normalize(),this.resize();const a=Jr.degToRad(this.perspectiveCamera.fov/2),o=Math.atan(Math.tan(a)*this.perspectiveCamera.aspect),l=new lt().lookAt(r,new A,this.camera.up),c=new A().setFromMatrixColumn(l,0),h=new A().setFromMatrixColumn(l,1);let d=s,u=0,m=0;for(const _ of[t.min.x,t.max.x])for(const x of[t.min.y,t.max.y])for(const p of[t.min.z,t.max.z]){const f=new A(_,x,p).sub(i),M=f.dot(r);u=Math.max(u,Math.abs(f.dot(c))),m=Math.max(m,Math.abs(f.dot(h))),d=Math.max(d,M+1.14*Math.abs(f.dot(c))/Math.tan(o),M+1.14*Math.abs(f.dot(h))/Math.tan(a))}this.controls.target.copy(i),this.camera.position.copy(i).addScaledVector(r,d),this.orthoFitHeight=Math.max(m,u/this.perspectiveCamera.aspect,.5)*1.14,this.orthographicCamera.zoom=1,this.resize(),this.camera.lookAt(i),this.camera.updateProjectionMatrix(),this.controls.update(),this.requestRender()}showAll(){this.remoteFilters={},this.nameFilter="",this.isolated=null,this.isolationHistory=[],this.hidden.clear(),this.zoneFilter="",this.spaceFilter="",this.zoneSelect.value="",this.populateSpaceFilter(),this.spaceSelect.value="",this.diagnosticOnly=!1,this.diagnosticOnlyToggle.checked=!1,fi.forEach(e=>{this.componentVisibility[e]=!0}),Object.values(this.componentInputs).forEach(e=>{e.checked=!0}),this.selected.clear(),this.updateNameFilterStatus(),this.rebuildGeometry(),this.fitView(),this.emitSelection()}isolateSelection(){this.selected.size&&(this.isolated&&this.isolated.size===this.selected.size&&[...this.selected].every(e=>this.isolated.has(e))||(this.isolationHistory.push(this.isolated?new Set(this.isolated):null),this.isolated=new Set(this.selected),this.rebuildGeometry(),this.fitView(this.selected),this.updateSelectionPanel()))}exitIsolation(){this.isolated&&(this.isolated=this.isolationHistory.length?this.isolationHistory.pop():null,this.applyVisibilityChange({fit:!0}))}hideSelection(){this.selected.size&&(this.selected.forEach(e=>this.hidden.add(e)),this.selected.clear(),this.applyVisibilityChange({fit:!0}),this.emitSelection())}clearSelection(){this.selected.size&&(this.selected.clear(),this.updateColors(),this.emitSelection())}bindEvents(){this.controls.addEventListener("change",()=>{this.needsRender=!0,this.syncNavigation()}),this.renderer.domElement.addEventListener("pointerdown",e=>{this.pointerDown={x:e.clientX,y:e.clientY}}),this.renderer.domElement.addEventListener("pointermove",e=>this.onPointerMove(e)),this.renderer.domElement.addEventListener("pointerup",e=>this.onPointerUp(e)),this.renderer.domElement.addEventListener("click",e=>this.onClick(e)),this.renderer.domElement.addEventListener("dblclick",e=>this.onDoubleClick(e)),this.renderer.domElement.addEventListener("pointerleave",()=>{this.hovered=null,this.tooltip.style.display="none",this.updateColors()}),this.root.addEventListener("keydown",e=>{if(typeof e.key=="string"){if(e.key==="Escape"&&this.measureMode){e.preventDefault(),this.toggleMeasureMode(!1),this.renderer.domElement.focus();return}if(!e.target.closest?.("input, select, textarea, [contenteditable]")){if(e.key==="Escape"&&(this.selected.clear(),this.isolated=null,this.rebuildGeometry(),this.emitSelection()),e.key.toLowerCase()==="f"&&this.fitView(this.selected.size?this.selected:null),e.key.toLowerCase()==="i"&&this.isolateSelection(),e.key.toLowerCase()==="m"){const t=!this.measureMode;this.measureButton.setAttribute("aria-pressed",String(t)),this.toggleMeasureMode(t)}e.key==="?"&&this.ribbon.open("help")}}}),this.root.tabIndex=0,this.root.setAttribute("aria-keyshortcuts","Escape F I M")}raycast(e){if(!this.mesh||!this.visibleFaceObjects.length)return null;const t=this.renderer.domElement.getBoundingClientRect();this.pointer.x=(e.clientX-t.left)/t.width*2-1,this.pointer.y=-((e.clientY-t.top)/t.height)*2+1,this.raycaster.setFromCamera(this.pointer,this.camera);const i=this.raycaster.intersectObject(this.mesh,!1).find(r=>!this.clipEnabled||this.clipPlane.distanceToPoint(r.point)>=-1e-6);if(!i||i.faceIndex===void 0)return null;const s=this.visibleFaceObjects[i.faceIndex];return s===void 0?null:{objectIndex:s,point:i.point.clone()}}onPointerMove(e){this.hoverPosition={x:e.offsetX,y:e.offsetY};const t=this.raycast(e)?.objectIndex??null;t!==this.hovered&&(this.hovered=t,this.updateColors()),this.refreshTooltip()}refreshTooltip(){if(this.hovered===null||!this.isObjectVisible(this.hovered)||!this.hoverPosition){this.tooltip.style.display="none";return}const e=this.objects[this.hovered];if(this.tooltip.replaceChildren(O("strong","",e.name??e.class)),this.tooltip.appendChild(O("div","eplusr-viewer__value",[e.zone_name,e.space_name].filter(Boolean).join(" · "))),this.renderBy==="result"&&this.results){const a=this.currentResultValue(this.hovered);this.tooltip.append(O("div","eplusr-viewer__probe-value",At(a)+(this.results.units?" "+Ln(this.results.units):"")),O("div","eplusr-viewer__value",this.results.label??"Result"),O("div","eplusr-viewer__value",Va(this.results,this.frame)))}this.tooltip.style.display="block";const{x:t,y:i}=this.hoverPosition,s=Math.max(6,Math.min(t+14,this.canvasHost.clientWidth-this.tooltip.offsetWidth-6)),r=i+this.tooltip.offsetHeight+20<this.canvasHost.clientHeight?i+14:Math.max(6,i-this.tooltip.offsetHeight-14);this.tooltip.style.left=s+"px",this.tooltip.style.top=r+"px"}updateHoverOutline(){const e=this.hovered,t=e+":"+this.selected.has(e)+":"+this.explodeAmount+":"+this.explodeMode+":"+this.visibleObjects.has(e);if(t===this.hoverOutlineKey||(this.hoverOutlineKey=t,this.hoverOutline?.geometry.dispose(),this.hoverOutline?.material.dispose(),this.hoverOutline&&this.scene.remove(this.hoverOutline),this.hoverOutline=null,e===null||this.selected.has(e)||!this.isObjectVisible(e)))return;const i=[],s=this.currentOffsets?.[e]??new A;for(let a=0;a<this.edgeObjects.length;a++)if(this.edgeObjects[a]===e)for(let o=0;o<6;o++)i.push(this.edgePositions[a*6+o]+s.getComponent(o%3));const r=new _t;r.setAttribute("position",new Ft(i,3)),this.hoverOutline=new Ns(r,new yn({color:"#3874a8",depthTest:!1})),this.hoverOutline.material.clippingPlanes=this.clipEnabled?[this.clipPlane]:[],this.hoverOutline.renderOrder=19,this.scene.add(this.hoverOutline)}onPointerUp(e){if(this.pointerClickReady=!1,!this.pointerDown)return;const t=Math.hypot(e.clientX-this.pointerDown.x,e.clientY-this.pointerDown.y);if(this.pointerDown=null,!(e.button!==0||t>4)){if(this.measureMode){const i=this.raycast(e);i&&this.addMeasurementPoint(i.point);return}this.pointerClickReady=!0}}onClick(e){if(!this.pointerClickReady||this.measureMode||e.button!==0||e.detail>1)return;this.selectionBeforeClick=new Set(this.selected);const t=this.raycast(e)?.objectIndex??null;if(t===null)!e.shiftKey&&!e.metaKey&&!e.ctrlKey&&this.selected.clear();else if(e.shiftKey||e.metaKey||e.ctrlKey){const i=this.selectionGroup(t),s=i.every(r=>this.selected.has(r));i.forEach(r=>s?this.selected.delete(r):this.selected.add(r))}else this.selected=new Set(this.selectionGroup(t));this.updateColors(),this.emitSelection()}onDoubleClick(e){if(!this.pointerClickReady||this.measureMode||e.button!==0||e.shiftKey||e.metaKey||e.ctrlKey)return;const t=this.raycast(e);t&&(this.selected=new Set(fm(this.objects,this.visibleObjects,t.objectIndex,this.selectionBeforeClick??new Set,this.drilledZone)),this.drilledZone=this.objects[t.objectIndex]?.zone_name??null,this.updateColors(),this.emitSelection())}buildViewportFeedback(){this.toolFeedback=O("div","eplusr-viewer__tool-feedback"),this.toolHint=O("div","eplusr-viewer__tool-hint"),this.measureStatus.setAttribute("role","status");const e=O("div","eplusr-viewer__tool-actions");this.restartMeasureButton=O("button","","Restart"),Kt(this.restartMeasureButton,"reset","Restart"),this.restartMeasureButton.setAttribute("aria-label","Restart measurement"),this.restartMeasureButton.title="Restart measurement",this.restartMeasureButton.type="button",this.restartMeasureButton.addEventListener("click",()=>this.clearMeasurement()),this.exitMeasureButton=O("button"),this.exitMeasureButton.type="button",Kt(this.exitMeasureButton,"clear","Exit measurement",!0),this.exitMeasureButton.addEventListener("click",()=>this.toggleMeasureMode(!1)),e.append(this.restartMeasureButton,this.exitMeasureButton),this.toolFeedback.append(this.measureStatus,this.toolHint,e),this.measureLabel=O("div","eplusr-viewer__measurement-label"),this.measureLabel.hidden=!0,this.canvasHost.append(this.toolFeedback,this.measureLabel),this.clearMeasureButton=O("button","","Clear measurement"),this.clearMeasureButton.type="button",Kt(this.clearMeasureButton,"clear","Clear measurement"),this.clearMeasureButton.addEventListener("click",()=>this.clearMeasurement()),this.measureButton.parentNode.appendChild(this.clearMeasureButton)}toggleMeasureMode(e){this.measureMode=!!e,this.measureButton.setAttribute("aria-pressed",String(this.measureMode)),!this.measureMode&&this.measurePoints.length===1&&this.clearMeasurement(),this.renderer.domElement.style.cursor=this.measureMode?"crosshair":"default",this.root.dispatchEvent(new CustomEvent("eplusr-view")),this.updateViewportFeedback(),this.requestRender()}clearMeasurement(){this.measurePoints=[],this.measurement=null,this.clearMeasurementGeometry(),this.root.dispatchEvent(new CustomEvent("eplusr-view")),this.updateViewportFeedback()}addMeasurementPoint(e){this.measurePoints.length>=2&&this.clearMeasurement(),this.measurePoints.push(e.clone()),this.measurePoints.length===2&&(this.measurement={distance:this.measurePoints[0].distanceTo(this.measurePoints[1])}),this.updateMeasurementGeometry(),this.root.dispatchEvent(new CustomEvent("eplusr-view")),this.updateViewportFeedback()}updateViewportFeedback(){if(!this.toolFeedback)return;const e=this.explodeMode!=="none"&&this.explodeAmount>0;let t="Inspect · "+(this.selectionModeSelect.selectedOptions[0]?.textContent??"Surfaces");if(this.measureMode?t=this.measurePoints.length===0?"Measure · Pick first point":this.measurePoints.length===1?"Measure · Pick second point":(e?"Display distance: ":"Distance: ")+At(this.measurement.distance)+" m":this.clipEnabled?t+=" · Clip "+this.clipAxis.toUpperCase()+" "+At(this.clipPlane.constant)+" m":e&&(t+=" · Exploded "+this.explodeMode),this.measureStatus.textContent!==t&&(this.measureStatus.textContent=t),this.toolFeedback.hidden=!(this.measureMode||this.clipEnabled||e||this.selectionMode!=="surface"),this.toolHint.textContent=this.measureMode?this.measurePoints.length===2?"Pick to measure again · Esc exits":"Pick on model · Esc cancels":"",this.toolFeedback.classList.toggle("eplusr-viewer__tool-feedback--active",this.measureMode),this.restartMeasureButton.hidden=!this.measureMode,this.restartMeasureButton.disabled=!this.measurePoints.length,this.exitMeasureButton.hidden=!this.measureMode,this.clearMeasureButton.disabled=!this.measurePoints.length,this.measureLabel.hidden=!this.measurement,this.measurement){this.measureLabel.textContent=At(this.measurement.distance)+" m"+(e?" · exploded view":"");const i=this.measurePoints[0].clone().add(this.measurePoints[1]).multiplyScalar(.5).project(this.camera);this.measureLabel.hidden=Math.abs(i.x)>1||Math.abs(i.y)>1||i.z<-1||i.z>1;const s=(i.x+1)*this.canvasHost.clientWidth/2,r=(1-i.y)*this.canvasHost.clientHeight/2;this.measureLabel.style.left=Math.max(8,Math.min(s,this.canvasHost.clientWidth-this.measureLabel.offsetWidth-8))+"px",this.measureLabel.style.top=Math.max(8,Math.min(r+10,this.canvasHost.clientHeight-this.measureLabel.offsetHeight-8))+"px"}}updateMeasurementGeometry(){this.clearMeasurementGeometry();const e=new _t().setFromPoints(this.measurePoints);if(this.measurePointsObject=new Ta(e,new Us({color:"#ef4444",size:9,sizeAttenuation:!1,depthTest:!1})),this.measurePointsObject.renderOrder=30,this.scene.add(this.measurePointsObject),this.measurePoints.length===2){const t=new _t().setFromPoints(this.measurePoints);this.measureLine=new Wo(t,new yn({color:"#ef4444",depthTest:!1})),this.measureLine.renderOrder=29,this.scene.add(this.measureLine)}this.requestRender()}clearMeasurementGeometry(){["measurePointsObject","measureLine"].forEach(e=>{const t=this[e];t&&(this.scene.remove(t),t.geometry.dispose(),t.material.dispose(),this[e]=null)}),this.requestRender()}emitSelection(){const e=[...this.selected].map(t=>({index:t,...this.objects[t],value:this.currentResultValue(t)}));this.rebuildSelectionOutline(),this.updateSelectionPanel(),this.container.dispatchEvent(new CustomEvent("eplusr-selection",{detail:e}))}currentResultValue(e){return this.results?this.results.mode==="provider"?this.results.loadedFrame===this.frame?this.results.values[e]:Number.NaN:this.results.values[this.frame*this.objects.length+e]:null}objectArea(e){const t=new Set(e);let i=0;const s=new A,r=new A,a=new A;for(let o=0;o<this.faceObjects.length;o+=1){if(!t.has(this.faceObjects[o]))continue;const l=o*9;s.set(this.positions[l],this.positions[l+1],this.positions[l+2]),r.set(this.positions[l+3],this.positions[l+4],this.positions[l+5]),a.set(this.positions[l+6],this.positions[l+7],this.positions[l+8]),i+=new qt(s,r,a).getArea()}return i}selectionResultStats(e){const t=e.map(s=>this.currentResultValue(s)).filter(Number.isFinite);if(!t.length)return null;const i=Dn(t);return{minimum:i[0],maximum:i[1],mean:t.reduce((s,r)=>s+r,0)/t.length}}updateSelectionPanel(){this.syncFilterUi(),this.fitSelectionButton.disabled=!this.selected.size;const e=this.selectionActionButtons,t=this.selected.size>0,i=this.isolated&&this.isolated.size===this.selected.size&&[...this.selected].every(x=>this.isolated.has(x));if(e.isolate.disabled=!t||i,e.hide.disabled=!t,e.clear.disabled=!t,e.exitIsolation.disabled=!this.isolated,this.isolationStatus.textContent=this.isolated?`Model › Isolated selection (${this.isolated.size} objects) › Level ${Math.max(1,this.isolationHistory.length)}`:"Model",!this.selected.size){this.selectionPanel.textContent="Click a surface to inspect its properties; double-click to select its Zone, then double-click again to select a surface.";return}const s=[...this.selected],r=s.length>1,a=x=>{const p=[...new Set(s.map(f=>this.objects[f][x]||"Not assigned"))];return p.length===1?p[0]:"Mixed ("+p.length+" values)"},o=r?[["Objects",s.length],["Zones",new Set(s.map(x=>this.objects[x].zone_name).filter(Boolean)).size],["Class",a("class")],["Surface type",a("surface_type")],["Zone",a("zone_name")],["Space",a("space_name")],["Construction",a("construction_name")],["Boundary",a("outside_boundary_condition")]]:this.objectPropertyRows(s[0]);if(o.push([r?"Total face area":"Face area",At(this.objectArea(s))+" m²"]),this.results){const x=this.results.units?" "+Ln(this.results.units):"",p=this.selectionResultStats(s);r?(o.push(["Result values",s.filter(f=>Number.isFinite(this.currentResultValue(f))).length+" / "+s.length]),p&&o.push(["Minimum",At(p.minimum)+x],["Object mean",At(p.mean)+x],["Maximum",At(p.maximum)+x])):o.push([this.results.label??"Result",At(this.currentResultValue(s[0]))+x])}const l=this.propertyList(o),c=O("h4","eplusr-viewer__selection-title",r?s.length+" objects selected":this.objects[s[0]].name),h=O("p","eplusr-viewer__area-note","Face area excludes openings. Shared boundaries are counted per IDF surface.");if(this.selectionPanel.replaceChildren(c,l,h),!r)return;const d=O("details","eplusr-viewer__active-object");d.open=!!this.activeDetailsOpen,d.appendChild(O("summary","","Inspect one selected object")),d.addEventListener("toggle",()=>{this.activeDetailsOpen=d.open});const u=this.field("Active object","select",d);s.forEach(x=>u.appendChild(qe(x,this.objects[x].name??"Object "+(x+1)))),this.selected.has(this.activeObjectIndex)||(this.activeObjectIndex=s[0]),u.value=String(this.activeObjectIndex);const m=O("div"),_=()=>{const x=Number(u.value);this.activeObjectIndex=x;const p=this.objectPropertyRows(x);this.results&&p.push([this.results.label??"Result",At(this.currentResultValue(x))+(this.results.units?" "+Ln(this.results.units):"")]),m.replaceChildren(this.propertyList(p))};u.addEventListener("change",_),_(),d.appendChild(m),this.selectionPanel.appendChild(d)}propertyList(e){const t=O("dl");return e.filter(([,i])=>i!=null&&i!=="").forEach(([i,s])=>{t.append(O("dt","",i),O("dd","",String(s)))}),t}objectPropertyRows(e){const t=this.objects[e];return[["Name",t.name],["Class",t.class],["Surface type",t.surface_type],["Zone",t.zone_name],["Space",t.space_name],["Construction",t.construction_name],["Boundary",t.outside_boundary_condition]]}downloadSnapshot(e="eplusr-view.png"){this.renderer.render(this.scene,this.camera);const t=this.renderer.domElement;this.ribbon.previewSnapshot(t.toDataURL("image/png"),e,t.width,t.height)}applyCommand(e,t={}){if(e==="result_query_error"&&this.queryPanel.fail(t.message),e==="select_ids"){const s=new Set((t.ids??[]).map(String));this.selected=new Set(this.objects.flatMap((r,a)=>s.has(String(r.id))&&this.isObjectVisible(a)?[a]:[])),this.updateColors(),this.emitSelection()}if(e==="filter"){this.remoteFilters=Object.fromEntries(Object.entries(t).map(([r,a])=>[r,a===null?null:(Array.isArray(a)?a:[a]).map(o=>String(o).toLowerCase())])),this.zoneFilter="",this.spaceFilter="",this.nameFilter=this.remoteFilters.name?.[0]??"";const s=(r,a)=>this.remoteFilters[r]?.length===1?this.objects.find(o=>String(o[a]??"").toLowerCase()===this.remoteFilters[r][0])?.[a]??"":"";this.zoneFilter=s("zone","zone_name"),this.spaceFilter=s("space","space_name"),fi.forEach(r=>{this.componentVisibility[r]=!this.remoteFilters.type||this.remoteFilters.type.includes(r)}),this.populateSpaceFilter(),this.applyVisibilityChange({fit:!0})}e==="background"&&(this.scene.background=new Re(t.value),this.updateColors());const i={wireframe:["showWireframe","wireButton"],x_ray:["xray","xrayButton"],axis:["showAxis","axisButton"],ground:["showGround","groundButton"]};if(i[e]){const[s,r]=i[e];this[s]=!!t.on,this[r].setAttribute("aria-pressed",String(this[s])),this.rebuildGeometry()}if(e==="time"&&this.setTime(t.frame??0).catch(s=>this.reportResultError(s)),e==="variable"&&this.setResultVariable(t.index??0).catch(s=>this.reportResultError(s)),e==="render_by"&&this.setRenderBy(t.value),e==="window_style"&&this.setWindowStyle(t.value),e==="view"&&this.setView(t.value??"iso"),e==="fit"&&this.fitView(this.selected.size?this.selected:null),e==="fit_model"&&this.fitView(),e==="fit_selection"&&this.selected.size&&this.fitView(this.selected),e==="projection"&&this.setProjection(t.value==="orthographic"),e==="range_mode"&&this.setRangeMode(t.value),e==="show_all"&&this.showAll(),e==="play"&&this.play(t.speed??this.playbackSpeed),e==="pause"&&this.pause(),e==="palette"&&this.setPalette(t.value??"viridis"),e==="range"&&this.setResultRange(Number(t.minimum),Number(t.maximum)),e==="aggregation"&&this.setAggregation(t.value??"frame").catch(s=>this.reportResultError(s)),e==="explode"&&(this.explodeSelect.value=t.value??"none",this.explodeSlider.value=String(t.amount??0),this.setExplode(this.explodeSelect.value,Number(this.explodeSlider.value))),e==="clip"&&(t.axis&&(this.clipAxisSelect.value=t.axis,this.setClipAxis(t.axis)),t.position!==void 0&&(this.clipSlider.value=String(t.position)),t.on!==void 0&&(this.clipEnabled=!!t.on,this.clipToggle.checked=this.clipEnabled),t.cap!==void 0&&(this.clipCap=!!t.cap,this.clipCapToggle.checked=this.clipCap),this.updateClipping()),e==="measure"){const s=!!t.on;this.measureButton.setAttribute("aria-pressed",String(s)),this.toggleMeasureMode(s)}e==="hide_selected"&&this.hideSelection(),e==="isolate"&&this.isolateSelection(),e==="exit_isolation"&&this.exitIsolation(),e==="clear_selection"&&this.clearSelection(),e==="panel"&&this.togglePanel(t.hidden??null),e==="diagnostics"&&(t.on!==void 0&&(this.showDiagnostics=!!t.on),t.only!==void 0&&(this.diagnosticOnly=!!t.only),this.diagnosticToggle.checked=this.showDiagnostics,this.diagnosticOnlyToggle.checked=this.diagnosticOnly,this.rebuildGeometry()),e==="select"&&(this.selected=new Set(t.indices??[]),this.updateColors(),this.emitSelection())}resize(){const e=this.root.clientWidth<960,t=this.root.clientHeight<500,i=t&&!this.short;this.short=t,this.root.classList.toggle("eplusr-viewer--short",t),this.ribbon?.resizeLayout();const s=t||(this.viewport?.clientWidth??this.root.clientWidth)<600,r=s?this.viewport??this.workspace:this.canvasHost;this.legend.parentNode!==r&&r.append(this.legend),this.navigation?.resize(s),i&&this.ribbon&&this.ribbon.collapse(!0),e!==this.compact&&(this.compact=e,this.root.classList.toggle("eplusr-viewer--compact",e));const a=Math.max(1,this.canvasHost.clientWidth),o=Math.max(1,this.canvasHost.clientHeight),l=this.perspectiveCamera.aspect;if(this.renderer.setPixelRatio(Math.min(window.devicePixelRatio||1,2)),this.renderer.setSize(a,o,!1),this.perspectiveCamera.aspect=a/o,l!==this.perspectiveCamera.aspect&&this.positions.length){const d=this.perspectiveCamera.position.clone().sub(this.controls.target);this.perspectiveCamera.position.copy(this.controls.target).add(d.multiplyScalar(Math.min(1,l)/Math.min(1,this.perspectiveCamera.aspect)))}this.perspectiveCamera.updateProjectionMatrix();const c=Math.max(1,this.visibleBox().getSize(new A).length()/2)*1.14,h=this.orthoFitHeight??c/Math.min(1,a/o);this.orthographicCamera.left=-h*a/o,this.orthographicCamera.right=h*a/o,this.orthographicCamera.top=h,this.orthographicCamera.bottom=-h,this.orthographicCamera.updateProjectionMatrix(),this.queryPanel?.resize(),this.ribbon?.syncOverflow(),this.requestRender()}requestRender(){this.needsRender=!0}updateCameraDepth(){const e=this.mesh?.geometry.boundingSphere;if(!e)return;const t=this.camera.getWorldDirection(new A),i=e.center.clone().sub(this.camera.position).dot(t),s=Math.max(e.radius*1.05,.1),r=this.camera.isPerspectiveCamera?Math.max(.001,i-s):i-s,a=Math.max(r+.1,i+s);(this.camera.near!==r||this.camera.far!==a)&&(this.camera.near=r,this.camera.far=a,this.camera.updateProjectionMatrix())}animate(){this.animationFrame=window.requestAnimationFrame(()=>this.animate()),(this.controls.update()||this.needsRender)&&(this.updateCameraDepth(),this.renderer.render(this.scene,this.camera),this.syncNavigation(),this.updateViewportFeedback(),this.needsRender=!1)}dispose(){this.pause(),this.resultAbortController&&this.resultAbortController.abort(),this.animationFrame&&window.cancelAnimationFrame(this.animationFrame),this.layoutObserver?.disconnect(),this.navigation?.dispose(),this.ribbon?.hints.dispose(),this.controls.dispose(),this.mesh&&this.mesh.geometry.dispose(),[this.surfaceMaterial,this.resultMaterial,this.backMaterial,this.pairedSurfaceMaterial,this.pairedResultMaterial,this.hiddenBackMaterial,this.glassMaterial,this.pairedGlassMaterial,this.hoverOutline?.material,this.edges?.material,this.points?.material,this.diagnosticPoints?.material,this.selectionOutline?.material,this.measurePointsObject?.material,this.measureLine?.material,this.clipCapMesh?.material,this.ground?.material,this.northLabel?.material].filter(Boolean).forEach(e=>e.dispose()),[this.edges?.geometry,this.hoverOutline?.geometry,this.points?.geometry,this.diagnosticPoints?.geometry,this.selectionOutline?.geometry,this.measurePointsObject?.geometry,this.measureLine?.geometry,this.clipCapMesh?.geometry,this.ground?.geometry].filter(Boolean).forEach(e=>e.dispose()),this.northLabel?.material?.map&&this.northLabel.material.map.dispose(),this.renderer.dispose(),this.container.innerHTML=""}}window.EplusrIdfViewer=Zg,window.EplusrViewerConnection=hc})();
