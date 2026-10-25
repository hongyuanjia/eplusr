import test from "node:test";
import assert from "node:assert/strict";
import * as THREE from "three";
import { boundaryColor, isGlazing, GLASS_COLOR, GLASS_OPACITY } from "../src/appearance.js";
import { sharedBoundaryGroups } from "../src/shared-boundaries.js";

// Compare actual linear-light colors, including the darkest real result-palette endpoint.
function contrast(a,b){const level=c=>c.r*0.2126+c.g*0.7152+c.b*0.0722;return(Math.max(level(a),level(b))+0.05)/(Math.min(level(a),level(b))+0.05);}

test("boundaries remain distinguishable across dark and light result/category fills",()=>{
  const background=new THREE.Color('#f3f5f7');
  for(const value of ['#440154','#3b528b','#21918c','#5ec962','#fde725','#d8b4a0','#f4f5f7']){
    const fill=new THREE.Color(value);assert.ok(contrast(fill,boundaryColor(fill,background))>=3,value);
  }
});

test("glass framing considers both the tint and a dark surrounding wall",()=>{
  const background=new THREE.Color('#f3f5f7'),wall=new THREE.Color('#440154');
  const edge=boundaryColor(new THREE.Color(GLASS_COLOR),background,GLASS_OPACITY,wall);
  assert.ok(contrast(wall,edge)>=3);
  assert.notEqual(edge.getHexString(),boundaryColor(wall,background).getHexString());
});

test("only glazed windows and glass doors receive glass appearance",()=>{
  assert.equal(isGlazing({surface_type:'Window'}),true);assert.equal(isGlazing({surface_type:'GlassDoor'}),true);
  assert.equal(isGlazing({surface_type:'Door'}),false);assert.equal(isGlazing({surface_type:'Wall'}),false);
});

test("glass uses independent draw groups while preserving paired side and source triangle IDs",()=>{
  const faces=new Int32Array([0,0,1,1,2]),pairs=new Int32Array([1,0,-1]),offsets=[0,0,0].map(()=>({x:0,y:0,z:0}));
  const grouped=sharedBoundaryGroups(faces,pairs,new Set([0,1,2]),offsets,new Set([0,1]));
  assert.deepEqual(grouped.groups,[{start:0,count:12,materialIndex:3},{start:12,count:3,materialIndex:0}]);
  assert.deepEqual([...faces],[0,0,1,1,2]);
});

// Independent real Three.js meshes must provide different depth-sort origins and a single glass pass.
test("transparent panes sort independently, retain source IDs, and render each flat pane once",async()=>{
  const {GlazingLayer}=await import('../src/glazing.js');
  const scene=new THREE.Scene(),positions=new Float32Array([0,0,0,2,0,0,0,0,2, 0,5,0,2,5,0,0,5,2]);
  const original=positions.slice();
  const layer=new GlazingLayer(scene,positions,new Int32Array([0,1]),new Set([0,1]));
  const [a,b]=[...layer.meshes.values()];
  assert.notDeepEqual(a.position.toArray(),b.position.toArray());
  assert.equal(a.material.forceSinglePass,true); assert.equal(a.material.depthWrite,false);
  assert.equal(a.userData.objectIndex,0); assert.equal(b.userData.objectIndex,1);
  layer.update(new Set([1]),[new THREE.Vector3(),new THREE.Vector3(3,0,0)],new Int32Array([0,1]),[]);
  assert.equal(a.visible,false);assert.equal(b.visible,true);assert.equal(b.material.side,THREE.FrontSide);
  layer.update(new Set([1]),[new THREE.Vector3(),new THREE.Vector3(3,0,0)],new Int32Array([0,1]),[],true);
  assert.equal(b.material.side,THREE.BackSide);
  assert.equal(b.position.x,3+b.userData.sourceCenter.x);
  assert.deepEqual(positions,original);
  layer.dispose();assert.equal(scene.children.length,0);
});

// A real pane must reject back-facing rays under the default rendering policy.
test("default glass shows and picks only the camera-facing side",async()=>{
  const {GlazingLayer}=await import('../src/glazing.js');
  const scene=new THREE.Scene();
  const positions=new Float32Array([0,0,0,2,0,0,0,0,2]);
  const layer=new GlazingLayer(scene,positions,new Int32Array([0]),new Set([0]));
  layer.update(new Set([0]),[new THREE.Vector3()],new Int32Array([0]),[]);
  scene.updateMatrixWorld(true);
  const mesh=layer.meshes.get(0);
  assert.equal(mesh.material.side,THREE.FrontSide);
  const front=new THREE.Raycaster(new THREE.Vector3(.25,-1,.5),new THREE.Vector3(0,1,0));
  const back=new THREE.Raycaster(new THREE.Vector3(.25,1,.5),new THREE.Vector3(0,-1,0));
  assert.equal(front.intersectObject(mesh).length,1);
  assert.equal(back.intersectObject(mesh).length,0);
  layer.dispose();
});
