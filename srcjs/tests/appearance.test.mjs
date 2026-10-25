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
