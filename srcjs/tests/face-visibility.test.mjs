import test from 'node:test';
import assert from 'node:assert/strict';
import {FrontSide,BackSide,DoubleSide} from 'three';
import {surfaceSide,faceIsVisible} from '../src/face-visibility.js';

const origin={x:0,y:0,z:0},above={x:0,y:0,z:5},below={x:0,y:0,z:-5};

test('single-sided drawing is the default for isolated and reciprocal surfaces',()=>{
  assert.equal(surfaceSide(),FrontSide);assert.equal(surfaceSide(false,true),FrontSide);
  assert.equal(surfaceSide(true),DoubleSide);assert.equal(surfaceSide(true,true),BackSide);
});

test('opposite coplanar faces never both draw boundaries from the same camera',()=>{
  for(const camera of [above,below]){
    const a=faceIsVisible([0,0,1],origin,camera);
    const b=faceIsVisible([0,0,-1],{x:.04,y:0,z:0},camera);
    assert.notEqual(a,b);
  }
});

test('orthographic facing uses direction rather than camera location',()=>{
  assert.equal(faceIsVisible([0,0,1],origin,below,{x:0,y:0,z:-1}),true);
  assert.equal(faceIsVisible([0,0,1],origin,above,{x:0,y:0,z:1}),false);
});

test('double-sided and normal inspections remain explicit and respect paired ownership',()=>{
  assert.equal(faceIsVisible([0,0,1],origin,below,null,{backFaces:true}),true);
  assert.equal(faceIsVisible([0,0,1],origin,above,null,{backFaces:true,paired:true}),false);
  assert.equal(faceIsVisible([0,0,1],origin,below,null,{backFaces:true,normalMode:true}),true);
  assert.equal(faceIsVisible([0,0,1],origin,below,null,{backFaces:false,normalMode:true}),false);
});
