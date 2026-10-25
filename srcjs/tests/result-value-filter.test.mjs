import test from 'node:test';
import assert from 'node:assert/strict';
import {matchesResultValue,validateResultBounds} from '../src/result-value-filter.js';
import {objectVisibilityReasons} from '../src/geometry-filter.js';

test('numeric visibility is inclusive and never treats missing data as zero',()=>{
  for(const value of [23,24,25]) assert.equal(matchesResultValue(value,[23,25]),true);
  for(const value of [22,26,NaN,Infinity]) assert.equal(matchesResultValue(value,[23,25]),false);
  assert.equal(matchesResultValue(NaN,null),true);
  assert.equal(matchesResultValue(0,[0,0]),true);
});

test('unbounded endpoints and reset preserve actual numeric values',()=>{
  assert.deepEqual(validateResultBounds('',25),[null,25]);
  assert.deepEqual(validateResultBounds(-3,null),[-3,null]);
  assert.equal(validateResultBounds(null,''),null);
  assert.equal(matchesResultValue(-100,[null,25]),true);
  assert.equal(matchesResultValue(26,[null,25]),false);
});

test('reversed or invalid limits cannot replace a valid interval',()=>{
  assert.throws(()=>validateResultBounds(25,23),/minimum/);
  assert.throws(()=>validateResultBounds('abc',23),/finite/);
  assert.throws(()=>validateResultBounds(Infinity,null),/finite/);
  assert.throws(()=>validateResultBounds(true,null),/finite/);
  assert.throws(()=>validateResultBounds([23],null),/finite/);
});

test('numeric limits intersect existing geometry criteria and explain exclusions',()=>{
  const object={name:'Zone A Floor',surface_type:'Floor',zone_name:'Zone A'};
  assert.deepEqual(objectVisibilityReasons(object,0,{filters:{name:['wall']},resultValueAllowed:false}),['Name filter','Result value filter']);
  assert.deepEqual(objectVisibilityReasons(object,0,{resultValueAllowed:true}),[]);
});
