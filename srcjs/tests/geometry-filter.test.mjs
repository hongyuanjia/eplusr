import test from 'node:test';
import assert from 'node:assert/strict';
import {componentFor, matchesNameFilter, objectVisibilityReasons} from '../src/geometry-filter.js';

// A real-style ceiling name distinguishes global matches from type-filtered visibility.
const ceiling={name:'Basement_Ceiling_2',zone_name:'Basement',space_name:'Room A',surface_type:'Ceiling',construction_name:'INT-FLOOR'};

test('name filtering is a literal case-insensitive object-name substring',()=>{
  assert.equal(matchesNameFilter(ceiling,['  BASEMENT_ceiling  ']),true);
  assert.equal(matchesNameFilter(ceiling,['Room A']),false);
  assert.equal(matchesNameFilter(ceiling,['INT-FLOOR']),false);
  assert.equal(matchesNameFilter(ceiling,['.*']),false);
  assert.equal(matchesNameFilter({...ceiling,name:'Room[1]'},['[1]']),true);
});

test('clearing a name restriction admits names again without inventing a selection',()=>{
  assert.equal(matchesNameFilter(ceiling,null),true);
  assert.equal(matchesNameFilter(ceiling,['missing']),false);
  assert.equal(matchesNameFilter(ceiling,[]),false);
});

test('name matches remain excluded by component filters with an explicit reason',()=>{
  assert.deepEqual(objectVisibilityReasons(ceiling,0,{filters:{name:['Basement_Ceiling_2'],type:['floor','wall','window']}}),['Type filter (roof / ceiling)']);
  assert.deepEqual(objectVisibilityReasons(ceiling,0,{filters:{name:['Basement'],type:['roof']}}),[]);
});

test('Zone, Space, exact surface and name criteria intersect rather than replace each other',()=>{
  const state={filters:{name:['ceiling'],zone:['basement'],space:['room a'],surface:['basement_ceiling_2'],type:['roof']}};
  assert.deepEqual(objectVisibilityReasons(ceiling,0,state),[]);
  assert.deepEqual(objectVisibilityReasons(ceiling,0,{...state,filters:{...state.filters,zone:['other'],space:['other'],surface:['other'],name:['floor']}}),['Zone filter','Space filter','Surface filter','Name filter']);
});

test('manual hiding and isolation survive changes to name criteria',()=>{
  const state={filters:{name:['basement']},hidden:new Set([0]),isolated:new Set([1])};
  assert.deepEqual(objectVisibilityReasons(ceiling,0,state),['Hidden manually','Isolation']);
  assert.deepEqual(objectVisibilityReasons(ceiling,0,{...state,filters:{}}),['Hidden manually','Isolation']);
});

test('component toggles, diagnostic scope and unknown objects remain explicit',()=>{
  assert.deepEqual(objectVisibilityReasons(ceiling,0,{componentVisibility:{roof:false},diagnosticOnly:true,diagnosticObjectSet:new Set()}),['Type filter (roof / ceiling)','Diagnostics filter']);
  assert.deepEqual(objectVisibilityReasons(undefined,0,{}),['Missing object']);
  assert.equal(componentFor({surface_type:'GlassDoor'}),'window');
  assert.equal(componentFor({category:'shading'}),'shading');
});
