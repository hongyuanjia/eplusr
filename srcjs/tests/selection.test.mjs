import test from 'node:test';
import assert from 'node:assert/strict';
import { doubleClickSelection } from '../src/selection.js';

// Use separate Zone and Space memberships to catch accidental Space-first drilling.
const objects = [{zone_name:'A',space_name:'Room 1'}, {zone_name:'A',space_name:'Room 2'}, {zone_name:'A'}, {zone_name:'B'}, {zone_name:'B'}, {}];
const visible = new Set([0,1,2,3,4,5]);

test('first double-click selects the complete Zone across Spaces', () => {
  assert.deepEqual(doubleClickSelection(objects,visible,0,new Set()),[0,1,2]);
  assert.deepEqual(doubleClickSelection(objects,visible,1,new Set([1])),[0,1,2]);
});

test('next double-click inside the selected Zone drills to the clicked face', () => {
  assert.deepEqual(doubleClickSelection(objects,visible,1,new Set([0,1,2])),[1]);
});

test('another Zone and a multi-Zone selection start at the target Zone', () => {
  assert.deepEqual(doubleClickSelection(objects,visible,3,new Set([0,1,2])),[3,4]);
  assert.deepEqual(doubleClickSelection(objects,visible,1,new Set([0,1,2,3,4])),[0,1,2]);
});

test('Zone drilling respects filters and hidden objects', () => {
  const filtered = new Set([0,2,3,5]);
  assert.deepEqual(doubleClickSelection(objects,filtered,0,new Set()),[0,2]);
  assert.deepEqual(doubleClickSelection(objects,filtered,2,new Set([0,2])),[2]);
  assert.deepEqual(doubleClickSelection(objects,filtered,1,new Set()),[]);
});

test('unassigned shading falls back to its face', () => {
  assert.deepEqual(doubleClickSelection(objects,visible,5,new Set([0,1,2])),[5]);
});

// Face inspection remains inside the entered Zone instead of alternating back to the Zone.
test('repeated face drilling stays inside the Zone and clearing starts over', () => {
  assert.deepEqual(doubleClickSelection(objects,visible,2,new Set([1]),'A'),[2]);
  assert.deepEqual(doubleClickSelection(objects,visible,0,new Set(),'A'),[0,1,2]);
  assert.deepEqual(doubleClickSelection(objects,visible,3,new Set([1]),'A'),[3,4]);
});
