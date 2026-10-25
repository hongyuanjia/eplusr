import test from "node:test";
import assert from "node:assert/strict";
import { findSharedBoundaries, sharedBoundaryGroups } from "../src/shared-boundaries.js";

// Create two reciprocal boundaries with independent source IDs and opposite winding.
function fixture() {
  const points = [[0,0,0],[2,0,0],[2,0,2],[0,0,2]];
  const reverse = [...points].reverse();
  const triangle = ring => [0,1,2,0,2,3].flatMap(index => ring[index]);
  const edges = ring => ring.flatMap((point,index) => [...point,...ring[(index+1)%ring.length]]);
  return {
    objects: [{name:'A',category:'surface',outside_boundary_condition:'Surface',outside_boundary_condition_object:'B'}, {name:'B',category:'surface',outside_boundary_condition:'Surface',outside_boundary_condition_object:'A'}],
    positions: new Float32Array([...triangle(points),...triangle(reverse)]),
    faces: new Int32Array([0,0,1,1]), edges: new Float32Array([...edges(points),...edges(reverse)]), edgeObjects: new Int32Array([0,0,0,0,1,1,1,1])
  };
}

test('only reciprocal coincident boundaries with opposite normals are paired',()=>{
  const f=fixture(),find=()=>findSharedBoundaries(f.objects,f.positions,f.faces,f.edges,f.edgeObjects);
  assert.deepEqual([...find()],[1,0]);
  f.objects[1].outside_boundary_condition_object='Other';assert.deepEqual([...find()],[-1,-1]);
  f.objects[1].outside_boundary_condition_object='A';f.positions.set(f.positions.slice(0,18),18);assert.deepEqual([...find()],[-1,-1]);
});

test('a real coordinate discrepancy is preserved rather than snapped',()=>{
  const f=fixture();for(let i=24;i<f.edges.length;i+=3)f.edges[i]+=0.0002;
  assert.deepEqual([...findSharedBoundaries(f.objects,f.positions,f.faces,f.edges,f.edgeObjects)],[-1,-1]);
});

test('different openings leave boundaries unresolved for geometry inspection',()=>{
  const f=fixture();f.objects.push({name:'Door',category:'subsurface',building_surface_name:'A'});
  assert.deepEqual([...findSharedBoundaries(f.objects,f.positions,f.faces,f.edges,f.edgeObjects)],[-1,-1,-1]);
});

test('single-Zone filtering and separated explosion retain independent source faces',()=>{
  const f=fixture(),pairs=new Int32Array([1,0]),offsets=[{x:0,y:0,z:0},{x:0,y:0,z:0}];
  const together=sharedBoundaryGroups(f.faces,pairs,new Set([0,1]),offsets);
  assert.deepEqual([...together.active],[1,1]);assert.deepEqual(together.groups,[{start:0,count:12,materialIndex:1}]);
  assert.deepEqual([...sharedBoundaryGroups(f.faces,pairs,new Set([0]),offsets).active],[0,0]);
  offsets[1].x=1;assert.deepEqual([...sharedBoundaryGroups(f.faces,pairs,new Set([0,1]),offsets).active],[0,0]);
  assert.deepEqual([...f.faces],[0,0,1,1]);
});

// Genuine source mismatches must be discoverable without silently snapping vertices or erasing objects.
test('shifted reciprocal boundaries are reported and retain their source coordinates',async()=>{
  const {mismatchedBoundaryIssues}=await import('../src/shared-boundaries.js');
  const f=fixture();assert.deepEqual(mismatchedBoundaryIssues(f.objects,f.edges,f.edgeObjects),[]);
  for(let i=24;i<f.edges.length;i+=3)f.edges[i]+=0.04;
  const original=f.edges.slice(),issues=mismatchedBoundaryIssues(f.objects,f.edges,f.edgeObjects);
  assert.equal(issues.length,1);assert.ok(Math.abs(issues[0].distance-0.04)<1e-6);
  assert.deepEqual(issues[0].object_indices,[0,1]);assert.deepEqual(f.edges,original);
  f.objects[1].outside_boundary_condition_object='Other';
  assert.deepEqual(mismatchedBoundaryIssues(f.objects,f.edges,f.edgeObjects),[]);
});
