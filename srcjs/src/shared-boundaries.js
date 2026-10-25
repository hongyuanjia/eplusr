// Compare polygon edges exactly in the serialized source coordinates, without welding nearby vertices.
function boundarySignatures(objects, edgePositions, edgeObjects) {
  const edges = objects.map(() => []);
  for (let segment = 0; segment < edgeObjects.length; segment += 1) {
    const start = segment * 6;
    const endpoints = [Array.from(edgePositions.slice(start, start + 3)).join(","), Array.from(edgePositions.slice(start + 3, start + 6)).join(",")];
    edges[edgeObjects[segment]].push(endpoints.sort().join(":"));
  }
  return edges.map(ring => ring.length >= 3 ? ring.sort().join(";") : null);
}

// Recover each object's triangle orientation from the same vertices used for drawing and picking.
export function faceNormals(objects, positions, faceObjects) {
  const normals = objects.map(() => null);
  for (let face = 0; face < faceObjects.length; face += 1) {
    const object = faceObjects[face];
    if (normals[object]) continue;
    const offset = face * 9;
    const a = [0, 1, 2].map(axis => positions[offset + 3 + axis] - positions[offset + axis]);
    const b = [0, 1, 2].map(axis => positions[offset + 6 + axis] - positions[offset + axis]);
    const cross = [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]];
    const length = Math.hypot(...cross);
    if (length > 1e-12) normals[object] = cross.map(value => value / length);
  }
  return normals;
}

// Recognize reciprocal EnergyPlus boundaries only when rings, openings and opposite normals agree.
export function findSharedBoundaries(objects, positions, faceObjects, edgePositions, edgeObjects) {
  const names = new Map(objects.map((object, index) => [object.name?.toLowerCase(), index]));
  const signatures = boundarySignatures(objects, edgePositions, edgeObjects);
  const normals = faceNormals(objects, positions, faceObjects);
  const holes = objects.map(() => []);
  objects.forEach((object, index) => {
    const parent = names.get(object.building_surface_name?.toLowerCase());
    if (object.category === "subsurface" && parent != null) holes[parent].push(signatures[index]);
  });
  const pairs = new Int32Array(objects.length).fill(-1);
  for (const category of ["surface", "subsurface"]) objects.forEach((object, index) => {
    if (object.category !== category || pairs[index] >= 0) return;
    const partner = names.get(object.outside_boundary_condition_object?.toLowerCase());
    if (partner == null || partner === index || objects[partner].category !== category) return;
    const other = objects[partner];
    if (other.outside_boundary_condition_object?.toLowerCase() !== object.name?.toLowerCase()) return;
    if (category === "surface" && [object, other].some(item => item.outside_boundary_condition?.toLowerCase() !== "surface")) return;
    if (category === "subsurface") {
      const parent = names.get(object.building_surface_name?.toLowerCase()), otherParent = names.get(other.building_surface_name?.toLowerCase());
      if (parent == null || otherParent == null || pairs[parent] !== otherParent) return;
    }
    if (!signatures[index] || signatures[index] !== signatures[partner] || JSON.stringify(holes[index].sort()) !== JSON.stringify(holes[partner].sort())) return;
    if (!normals[index] || !normals[partner] || normals[index].reduce((sum, value, axis) => sum + value * normals[partner][axis], 0) > -0.999999) return;
    pairs[index] = partner;
    pairs[partner] = index;
  });
  return pairs;
}

// Assign contiguous draw groups while leaving source triangle indices and ray-picking identities intact.
export function sharedBoundaryGroups(faceObjects, pairs, visible, offsets, glazing = new Set()) {
  const active = new Uint8Array(pairs.length);
  pairs.forEach((partner, index) => {
    if (partner < 0 || !visible.has(index) || !visible.has(partner)) return;
    const a = offsets[index], b = offsets[partner];
    if (a && b && (a.x - b.x) ** 2 + (a.y - b.y) ** 2 + (a.z - b.z) ** 2 <= 1e-12) active[index] = 1;
  });
  const groups = [];
  faceObjects.forEach((object, face) => {
    const materialIndex = (active[object] ? 1 : 0) + (glazing.has(object) ? 2 : 0);
    const last = groups.at(-1);
    if (last?.materialIndex === materialIndex) last.count += 3;
    else groups.push({ start: face * 3, count: 3, materialIndex });
  });
  return { active, groups };
}

// Report reciprocal boundaries whose actual rings disagree, preserving every original coordinate.
export function mismatchedBoundaryIssues(objects, edgePositions, edgeObjects) {
  const names=new Map(objects.map((object,index)=>[object.name?.toLowerCase(),index]));
  const rings=objects.map(()=>[]);
  edgeObjects.forEach((index,segment)=>rings[index].push(Array.from(edgePositions.subarray(segment*6,segment*6+3))));
  // Point-to-segment distance ignores harmless additional collinear vertices in equivalent rings.
  const distance=(point,ring)=>Math.sqrt(Math.min(...ring.map((a,index)=>{
    const b=ring[(index+1)%ring.length],d=b.map((value,axis)=>value-a[axis]);
    const length=d.reduce((sum,value)=>sum+value*value,0);
    const t=length?Math.max(0,Math.min(1,d.reduce((sum,value,axis)=>sum+value*(point[axis]-a[axis]),0)/length)):0;
    return point.reduce((sum,value,axis)=>sum+(value-a[axis]-t*d[axis])**2,0);
  })));
  const issues=[];
  objects.forEach((object,index)=>{
    const partner=names.get(object.outside_boundary_condition_object?.toLowerCase());
    if(partner==null||partner<=index||object.category!=="surface"||objects[partner].category!=="surface")return;
    if(objects[partner].outside_boundary_condition_object?.toLowerCase()!==object.name?.toLowerCase())return;
    if([object,objects[partner]].some(item=>item.outside_boundary_condition?.toLowerCase()!=="surface"))return;
    const a=rings[index],b=rings[partner];if(a.length<3||b.length<3)return;
    const gap=Math.max(...a.map(point=>distance(point,b)),...b.map(point=>distance(point,a)));
    if(gap<1e-5)return;
    const point=[0,1,2].map(axis=>[...a,...b].reduce((sum,p)=>sum+p[axis],0)/(a.length+b.length));
    issues.push({type:"boundary_mismatch",severity:"warning",object_indices:[index,partner],point,distance:gap,
      label:object.name+" ↔ "+objects[partner].name+": source boundary mismatch up to "+Number(gap.toPrecision(4))+" m. Source geometry retained."});
  });
  return issues;
}
