// Drill through visible semantic boundaries without including hidden objects or inventing a Space level.
export function doubleClickSelection(objects, visibleObjects, objectIndex, previousSelection, drilledZone = null) {
  const object = objects[objectIndex];
  if (!object || !visibleObjects.has(objectIndex)) return [];
  const zone = object.zone_name;
  if (!zone) return [objectIndex];
  const group = [...visibleObjects].filter(index => objects[index]?.zone_name === zone);
  // Inspect the selection before the first click: native double-click dispatch includes two clicks.
  const wholeZone = group.length === previousSelection.size && group.every(index => previousSelection.has(index));
  // Once inside a Zone, further double-clicks stay at face level until the context changes.
  const insideZone = zone === drilledZone && previousSelection.size > 0 &&
    [...previousSelection].every(index => objects[index]?.zone_name === zone);
  return wholeZone || insideZone ? [objectIndex] : group;
}
