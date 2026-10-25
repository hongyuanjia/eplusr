import {FrontSide,BackSide,DoubleSide} from "three";

// Keep default fills and glass single-sided; double-sided inspection is an explicit opt-in.
export function surfaceSide(backFaces=false,paired=false,normalMode=false){
  if(!backFaces||normalMode)return FrontSide;
  return paired?BackSide:DoubleSide;
}

// Apply the same facing rule to edge/highlight buffers that WebGL applies to filled triangles.
export function faceIsVisible(normal,center,cameraPosition,viewDirection=null,{backFaces=false,paired=false,normalMode=false}={}){
  if(!normal)return true;
  const view=viewDirection?[-viewDirection.x,-viewDirection.y,-viewDirection.z]
    :[cameraPosition.x-center.x,cameraPosition.y-center.y,cameraPosition.z-center.z];
  const facing=normal.reduce((sum,value,axis)=>sum+value*view[axis],0);
  if(backFaces&&normalMode&&!paired)return true; // Normal inspection has an explicit red back-face pass.
  const side=surfaceSide(backFaces,paired,normalMode);
  return side===DoubleSide||(side===FrontSide?facing>1e-9:facing< -1e-9);
}
