import * as THREE from "three";
import {surfaceSide} from "./face-visibility.js";
import {GLASS_COLOR, GLASS_OPACITY} from "./appearance.js";

// Give each transparent pane its own center so Three.js can sort it relative to other panes.
export class GlazingLayer {
  // Preserve source coordinates and object IDs; only the render mesh partition changes.
  constructor(scene, positions, faceObjects, glazing) {
    this.scene=scene; this.meshes=new Map();
    this.material=new THREE.MeshBasicMaterial({color:GLASS_COLOR,transparent:true,opacity:GLASS_OPACITY,
      depthWrite:false,side:THREE.FrontSide,forceSinglePass:true,
      polygonOffset:true,polygonOffsetFactor:1,polygonOffsetUnits:1});
    this.pairedMaterial=this.material.clone(); this.pairedMaterial.side=THREE.FrontSide;
    const triangles=new Map([...glazing].map(index=>[index,[]]));
    faceObjects.forEach((index,face)=>{
      if(triangles.has(index))triangles.get(index).push(...positions.subarray(face*9,face*9+9));
    });
    for(const [index,values] of triangles){
      if(!values.length)continue;
      const geometry=new THREE.BufferGeometry();
      geometry.setAttribute("position",new THREE.Float32BufferAttribute(values,3));
      geometry.computeBoundingBox();
      const center=geometry.boundingBox.getCenter(new THREE.Vector3());
      geometry.translate(-center.x,-center.y,-center.z);
      geometry.computeBoundingSphere();
      const mesh=new THREE.Mesh(geometry,this.material);
      mesh.position.copy(center); mesh.userData.sourceCenter=center; mesh.userData.objectIndex=index;
      mesh.name="EnergyPlus glass "+index; mesh.visible=false;
      scene.add(mesh); this.meshes.set(index,mesh);
    }
  }

  // Visibility, explosion and clipping use the same object state as opaque fills and picking.
  update(visible, offsets, paired, planes, backFaces=false) {
    for(const [material,isPaired] of [[this.material,false],[this.pairedMaterial,true]]){
      const side=surfaceSide(backFaces,isPaired);
      if(material.side!==side){material.side=side;material.needsUpdate=true;}
    }
    for(const [index,mesh] of this.meshes){
      mesh.visible=visible.has(index);
      mesh.position.copy(mesh.userData.sourceCenter);
      if(offsets[index])mesh.position.add(offsets[index]);
      mesh.material=paired[index]?this.pairedMaterial:this.material;
    }
    this.setClipping(planes);
  }

  // Shared materials keep section-plane changes consistent across every window.
  setClipping(planes) {
    this.material.clippingPlanes=planes;
    this.pairedMaterial.clippingPlanes=planes;
  }

  // Release per-pane buffers when the widget or source model is replaced.
  dispose() {
    for(const mesh of this.meshes.values()){this.scene.remove(mesh);mesh.geometry.dispose();}
    this.material.dispose(); this.pairedMaterial.dispose(); this.meshes.clear();
  }
}
