// Decode little-endian binary vectors packed by the R scene serializer.
function decodeBase64(value) {
  if (!value) return new ArrayBuffer(0);
  const binary = window.atob(value);
  const bytes = new Uint8Array(binary.length);
  for (let index = 0; index < binary.length; index += 1) {
    bytes[index] = binary.charCodeAt(index);
  }
  return bytes.buffer;
}

export function float32(value) {
  return new Float32Array(decodeBase64(value));
}

export function int32(value) {
  return new Int32Array(decodeBase64(value));
}
