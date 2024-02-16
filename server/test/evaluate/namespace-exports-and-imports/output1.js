function b() {
  return 2;
}
// --- Environment ---
const exports = { 'namespace-exports-and-imports/input1.js': { a: 'a', b: 'b' } }
const values = {
  'namespace-exports-and-imports/input1.js': { a: '[Function a]', b: '[Function b]' }
}