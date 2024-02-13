function f(x) {
  return x + x;
}
// --- Environment ---
const exports = {
  'default-exports-and-imports/input1.js': { [Symbol(defaultExport)]: '[Function f]' }
}
const values = { 'default-exports-and-imports/input1.js': { f: '[Function f]' } }