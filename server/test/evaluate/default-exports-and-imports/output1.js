function f(x) {
  return x + x;
}
// --- Environment ---
const exports = { 'input1.js': { [Symbol(defaultExport)]: '[Function f]' } }
const values = { 'input1.js': { f: '[Function f]' } }