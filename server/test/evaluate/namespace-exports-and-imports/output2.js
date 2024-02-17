6
// --- Environment ---
const exports = {
  'namespace-exports-and-imports/input1.js': { a: 'a', b: 'b', [Symbol(defaultExport)]: 'c' }
}
const values = {
  'namespace-exports-and-imports/input1.js': { a: '[Function a]', b: '[Function b]', c: 3 },
  'namespace-exports-and-imports/input2.js': { input1: '[object Object]' }
}