a
// ---
b
// ---
c
// ---
d
// --- Environment ---
const exports = {
  'commonjs/input1.js': {
    a: '__export1',
    b: 'B',
    c: 'C',
    d: '__export2',
    [Symbol(namespaceExport)]: Symbol(namespaceExport)
  }
}
const values = {
  'commonjs/input1.js': {
    __export1: '[Function (anonymous)]',
    B: '[Function B]',
    C: '[Function C]',
    __export2: 3,
    [Symbol(namespaceExport)]: '[object Object]'
  }
}