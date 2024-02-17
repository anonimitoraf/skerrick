Symbol(defaultExport)
// --- Environment ---
const exports = {
  'namespace-exports-and-imports/input1.js': {
    a: 'a',
    b: 'b',
    [Symbol(namespaceExport)]: Symbol(namespaceExport),
    [Symbol(defaultExport)]: 'c'
  }
}
const values = {
  'namespace-exports-and-imports/input1.js': {
    a: '[Function a]',
    b: '[Function b]',
    c: 3,
    [Symbol(namespaceExport)]: '[object Object]'
  }
}