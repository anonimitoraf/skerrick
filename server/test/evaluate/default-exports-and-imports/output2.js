20
// --- Environment ---
const exports = {
  'default-exports-and-imports/input1.js': {
    [Symbol(namespaceExport)]: Symbol(namespaceExport),
    [Symbol(defaultExport)]: 'f'
  },
  'default-exports-and-imports/input2.js': {
    [Symbol(namespaceExport)]: Symbol(namespaceExport),
    [Symbol(defaultExport)]: 'y'
  }
}
const values = {
  'default-exports-and-imports/input1.js': { f: '[Function f]', [Symbol(namespaceExport)]: '[object Object]' },
  'default-exports-and-imports/input2.js': {
    input1: 'input1.js :: Symbol(defaultExport)',
    y: 20,
    [Symbol(namespaceExport)]: '[object Object]'
  }
}