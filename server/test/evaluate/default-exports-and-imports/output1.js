function f(x) {
  return x + x;
}
// --- Environment ---
const exports = {
  'default-exports-and-imports/input1.js': {
    [Symbol(namespaceExport)]: Symbol(namespaceExport),
    [Symbol(defaultExport)]: 'f'
  }
}
const values = {
  'default-exports-and-imports/input1.js': {
    f: '[Function f]',
    [Symbol(namespaceExport)]: { default: [Function: f] }
  }
}