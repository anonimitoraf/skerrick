
// --- Environment ---
const exports = {
  'test-raf-module/index.js': {
    lowercasize: '[Function lowercasize]',
    [Symbol(defaultExport)]: '[Function capitalize]'
  }
}
const values = {
  'imports-built-ins/input.js': {
    testModule: {},
    capitalize: 'index.js :: Symbol(defaultExport)',
    upper: 'HELLO'
  },
  'test-raf-module/index.js': {
    capitalize: '[Function capitalize]',
    lowercasize: '[Function lowercasize]'
  }
}