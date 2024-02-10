20
// --- Environment ---
const exports = {
  'input1.js': { [Symbol(defaultExport)]: '[Function f]' },
  'input2.js': { [Symbol(defaultExport)]: 20 }
}
const values = {
  'input1.js': { f: '[Function f]' },
  'input2.js': { input1: 'input1.js :: Symbol(defaultExport)', y: 20 }
}