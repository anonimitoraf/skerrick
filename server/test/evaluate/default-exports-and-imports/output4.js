400
// --- Environment ---
const exports = {
  'input1.js': { [Symbol(defaultExport)]: '[Function f]' },
  'input2.js': { [Symbol(defaultExport)]: 20 },
  'input3.js': { [Symbol(defaultExport)]: '[Function Foo]' },
  'input4.js': { [Symbol(defaultExport)]: '[Function __defaultExport1]' }
}
const values = {
  'input1.js': { f: '[Function f]' },
  'input2.js': { input1: 'input1.js :: Symbol(defaultExport)', y: 20 },
  'input3.js': {
    input2: 'input2.js :: Symbol(defaultExport)',
    Foo: '[Function Foo]',
    foo: '[Instance Foo]'
  },
  'input4.js': {
    Foo: 'input3.js :: Symbol(defaultExport)',
    foo: '[Instance Foo]',
    Baz: '[Function Baz]',
    __defaultExport1: '[Function __defaultExport1]'
  }
}