500
// --- Environment ---
const exports = {
  'default-exports-and-imports/input1.js': { [Symbol(defaultExport)]: '[Function f]' },
  'default-exports-and-imports/input2.js': { [Symbol(defaultExport)]: 20 },
  'default-exports-and-imports/input3.js': { [Symbol(defaultExport)]: '[Function Foo]' },
  'default-exports-and-imports/input4.js': { [Symbol(defaultExport)]: '[Function __defaultExport1]' },
  'default-exports-and-imports/input5.js': { [Symbol(defaultExport)]: '[Function __defaultExport2]' }
}
const values = {
  'default-exports-and-imports/input1.js': { f: '[Function f]' },
  'default-exports-and-imports/input2.js': { input1: 'input1.js :: Symbol(defaultExport)', y: 20 },
  'default-exports-and-imports/input3.js': {
    input2: 'input2.js :: Symbol(defaultExport)',
    Foo: '[Function Foo]',
    foo: '[Instance Foo]'
  },
  'default-exports-and-imports/input4.js': {
    Foo: 'input3.js :: Symbol(defaultExport)',
    foo: '[Instance Foo]',
    Baz: '[Function Baz]',
    __defaultExport1: '[Function __defaultExport1]'
  },
  'default-exports-and-imports/input5.js': {
    input4: 'input4.js :: Symbol(defaultExport)',
    g: 500,
    __defaultExport2: '[Function __defaultExport2]'
  }
}