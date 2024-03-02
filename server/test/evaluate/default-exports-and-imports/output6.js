500
// --- Environment ---
const exports = {
  'default-exports-and-imports/input1.js': {
    [Symbol(namespaceExport)]: Symbol(namespaceExport),
    [Symbol(defaultExport)]: 'f'
  },
  'default-exports-and-imports/input2.js': {
    [Symbol(namespaceExport)]: Symbol(namespaceExport),
    [Symbol(defaultExport)]: 'y'
  },
  'default-exports-and-imports/input3.js': {
    [Symbol(namespaceExport)]: Symbol(namespaceExport),
    [Symbol(defaultExport)]: 'Foo'
  },
  'default-exports-and-imports/input4.js': {
    [Symbol(namespaceExport)]: Symbol(namespaceExport),
    [Symbol(defaultExport)]: '__defaultExport1'
  },
  'default-exports-and-imports/input5.js': {
    [Symbol(namespaceExport)]: Symbol(namespaceExport),
    [Symbol(defaultExport)]: '__defaultExport2'
  }
}
const values = {
  'default-exports-and-imports/input1.js': { f: '[Function f]', [Symbol(namespaceExport)]: '[object Object]' },
  'default-exports-and-imports/input2.js': {
    input1: 'input1.js :: Symbol(defaultExport)',
    y: 20,
    [Symbol(namespaceExport)]: '[object Object]'
  },
  'default-exports-and-imports/input3.js': {
    input2: 'input2.js :: Symbol(defaultExport)',
    Foo: '[Function Foo]',
    foo: '[Instance Foo]',
    [Symbol(namespaceExport)]: '[object Object]'
  },
  'default-exports-and-imports/input4.js': {
    Foo: 'input3.js :: Symbol(defaultExport)',
    foo: '[Instance Foo]',
    Baz: '[Function Baz]',
    __defaultExport1: '[Function __defaultExport1]',
    [Symbol(namespaceExport)]: '[object Object]'
  },
  'default-exports-and-imports/input5.js': {
    input4: 'input4.js :: Symbol(defaultExport)',
    g: 500,
    __defaultExport2: '[Function __defaultExport2]',
    [Symbol(namespaceExport)]: '[object Object]'
  },
  'default-exports-and-imports/input6.js': { input5: 'input5.js :: Symbol(defaultExport)' }
}