A
// ---
A1
// ---
Symbol(namespaceExport)
// --- Environment ---
const exports = {
  'commonjs/input1.js': {
    a: '__export3',
    b: 'B',
    c: 'C',
    d: '__export4',
    [Symbol(namespaceExport)]: Symbol(namespaceExport)
  },
  'commonjs/input2.js': {
    x: '__export6',
    y: 'y',
    z: 'z',
    [Symbol(namespaceExport)]: Symbol(namespaceExport),
    [Symbol(defaultExport)]: '__export5'
  }
}
const values = {
  'commonjs/input1.js': {
    __export3: '[Function (anonymous)]',
    B: '[Function B]',
    C: '[Function C]',
    __export4: 3,
    [Symbol(namespaceExport)]: '[object Object]'
  },
  'commonjs/input2.js': {
    input: '[object Object]',
    z: 'A',
    b: '[Function B]',
    y: 'A1',
    __export5: '[object Object]',
    __export6: 'A1A,AA1',
    [Symbol(namespaceExport)]: '[object Object]'
  }
}