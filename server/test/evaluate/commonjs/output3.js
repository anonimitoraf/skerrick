A
// ---
A
// ---
{
  "X": [
    "A1A",
    "AA1"
  ],
  "Y": "A1",
  "Z": "A"
}
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
  },
  'commonjs/input3.js': {
    input2: 'input2.js :: Symbol(namespaceExport)',
    p: 'A1',
    q: 'A',
    input2Default: 'input2.js :: Symbol(defaultExport)',
    r: 'A1',
    s: 'A',
    z: 'input2.js :: z',
    y: 'input2.js :: y',
    x: 'input2.js :: x',
    X: 'A1A,AA1',
    Y: 'A1',
    Z: 'A'
  }
}