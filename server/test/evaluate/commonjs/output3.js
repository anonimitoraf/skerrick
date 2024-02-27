2
// ---
2
// ---
{
  "W": "hardcoded",
  "X": [
    "A1A",
    "AA1"
  ],
  "Y": "A1",
  "Z": 2
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
    w: '__export7',
    z: '__spread_v_z_9',
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
    v: '[object Object]',
    __export5: '[object Object]',
    __export6: 'A1A,AA1',
    __export7: 'hardcoded',
    __spread_v_w_8: 1,
    __spread_v_z_9: 2,
    [Symbol(namespaceExport)]: '[object Object]'
  },
  'commonjs/input3.js': {
    input2: 'input2.js :: Symbol(namespaceExport)',
    p: 'A1',
    q: 2,
    input2Default: 'input2.js :: Symbol(defaultExport)',
    r: 'A1',
    s: 2,
    z: 'input2.js :: z',
    y: 'input2.js :: y',
    x: 'input2.js :: x',
    w: 'input2.js :: w',
    W: 'hardcoded',
    X: 'A1A,AA1',
    Y: 'A1',
    Z: 2
  }
}