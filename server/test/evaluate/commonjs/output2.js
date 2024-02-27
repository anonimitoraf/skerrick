A
// ---
{
  "w": 1,
  "z": 2
}
// ---
Symbol(namespaceExport)
// --- Environment ---
const exports = {
  'commonjs/input1.js': {
    a: '__export1',
    b: 'B',
    c: 'C',
    d: '__export2',
    [Symbol(namespaceExport)]: Symbol(namespaceExport)
  },
  'commonjs/input2.js': {
    x: '__export4',
    y: 'y',
    w: '__export5',
    z: '__spread_v_z_7',
    [Symbol(namespaceExport)]: Symbol(namespaceExport),
    [Symbol(defaultExport)]: '__export3'
  }
}
const values = {
  'commonjs/input1.js': {
    __export1: '[Function (anonymous)]',
    B: '[Function B]',
    C: '[Function C]',
    __export2: 3,
    [Symbol(namespaceExport)]: '[object Object]'
  },
  'commonjs/input2.js': {
    input: '[object Object]',
    z: 'A',
    b: '[Function B]',
    y: 'A1',
    v: '[object Object]',
    __export3: '[object Object]',
    __export4: 'A1A,AA1',
    __export5: 'hardcoded',
    __spread_v_w_6: 1,
    __spread_v_z_7: 2,
    [Symbol(namespaceExport)]: '[object Object]'
  }
}