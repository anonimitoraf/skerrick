A
// ---
{
  "w": 1,
  "z": 2
}
// ---
{
  "x": [
    "A1A",
    "AA1"
  ],
  "y": "A1",
  "w": "hardcoded",
  "z": 2,
  "default": {
    "x": [
      "A1A",
      "AA1"
    ],
    "y": "A1",
    "w": "hardcoded",
    "z": 2
  }
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
    [Symbol(namespaceExport)]: { a: [Function (anonymous)], b: [Function: B], c: [class C], d: 3 }
  },
  'commonjs/input2.js': {
    input: { a: [Function (anonymous)], b: [Function: B], c: [class C], d: 3 },
    z: 'A',
    b: '[Function B]',
    y: 'A1',
    v: { w: 1, z: 2 },
    __export5: { x: [ 'A1A', 'AA1' ], y: 'A1', w: 'hardcoded', z: 2 },
    __export6: [ 'A1A', 'AA1' ],
    __export7: 'hardcoded',
    __spread_v_w_8: 1,
    __spread_v_z_9: 2,
    [Symbol(namespaceExport)]: {
      x: [ 'A1A', 'AA1' ],
      y: 'A1',
      w: 'hardcoded',
      z: 2,
      default: { x: [ 'A1A', 'AA1' ], y: 'A1', w: 'hardcoded', z: 2 }
    }
  }
}