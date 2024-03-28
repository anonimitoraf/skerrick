20
// ---
30
// ---
30
// ---
60
// ---
90
// ---
400
// ---

// ---
1.5
// ---
1.5
// ---
2
// ---
3
// ---
3
// ---
4
// ---

// --- Environment ---
const exports = {
  'exports-and-imports/input1.js': {
    f: 'f',
    f1: 'f',
    g: 'g',
    g1: 'g',
    g2: 'g',
    h: 'h',
    x: 'x',
    y: 'y',
    x1: 'x',
    [Symbol(namespaceExport)]: Symbol(namespaceExport)
  }
}
const values = {
  'exports-and-imports/input1.js': {
    f: '[Function f]',
    g: '[Function g]',
    h: '[Function h]',
    x: 1.5,
    y: 2,
    [Symbol(namespaceExport)]: {
      f: [Function: f],
      f1: [Function: f],
      g: [Function: g],
      g1: [Function: g],
      g2: [Function: g],
      h: [Function: h],
      x: 1.5,
      y: 2,
      x1: 1.5
    }
  },
  'exports-and-imports/input2.js': {
    FFFFF1: 'input1.js :: f1',
    f: 'input1.js :: f',
    g2: 'input1.js :: g2',
    g1: 'input1.js :: g1',
    g: 'input1.js :: g',
    h: 'input1.js :: h',
    y: 'input1.js :: y',
    x1: 'input1.js :: x1',
    x: 'input1.js :: x'
  }
}