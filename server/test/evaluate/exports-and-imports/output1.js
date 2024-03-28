84
// ---
f
// ---
f1
// ---
f
// ---
h
// ---
x1
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
  }
}