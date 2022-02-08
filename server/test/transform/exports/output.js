const x = 1;
registerValueExport("/input.js", "x", "x");
return registerValue("/input.js", "x", x);
// ---
function f(x) {
  return x;
}

registerValueExport("/input.js", "f", "f")
registerValue("/input.js", "f", f)
// ---
const x = 1;
registerValue("/input.js", "x", x)
registerValueExport("/input.js", "x", "x")
// ---
const x = 1;
registerValue("/input.js", "x", x)
const y = 2;
registerValue("/input.js", "y", y)
registerValueExport("/input.js", "y", "y")
registerValueExport("/input.js", "x", "x")
// ---
const x = 1;
registerValue("/input.js", "x", x)
const y = 2;
registerValue("/input.js", "y", y)
registerValueExport("/input.js", "y", "y1")
registerValueExport("/input.js", "x", "x1")
// ---
const x = 1;
registerValue("/input.js", "x", x)
const y = 2;
registerValue("/input.js", "y", y)
registerValueExport("/input.js", "y", "y")
registerValueExport("/input.js", "x", "x1")
// ---
const x = 1;
registerValue("/input.js", "x", x)
registerValueExport("/input.js", "y", "y")
registerValueExport("/input.js", "x", "x1")
// ---
registerValueExport("/input.js", "y", "y")
registerValueExport("/input.js", "x", "x1")
registerValueExport("/input.js", "a", "a")
registerValueExport("/input.js", "b", "b")
// ---
const x = 1;
registerValue("/input.js", "x", x)
x;
registerDefaultValueExport("/input.js", "x")
// ---
const a = 1;
registerValueExport("/input.js", "a", "a");
registerValue("/input.js", "a", a);
const b = 1;
registerValueExport("/input.js", "b", "b");
registerValue("/input.js", "b", b);
const c = 1;
registerValueExport("/input.js", "c", "c");
return registerValue("/input.js", "c", c);