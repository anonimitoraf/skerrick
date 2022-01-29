const x = 1;
namespaceRegisterExport("/input.js", "x", x)
return namespaceRegisterValue("/input.js", "x", x);
// ---
function f(x) {
  return x;
}

namespaceRegisterExport("/input.js", "f", f)
return namespaceRegisterValue("/input.js", "f", f);
// ---
const x = 1;
namespaceRegisterExport("/input.js", "x", x)
return namespaceRegisterValue("/input.js", "x", x);
// ---
const x = 1;
const y = 2;
namespaceRegisterExport("/input.js", "y", y)
namespaceRegisterExport("/input.js", "x", x)
namespaceRegisterValue("/input.js", "x", x);
return namespaceRegisterValue("/input.js", "y", y);
// ---
const x = 1;
const y = 2;
namespaceRegisterExport("/input.js", "y1", y)
namespaceRegisterExport("/input.js", "x1", x)
namespaceRegisterValue("/input.js", "x", x);
return namespaceRegisterValue("/input.js", "y", y);
// ---
const x = 1;
const y = 2;
namespaceRegisterExport("/input.js", "y", y)
namespaceRegisterExport("/input.js", "x1", x)
namespaceRegisterValue("/input.js", "x", x);
return namespaceRegisterValue("/input.js", "y", y);
// ---
const x = 1;
namespaceRegisterExport("/input.js", "y", y)
namespaceRegisterExport("/input.js", "x1", x)
return namespaceRegisterValue("/input.js", "x", x);
// ---
namespaceRegisterExport("/input.js", "y", y)
namespaceRegisterExport("/input.js", "x1", x)
// ---
// TODO Default exports
const x = 1;
export default x;
return namespaceRegisterValue("/input.js", "x", x);