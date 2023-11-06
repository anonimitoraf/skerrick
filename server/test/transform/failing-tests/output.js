const a = 1;
registerValue("/input.js", "a", a);
export const x = 1,
      y = 2,
      z = 3;
registerValue("/input.js", "z", z);
registerExport("/input.js", "z", "z");
registerValue("/input.js", "y", y);
registerExport("/input.js", "y", "y");
registerValue("/input.js", "x", x);
registerExport("/input.js", "x", "x");
registerValue("/input.js", "z", z);
registerValue("/input.js", "y", y);
registerValue("/input.js", "x", x);
const b = 1;
return registerValue("/input.js", "b", b);