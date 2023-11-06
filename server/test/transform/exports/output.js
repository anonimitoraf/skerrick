const x = 1;
registerValue("/input.js", "x", x);
registerExport("/input.js", "x", "x");
return registerValue("/input.js", "x", x);
// ---
const x = 1,
      y = 2;
registerValue("/input.js", "y", y);
registerExport("/input.js", "y", "y");
registerValue("/input.js", "x", x);
registerExport("/input.js", "x", "x");
registerValue("/input.js", "y", y);
return registerValue("/input.js", "x", x);
// ---
function f() {}

registerValue("/input.js", "f", f);
registerExport("/input.js", "f", "f");
return registerValue("/input.js", "f", f);
// ---
class C {}

registerValue("/input.js", "C", C);
registerExport("/input.js", "C", "C");
return registerValue("/input.js", "C", C);
// ---
function* gen() {}

registerValue("/input.js", "gen", gen);
registerExport("/input.js", "gen", "gen");
return registerValue("/input.js", "gen", gen);
// ---
function f(x) {
  return x;
}

registerValue("/input.js", "f", f);
registerExport("/input.js", "f", "f");
return registerValue("/input.js", "f", f);
// ---
const x = 1;
registerValue("/input.js", "x", x);
return registerExport("/input.js", "x", "x");
// ---
const x = 1;
registerValue("/input.js", "x", x);
const y = 2;
registerValue("/input.js", "y", y);
registerExport("/input.js", "y", "y");
return registerExport("/input.js", "x", "x");
// ---
const x = 1;
registerValue("/input.js", "x", x);
const y = 2;
registerValue("/input.js", "y", y);
registerExport("/input.js", "y", "y1");
return registerExport("/input.js", "x", "x1");
// ---
const x = 1;
registerValue("/input.js", "x", x);
const y = 2;
registerValue("/input.js", "y", y);
registerExport("/input.js", "y", "y");
return registerExport("/input.js", "x", "x1");
// ---
const a = 1,
      b = 2,
      y = 3;
registerValue("/input.js", "y", y);
registerValue("/input.js", "b", b);
registerValue("/input.js", "a", a);
registerExport("/input.js", "y", "y");
registerExport("/input.js", "x", "x1");
registerExport("/input.js", "a", "a");
return registerExport("/input.js", "b", "b");
// ---
const x = 1;
registerValue("/input.js", "x", x);
registerValue("/input.js", "x", x);
return registerDefaultExport("/input.js", "x");
// ---
class C {}

registerValue("/input.js", "C", C);
registerDefaultExport("/input.js", "C");
return registerValue("/input.js", "C", C);
// ---
function f() {}

registerValue("/input.js", "f", f);
registerDefaultExport("/input.js", "f");
return registerValue("/input.js", "f", f);
// ---
function* gen() {}

registerValue("/input.js", "gen", gen);
registerDefaultExport("/input.js", "gen");
return registerValue("/input.js", "gen", gen);
// ---
class __defaultExport1 {}

registerValue("/input.js", "__defaultExport1", __defaultExport1);
return registerDefaultExport("/input.js", "__defaultExport1");
// ---
function __defaultExport2() {}

registerValue("/input.js", "__defaultExport2", __defaultExport2);
return registerDefaultExport("/input.js", "__defaultExport2");
// ---
function* __defaultExport3() {}

registerValue("/input.js", "__defaultExport3", __defaultExport3);
return registerDefaultExport("/input.js", "__defaultExport3");