const x = 1;
doRegisterValue("/input.js", "x", x);
doRegisterExport("/input.js", "x", "x");
return doRegisterValue("/input.js", "x", x);
// ---
const x = 1,
      y = 2;
doRegisterValue("/input.js", "y", y);
doRegisterExport("/input.js", "y", "y");
doRegisterValue("/input.js", "x", x);
doRegisterExport("/input.js", "x", "x");
doRegisterValue("/input.js", "y", y);
return doRegisterValue("/input.js", "x", x);
// ---
function g() {}

doRegisterValue("/input.js", "g", g);

function f() {}

doRegisterValue("/input.js", "f", f);
doRegisterExport("/input.js", "f", "f");
return doRegisterValue("/input.js", "f", f);
// ---
class C {}

doRegisterValue("/input.js", "C", C);
doRegisterExport("/input.js", "C", "C");
return doRegisterValue("/input.js", "C", C);
// ---
function* gen() {}

doRegisterValue("/input.js", "gen", gen);
doRegisterExport("/input.js", "gen", "gen");
return doRegisterValue("/input.js", "gen", gen);
// ---
function f(x) {
  return x;
}

doRegisterValue("/input.js", "f", f);
doRegisterExport("/input.js", "f", "f");
return doRegisterValue("/input.js", "f", f);
// ---
const x = 1;
doRegisterValue("/input.js", "x", x);
return doRegisterExport("/input.js", "x", "x");
// ---
const x = 1;
doRegisterValue("/input.js", "x", x);
const y = 2;
doRegisterValue("/input.js", "y", y);
doRegisterExport("/input.js", "y", "y");
return doRegisterExport("/input.js", "x", "x");
// ---
const x = 1;
doRegisterValue("/input.js", "x", x);
const y = 2;
doRegisterValue("/input.js", "y", y);
doRegisterExport("/input.js", "y", "y1");
return doRegisterExport("/input.js", "x", "x1");
// ---
const x = 1;
doRegisterValue("/input.js", "x", x);
const y = 2;
doRegisterValue("/input.js", "y", y);
doRegisterExport("/input.js", "y", "y");
return doRegisterExport("/input.js", "x", "x1");
// ---
const a = 1,
      b = 2,
      y = 3;
doRegisterValue("/input.js", "y", y);
doRegisterValue("/input.js", "b", b);
doRegisterValue("/input.js", "a", a);
doRegisterExport("/input.js", "y", "y");
doRegisterExport("/input.js", "x", "x1");
doRegisterExport("/input.js", "a", "a");
return doRegisterExport("/input.js", "b", "b");
// ---
return doRegisterExport("/input.js", "x", "x var");
// ---
const x = 1;
doRegisterValue("/input.js", "x", x);
doRegisterValue("/input.js", "x", x);
return doRegisterDefaultExport("/input.js", "x");
// ---
class C {}

doRegisterValue("/input.js", "C", C);
doRegisterDefaultExport("/input.js", "C");
return doRegisterValue("/input.js", "C", C);
// ---
function f() {}

doRegisterValue("/input.js", "f", f);
doRegisterDefaultExport("/input.js", "f");
return doRegisterValue("/input.js", "f", f);
// ---
function* gen() {}

doRegisterValue("/input.js", "gen", gen);
doRegisterDefaultExport("/input.js", "gen");
return doRegisterValue("/input.js", "gen", gen);
// ---
class __defaultExport1 {}

doRegisterValue("/input.js", "__defaultExport1", __defaultExport1);
return doRegisterDefaultExport("/input.js", "__defaultExport1");
// ---
function __defaultExport2() {}

doRegisterValue("/input.js", "__defaultExport2", __defaultExport2);
return doRegisterDefaultExport("/input.js", "__defaultExport2");
// ---
function* __defaultExport3() {}

doRegisterValue("/input.js", "__defaultExport3", __defaultExport3);
return doRegisterDefaultExport("/input.js", "__defaultExport3");
// ---
return doRegisterDefaultExport("/input.js", "x");