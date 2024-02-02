const a = 1;
doRegisterValue("/input.js", "a", a);
const b = 1;
doRegisterValue("/input.js", "b", b);

function f() {
  const c = 42;
}

return doRegisterValue("/input.js", "f", f);