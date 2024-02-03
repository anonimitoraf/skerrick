doRegisterDefaultImport("/input.js", "D", "./input-1");
doRegisterImport("/input.js", "B", "./input-1", "b");
doRegisterImport("/input.js", "a", "./input-1", "a");
doRegisterDefaultImport("/input.js", "input1", "./input-1");
const x = 42;
doRegisterValue("/input.js", "x", x);
doRegisterExport("/input.js", "x", "x");
doRegisterValue("/input.js", "x", x);
return console.log({
  a,
  B,
  D,
  input1
});