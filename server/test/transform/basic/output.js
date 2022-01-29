const a = 1;
return namespaceRegisterValue("/input.js", "a", a);
// ---
let a = 1;
return namespaceRegisterValue("/input.js", "a", a);
// ---
var a = 1;
return namespaceRegisterValue("/input.js", "a", a);
// ---
return a = 2;