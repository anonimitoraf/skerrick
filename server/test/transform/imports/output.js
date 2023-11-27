doRegisterImport("/input.js", "H", "/somewhere", "h");
doRegisterImport("/input.js", "g", "/somewhere", "g");
return doRegisterImport("/input.js", "f", "/somewhere", "f");
// ---
return doRegisterDefaultImport("/input.js", "defaultExport", "module-name");
// ---
doRegisterImport("/input.js", "G", "/somewhere", "g");
doRegisterImport("/input.js", "f", "/somewhere", "f");
return doRegisterDefaultImport("/input.js", "defaultExport", "/somewhere");
// ---
return doRegisterNamespaceImport("/input.js", "a", "module-name");
// ---
doRegisterNamespaceImport("/input.js", "name", "module-name");
return doRegisterDefaultImport("/input.js", "defaultExport", "module-name");
// ---
