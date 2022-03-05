// COPIED FROM https://github.com/ashleydavis/typescript-compilation-example/blob/master/src/index.ts
import ts from "typescript";

//
// A snippet of TypeScript code that has a semantic/type error in it.
//
const code
  = "function foo(input: number) {\n"
  + "    console.log('Hello!');\n"
  + "};\n"
  + "// foo('x');"
  ;

//
// Result of compiling TypeScript code.
//
export interface CompilationResult {
  code?: string;
  diagnostics: ts.Diagnostic[]
};

//
// Check and compile in-memory TypeScript code for errors.
//
function compileTypeScriptCode(code: string, libs: string[]): CompilationResult {
  const options = ts.getDefaultCompilerOptions();
  const realHost = ts.createCompilerHost(options, true);

  const dummyFilePath = "/in-memory-file.ts";
  const dummySourceFile = ts.createSourceFile(dummyFilePath, code, ts.ScriptTarget.Latest);
  let outputCode: string | undefined = undefined;

  const host: ts.CompilerHost = {
    fileExists: filePath => filePath === dummyFilePath || realHost.fileExists(filePath),
    directoryExists: realHost.directoryExists && realHost.directoryExists.bind(realHost),
    getCurrentDirectory: realHost.getCurrentDirectory.bind(realHost),
    getDirectories: realHost.getDirectories?.bind(realHost),
    getCanonicalFileName: fileName => realHost.getCanonicalFileName(fileName),
    getNewLine: realHost.getNewLine.bind(realHost),
    getDefaultLibFileName: realHost.getDefaultLibFileName.bind(realHost),
    getSourceFile: (fileName, languageVersion, onError, shouldCreateNewSourceFile) => fileName === dummyFilePath
      ? dummySourceFile
      : realHost.getSourceFile(fileName, languageVersion, onError, shouldCreateNewSourceFile),
    readFile: filePath => filePath === dummyFilePath
      ? code
      : realHost.readFile(filePath),
    useCaseSensitiveFileNames: () => realHost.useCaseSensitiveFileNames(),
    writeFile: (fileName, data) => outputCode = data,
  };

  const rootNames = libs.map(lib => require.resolve(`typescript/lib/lib.${lib}.d.ts`));
  const program = ts.createProgram(rootNames.concat([dummyFilePath]), options, host);
  const emitResult = program.emit();
  const diagnostics = ts.getPreEmitDiagnostics(program);
  return {
    code: outputCode,
    diagnostics: emitResult.diagnostics.concat(diagnostics)
  };
}

console.log("==== Evaluating code ====");
console.log(code);
console.log();

const libs = ['es2015'];
const result = compileTypeScriptCode(code, libs);

console.log("==== Output code ====");
console.log(result.code);
console.log();

console.log("==== Diagnostics ====");
for (const diagnostic of result.diagnostics) {
  console.log(diagnostic.messageText);
}
console.log();

////////////////

console.log("==== Evaluating code ====");
const code2 = 'foo(123)'
console.log(code2);
console.log();

const result2 = compileTypeScriptCode(code2, libs);

console.log("==== Output code ====");
console.log(result2.code);
console.log();

console.log("==== Diagnostics ====");
for (const diagnostic of result2.diagnostics) {
  console.log(diagnostic.messageText);
}
console.log();
