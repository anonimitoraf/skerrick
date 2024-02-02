import path from "path";
import fs from "fs";
import { transform } from "../../src/repl/transform";

const dirs = [
  // "basic",
  // "exports",
  // "imports",
  "failing-tests",
];

for (const dir of dirs) {
  const inputPath = path.join(__dirname, dir, "input.js");
  const outputPath = path.join(__dirname, dir, "output.js");

  const delimiter = "// ---";
  const blocksToEval = fs.readFileSync(inputPath, "utf-8").split(delimiter);
  const output = blocksToEval
    // Ignore comments
    .filter((line) => !/^\s*\/\//.test(line))
    .map((code) => {
      try {
        return transform("/input.js", code);
      } catch (e) {
        const err = e.stack || e.message;
        return `${code}\n${err}`;
      }
    })
    .join("\n" + delimiter + "\n");
  fs.writeFileSync(outputPath, output);
}
