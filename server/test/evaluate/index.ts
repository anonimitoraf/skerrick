import util from "util";
import path from "path";
import fs from "fs";
import { evaluate } from "../../src/repl/evaluate";
import { serve } from "../../src";
import {
  exportsLookup,
  resetExportsLookup,
  resetValuesLookup,
  valuesLookup,
  Import,
} from "../../src/repl/state";
import _ from "lodash";

const stopServer = serve();

const isScratch = process.argv[2] === "scratch";

const dirs = isScratch
  ? ["scratchpad"]
  : [
      // "basic",
      "exports-and-imports",
      // "default-exports-and-imports",
      // "imports-built-ins",
      // "commonjs",
      // "async",
      // "native-addons",
      // "dynamic-imports",
    ];

const delimiter = "// ---";

(async () => {
  for (const dir of dirs) {
    resetValuesLookup();
    resetExportsLookup();

    console.log("--------------------------------------------------------");
    console.log(" Evaluation: " + dir);
    console.log("--------------------------------------------------------");

    const rootDir = path.join(__dirname, dir);

    const testCases = fs
      .readdirSync(rootDir)
      .filter((f) => /^input.*/.test(f))
      .map((f) => [f, f.replace(/^input(.*)/, "output$1")])
      .map(([input, output]) => [
        path.join(rootDir, input),
        path.join(rootDir, output),
      ]);

    for (const [inputPath, outputPath] of testCases) {
      const inputBlocksToEval = fs
        .readFileSync(inputPath, "utf-8")
        .split(delimiter);
      fs.writeFileSync(outputPath, "");
      await Promise.all(
        inputBlocksToEval.map((code) => {
          try {
            // `/` to force the filename to be treated as absolute
            return evaluate(inputPath, code);
          } catch (e) {
            return Promise.resolve(e.stack || e.message);
          }
        })
      ).then((xs) =>
        fs.writeFileSync(
          outputPath,
          xs
            .map((x) =>
              typeof x === "object" ? JSON.stringify(x, null, 2) : x
            )
            .join("\n" + delimiter + "\n")
        )
      );

      const simplifyLookup = (lookup) =>
        _(lookup)
          .toPairs()
          .map(([ns, values]) => [
            path.basename(ns),
            _(values)
              .toPairs()
              .map(([key, val]) => {
                let valFormatted = val;
                // Format imports into something like 'input1.js -> f1'
                if (val instanceof Import)
                  valFormatted = `${path.basename(
                    val.importedNamespace
                  )} :: ${val.local.toString()}`;
                else if (typeof val === "function")
                  valFormatted = `[Function ${val.name}]`;
                return [key, valFormatted];
              })
              .fromPairs()
              .value(),
          ])
          .fromPairs()
          .value();

      const simplifiedValues = simplifyLookup(valuesLookup);
      const simplifiedExports = simplifyLookup(exportsLookup);

      fs.appendFileSync(
        outputPath,
        [
          "// --- Environment ---",
          "const exports = " +
            util.inspect(simplifiedExports, { depth: Infinity }),
          "const values = " +
            util.inspect(simplifiedValues, { depth: Infinity }),
        ].join("\n")
      );
    }
  }
})();

stopServer();
