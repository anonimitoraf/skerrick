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

const test = process.argv[2];

const dirs = test
  ? [test]
  : [
      "basic",
      "exports-and-imports",
      "default-exports-and-imports",
      "namespace-exports-and-imports",
      "async",
      "commonjs",
      // TODO Auto-eval all imports?
      // "imports-built-ins",
      // "dynamic-imports",
      // "native-addons",
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
        }),
      ).then((xs) =>
        fs.writeFileSync(
          outputPath,
          xs
            .map((x) =>
              typeof x === "object" ? JSON.stringify(x, null, 2) : x,
            )
            .join("\n" + delimiter + "\n"),
        ),
      );

      const simplifyLookup = (lookup) => {
        const simplified = {};
        for (const [ns, values] of Object.entries(lookup)) {
          const formatValue = (value: any) => {
            // Format imports into something like 'input1.js -> f1'
            if (value instanceof Import) {
              return `${path.basename(
                value.importedNamespace,
              )} :: ${value.local.toString()}`;
            } else if (typeof value === "function")
              return `[Function ${value.name || "(anonymous)"}]`;
            else if (typeof value === "object" && value !== null) {
              return value.toString();
            }
            return value;
          };
          // Ensure that both string keys and symbol keys are included
          for (const key of Object.getOwnPropertyNames(values)) {
            const value = lookup[ns][key];
            _.set(
              simplified,
              [ns.split("/").slice(-2).join("/"), key],
              formatValue(value),
            );
          }
          for (const key of Object.getOwnPropertySymbols(values)) {
            const value = lookup[ns][key];
            _.set(
              simplified,
              [ns.split("/").slice(-2).join("/"), key],
              formatValue(value),
            );
          }
        }
        return simplified;
      };

      const simplifiedValues = simplifyLookup(valuesLookup);
      const simplifiedExports = simplifyLookup(exportsLookup);

      fs.appendFileSync(
        outputPath,
        [
          "",
          "// --- Environment ---",
          "const exports = " +
            util.inspect(simplifiedExports, { depth: Infinity }),
          "const values = " +
            util.inspect(simplifiedValues, { depth: Infinity }),
        ].join("\n"),
      );
    }
  }
})();

stopServer();
