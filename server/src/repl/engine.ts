import vm from "vm";
import _ from "lodash";
import * as babel from "@babel/core";
import { DEBUG } from "./utils";
import { exportNamedDeclaration } from "./export-named-declaration";
import { configureContext, nonGlobals, valuesLookup } from "./state";
import { exportDefault } from "./export-default";
import { program } from "./program";
import { expressionStatement } from "./expression-statement";
import { importDeclaration } from "./import-declaration";

// --- Transformation ---

export function transform(namespace: string, code: string) {
  const output = babel.transformSync(code, {
    plugins: [transformer()],
    filename: namespace,
    parserOpts: {
      allowUndeclaredExports: true,
    },
  });
  return output?.code;
}

function transformer() {
  return () => ({
    visitor: {
      Program: program,
      ExpressionStatement: expressionStatement,
      ExportNamedDeclaration: exportNamedDeclaration,
      ExportDefaultDeclaration: exportDefault,
      ImportDeclaration: importDeclaration,
    },
  });
}

export function evaluate(namespace: string, code: string) {
  const codeTransformed = transform(namespace, code);
  DEBUG("transform", codeTransformed);

  const context = valuesLookup[namespace] ?? {};
  configureContext(context);

  const result = vm.runInContext(
    `
      (function () {
        "use strict";
        try {
            ${codeTransformed}
        } catch (e) {
          console.error(e);
        }
      })();
    `,
    vm.createContext(context),
    { filename: namespace, displayErrors: true }
  );
  DEBUG("evaluate", nonGlobals(valuesLookup[namespace]));
  return result;
}
