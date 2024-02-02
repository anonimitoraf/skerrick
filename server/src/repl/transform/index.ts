import _ from "lodash";
import * as babel from "@babel/core";
import { exportNamedDeclaration } from "./export-named-declaration";
import { exportDefault } from "./export-default";
import { program } from "./program";
import { expressionStatement } from "./expression-statement";
import { importDeclaration } from "./import-declaration";

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
      // ImportDeclaration: importDeclaration,
    },
  });
}
