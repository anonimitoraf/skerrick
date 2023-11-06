import vm from "vm";
import _ from "lodash";
import * as babel from "@babel/core";
import * as t from "@babel/types";
import { NodePath } from "@babel/traverse";
import { PluginPass } from "@babel/core";
import { extractFileName } from "./utils";
import { DEBUG } from "./utils";
import { exportNamedDeclaration } from "./export-named-declaration";
import {
  configureContext,
  nonGlobals,
  registerValue,
  valuesLookup,
} from "./state";
import { exportDefault } from "./export-default";

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
      Program(path: NodePath<t.Program>, state: PluginPass) {
        const fileName = extractFileName(state);
        for (const [bindingKey, binding] of Object.entries(
          path.scope.bindings
        )) {
          // NOTE: Imports are not bound/stored as values within the namespace. They are instead
          // resolved dynamically when evaluating code.
          if (
            binding.path.type === "ImportSpecifier" ||
            binding.path.type === "ImportDefaultSpecifier" ||
            binding.path.type === "ImportNamespaceSpecifier"
          ) {
            continue;
          }
          const parent = binding.path.parentPath;
          if (!parent) continue;

          const regValue = registerValue(
            fileName,
            binding.identifier.name,
            binding.identifier
          );
          // For variable declarations, the parent is "VariableDeclaration".
          // If we insert after the path (not the parent), we get something like:
          // `const x = 10, <inserted here>` which we don't want.
          // Instead we want something like:
          // ```
          // const x = 10;
          // <inserted here>
          // ```
          if (parent.type === "Program") {
            binding.path.insertAfter(regValue);
          } else {
            parent.insertAfter(regValue);
          }
        }
      },
      ExpressionStatement(path: NodePath<t.ExpressionStatement>) {
        // Not a global declaration
        if (path.scope.block.type !== "Program") return;

        const isLastChild = path.getAllNextSiblings().length <= 0;
        if (!isLastChild) return;

        // E.g. `1 + 1`, we want to wrap as `return 1 + 1`
        const toReturn = t.returnStatement(path.node.expression);
        path.replaceWith(toReturn);
      },
      ExportNamedDeclaration: exportNamedDeclaration,
      ExportDefaultDeclaration: exportDefault,
    },
  });
}

export function evaluate(namespace: string, code: string) {
  const codeTransformed = transform(namespace, code);
  DEBUG("transform", codeTransformed);

  const context = valuesLookup.get(namespace) ?? {};
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
  DEBUG("evaluate", nonGlobals(valuesLookup.get(namespace)));
  return result;
}
