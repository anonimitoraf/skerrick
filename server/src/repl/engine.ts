import vm from "vm";
import _ from "lodash";
import * as babel from "@babel/core";
import * as t from "@babel/types";
import { NodePath } from "@babel/traverse";
import { PluginPass } from "@babel/core";

// --- State ---

const symbols = {
  defaultExport: Symbol("[[defaultExport]]"),
  namespaceExport: Symbol("[[namespaceExport]]"),
};

type NS = string;
type NSMembers = Record<string | symbol, any>;
const namespaces = new Map<NS, NSMembers>();

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
          // console.log('BINDING:', bindingKey, 'path node type:', binding.path.type);
          // NOTE: Imports are not bound/stored as values within the namespace. They are instead
          // resolved dynamically when evaluating code.
          if (
            binding.path.type === "ImportSpecifier" ||
            binding.path.type === "ImportDefaultSpecifier" ||
            binding.path.type === "ImportNamespaceSpecifier"
          ) {
            continue;
          }
          const registerValueExpr = t.expressionStatement(
            t.callExpression(t.identifier(registerValue.name), [
              t.stringLiteral(fileName),
              t.stringLiteral(binding.identifier.name),
              binding.identifier,
            ])
          );
          const parent = binding.path.parentPath;
          if (!parent) continue;
          // For variable declarations, the parent is "VariableDeclaration".
          // If we insert after the path (not the parent), we get something like:
          // `const x = 10, <inserted here>` which we don't want.
          // Instead we want something like:
          // ```
          // const x = 10;
          // <inserted here>
          // ```
          if (parent.type !== "Program") {
            parent.insertAfter(registerValueExpr);
          } else {
            binding.path.insertAfter(registerValueExpr);
          }
        }
      },
      ExpressionStatement(path: NodePath<t.ExpressionStatement>) {
        if (path.scope.block.type !== "Program") {
          return; // Not a global declaration
        }

        const isLastChild = path.getAllNextSiblings().length <= 0;
        if (!isLastChild) return;

        // E.g. `1 + 1`, we want to wrap as `return 1 + 1`
        const toReturn = t.returnStatement(path.node.expression);
        path.replaceWith(toReturn);
      },
      ExportDefaultDeclaration(
        path: NodePath<t.ExportDefaultDeclaration>,
        state: PluginPass
      ) {
        const fileName = extractFileName(state);

        let local: t.Identifier;
        const { declaration } = path.node;
        // Non-named fn or class
        if (
          t.isFunctionDeclaration(declaration) ||
          t.isClassDeclaration(declaration)
        ) {
          if (declaration.id === null || declaration.id === undefined) {
            const id = t.identifier(_.uniqueId("__defaultExport"));
            declaration.id = id;
            const registerValueExpr = t.expressionStatement(
              t.callExpression(t.identifier(registerValue.name), [
                t.stringLiteral(fileName),
                t.stringLiteral(id.name),
                id,
              ])
            );
            path.insertAfter(registerValueExpr);
          }
          local = declaration.id;
        } else if (t.isIdentifier(declaration)) {
          local = declaration;
        } else {
          return unexpected(`Default export: ${declaration.type}`);
        }

        const registerDefaultExportExpr = t.expressionStatement(
          t.callExpression(t.identifier(registerDefaultValueExport.name), [
            t.stringLiteral(fileName),
            t.stringLiteral(local.name),
          ])
        );
        path.replaceWith(path.node.declaration);
        path.insertAfter(registerDefaultExportExpr);
      },
    },
  });
}

export function evaluate(namespace: string, code: string) {
  const codeTransformed = transform(namespace, code);
  const context = namespaces.get(namespace) ?? {};

  // Configure context
  for (const k of Object.getOwnPropertyNames(global)) {
    context[k] = global[k];
  }
  context[registerValue.name] = registerValue;
  context[registerDefaultValueExport.name] = registerDefaultValueExport;

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
    { filename: namespace }
  );

  const ns = namespaces.get(namespace);
  console.log("namespaces", ns?.x, ns?.y);
  return result;
}

// --- Utils ---

function registerValue(namespace: string, key: string, value: any) {
  const values = namespaces.get(namespace) || {};
  namespaces.set(namespace, values);
  values[key] = value;
  return value;
}

function registerDefaultValueExport(namespace: string, local: string) {
  const values = namespaces.get(namespace) || {};
  namespaces.set(namespace, values);
  values[symbols.defaultExport] = local;
  return symbols.defaultExport.toString();
}

function extractFileName(state: PluginPass) {
  const { filename } = state.file.opts;
  if (!filename) {
    throw Error("No filename");
  }
  return filename;
}

function unexpected(thing: string) {
  throw Error("Unexpected: " + thing);
}
