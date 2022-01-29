import _ from 'lodash';
import * as babel from '@babel/core';
import * as t from '@babel/types';
import { NodePath } from '@babel/traverse';

const symbols = {
  "default": Symbol('*defaultExport*')
}

type Namespace = string;

type NamespaceValuesByKey = Map<string, any>;
const namespaces = new Map<Namespace, NamespaceValuesByKey>();

// Exports have an exported name and a local name (they could be the same). When looking up
// an export, the lookup key is exported name. Which internally (within the namespace), resolves
// to some value pointed to, by the local name.
interface Export {
  exported: string | symbol;
  local: string;
}
type ExportsByExported = Map<Export['exported'], Export>;
const namespaceExports = new Map<Namespace, ExportsByExported>();

// Imports have an imported name and a local name (they could be the same). When constructing
// the env's scope, we want to use the latter (if defined).
// The former is used to resolve the import from the source module (of the import)
interface Import {
  importedNamespace: Namespace
  imported: string | symbol;
  local: string;
}
type ImportsByLocal = Map<Import['local'], Import>;
const namespaceImports = new Map<Namespace, ImportsByLocal>();

export function evaluate(namespace: string, code: string) {
  const ns: NamespaceValuesByKey = namespaces.get(namespace) || new Map();
  const nsImports: ImportsByLocal = namespaceImports.get(namespace) || new Map();

  // NOTE THIS DOES NOT WORK SINCE IMPORTS NEED TO BE REGISTERED BEFORE EVALUATION. AT THE MOMENT
  // THEY'RE REGISTERED DURING EVALUATION
  const nsImportsForScope = _([...nsImports.entries()])
    .map(([local, { importedNamespace, imported }]) => {
      const exported = namespaceExports.get(importedNamespace)?.get(imported);
      const exportedValue = exported && namespaces.get(importedNamespace)?.get(exported.local);
      return [local, exportedValue];
    })
    .fromPairs()
    .value();
  console.log('EVAL ns imports ', nsImports);

  const codeTransformed = transform(namespace, code);
  return eval(`
  with (nsImportsForScope) {
    with (ns) {
      (function () {
        "use strict";
        try {
          ${codeTransformed}
        } catch (e) {
          console.error(e);
        }
      })();
    }
  }`);
}

function namespaceRegisterValue(namespace: string, key: string, value: any) {
  const nsValues: NamespaceValuesByKey = namespaces.get(namespace) || new Map();
  namespaces.set(namespace, nsValues);
  nsValues.set(key, value);
  return value;
}

function namespaceRegisterExport(
  namespace: string,
  local: Export['local'],
  exported: Export['exported']
) {
  const nsExports: ExportsByExported = namespaceExports.get(namespace) || new Map();
  namespaceExports.set(namespace, nsExports);
  nsExports.set(exported, { exported, local });
  return exported;
}

function namespaceRegisterDefaultExport(
  namespace: string,
  local: Export['local']
) {
  const nsExports: ExportsByExported = namespaceExports.get(namespace) || new Map();
  namespaceExports.set(namespace, nsExports);
  nsExports.set(symbols["default"], { exported: symbols["default"], local });
  return symbols["default"].toString();
}

function namespaceRegisterImport(
  namespace: string,
  local: Import['local'],
  imported: Import['imported'],
  importedNamespace: string
) {
  const nsImports: ImportsByLocal = namespaceImports.get(namespace) || new Map();
  namespaceImports.set(namespace, nsImports);
  console.log('all ns imports', namespaceImports);
  nsImports.set(local, { imported, local, importedNamespace });
  console.log('ns imports', local, imported, importedNamespace);
  return local;
}

function notImplementedYet(feature) {
  throw Error('Sorry not implemented yet: ' + feature);
}

function unexpected(thing) {
  throw Error('Unexpected: ' + thing);
}

export function transform(namespace: string, code: string) {
  const output = babel.transformSync(code, {
    plugins: [transformer],
    filename: namespace,
    parserOpts: {
      allowUndeclaredExports: true
    }
  });
  return output?.code;
}

function extractFileName(state) {
  return state.file.opts.filename;
}

function transformer() {
  return {
    visitor: {
      Program(path: NodePath<t.Program>, state) {
        const fileName = extractFileName(state);
        for (const [, binding] of Object.entries(path.scope.bindings)) {
          const registerValue = t.callExpression(
            t.identifier(namespaceRegisterValue.name), [
            t.stringLiteral(fileName),
            t.stringLiteral(binding.identifier.name),
            binding.identifier
          ]);
          path.node.body.push(t.expressionStatement(registerValue));
        }
      },
      ExpressionStatement(path: NodePath<t.ExpressionStatement>, state) {
        if (path.scope.block.type !== 'Program') {
          return; // Not a global declaration
        }

        const isLastChild = path.getAllNextSiblings().length <= 0;
        if (!isLastChild) return;

        // E.g. `1 + 1`, we want to wrap as `return 1 + 1`
        const toReturn = t.returnStatement(path.node.expression);
        path.replaceWith(toReturn);
      },
      ExportNamedDeclaration(path: NodePath<t.ExportNamedDeclaration>, state) {
        const fileName = extractFileName(state);

        // E.g. `export { x, y as y1 }`
        if (path.node.specifiers.length > 0) {
          // e.g. `export { x, y as y1 }`
          for (const specifier of (path.node.specifiers || [])) {
            if (specifier.type !== 'ExportSpecifier') continue;

            const registerExport = t.callExpression(
              t.identifier(namespaceRegisterExport.name), [
              t.stringLiteral(fileName),
              specifier.exported.type === 'StringLiteral' ? specifier.exported : t.stringLiteral(specifier.exported.name),
              specifier.local
            ]);
            path.insertAfter(registerExport);
          }
          path.remove();
          return;
        }

        // E.g. `export const x = 1` => `const x = 1`
        if (path.node.declaration) {
          path.replaceWith(path.node.declaration);
        }

        for (const [, binding] of Object.entries(path.scope.getProgramParent().bindings)) {
          const registerExport = t.callExpression(
            t.identifier(namespaceRegisterExport.name), [
            t.stringLiteral(fileName),
            t.stringLiteral(binding.identifier.name),
            binding.identifier
          ]);
          path.insertAfter(registerExport);
        }
      },
      // ExportDefaultDeclaration(path: NodePath<t.ExportDefaultDeclaration>, state) {
      //   const fileName = extractFileName(state);
      //   const registerDefaultExport = t.callExpression(
      //     t.identifier(namespaceRegisterDefaultExport.name), [
      //     t.stringLiteral(fileName),
      //     t.identifier('defaultExportSym'),
      //     path.node.declaration
      //   ]);
      //   path.replaceWith(path.node.declaration);
      //   path.insertAfter(registerDefaultExport);
      // }
      ImportDeclaration(path: NodePath<t.ImportDeclaration>, state) {
        const fileName = extractFileName(state);

        for (const specifier of path.node.specifiers) {
          switch (specifier.type) {
            case 'ImportNamespaceSpecifier':
              return notImplementedYet('import * as ns from "./blah"');
            case 'ImportDefaultSpecifier':
              return notImplementedYet('import default from "./blah"');
            case 'ImportSpecifier':
              const registerImport = t.callExpression(
                t.identifier(namespaceRegisterImport.name), [
                t.stringLiteral(fileName),
                t.stringLiteral(specifier.local.name),
                specifier.imported.type === 'StringLiteral'
                  ? specifier.imported
                  : t.stringLiteral(specifier.imported.name),
                path.node.source
              ]);
              path.replaceWith(registerImport);
              break;
            default: return unexpected(`Import specifier type ${(specifier as any).type}`)
          }
        }
      }
    }
  }
}
