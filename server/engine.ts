import _ from 'lodash';
import path from 'path';
import * as babel from '@babel/core';
import * as t from '@babel/types';
import { NodePath } from '@babel/traverse';
import { PluginPass } from '@babel/core';

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
  const codeTransformed = transform(namespace, code);

  const ns: NamespaceValuesByKey = namespaces.get(namespace) || new Map();
  const nsImports: ImportsByLocal = namespaceImports.get(namespace) || new Map();

  const nsImportsForScope = _([...nsImports.entries()])
    .map(([local, { importedNamespace, imported }]) => {
      const nsExports = namespaceExports.get(importedNamespace);
      const exported = nsExports?.get(imported);
      const exportedValue = exported && namespaces.get(importedNamespace)?.get(exported.local);
      return [local, exportedValue];
    })
    .fromPairs()
    .value();

  const nsForScope = _([...ns.entries()])
    .map(([k, v]) => [k, v])
    .fromPairs()
    .value();

  // console.log('all imports', namespaceImports);
  // console.log('ns imports for scope', nsImportsForScope);

  return eval(`
  with (nsImportsForScope) {
    with (nsForScope) {
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
  const absoluteImportedNamespace = path.join(path.dirname(namespace), importedNamespace)
  const nsImports: ImportsByLocal = namespaceImports.get(namespace) || new Map();
  namespaceImports.set(namespace, nsImports);
  nsImports.set(local, { imported, local, importedNamespace: absoluteImportedNamespace });
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
      allowUndeclaredExports: true,
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
      Program(path: NodePath<t.Program>, state: PluginPass) {
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
      ExpressionStatement(path: NodePath<t.ExpressionStatement>, state: PluginPass) {
        if (path.scope.block.type !== 'Program') {
          return; // Not a global declaration
        }

        const isLastChild = path.getAllNextSiblings().length <= 0;
        if (!isLastChild) return;

        // E.g. `1 + 1`, we want to wrap as `return 1 + 1`
        const toReturn = t.returnStatement(path.node.expression);
        path.replaceWith(toReturn);
      },
      ExportNamedDeclaration(path: NodePath<t.ExportNamedDeclaration>, state: PluginPass) {
        const fileName = extractFileName(state);

        // E.g. `export { x, y as y1 }`
        if (path.node.specifiers.length > 0) {
          // e.g. `export { x, y as y1 }`
          for (const specifier of (path.node.specifiers || [])) {
            if (specifier.type !== 'ExportSpecifier') continue;

            const registerExport = t.callExpression(
              t.identifier(namespaceRegisterExport.name), [
              t.stringLiteral(fileName),
              t.stringLiteral(specifier.local.name),
              specifier.exported.type === 'StringLiteral' ? specifier.exported : t.stringLiteral(specifier.exported.name),
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
            t.stringLiteral(binding.identifier.name)
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
      ImportDeclaration(path: NodePath<t.ImportDeclaration>, state: PluginPass) {
        const fileName = extractFileName(state);

        for (const specifier of path.node.specifiers) {
          switch (specifier.type) {
            case 'ImportNamespaceSpecifier':
              return notImplementedYet('import * as ns from "./blah"');
            case 'ImportDefaultSpecifier':
              return notImplementedYet('import default from "./blah"');
            case 'ImportSpecifier':
              namespaceRegisterImport(
                fileName,
                specifier.local.name,
                specifier.imported.type === 'StringLiteral'
                  ? specifier.imported.value
                  : specifier.imported.name,
                path.node.source.value
              )
              path.remove();
              break;
            default: return unexpected(`Import specifier type ${(specifier as any).type}`)
          }
        }
      }
    }
  }
}
