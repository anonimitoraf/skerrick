import _ from 'lodash';
import path from 'path';
import * as babel from '@babel/core';
import * as t from '@babel/types';
import { Binding, NodePath, Scope } from '@babel/traverse';
import { PluginPass } from '@babel/core';

const symbols = {
  defaultExport: Symbol('[[defaultExport]]'),
  namespaceExport: Symbol('[[namespaceExport]]')
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
const valueExports = new Map<Namespace, ExportsByExported>();

// Imports have an imported name and a local name (they could be the same). When constructing
// the env's scope, we want to use the latter (if defined).
// The former is used to resolve the import from the source module (of the import)
interface Import {
  importedNamespace: Namespace
  imported: string | symbol;
  local: string;
}
type ImportsByLocal = Map<Import['local'], Import>;
const valueImports = new Map<Namespace, ImportsByLocal>();

interface NamespaceImport {
  importedNamespace: Namespace
  local: string;
}
type NamespaceImportsByLocal = Map<NamespaceImport['local'], NamespaceImport>;
const namespaceImports = new Map<Namespace, NamespaceImportsByLocal>();

export function evaluate(namespace: string, code: string, debug?: boolean) {
  const codeTransformed = transform(namespace, code);

  if (debug) {
    console.log(`code transformed:\n${codeTransformed}`);
    console.log();
  }

  const ns: NamespaceValuesByKey = namespaces.get(namespace) || new Map();
  const nsImports: ImportsByLocal = valueImports.get(namespace) || new Map();

  const nsImportsForScope = _([...nsImports.entries()])
    .map(([local, { importedNamespace, imported }]) => {
      if (imported === symbols.namespaceExport) {
        return [local, constructNamespaceExport(importedNamespace)];
      } else {
        const nsExports = valueExports.get(importedNamespace);
        const exported = nsExports?.get(imported);
        const exportedValue = exported && namespaces.get(importedNamespace)?.get(exported.local);
        return [local, exportedValue];
      }
    })
    .fromPairs()
    .value();

  const nsForScope = _([...ns.entries()])
    .map(([k, v]) => [k, v])
    .fromPairs()
    .value();

  if (debug) {
    // console.log('all exports', namespaceExports);
    // console.log('all imports', namespaceImports);
    // console.log('ns imports for scope', nsImportsForScope);
  }

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

function constructNamespaceExport(namespace: string) {
  const values: NamespaceValuesByKey = namespaces.get(namespace) || new Map();
  const exports: ExportsByExported = valueExports.get(namespace) || new Map();
  const nsExport = _([...values.entries()])
    .map(([k, v]) => exports.get(k) && [k, v])
    .filter(x => !!x)
    .fromPairs()
    .value();
  return nsExport;
}

function registerValue(namespace: string, key: string, value: any) {
  const values: NamespaceValuesByKey = namespaces.get(namespace) || new Map();
  namespaces.set(namespace, values);
  values.set(key, value);
  return value;
}

function registerValueExport(
  namespace: string,
  local: Export['local'],
  exported: Export['exported']
) {
  const nsExports: ExportsByExported = valueExports.get(namespace) || new Map();
  valueExports.set(namespace, nsExports);
  nsExports.set(exported, { exported, local });
  return exported;
}

function registerDefaultValueExport(
  namespace: string,
  local: Export['local']
) {
  const exports: ExportsByExported = valueExports.get(namespace) || new Map();
  valueExports.set(namespace, exports);
  exports.set(symbols.defaultExport, { exported: symbols.defaultExport, local });
  return symbols.defaultExport.toString();
}

function registerValueImport(
  importingNamespace: string,
  local: Import['local'],
  imported: Import['imported'],
  importedNamespace: string
) {
  const absoluteImportedNamespace = path.join(path.dirname(importingNamespace), importedNamespace)
  const imports: ImportsByLocal = valueImports.get(importingNamespace) || new Map();
  valueImports.set(importingNamespace, imports);
  imports.set(local, { imported, local, importedNamespace: absoluteImportedNamespace });
  return local;
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

function extractFileName(state: PluginPass) {
  const { filename } = state.file.opts;
  if (!filename) {
    throw Error('No filename');
  }
  return filename;
}

function transformer() {
  return {
    visitor: {
      Program(path: NodePath<t.Program>, state: PluginPass) {
        const fileName = extractFileName(state);
        for (const [, binding] of Object.entries(path.scope.bindings)) {
          const registerValueExpr = t.callExpression(
            t.identifier(registerValue.name), [
              t.stringLiteral(fileName),
              t.stringLiteral(binding.identifier.name),
              binding.identifier
            ]);
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
          if (parent.type !== 'Program') {
            parent.insertAfter(registerValueExpr);
          } else {
            binding.path.insertAfter(registerValueExpr);
          }
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

            const registerExportExpr = t.callExpression(
              t.identifier(registerValueExport.name), [
                t.stringLiteral(fileName),
                t.stringLiteral(specifier.local.name),
                specifier.exported.type === 'StringLiteral' ? specifier.exported : t.stringLiteral(specifier.exported.name),
              ]);
            path.insertAfter(registerExportExpr);
          }
          path.remove();
          return;
        }

        const processedBindingsByScope: Map<Scope, Set<Binding>> = state['processedBindingsByScope'] as any || new Map();
        state['processedBindingsByScope'] = processedBindingsByScope;

        const scope = path.scope;
        for (const [bindingKey, binding] of Object.entries(scope.bindings)) {

          const processedBindings = processedBindingsByScope.get(scope) || new Set();
          processedBindingsByScope.set(scope, processedBindings);
          if (processedBindings.has(binding)) {
            // console.log(`Processed binding ${bindingKey} previously. Ignoring...`);
            continue;
          } else {
            processedBindings.add(binding);
          }

          const registerExportExpr = t.callExpression(
            t.identifier(registerValueExport.name), [
              t.stringLiteral(fileName),
              t.stringLiteral(binding.identifier.name),
              t.stringLiteral(binding.identifier.name)
            ]);
          const { path } = binding;
          const isExportedVar = ancestorsAre(path, [
            'VariableDeclarator',
            'VariableDeclaration',
            'ExportNamedDeclaration'
          ]);
          const isExportedFn = ancestorsAre(path, [
            'FunctionDeclaration',
            'ExportNamedDeclaration'
          ])
          if (isExportedVar || isExportedFn) {
            binding.path.parentPath?.insertAfter(registerExportExpr);
          }
        }
        // E.g. `export const x = 1` => `const x = 1`
        if (path.node.declaration) {
          path.replaceWith(path.node.declaration);
        }
      },
      ExportDefaultDeclaration(path: NodePath<t.ExportDefaultDeclaration>, state: PluginPass) {
        const fileName = extractFileName(state);

        let local: t.Identifier;
        const { declaration } = path.node;
        if (t.isClassDeclaration(declaration) || t.isFunctionDeclaration(declaration)) {
          local = declaration.id!;
        } else if (t.isIdentifier(declaration)) {
          local = declaration;
        } else {
          return unexpected(`Default export: ${declaration.type}`);
        }

        const registerDefaultExportExpr = t.callExpression(
          t.identifier(registerDefaultValueExport.name), [
            t.stringLiteral(fileName),
            t.stringLiteral(local.name)
          ]);
        path.replaceWith(path.node.declaration);
        path.insertAfter(registerDefaultExportExpr);
      },
      ImportDeclaration: {
        enter: (path: NodePath<t.ImportDeclaration>, state: PluginPass) => {
          const fileName = extractFileName(state);

          if (path.node.specifiers.length <= 0) {
            // TODO Importing for side-effects
          }

          for (const specifier of path.node.specifiers) {
            switch (specifier.type) {
              case 'ImportNamespaceSpecifier':
                registerValueImport(
                  fileName,
                  specifier.local.name,
                  symbols.namespaceExport,
                  path.node.source.value
                );
                break;
              case 'ImportDefaultSpecifier':
                registerValueImport(
                  fileName,
                  specifier.local.name,
                  symbols.defaultExport,
                  path.node.source.value
                );
                break;
              case 'ImportSpecifier':
                registerValueImport(
                  fileName,
                  specifier.local.name,
                  specifier.imported.type === 'StringLiteral'
                    ? specifier.imported.value
                    : specifier.imported.name,
                  path.node.source.value
                );
                break;
              default:
                return unexpected(`Import specifier type ${(specifier as any).type}`)
            }
          }
        },
        exit: (path: NodePath<t.ImportDeclaration>, state: PluginPass) => {
          path.remove();
        }
      }
    }
  }
}

function ancestorsAre(node: any, types: babel.Node['type'][]) {
  let isSatisfied = true;
  for (const t of types) {
    if (node.type !== t) {
      return false;
    }
    node = node.parentPath;
  }
  return isSatisfied;
}

function notImplementedYet(feature) {
  throw Error('Sorry not implemented yet: ' + feature);
}

function unexpected(thing) {
  throw Error('Unexpected: ' + thing);
}
