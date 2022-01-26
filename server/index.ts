import path from 'path';
import * as babel from '@babel/core';
import * as t from '@babel/types';
import { NodePath } from '@babel/traverse';
import express from 'express';
import process from 'process';
import stripColor from 'strip-color';
import captureConsole from 'capture-console';

const serverPort = 4321;
const server = express();

let stdout = '';
let stderr = '';

server.use(express.urlencoded({ extended: true }));
server.use(express.json());

server.post('/eval', (req, res) => {
  const { code, modulePath } = req.body;

  if (!modulePath || !code) {
    throw new Error(`Both modulePath and code are required in the req body!`);
  }
  if (!path.isAbsolute(modulePath)) {
    return res.status(500).send(`Only absolute paths allowed! Got ${modulePath} instead`);
  }

  const result = evaluate(modulePath, code);

  res.status(200).send({ result, stdout, stderr });

  // Clean up for the next request
  stdout = '';
  stderr = '';
})

server.listen(serverPort, () => {
  console.log(`JIVE server listening on port ${serverPort}`);

  captureConsole.startCapture(process.stdout, function (v) {
    stdout = stripColor(v);
  });

  captureConsole.startCapture(process.stderr, function (v) {
    stderr = stripColor(v);
  });
});

// --------------------------------------------------------------------------------------------------------

const namespaces = new Map();
const namespaceExports = new Map<string, Map<string | Symbol, any>>();

export function evaluate(modulePath, code) {
  const ns = namespaces.get(modulePath) || {};
  namespaces.set(modulePath, ns);

  const codeTransformed = transform(modulePath, code);
  // console.log('code transformed =', codeTransformed);
  return eval(`with (ns) {
    (function () {
      "use strict";
       try {
         ${codeTransformed}
       } catch (e) {
         console.error(e);
       }
    })();
  }`);
}

function namespaceRegisterValue(modulePath, key, value) {
  const ns = namespaces.get(modulePath) || {};
  namespaces.set(modulePath, ns);
  ns[key] = value;
  return value;
}

function namespaceRegisterExport(modulePath, key) {
  const ns = namespaceExports.get(modulePath) || new Map();
  namespaceExports.set(modulePath, ns);
  ns.set(key, key);
  return key;
}

const defaultExportSym = Symbol('*defaultExport*');

function namespaceRegisterDefaultExport(modulePath, value) {
  const ns = namespaceExports.get(modulePath) || new Map();
  namespaceExports.set(modulePath, ns);
  ns.set(defaultExportSym, value);
  return defaultExportSym.toString();
}

export function transform(modulePath, code) {
  const output = babel.transformSync(code, {
    plugins: [transformer],
    filename: modulePath
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

        for (const [, binding] of Object.entries(path.scope.bindings)) {
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
    }
  }
}

function notImplementedYet(feature) {
  throw Error('Sorry not implemented yet: ' + feature);
}

function unexpected(thing) {
  throw Error('Unexpected: ' + thing);
}
