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

function registerValue(modulePath, key, value) {
  const ns = namespaces.get(modulePath) || {};
  namespaces.set(modulePath, ns);
  ns[key] = value;
}

function transform(modulePath, code) {
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
      VariableDeclaration(path: NodePath<t.VariableDeclaration>, state) {
        // console.log(path.scope.bindings);
        // console.log(Object.values(path.scope.bindings).map(v => v.path.scope));
        const fileName = extractFileName(state);
        if (path.scope.block.type !== 'Program') {
          return; // Not a global declaration
        }
        for (const [key, binding] of Object.entries(path.scope.bindings)) {
          const uniqueId = path.scope.generateUid('var_' + key);
          const register = t.callExpression(
            t.identifier(registerValue.name), [
            t.stringLiteral(fileName),
            t.stringLiteral(key),
            t.identifier(uniqueId)
          ]);
          path.scope.rename(key, uniqueId);
          path.insertAfter(register);
        }
      },
      FunctionDeclaration(path: NodePath<t.FunctionDeclaration>, state) {
        // console.log(path.scope.bindings);
        const fileName = extractFileName(state);
        const { id } = path.node;
        if (path.scope.parentBlock.type !== 'Program' || !id) {
          return; // Not a global declaration
        }
        const uniqueId = path.scope.generateUid('fn_' + id.name);
        const register = t.callExpression(
          t.identifier(registerValue.name), [
          t.stringLiteral(fileName),
          t.stringLiteral(id.name),
          t.identifier(uniqueId)
        ]);
        path.scope.rename(id.name, uniqueId);
        path.insertAfter(register);
      }
    }
  }
}
