import * as babel from '@babel/core';
import * as t from '@babel/types';
import { NodePath } from '@babel/traverse';

const namespaces = new Map();

export function evaluate(fileName, code) {
  const ns = namespaces.get(fileName) || {};
  namespaces.set(fileName, ns);

  const codeTransformed = transform(fileName, code)?.replace(/;$/, '');
  // console.log('code transformed =', codeTransformed);
  return eval(`with (ns) {
    (function () {
      "use strict";
       ${codeTransformed}
    })();
  }`);
}

function registerValue(fileName, key, value) {
  const ns = namespaces.get(fileName) || {};
  namespaces.set(fileName, ns);
  ns[key] = value;
}

function transform(fileName, code) {
  const output = babel.transformSync(code, {
    plugins: [transformer],
    filename: fileName
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
