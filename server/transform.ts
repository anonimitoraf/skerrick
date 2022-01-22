import * as babel from '@babel/core';
import * as t from '@babel/types';
import { NodePath } from '@babel/traverse';
import assert from 'assert';

/** Plan of attack:
 * - Top-level variable/function declarations and assignments
 *   get stored in a registry (a Map) of type "module-path" -> [ "vars_and_fns" ]
 * - Note that RHS are expressions. It's probably too hard to
 * - Imports are patched such that they access the aforementioned registry
 */

// See https://stackoverflow.com/a/35874967
// function updateModule(state, mutator) {
//   const fileName = state.file.opts.filename;
//   console.log('Updating module:', fileName);
//   let members = modules.get(fileName) || [];
//   members = mutator(members);
//   modules.set(fileName, members);
// }

function extractFileName(state) {
  return state.file.opts.filename;
}

function transformer() {
  return {
    visitor: {
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
            t.identifier('registerMember'), [
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
          t.identifier('registerValue'), [
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

export default function transform(fileName, code) {
  const output = babel.transformSync(code, {
    plugins: [transformer],
    filename: fileName
  });
  return output?.code;
}

[
  ...[
  //   'const n = 1',
  //   'const n = 1 + 1',
  //   'const n = m + 1',
  //   'const n = function () {}',
  //   'let n'
    '{ const x = 4 }'
  ],
  // ...[
  //   'const [a, b] = [1, 2]',
  //   'const [a, [b, c]] = [1, [2, 3]]',
  // ],
  // ...[
  //   'const { a: A, b } = {}'
  // ],
  // ...[
  //   'const f = function () {}'
  // ],
  ...[
    `function f (a) {
       function g () {}
       const b = a + 1;
     }`
  ],
].forEach(expr => {
  console.log(transform('blah.js', expr));
  console.log();
})
