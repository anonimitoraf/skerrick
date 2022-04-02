import * as babel from '@babel/core';
import * as types from '@babel/types';
import { NodePath } from '@babel/traverse';
import { PluginPass } from '@babel/core';

interface CustomOpts { }

const extractOpts = (state: PluginPass) => state.opts as CustomOpts;

export const wrapImports = ({ types: t }: { types: typeof types }) => ({
  visitor: {
    CallExpression (path: NodePath<types.CallExpression>, state: PluginPass) {
      if (
        t.isIdentifier(path.node.callee, {name: 'require'}) &&
        path.node.arguments.length === 1
      ) {
        // For example:
        // const a = new SkerrickWrappedImport(() => require("./a"));
        // const b = new SkerrickWrappedImport(() => require("./b")("b"));
        const declaratorParent = path.findParent(path => path.isVariableDeclarator()) as NodePath<types.VariableDeclarator>;
        if (declaratorParent && declaratorParent.node.init) {
          declaratorParent.node.init = t.newExpression(
            t.identifier(SkerrickWrappedImport.name),
            [t.arrowFunctionExpression([], declaratorParent.node.init)]
          );
        }
      }
    }
  }
});


class SkerrickWrappedImport<T> {
  constructor(private valueThunk: () => T) {}

  value(): T { return this.valueThunk(); }
}

// TODO Convert these to tests
function transform(code: string) {
  const output = babel.transformSync(code, {
    plugins: [wrapImports],
    parserOpts: {
      allowUndeclaredExports: true,
    }
  });
  return output?.code;
}

[
  'const a = require("./a"); const b = require("./b")("b"); require("./c");'
].forEach(c => console.log(transform(c)));
