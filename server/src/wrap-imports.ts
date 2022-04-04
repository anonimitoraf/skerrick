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
            t.identifier(SkerrickWrappedBinding.name),
            [t.arrowFunctionExpression([], declaratorParent.node.init)]
          );
        }
      }
    }
  }
});

export class SkerrickWrappedBinding<T> {
  constructor(private valueThunk: () => T) {}
  value(): T { return this.valueThunk(); }
}
