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
        t.isStringLiteral(path.node.arguments[0]) &&
        path.node.arguments.length === 1
      ) {
        // const program = path.findParent(t.isProgram) as NodePath<types.Program>;
        // const dependencyName = path.node.arguments[0].value;

        // const importAlias = path.scope.generateUidIdentifier(`${extractOpts(state).importIdentifierPrefix || ''}${dependencyName}`);
        // const importDeclaration = t.importDeclaration(
        //   [t.importNamespaceSpecifier(importAlias)],
        //   t.stringLiteral(dependencyName)
        // );

        // const lastImportIdx = program.node.body.map(stmt => t.isImportDeclaration(stmt)).lastIndexOf(true);
        // program.node.body.splice(lastImportIdx + 1, 0, importDeclaration);
        // path.replaceWith(importAlias);

        const wrappedImport = t.newExpression(
          t.identifier(SkerrickWrappedImport.name),
          [t.arrowFunctionExpression([], path.node)]
        );
        path.replaceWith(wrappedImport);
        path.stop();
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
  'const a = require("./a"); const b = require("./b")("b");'
].forEach(c => console.log(transform(c)));
