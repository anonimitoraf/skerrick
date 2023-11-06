import * as t from "@babel/types";
import { NodePath, PluginPass } from "@babel/core";
import { extractFileName, debug } from "./utils";
import _ from "lodash";
import { registerExport, registerValue } from "./state";

export function exportNamedDeclaration(
  path: NodePath<t.ExportNamedDeclaration>,
  state: PluginPass
) {
  const fileName = extractFileName(state);
  // E.g. `export { x, y as y1 }`
  if (path.node.specifiers.length > 0) {
    for (const specifier of path.node.specifiers || []) {
      if (specifier.type !== "ExportSpecifier") continue;

      const registerExportExpr = t.expressionStatement(
        t.callExpression(t.identifier(registerExport.name), [
          t.stringLiteral(fileName),
          t.stringLiteral(specifier.local.name),
          specifier.exported.type === "StringLiteral"
            ? specifier.exported
            : t.stringLiteral(specifier.exported.name),
        ])
      );
      path.insertAfter(registerExportExpr);
    }
    path.remove();
    return;
  }

  // E.g. export const x = 1, y = 2, z = 3;
  const topDeclaration = path.node.declaration;
  if (
    t.isVariableDeclaration(topDeclaration) &&
    topDeclaration.declarations.length > 0
  ) {
    for (const declaration of topDeclaration.declarations) {
      const id = declaration.id as t.Identifier;
      const registerValueExpr = t.expressionStatement(
        t.callExpression(t.identifier(registerValue.name), [
          t.stringLiteral(fileName),
          t.stringLiteral(id.name),
          id,
        ])
      );
      const registerExportExpr = t.expressionStatement(
        t.callExpression(t.identifier(registerExport.name), [
          t.stringLiteral(fileName),
          t.stringLiteral(id.name),
          t.stringLiteral(id.name),
        ])
      );
      path.insertAfter([registerValueExpr, registerExportExpr]);
    }
    path.replaceWith(topDeclaration);
    return;
  }

  // E.g. export const x = 1;
  const scope = path.scope;
  for (const [bindingKey, binding] of Object.entries(scope.bindings)) {
    const registerValueExpr = t.expressionStatement(
      t.callExpression(t.identifier(registerValue.name), [
        t.stringLiteral(fileName),
        t.stringLiteral(binding.identifier.name),
        binding.identifier,
      ])
    );
    const registerExportExpr = t.expressionStatement(
      t.callExpression(t.identifier(registerExport.name), [
        t.stringLiteral(fileName),
        t.stringLiteral(binding.identifier.name),
        t.stringLiteral(binding.identifier.name),
      ])
    );

    const parent = binding.path.parentPath;
    parent?.insertAfter([registerValueExpr, registerExportExpr]);

    // TODO When is path.node.declaration undefined?
    if (path.node.declaration) path.replaceWith(path.node.declaration);
  }
}
