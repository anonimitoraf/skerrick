import * as t from "@babel/types";
import { NodePath, PluginPass } from "@babel/core";
import { extractFileName } from "../utils";
import _ from "lodash";
import { registerDefaultExport, registerValue } from "../state";

export function exportDefault(
  path: NodePath<t.ExportDefaultDeclaration>,
  state: PluginPass
) {
  const fileName = extractFileName(state);
  const { declaration } = path.node;

  let local: t.Identifier;
  // Identifiers
  if (t.isIdentifier(declaration)) {
    local = declaration;
  }
  // Expressions
  else if (t.isExpression(declaration)) {
    local = t.identifier(_.uniqueId("__defaultExport"));
  }
  // Class or Function declaration
  else {
    if (declaration.id === null || declaration.id === undefined) {
      declaration.id = t.identifier(_.uniqueId("__defaultExport"));
    }
    local = declaration.id;
  }

  path.insertAfter([
    registerValue(fileName, local.name, local),
    registerDefaultExport(fileName, local.name),
  ]);

  // To avoid `export default x` -> `x`
  // We just remove the declaration
  if (t.isIdentifier(path.node.declaration)) path.remove();
  // Replace something like
  // `export class C {}` -> `class C {}`
  else path.replaceWith(path.node.declaration);
}
