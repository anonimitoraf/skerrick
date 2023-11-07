import * as t from "@babel/types";
import { NodePath } from "@babel/core";

export function expressionStatement(path: NodePath<t.ExpressionStatement>) {
  // Not a global declaration
  if (path.scope.block.type !== "Program") return;

  const isLastChild = path.getAllNextSiblings().length <= 0;
  if (!isLastChild) return;

  // E.g. `1 + 1`, we want to wrap as `return 1 + 1`
  const toReturn = t.returnStatement(path.node.expression);
  path.replaceWith(toReturn);
}
