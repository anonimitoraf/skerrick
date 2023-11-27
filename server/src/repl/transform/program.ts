import * as t from "@babel/types";
import { NodePath, PluginPass } from "@babel/core";
import { DEBUG, extractFileName } from "../utils";
import { registerValue } from "../state";

export function program(
  path: NodePath<t.ExportDefaultDeclaration>,
  state: PluginPass
) {
  const fileName = extractFileName(state);
  for (const [bindingKey, binding] of Object.entries(path.scope.bindings)) {
    // NOTE: Imports are not bound/stored as values within the namespace. They are instead
    // resolved dynamically when evaluating code.
    if (
      binding.path.type === "ImportSpecifier" ||
      binding.path.type === "ImportDefaultSpecifier" ||
      binding.path.type === "ImportNamespaceSpecifier"
    ) {
      continue;
    }
    const parent = binding.path.parentPath;
    if (!parent) continue;

    const regValue = registerValue(
      fileName,
      binding.identifier.name,
      binding.identifier
    );
    // For variable declarations, the parent is "VariableDeclaration".
    // If we insert after the path (not the parent), we get something like:
    // `const x = 10, <inserted here>` which we don't want.
    // Instead we want something like:
    // ```
    // const x = 10;
    // <inserted here>
    // ```
    if (parent.type === "Program") {
      binding.path.insertAfter(regValue);
    } else {
      parent.insertAfter(regValue);
    }
  }
}
