import * as t from "@babel/types";
import { NodePath, PluginPass } from "@babel/core";
import { DEBUG, extractFileName } from "../utils";
import { registerValue } from "../state";

// TODO Also transform all top-level assignments like `const x = 42` into just `x = 42`
// so that the namespace's context gets updated
export function program(
  path: NodePath<t.ExportDefaultDeclaration>,
  state: PluginPass
) {
  const fileName = extractFileName(state);
  for (const [bindingKey, binding] of Object.entries(path.scope.bindings)) {
    console.log("binding key", bindingKey);
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
    // For multiple variable declarations, the parent is "VariableDeclaration".
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
