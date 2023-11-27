import { NodePath, PluginPass } from "@babel/core";
import * as t from "@babel/types";
import { registerDefaultImport, registerImport } from "./state";
import { extractFileName } from "./utils";

export function importDeclaration(
  path: NodePath<t.ImportDeclaration>,
  state: PluginPass
) {
  const fileName = extractFileName(state);

  // TODO Deal with built-ins
  for (const specifier of path.node.specifiers) {
    switch (specifier.type) {
      case "ImportSpecifier": {
        const importedName =
          specifier.imported.type === "StringLiteral"
            ? specifier.imported.value
            : specifier.imported.name;
        path.insertAfter(
          registerImport(
            fileName,
            specifier.local.name,
            path.node.source.value,
            importedName
          )
        );
        continue;
      }
      case "ImportDefaultSpecifier": {
        path.insertAfter(
          registerDefaultImport(
            fileName,
            specifier.local.name,
            path.node.source.value
          )
        );
        continue;
      }
    }
  }
  path.remove();
}
