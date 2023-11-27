import { NodePath, PluginPass } from "@babel/core";
import * as t from "@babel/types";
import {
  registerDefaultImport,
  registerImport,
  registerNamespaceImport,
} from "./state";
import { extractFileName, unexpected } from "./utils";

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
        if (importedName === "default") {
          path.insertAfter(
            registerDefaultImport(
              fileName,
              specifier.local.name,
              path.node.source.value
            )
          );
        } else {
          path.insertAfter(
            registerImport(
              fileName,
              specifier.local.name,
              path.node.source.value,
              importedName
            )
          );
        }
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
      case "ImportNamespaceSpecifier": {
        path.insertAfter(
          registerNamespaceImport(
            fileName,
            specifier.local.name,
            path.node.source.value
          )
        );
        continue;
      }
      default:
        return unexpected(`specifier type ${(specifier as any).type}`);
    }
  }
  path.remove();
}
