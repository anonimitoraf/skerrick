import * as t from "@babel/types";
import { NodePath, PluginPass } from "@babel/core";
import { extractFileName, DEBUG } from "./utils";
import _ from "lodash";
import { registerValue, registerExport } from "./state";

export function exportNamedDeclaration(
  path: NodePath<t.ExportNamedDeclaration>,
  state: PluginPass
) {
  const fileName = extractFileName(state);

  // E.g. `export { x, y as y1 }`
  if (path.node.specifiers.length > 0) {
    for (const specifier of path.node.specifiers || []) {
      if (specifier.type !== "ExportSpecifier") continue;

      const exportAs =
        specifier.exported.type === "StringLiteral"
          ? specifier.exported.value
          : specifier.exported.name;
      path.insertAfter(
        registerExport(fileName, specifier.local.name, exportAs)
      );
    }
    path.remove();
    return;
  }

  // Process exports that have multiple lines of declarations
  // See examples below
  const topDeclaration = path.node.declaration;
  if (
    t.isVariableDeclaration(topDeclaration) &&
    topDeclaration.declarations.length > 0
  ) {
    for (const declaration of topDeclaration.declarations) {
      // E.g.
      // const o = { x: 1, y: 2 };
      // export const { x, y: y1 } = o;
      if (t.isObjectPattern(declaration.id)) {
        // TODO
        // OR SHOULD I JUST FIND A WAY TO MAKE IT WORK WITH BINDINGS
        continue;
      }

      // E.g. export const x = 1, y = 2, z = 3;
      const id = declaration.id as t.Identifier;
      path.insertAfter([
        registerValue(fileName, id.name, id),
        registerExport(fileName, id.name, id.name),
      ]);
    }
    path.replaceWith(topDeclaration);
    return;
  }

  return unexpected("Unhandled ExportNamedDeclaration case");
}

    // TODO When is path.node.declaration undefined?
    if (path.node.declaration) path.replaceWith(path.node.declaration);
  }
}
