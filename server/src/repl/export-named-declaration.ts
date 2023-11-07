import * as t from "@babel/types";
import { NodePath, PluginPass } from "@babel/core";
import { extractFileName, DEBUG, unexpected } from "./utils";
import _ from "lodash";
import { registerValue, registerExport } from "./state";
import { Identifier } from "typescript";

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

  // Process exports that have multiple lines of declarations. See examples below
  const topDeclaration = path.node.declaration;
  if (
    t.isVariableDeclaration(topDeclaration) &&
    topDeclaration.declarations.length > 0
  ) {
    for (const declaration of topDeclaration.declarations) {
      const id = declaration.id as t.Identifier;
      // E.g.
      // const o = { x: 1, y: 2 };
      // export const { x, y: y1 } = o;
      if (t.isObjectPattern(declaration.id)) {
        processObjectPattern(fileName, path, declaration.id);
        continue;
      }

      // E.g. export const x = 1, y = 2, z = 3;
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

function processObjectPattern(
  fileName: string,
  path: NodePath<t.ExportNamedDeclaration>,
  objectPattern: t.ObjectPattern
) {
  path.traverse({
    ObjectPattern: (objProp) => {
      for (const [k] of Object.entries(objProp.scope.bindings)) {
        DEBUG("transform", "BINDING", k, objProp.scope.hasOwnBinding(k));
      }
    },
  });
  // DEBUG("transform", "PROPERTIES", objectPattern.properties);
  for (const prop of objectPattern.properties) {
    switch (prop.type) {
      case "ObjectProperty": {
        // TODO
        processObjectProperty(fileName, path, prop);
        break;
      }
      case "RestElement": {
        // TODO
      }
      default:
        return unexpected("Object pattern property type " + prop.type);
    }
  }
}

function processObjectProperty(
  fileName: string,
  path: NodePath<t.ExportNamedDeclaration>,
  property: t.ObjectProperty
) {
  const key = property.key as t.Identifier;
  const value = property.value;
  path.insertAfter(t.stringLiteral("world"));
  // path.insertAfter([
  //   registerValue(fileName, key.name, key),
  //   registerExport(fileName, key.name, id.name),
  // ]);
}
