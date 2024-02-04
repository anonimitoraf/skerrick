import fsPath from "path";
import { createRequire } from "module";
import { exportsLookup, symbols, valuesLookup } from "./state";

export function generateRequire(
  importingNamespace: string,
  importedNamespace: string
) {
  const importedNamespaceResolved = resolveImportPath(
    importingNamespace,
    importedNamespace
  );
  const isBuiltIn = !fsPath.isAbsolute(importedNamespace);
  if (isBuiltIn) {
    return createRequire(importingNamespace)(importedNamespaceResolved);
  }

  // TODO TEST
  const defaultExport =
    exportsLookup[importedNamespaceResolved]?.[symbols.defaultExport];
  const result =
    defaultExport &&
    valuesLookup[importedNamespaceResolved]?.[defaultExport.local];
  return result;
}

/** Returns the full file path of an import. For example: './input' -> '/Users/.../input.js' */
export function resolveImportPath(
  importingNamespace: string,
  importedNamespace: string
) {
  try {
    const req = createRequire(importingNamespace);
    return req.resolve(importedNamespace);
  } catch (e) {
    console.error("Failed to normalize import path: ", e);
    throw e;
  }
}
