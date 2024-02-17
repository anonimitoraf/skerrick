import fs from 'fs'
import path from 'path'
import { createRequire } from 'module'
import { doRegisterNamespaceImport, valuesLookup } from './state'
import { evaluate } from './evaluate'
import _ from 'lodash'

export function generateRequire(namespace: string, importedNamespace: string) {
  const importedNamespaceResolved = resolveImportPath(
    namespace,
    importedNamespace,
  )
  const isBuiltIn = !path.isAbsolute(importedNamespace)
  if (isBuiltIn) {
    return createRequire(namespace)(importedNamespaceResolved)
  }
  const namespaceImport = doRegisterNamespaceImport(
    namespace,
    _.uniqueId('__require'),
    importedNamespace,
  )
  return namespaceImport
}

/**
 * Returns the full file path of an import. For example: './input' ->
 * '/Users/.../input.js'. Has the side effect of also evaluating the resolved
 * module.
 */
export function resolveImportPath(
  importingNamespace: string,
  importedNamespace: string,
) {
  try {
    const req = createRequire(importingNamespace)
    const importPath = req.resolve(importedNamespace)
    // 2 cases here:
    // - built-in modules (e.g. "path")
    if (!path.isAbsolute(importPath)) {
      valuesLookup[importPath] = require(importPath)
    }
    // - third-party modules (e.g. "lodash") or local modules (e.g. "./input.js")
    else if (!valuesLookup[importPath]) {
      evaluate(importPath, fs.readFileSync(importPath, { encoding: 'utf-8' }))
    }
    return importPath
  } catch (e) {
    console.error('Failed to normalize import path: ', e)
    throw e
  }
}
