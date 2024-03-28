import vm from 'vm'
import * as t from '@babel/types'
import { objGet } from './utils'
import { generateRequire, resolveImportPath } from './require'
import _ from 'lodash'
import path from 'path'

export const symbols = {
  defaultExport: Symbol('defaultExport'),
  namespaceExport: Symbol('namespaceExport'),
  proxyType: Symbol('proxyType'),
}

type Lookup<TVal = any> = Record<string | symbol, TVal>
const emptyLookup = (): Lookup => ({})

/** Object used to resolve an imported value */
export class Import {
  /** @param local is the local identifier in the `importedNamespace` */
  constructor(
    public namespace: string,
    public importedNamespace: string,
    public local: string | symbol,
  ) {}
}

/** Values by namespace */
export let valuesLookup: Lookup<Lookup> = {}
export const resetValuesLookup = () => (valuesLookup = {})
/** Exports by namespace. Keys: export identifiers, values: local identifiers */
export let exportsLookup: Lookup<Lookup<string | symbol>> = {}
export const resetExportsLookup = () => (exportsLookup = {})

function doRegisterValue(namespace: string, key: string, value: any) {
  const values = objGet(valuesLookup, namespace, {})
  values[key] = value
  return value
}

/**
 * E.g. `module.exports = { a, ...b, c }`, b is a spread element. Each field of
 * `b` needs to be registered as a value and export in the namespace.
 */
function doRegisterSpreadExport(namespace: string, spreadKey: string) {
  const values = objGet(valuesLookup, namespace, {})
  if (!(spreadKey in values)) {
    throw new Error(
      `Failed to export spread element due to missing local ${spreadKey}`,
    )
  }
  const value = values[spreadKey]
  if (typeof value !== 'object' || value === null) {
    throw new Error(
      `Failed to export spread element ${spreadKey} because it is not an object`,
    )
  }

  const exportsValues = objGet(exportsLookup, namespace, {})
  if (!exportsValues[symbols.namespaceExport])
    doRegisterNamespaceExport(namespace)

  Object.entries(value).forEach(([key, prop]) => {
    const id = _.uniqueId(`__spread_${spreadKey}_${key}_`)
    values[id] = prop
    exportsValues[key] = id
  })
}

/**
 * @param local is the identifier within the namespace
 * @param exported is the identifier exposed to other namespaces
 */
export function doRegisterExport(
  namespace: string,
  local: string,
  exported: string,
) {
  const exportsValues = objGet(exportsLookup, namespace, {})
  if (!exportsValues[symbols.namespaceExport])
    doRegisterNamespaceExport(namespace)

  const values = objGet(valuesLookup, namespace, {})
  if (!(local in values)) {
    throw new Error(`Failed named export due to missing local ${local}`)
  }
  exportsValues[exported] = local
  return exported
}

function doRegisterDefaultExport(namespace: string, local: string) {
  const exportsValues = objGet(exportsLookup, namespace, {})
  if (!exportsValues[symbols.namespaceExport])
    doRegisterNamespaceExport(namespace)

  const values = objGet(valuesLookup, namespace, {})
  if (!(local in values)) {
    throw new Error(`Failed default export due to missing local ${local}`)
  }
  exportsValues[symbols.defaultExport] = local
  return symbols.defaultExport.toString()
}

export function doRegisterNamespaceExport(namespace: string) {
  const exportsValues = objGet(exportsLookup, namespace, {})
  exportsValues[symbols.namespaceExport] = symbols.namespaceExport

  const namespaceObj = new Proxy(
    { [symbols.proxyType]: 'namespace' },
    {
      get(target, prop) {
        let key = prop
        if (prop === 'default') key = symbols.defaultExport

        const local = exportsValues[key]
        const values = objGet(valuesLookup, namespace, {})

        return values[local] ?? target[prop]
      },
      getOwnPropertyDescriptor() {
        return { enumerable: true, configurable: true }
      },
      ownKeys(_target) {
        const exportValues = objGet(exportsLookup, namespace, {})
        const keys = [
          ...Object.getOwnPropertyNames(exportValues),
          ...Object.getOwnPropertySymbols(exportValues),
        ]
        const normalisedKeys = _(keys)
          .map((key) => (key === symbols.defaultExport ? 'default' : key))
          .value()
        return normalisedKeys
      },
    },
  )

  const values = objGet(valuesLookup, namespace, {})
  values[symbols.namespaceExport] = namespaceObj
  return namespaceObj
}

// E.g. module.exports = require('/re-exported-module')
// E.g. export * from '/re-exported-module'
// E.g. export { a, b } from '/re-exported-module'
// TODO Support only exporting a subset of exports
export function doRegisterNamespaceReExport(
  namespace: string,
  reExportedNamespace: string,
) {
  const exportsValues = objGet(exportsLookup, namespace, {})
  exportsValues[symbols.namespaceExport] = symbols.namespaceExport

  // From this point on, the re-exported namespace will be referred to as
  // "aux namespace" for brevity

  const auxNamespaceObj = new Proxy(
    { [symbols.proxyType]: 'namespace:re-export' },
    {
      get(target, prop) {
        let key = prop
        if (prop === 'default') key = symbols.defaultExport

        const auxExports = objGet(exportsLookup, reExportedNamespace, {})
        const auxLocal = auxExports[key]
        const auxValues = objGet(valuesLookup, reExportedNamespace, {})

        return auxValues[auxLocal] ?? target[prop]
      },
      getOwnPropertyDescriptor() {
        return { enumerable: true, configurable: true }
      },
      ownKeys(_target) {
        const auxExports = objGet(exportsLookup, reExportedNamespace, {})
        const keys = [
          ...Object.getOwnPropertyNames(auxExports),
          ...Object.getOwnPropertySymbols(auxExports),
        ]
        const normalisedKeys = _(keys)
          .map((key) => (key === symbols.defaultExport ? 'default' : key))
          .value()
        return normalisedKeys
      },
    },
  )

  const values = objGet(valuesLookup, namespace, {})
  values[symbols.namespaceExport] = auxNamespaceObj
  return auxNamespaceObj
}

function doRegisterImport(
  namespace: string,
  localName: string,
  importedNamespace: string,
  importedName: string,
) {
  const importedNamespaceResolved = resolveImportPath(
    namespace,
    importedNamespace,
  )
  const importedNamespaceExports = objGet(
    exportsLookup,
    importedNamespaceResolved,
    {},
  )
  if (!(importedName in importedNamespaceExports)) {
    throw new Error(
      `Failed import due to missing export ${importedName} from namespace ${importedNamespaceResolved}`,
    )
  }
  const values = objGet(valuesLookup, namespace, {})
  values[localName] = new Import(
    namespace,
    importedNamespaceResolved,
    importedName,
  )
}

function doRegisterDefaultImport(
  namespace: string,
  localName: string,
  importedNamespace: string,
) {
  const importedNamespaceResolved = resolveImportPath(
    namespace,
    importedNamespace,
  )
  const importedNamespaceExports = objGet(
    exportsLookup,
    importedNamespaceResolved,
    {},
  )
  if (!(symbols.defaultExport in importedNamespaceExports)) {
    throw new Error(
      `Failed import due to missing default export from namespace ${importedNamespaceResolved}`,
    )
  }
  const values = objGet(valuesLookup, namespace, {})
  values[localName] = new Import(
    namespace,
    importedNamespaceResolved,
    symbols.defaultExport,
  )
}

export function doRegisterNamespaceImport(
  namespace: string,
  localNamespaceName: string,
  importedNamespace: string,
) {
  const importedNamespaceResolved = resolveImportPath(
    namespace,
    importedNamespace,
  )
  const importedNamespaceExports = objGet(
    exportsLookup,
    importedNamespaceResolved,
    {},
  )
  if (!(symbols.namespaceExport in importedNamespaceExports)) {
    throw new Error(
      `Failed import due to missing namespace export from namespace ${importedNamespaceResolved}`,
    )
  }
  const values = objGet(valuesLookup, namespace, {})
  values[localNamespaceName] = new Import(
    namespace,
    importedNamespaceResolved,
    symbols.namespaceExport,
  )
}

/** Returns the context for the evaluation VM */
export function generateContext(namespace: string) {
  const base = Object.create(null)
  for (const k of Object.getOwnPropertyNames(global)) {
    base[k] = global[k]
  }

  base['__dirname'] = path.dirname(namespace)
  base['__filename'] = namespace
  base['require'] = (importedNamespace: string) =>
    generateRequire(namespace, importedNamespace)
  // TODO Is there any need to add module and exports? given
  // that we process them at compile time?

  base[doRegisterValue.name] = doRegisterValue
  base[doRegisterExport.name] = doRegisterExport
  base[doRegisterDefaultExport.name] = doRegisterDefaultExport
  base[doRegisterSpreadExport.name] = doRegisterSpreadExport
  base[doRegisterNamespaceExport.name] = doRegisterNamespaceExport
  base[doRegisterImport.name] = doRegisterImport
  base[doRegisterDefaultImport.name] = doRegisterDefaultImport
  base[doRegisterNamespaceImport.name] = doRegisterNamespaceImport

  const dynamicContext = new Proxy(base, {
    get(target, prop) {
      const values = objGet(valuesLookup, namespace, emptyLookup())
      const value = values[prop]
      if (prop in values) {
        if (!(value instanceof Import)) return value
        // Otherwise, resolve an import
        const exportsValues = objGet(
          exportsLookup,
          value.importedNamespace,
          emptyLookup(),
        )
        const importedNamespaceValues = objGet(
          valuesLookup,
          value.importedNamespace,
          emptyLookup(),
        )
        const importedNamespaceLocal = exportsValues[value.local]
        return importedNamespaceValues[importedNamespaceLocal]
      }
      // Lastly, fallback to globals
      return target[prop]
    },
    // TODO Also use `set` to conveniently update lookups?
  })
  return vm.createContext(dynamicContext)
}

export function nonGlobals(context: Record<string | symbol, any> = {}) {
  return Reflect.ownKeys(context)
    .filter((k) => !(k in global))
    .map((k) => [k, context[k]])
}

// --- Transform utils ---

export function registerValue(
  fileName: string,
  key: string,
  expression: t.Expression,
) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterValue.name), [
      t.stringLiteral(fileName),
      t.stringLiteral(key),
      expression,
    ]),
  )
}

export function registerDefaultExport(fileName: string, key: string) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterDefaultExport.name), [
      t.stringLiteral(fileName),
      t.stringLiteral(key),
    ]),
  )
}

export function registerNamespaceExport(fileName: string) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterNamespaceExport.name), [
      t.stringLiteral(fileName),
    ]),
  )
}

export function registerExport(
  fileName: string,
  key: string,
  exportAs: string,
) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterExport.name), [
      t.stringLiteral(fileName),
      t.stringLiteral(key),
      t.stringLiteral(exportAs),
    ]),
  )
}

/** @param key is the identifier of the variable to be spread */
export function registerSpreadExport(fileName: string, key: string) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterSpreadExport.name), [
      t.stringLiteral(fileName),
      t.stringLiteral(key),
    ]),
  )
}

export function registerImport(
  namespace: string,
  localName: string,
  importedNamespace: string,
  importedName: string,
) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterImport.name), [
      t.stringLiteral(namespace),
      t.stringLiteral(localName),
      t.stringLiteral(importedNamespace),
      t.stringLiteral(importedName),
    ]),
  )
}

export function registerDefaultImport(
  namespace: string,
  localName: string,
  importedNamespace: string,
) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterDefaultImport.name), [
      t.stringLiteral(namespace),
      t.stringLiteral(localName),
      t.stringLiteral(importedNamespace),
    ]),
  )
}

export function registerNamespaceImport(
  namespace: string,
  localNamespaceName: string,
  importedNamespace: string,
) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterNamespaceImport.name), [
      t.stringLiteral(namespace),
      t.stringLiteral(localNamespaceName),
      t.stringLiteral(importedNamespace),
    ]),
  )
}
