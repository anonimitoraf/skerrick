import * as t from "@babel/types";
import { objGet } from "./utils";

export const symbols = {
  defaultExport: Symbol("[[defaultExport]]"),
  namespaceExport: Symbol("[[namespaceExport]]"),
};

type Lookup<TVal = any> = Record<string | symbol, TVal>;

/** This lookup gets used as the context during evaluation */
export const valuesLookup: Lookup<Lookup> = {};
/** This lookup gets looked up when an import is specified */
export const exportsLookup: Lookup<Lookup> = {};

function doRegisterValue(namespace: string, key: string, value: any) {
  const values = objGet(valuesLookup, namespace, {});
  values[key] = value;
  return value;
}

function doRegisterExport(namespace: string, local: string, exported: string) {
  const exportsValues = objGet(exportsLookup, namespace, {});

  const values = objGet(valuesLookup, namespace, {});
  if (!(local in values)) {
    throw new Error(`Failed named export due to missing local ${local}`);
  }

  exportsValues[exported] = values[local];
  return exported;
}

function doRegisterDefaultExport(namespace: string, local: string) {
  const exportsValues = objGet(exportsLookup, namespace, {});
  const values = objGet(valuesLookup, namespace, {});
  if (!(local in values)) {
    throw new Error(`Failed default export due to missing local ${local}`);
  }

  exportsValues[symbols.defaultExport] = values[local];
  return symbols.defaultExport.toString();
}

function doRegisterImport(
  namespace: string,
  localName: string,
  importedNamespace: string,
  importedName: string
) {
  // TODO FIX THIS FUNCTION: REMEMBER THAT THIS RUNS AFTER THE
  // VM CONTEXT HAS BEEN ESTABLISHED
  const exportsValues = objGet(exportsLookup, importedNamespace, {});
  if (!(importedName in exportsValues)) {
    throw new Error(
      `Failed import due to missing export ${importedName} from namespace ${importedNamespace}`
    );
  }

  const values = objGet(valuesLookup, namespace, {});
  values[localName] = exportsValues[importedName];
}

function doRegisterDefaultImport(
  namespace: string,
  localName: string,
  importedNamespace: string
) {
  const exportsValues = objGet(exportsLookup, importedNamespace, {});
  if (!(symbols.defaultExport in exportsValues)) {
    throw new Error(
      `Failed import due to missing default export from namespace ${importedNamespace}`
    );
  }

  const values = objGet(valuesLookup, namespace, {});
  values[localName] = exportsValues[symbols.defaultExport];
}

function doRegisterNamespaceImport(
  namespace: string,
  localNamespaceName: string,
  importedNamespace: string
) {
  const exportsOfImportedNamespace = objGet(
    exportsLookup,
    importedNamespace,
    {}
  );
  const values = objGet(valuesLookup, namespace, {});
  values[localNamespaceName] = exportsOfImportedNamespace;
}

/** Mutates the context so that it has globals and other important members */
export function configureContext(context: Record<string | symbol, any>) {
  // Configure context
  for (const k of Object.getOwnPropertyNames(global)) {
    context[k] = global[k];
  }
  context[doRegisterValue.name] = doRegisterValue;
  context[doRegisterDefaultExport.name] = doRegisterDefaultExport;
  context[doRegisterImport.name] = doRegisterImport;
  context[doRegisterDefaultImport.name] = doRegisterDefaultImport;
  context[doRegisterNamespaceImport.name] = doRegisterNamespaceImport;
}

export function nonGlobals(context: Record<string | symbol, any> = {}) {
  return Reflect.ownKeys(context)
    .filter((k) => !(k in global))
    .map((k) => [k, context[k]]);
}

// --- Transform utils ---

export function registerValue(
  fileName: string,
  key: string,
  identifier: t.Identifier
) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterValue.name), [
      t.stringLiteral(fileName),
      t.stringLiteral(key),
      identifier,
    ])
  );
}

export function registerDefaultExport(fileName: string, key: string) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterDefaultExport.name), [
      t.stringLiteral(fileName),
      t.stringLiteral(key),
    ])
  );
}

export function registerExport(
  fileName: string,
  key: string,
  exportAs: string
) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterExport.name), [
      t.stringLiteral(fileName),
      t.stringLiteral(key),
      t.stringLiteral(exportAs),
    ])
  );
}

export function registerImport(
  namespace: string,
  localName: string,
  importedNamespace: string,
  importedName: string
) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterImport.name), [
      t.stringLiteral(namespace),
      t.stringLiteral(localName),
      t.stringLiteral(importedNamespace),
      t.stringLiteral(importedName),
    ])
  );
}

export function registerDefaultImport(
  namespace: string,
  localName: string,
  importedNamespace: string
) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterDefaultImport.name), [
      t.stringLiteral(namespace),
      t.stringLiteral(localName),
      t.stringLiteral(importedNamespace),
    ])
  );
}

export function registerNamespaceImport(
  namespace: string,
  localNamespaceName: string,
  importedNamespace: string
) {
  return t.expressionStatement(
    t.callExpression(t.identifier(doRegisterNamespaceImport.name), [
      t.stringLiteral(namespace),
      t.stringLiteral(localNamespaceName),
      t.stringLiteral(importedNamespace),
    ])
  );
}
