import * as t from "@babel/types";

export const symbols = {
  defaultExport: Symbol("[[defaultExport]]"),
  namespaceExport: Symbol("[[namespaceExport]]"),
};

type NS = string;
type Lookup = Record<string | symbol, any>;

export const valuesLookup = new Map<NS, Lookup>();
export const exportsLookup = new Map<NS, Lookup>();

function doRegisterValue(namespace: string, key: string, value: any) {
  const values = valuesLookup.get(namespace) || {};
  valuesLookup.set(namespace, values);
  values[key] = value;
  return value;
}

function doRegisterExport(namespace: string, local: string, exported: string) {
  const exportsValues = exportsLookup.get(namespace) || {};
  exportsLookup.set(namespace, exportsValues);

  const values = valuesLookup.get(namespace) || {};
  if (!(local in values)) {
    console.error("Failed named export due to missing local", local);
  }

  exportsValues[exported] = values[local];
  return exported;
}

function doRegisterDefaultExport(namespace: string, local: string) {
  const exportsValues = exportsLookup.get(namespace) || {};
  exportsLookup.set(namespace, exportsValues);

  const values = valuesLookup.get(namespace) || {};
  if (!(local in values)) {
    console.error("Failed default export due to missing local", local);
  }

  exportsValues[symbols.defaultExport] = values[local];
  return symbols.defaultExport.toString();
}

/** Mutates the context so that it has globals and other important members */
export function configureContext(context: Record<string | symbol, any>) {
  // Configure context
  for (const k of Object.getOwnPropertyNames(global)) {
    context[k] = global[k];
  }
  context[doRegisterValue.name] = doRegisterValue;
  context[doRegisterDefaultExport.name] = doRegisterDefaultExport;
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
