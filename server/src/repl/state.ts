export const symbols = {
  defaultExport: Symbol("[[defaultExport]]"),
  namespaceExport: Symbol("[[namespaceExport]]"),
};

type NS = string;
type Lookup = Record<string | symbol, any>;

export const valuesLookup = new Map<NS, Lookup>();
export const exportsLookup = new Map<NS, Lookup>();

export function registerValue(namespace: string, key: string, value: any) {
  const values = valuesLookup.get(namespace) || {};
  valuesLookup.set(namespace, values);
  values[key] = value;
  return value;
}

export function registerExport(
  namespace: string,
  local: string,
  exported: string
) {
  const exportsValues = exportsLookup.get(namespace) || {};
  exportsLookup.set(namespace, exportsValues);

  const values = valuesLookup.get(namespace) || {};
  if (!(local in values)) {
    console.error("Failed named export due to missing local", local);
  }

  exportsValues[exported] = values[local];
  return exported;
}

export function registerDefaultExport(namespace: string, local: string) {
  const exportsValues = exportsLookup.get(namespace) || {};
  exportsLookup.set(namespace, exportsValues);

  const values = valuesLookup.get(namespace) || {};
  if (!(local in values)) {
    console.error("Failed default export due to missing local", local);
  }

  exportsValues[symbols.defaultExport] = values[local];
  return symbols.defaultExport.toString();
}
