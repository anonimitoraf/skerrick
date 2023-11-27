import { PluginPass } from "@babel/core";

export function extractFileName(state: PluginPass) {
  const { filename } = state.file.opts;
  if (!filename) {
    throw Error("No filename");
  }
  return filename;
}

export function unexpected(thing: string) {
  throw Error("Unexpected: " + thing);
}

const enabled = ["transform", "evaluate"];
export function DEBUG(type: string, ...msg: any[]) {
  if (!enabled.includes(type)) return;
  console.debug(...msg);
}

/** Checks if `node` as any ancestors of `type` */
export function ancestorsAre(node: any, types: babel.Node["type"][]) {
  let isSatisfied = true;
  for (const t of types) {
    if (node.type !== t) {
      return false;
    }
    node = node.parentPath;
  }
  return isSatisfied;
}

/** Tries to get key from obj. If it doesn't exist, sets the key's value to `defaultValue` and returns it */
export function objGet(
  obj: Record<string | symbol, any>,
  key: string | symbol,
  defaultValue: any
) {
  if (!(key in obj)) obj[key] = defaultValue;
  return defaultValue;
}
