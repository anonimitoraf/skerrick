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
export function debug(type: string, ...msg: any[]) {
  if (!enabled.includes(type)) return;
  console.debug(...msg);
}