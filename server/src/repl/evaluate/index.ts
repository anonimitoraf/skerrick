import path from "path";
import vm from "vm";
import { configureContext, nonGlobals, valuesLookup } from "../state";
import { transform } from "../transform";
import { DEBUG } from "../utils";
import { createRequire } from "module";

export function evaluate(namespace: string, code: string) {
  const codeTransformed = transform(namespace, code);
  DEBUG("transform", codeTransformed);

  const context = valuesLookup[namespace] ?? {};
  configureContext(context);

  const result = vm.runInContext(
    `
      (function () {
        "use strict";
        try {
            ${codeTransformed}
        } catch (e) {
          console.error(e);
        }
      })();
    `,
    vm.createContext(context),
    { filename: namespace, displayErrors: true }
  );
  DEBUG("evaluate", nonGlobals(valuesLookup[namespace]));
  return result;
}

/** `requireStub` is a custom implementation of NodeJS's require */
function requireStub(namespaceToImport: string) {}

// TODO Consolidate this with requireStub
function generateRequire(namespace: string, namespaceToImport: string) {
  const require = createRequire(namespace);

  // TODO Document example of this
  const fullImportPath = require.resolve(namespaceToImport);
  const isBuiltIn = !path.isAbsolute(fullImportPath);
  const result = require(fullImportPath);
  if (isBuiltIn) return result;
}
