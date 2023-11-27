import vm from "vm";
import { configureContext, nonGlobals, valuesLookup } from "../state";
import { transform } from "../transform";
import { DEBUG } from "../utils";

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
