import vm from "vm";
import { generateContext } from "../state";
import { transform } from "../transform";

export function evaluate(namespace: string, code: string) {
  const codeTransformed = transform(namespace, code);

  const context = generateContext(namespace);
  // TODO Pass in generated 'require'
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
  return result;
}
