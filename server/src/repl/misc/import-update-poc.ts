import { evaluate } from "../evaluate";

evaluate("/private/tmp/something.js", "export const f = (x) => x * x");
evaluate(
  "/private/tmp/something-else.js",
  `
  import { f } from "./something.js"
  console.log(f(2))
`
);
// Change f
evaluate("/private/tmp/something.js", "export const f = (x) => x * x * x");
evaluate("/private/tmp/something-else.js", " console.log(f(2)) ");
