import vm from "vm";
import { evaluate } from "./engine";

const results = [
  // evaluate("/ns", "const x = 1; export default x"),
  evaluate("/ns", "const x = 1; export default function f () {}"),
  evaluate("/ns", "x = x + x; export default x;"),
  // evaluate("/ns", "export default [x]"),
];

console.log("");
console.log(results.join("\n\n"));
