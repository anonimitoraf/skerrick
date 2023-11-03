import vm from "vm";
import { evaluate } from "./engine";

const results = [
  evaluate("/ns", "const x = 1; export default x;"),
  evaluate("/ns", "x = x + x"),
  evaluate("/ns", "const y = { x }"),
  evaluate("/ns", "y.x++"),
];

console.log("");
console.log(results.join("\n\n"));
