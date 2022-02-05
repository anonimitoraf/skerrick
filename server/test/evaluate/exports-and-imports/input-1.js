export function f(x) {
  return x + x;
}
f(42);
// ---
export { f };
// ---
export { f as f1 };
// ---
export function g(x) {
  return f(x) + x;
}
export { g as g1 };
export { f, g as g2 };
// ---
function h (x) {
  return g(x) + x;
}
export { h };
