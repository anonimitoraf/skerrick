export const x = 1;
// ---
export function f (x) { return x }
// ---
const x = 1;
export { x };
// ---
const x = 1;
const y = 2;
export { x, y };
// ---
const x = 1;
const y = 2;
export { x as x1, y as y1 };
// ---
const x = 1;
const y = 2;
export { x as x1, y };
// ---
const x = 1;
export { x as x1, y };
// ---
export { x as x1, y };
export { a };
export { b };
// ---
const x = 1;
export default x;
// ---
export const a = 1;
export const b = 1;
export const c = 1;
