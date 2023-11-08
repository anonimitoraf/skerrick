export const x = 1;
// ---
export const x = 1, y = 2;
// ---
export function f () {}
// ---
export class C { }
// ---
export function* gen() { }
// ---
// FAILING
// export const { x, y: y1 } = o;
// ---
// FAILING
// export const [ x, y ] = array;
// ---
export function f(x) {
  return x;
}
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
const a = 1,
  b = 2,
  y = 3;
export { x as x1, y };
export { a };
export { b };
// ---
export { x as "x var" };
// ---
const x = 1;
export default x;
// ---
export default class C {}
// ---
export default function f() {}
// ---
export default function* gen() {}
// ---
export default class {}
// ---
export default function () {}
// ---
export default function* () {}
// ---
export { x as default };
