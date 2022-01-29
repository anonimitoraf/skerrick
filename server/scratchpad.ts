import { transform } from './engine'

function transformLog(code) {
  const modulePath = '/some-module.js';
  console.log(transform(modulePath, code));
  console.log();
}

// transformLog('export const x = 42;');
// transformLog('const y = 21; export { y as y1 };');
transformLog('export { y as y2 };');
transformLog('export { y };');
// transformLog('x = 4');
// transformLog('export function f() {}');
// transformLog('export function f () { }');
// transformLog('export const x = 42');
// transformLog('const x = 42; x = 35; const y = 32; export function f() {}; const { a, b } = {}; const [c, d] = []');
// transformLog('const x = 42; const y = 21; export { x, y };');


export const x = 56;
x + x;

// export { x };
