import { transform } from './engine'

function transformLog(code) {
  const modulePath = '/some-module.js';
  console.log(transform(modulePath, code));
  console.log();
}

// transformLog('export const x = 42;');
// transformLog('const y = 21; export { y as y1 };');
transformLog('import { x } from "./blah"; export { y as y2 };');
transformLog('export { y };');
