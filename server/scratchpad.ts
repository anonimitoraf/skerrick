import { evaluate } from './api'

const ns1 = '/ns-1'
const ns2 = '/ns-2'

// evaluate(ns1, `let x = 5`)
// evaluate(ns1, 'console.log(++x)')
// evaluate(ns1, 'console.log(Math.PI)')

// evaluate(ns2, `let x = 55`)
// evaluate(ns2, 'console.log(++x)')

// evaluate(ns1, 'console.log(++x)')
// evaluate(ns1, `const x = 555`)
// evaluate(ns1, `x = 666`)
// evaluate(ns1, 'console.log(++x)')

// evaluate(ns1, 'const f = function (x) { return x * x; }')
// evaluate(ns1, 'console.log(f(x))')
console.log(evaluate(ns1, '1 + 1; 2 + 2;'))

// function f(ns) {
//   with (ns) {
//     return x++;
//   }
// }

// const ns = { x: 42 };
// console.log(f(ns));
// console.log(ns);
