import { f } from './input-1';
f()

const t = require('./input-1');
console.log(t)


import t1 from './input-1';
console.log(t1);
t1.g.toString()
t1.h.toString()

import * as t2 from './input-1';
t2.default.g()
t2.h()



import _input from "./input-1";
var module = {
  exports: {}
};
var exports = module.exports;
const t = _input;
export default module.exports;
