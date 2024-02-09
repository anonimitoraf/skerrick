import { f, f1 as FFFFF1 } from "./input1.js";
f(10);
// ---
FFFFF1(15);
// ---
import { g, g1, g2 } from "./input1.js";
g(10);
// ---
g1(20);
// ---
g2(30);
// ---
import { h } from "./input1.js";
h(100);
// ---
import { x, x1, y } from "./input1.js";
// ---
x;
// ---
x1;
// ---
y;
// ---
f(x);
// ---
f(x1);
// ---
f(y);
// ---
// FAILING
// import * as input1 from "./input1.js";
// input1.f(2);
