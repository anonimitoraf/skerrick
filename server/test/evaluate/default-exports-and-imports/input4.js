import Foo from "./input3.js";

const foo = new Foo();

class Baz {
  foo() {
    return foo;
  }
  toString() {
    return "[Instance Baz]";
  }
}

/** Returns the bar of foo */
export default function () {
  return foo.bar();
}

new Baz().foo().bar();
