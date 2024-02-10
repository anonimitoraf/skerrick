import input2 from "./input2.js";

export default class Foo {
  bar() {
    return input2 * input2;
  }
  toString() {
    return "[Instance Foo]";
  }
}

const foo = new Foo();
foo.bar();
