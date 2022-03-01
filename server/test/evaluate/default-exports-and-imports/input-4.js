import Foo from './input-3.js'

const foo = new Foo();

class Baz {
  foo () { return foo; }
}

/** Returns the bar of foo */
export default function () {
  return foo.bar();
}

new Baz().foo().bar();
