import Foo from '/input-3.js'

const foo = new Foo();

class Baz {
  foo () { return foo; }
}

export default Baz;

new Baz().foo().bar();
