import defaultExport from './input-2.js'

export default class Foo {
  bar () {
    return defaultExport * defaultExport;
  }
}

const foo = new Foo();
foo.bar();
