const namespaces = new Map();

let someGlobal = 999;

function registerValue(nsKey, key, value) {
  const ns = namespaces.get(nsKey) || {};
  namespaces.set(nsKey, ns);
  ns[key] = value;
}

function evaluate(nsKey, code) {
  const ns = namespaces.get(nsKey) || {};
  namespaces.set(nsKey, ns);
  eval(`with (ns) {
    ${code}
  }`)
}

evaluate('ns-1', `registerValue('ns-1', 'x', 5)`)
evaluate('ns-1', 'console.log(++x)')
evaluate('ns-1', 'console.log(someGlobal)')
evaluate('ns-1', 'someGlobal++')
evaluate('ns-1', 'console.log(someGlobal)')

function f(ns) {
  with (ns) {
    return x++;
  }
}

const ns = { x: 42 };
console.log(f(ns));
console.log(ns);
