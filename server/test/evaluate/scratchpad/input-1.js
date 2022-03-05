// function f() {
//   return 3;
// }

// export { f };

function g() {
  return 11;
}

module.exports.g = g;
// export { g as h }

// -------------

// var module = {
//   exports: {}
// };
// var exports = module.exports;

function g() {
  return 15;
}
export { g };
export default { g };
