exports.a = function () {
  return "A";
};
// ---
const B = (n) => n + 1;
exports.b = B;
// ---
class C {
  getC() {
    return 2;
  }
}
exports["c"] = C;
// ---
module.exports.d = 3;
