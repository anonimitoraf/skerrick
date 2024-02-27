const input = require("./input1");
const z = input.a();
// ---
const { b } = require("./input1");
const y = b(z);
const v = { w: 1, z: 2 };
// ---
module.exports = { x: [y + z, z + y], y, ...v, w: "hardcoded" };
