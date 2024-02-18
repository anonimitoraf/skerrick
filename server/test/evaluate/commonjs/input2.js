const input = require("./input1");
const z = input.a();
// ---
const { b } = require("./input1");
const y = b(z);
// ---
module.exports = { x: [y + z, z + y], y, z };
