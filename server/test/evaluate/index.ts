import path from 'path'
import fs from 'fs'
import { evaluate } from '../../engine'

const dirs = [
  // 'basic',
  'exports-and-imports'
]

for (const dir of dirs) {
  const delimiter = "// ---";

  const rootDir = path.join(__dirname, dir);

  const inputAndOutputPaths = fs.readdirSync(rootDir)
    .filter(f => /^input.*/.test(f))
    .map(f => [f, f.replace(/^input.*/, 'output')])
    .map(([input, output]) => [path.join(rootDir, input), path.join(rootDir, output)]);

  for (const [inputPath, outputPath] of inputAndOutputPaths) {
    const inputBlocksToEval = fs.readFileSync(inputPath, 'utf-8').split(delimiter);
    const output = inputBlocksToEval
      .map(code => {
        try {
          return evaluate('/' + path.basename(inputPath), code);
        } catch (e) {
          return e.stack || e.message;
        }
      })
      .join('\n' + delimiter + '\n');
    fs.writeFileSync(outputPath, output);
  }
}
