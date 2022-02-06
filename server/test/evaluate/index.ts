import path from 'path'
import fs from 'fs'
import { evaluate } from '../../engine'

const isScratch = process.argv[2] === 'scratch';

const dirs = isScratch
  ? ['scratchpad']
  : [
    'basic',
    'exports-and-imports',
    'default-exports-and-imports'
  ]

for (const dir of dirs) {
  const delimiter = "// ---";

  const rootDir = path.join(__dirname, dir);

  const inputAndOutputPaths = fs.readdirSync(rootDir)
    .filter(f => /^input.*/.test(f))
    .map(f => [f, f.replace(/^input(.*)/, 'output$1')])
    .map(([input, output]) => [path.join(rootDir, input), path.join(rootDir, output)]);

  for (const [inputPath, outputPath] of inputAndOutputPaths) {
    const inputBlocksToEval = fs.readFileSync(inputPath, 'utf-8').split(delimiter);
    const output = inputBlocksToEval
      .map(code => {
        try {
          // `/` to force the filename to be treated as absolute
          return evaluate('/' + path.basename(inputPath), code, isScratch);
        } catch (e) {
          return e.stack || e.message;
        }
      })
      .join('\n' + delimiter + '\n');
    fs.writeFileSync(outputPath, output);
  }
}
