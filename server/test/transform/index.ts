import path from 'path'
import fs from 'fs'
import { transform } from '../../engine'
import { serve } from '../..';

const stopServer = serve();

const dirs = [
  'basic',
  'exports',
  'imports'
]

for (const dir of dirs) {
  const inputPath = path.join(__dirname, dir, 'input.js');
  const outputPath = path.join(__dirname, dir, 'output.js');

  const delimiter = "// ---";
  const blocksToEval = fs.readFileSync(inputPath, 'utf-8').split(delimiter);
  const output = blocksToEval
    .map(code => {
      try {
        return transform('/input.js', code);
      } catch (e) {
        return e.stack || e.message;
      }
    })
    .join('\n' + delimiter + '\n');
  fs.writeFileSync(outputPath, output);
}

stopServer();
