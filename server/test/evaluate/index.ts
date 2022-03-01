import path from 'path'
import fs from 'fs'
import { evaluate } from '../../src/engine'
import { serve } from '../..';

const stopServer = serve();

const isScratch = process.argv[2] === 'scratch';

const dirs = isScratch
  ? ['scratchpad']
  : [
    'basic',
    'exports-and-imports',
    'default-exports-and-imports',
    'imports-built-ins',
    'commonjs',
    'async',
    'native-addons',
    'dynamic-imports'
  ]

const delimiter = "// ---";

(async () => {
  for (const dir of dirs) {
    console.log('--------------------------------------------------------');
    console.log(' Evaluation: ' + dir);
    console.log('--------------------------------------------------------');

    const rootDir = path.join(__dirname, dir);

    const inputAndOutputPaths = fs.readdirSync(rootDir)
      .filter(f => /^input.*/.test(f))
      .map(f => [f, f.replace(/^input(.*)/, 'output$1')])
      .map(([input, output]) => [path.join(rootDir, input), path.join(rootDir, output)]);

    for (const [inputPath, outputPath] of inputAndOutputPaths) {
      const inputBlocksToEval = fs.readFileSync(inputPath, 'utf-8').split(delimiter);
      fs.writeFileSync(outputPath, '');
      await Promise.all(inputBlocksToEval
        .map(code => {
          try {
            // `/` to force the filename to be treated as absolute
            return evaluate(inputPath, code, true, isScratch);
          } catch (e) {
            return Promise.resolve(e.stack || e.message);
          }
        })).then(xs => fs.writeFileSync(outputPath, xs
          .map(x => typeof x === 'object' ? JSON.stringify(x, null, 2) : x)
          .join('\n' + delimiter + '\n')
        ));
    }
  }
})();

stopServer();
