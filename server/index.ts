import path from 'path';
import express from 'express';
import process from 'process';
import stripColor from 'strip-color';
import captureConsole from 'capture-console';
import { evaluate } from './engine';

const serverPort = 4321;
const server = express();

let stdout = '';
let stderr = '';

server.use(express.urlencoded({ extended: true }));
server.use(express.json());

server.post('/eval', (req, res) => {
  const { code, modulePath } = req.body;

  if (!modulePath || !code) {
    throw new Error(`Both modulePath and code are required in the req body!`);
  }
  if (!path.isAbsolute(modulePath)) {
    return res.status(500).send(`Only absolute paths allowed! Got ${modulePath} instead`);
  }

  try {
    const result = evaluate(modulePath, code);
    res.status(200).send({ result, stdout, stderr });
  } catch (e) {
    res.status(200).send({ stderr: removeEscapeCodes(e.stack || e.message) });
  }

  // Clean up for the next request
  stdout = '';
  stderr = '';
})

server.listen(serverPort, () => {
  console.log(`JIVE server listening on port ${serverPort}`);

  captureConsole.startCapture(process.stdout, function (v) {
    stdout = stripColor(v);
  });

  captureConsole.startCapture(process.stderr, function (v) {
    stderr = stripColor(v);
  });
});

const escapeCodeRe = /[\u001b\u009b][[()#;?]*(?:[0-9]{1,4}(?:;[0-9]{0,4})*)?[0-9A-ORZcf-nqry=><]/g;
function removeEscapeCodes(s: string | undefined) {
  return s?.replace(escapeCodeRe, '');
}
