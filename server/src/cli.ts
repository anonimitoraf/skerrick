#!/usr/bin/env node

import { serve } from '.';

const [,, port, entrypoint, evalImports] = process.argv;
console.log('--- Port:', port);
console.log('--- Entry point', entrypoint);
console.log('--- Eval imports?', evalImports === 'true');
serve(parseInt(port), entrypoint, evalImports === 'true');
