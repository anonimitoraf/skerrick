#!/usr/bin/env node

import { serve } from '.';

const [,, port, entrypoint] = process.argv;
console.log('--- Port:', port);
console.log('--- Entry point', entrypoint);
serve(parseInt(port), entrypoint);
