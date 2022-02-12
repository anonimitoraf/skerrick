#!/usr/bin/env node

import { serve } from '.';

const [,, port] = process.argv;
serve(parseInt(port));
