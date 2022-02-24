import * as path from 'path';
path.basename('/blah/bleh.png');
// ---
import pathDefault from 'path';
pathDefault.extname('/blah/bleh.png');
// ---
import { Buffer } from 'buffer';
Buffer.from('blah').toString();
