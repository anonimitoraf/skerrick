import _ from 'lodash';
import * as t from '@babel/types';
import { NodePath } from '@babel/traverse';
import { PluginPass } from '@babel/core';

// export const cjsToES = {
//   visitor: {
//     VariableDeclaration(path: NodePath<t.VariableDeclaration>, state: PluginPass) {
//       const { node } = path;
//       for (const d of node.declarations) {
//         if (d.init?.type === 'CallExpression'
//           && d.init?.callee?.type === 'Identifier'
//           && d.init?.callee?.name === 'require') {
//           path.insertBefore(t.importDe)
//         }
//       }
//       path.remove();
//     }
//   }
// }
