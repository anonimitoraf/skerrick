import { NodePath, PluginPass } from '@babel/core'
import * as t from '@babel/types'

// E.g. `export from './module'`
export function exportAllDeclaration(
  path: NodePath<t.ExportAllDeclaration>,
  state: PluginPass,
) {}
// TODO
