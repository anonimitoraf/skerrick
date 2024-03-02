import { NodePath, PluginPass } from '@babel/core'
import * as t from '@babel/types'

const list = ['a', 'b', 'c'] as const
const list2 = [...list, 'd1'] as const
type listType = (typeof list2)[number]

export function exportAllDeclaration(
  path: NodePath<t.ExportAllDeclaration>,
  state: PluginPass,
) {}
// TODO
