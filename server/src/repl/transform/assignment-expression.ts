import * as t from '@babel/types'
import { NodePath, PluginPass } from '@babel/core'
import { registerExport, registerValue } from '../state'
import { extractFileName, unexpected } from '../utils'
import _ from 'lodash'

export function assignmentExpression(
  path: NodePath<t.AssignmentExpression>,
  state: PluginPass,
) {
  const fileName = extractFileName(state)
  // Not a global declaration
  if (path.scope.block.type !== 'Program') return

  // Detect `exports.x = something`
  const { left, right } = path.node
  const isExports =
    left.type === 'MemberExpression' &&
    left.object.type === 'Identifier' &&
    left.object.name === 'exports'
  if (isExports) {
    const local = getRHSAsLocal(path, state, right)
    let exported: string
    switch (left.property.type) {
      case 'Identifier': {
        exported = left.property.name
        break
      }
      case 'StringLiteral': {
        exported = left.property.value
        break
      }
      default:
        return unexpected(`LHS type ${left.property.type}`)
    }
    path.replaceWith(registerExport(fileName, local, exported))
  }
}

/**
 * For cases like `exports.x = function () {}`, the RHS needs to be assigned to
 * an addressable identifier. For example:
 *
 * ```
 * const __someExport = function () {}
 * exports.x = __someExport
 * ```
 */
function getRHSAsLocal(
  path: NodePath<t.AssignmentExpression>,
  state: PluginPass,
  expr: t.Expression,
) {
  const fileName = extractFileName(state)
  switch (expr.type) {
    case 'Identifier':
      return expr.name
    default: {
      const local = t.identifier(_.uniqueId('__export'))
      path.parentPath.insertBefore([registerValue(fileName, local.name, expr)])
      return local.name
    }
  }
}
