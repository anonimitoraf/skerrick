import * as t from '@babel/types'
import { NodePath, PluginPass } from '@babel/core'
import {
  registerDefaultExport,
  registerExport,
  registerNamespaceExport,
  registerValue,
} from '../state'
import { extractFileName, unexpected } from '../utils'
import _ from 'lodash'
import { isIdentifierMember, isMemberMemberIdentifier } from './utils'

export function assignmentExpression(
  path: NodePath<t.AssignmentExpression>,
  state: PluginPass,
) {
  const fileName = extractFileName(state)
  // Not a global declaration
  if (path.scope.block.type !== 'Program') return

  const { left, right } = path.node

  // Detect `exports.x = something`
  const isExports = isIdentifierMember(left) && left.object.name === 'exports'
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
    return
  }

  // Detect `module.exports = {}`
  const isModuleExports =
    isIdentifierMember(left) &&
    left.object.name === 'module' &&
    left.property.type === 'Identifier' &&
    left.property.name === 'exports'
  if (isModuleExports) {
    const local = getRHSAsLocal(path, state, right)
    const bindings = getRHSMultiple(path, state, right)
    path.replaceWithMultiple([
      ...bindings.map(({ local, exportAs }) =>
        registerExport(fileName, local, exportAs),
      ),
      registerDefaultExport(fileName, local),
      registerNamespaceExport(fileName),
    ])
    return
  }

  // Detect `module.exports.x = 42`
  const isModuleExportsField =
    isMemberMemberIdentifier(left) &&
    left.object.object.name === 'module' &&
    left.object.property.type === 'Identifier' &&
    left.object.property.name === 'exports'
  if (isModuleExportsField) {
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
    return
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

/**
 * For example: `module.exports = { a, b }`, RHS would be an object expression
 * with 2 ObjectProperties
 */
function getRHSMultiple(
  path: NodePath<t.AssignmentExpression>,
  state: PluginPass,
  expr: t.Expression,
) {
  switch (expr.type) {
    case 'ObjectExpression':
      return extractObjExprBindings(path, state, expr)
    // TODO Process SpreadElementExpressions
    default:
      return unexpected(`RHS type ${expr.type}`)
  }
}

function extractObjExprBindings(
  path: NodePath<t.AssignmentExpression>,
  state: PluginPass,
  expr: t.ObjectExpression,
) {
  const fileName = extractFileName(state)
  const { properties } = expr

  const bindings = properties.map((prop) => {
    if (prop.type !== 'ObjectProperty')
      return unexpected(`ObjectExpression property type ${prop.type}`)

    let exportAs: string
    let local: string
    switch (prop.key.type) {
      case 'Identifier':
        exportAs = prop.key.name
        break
      case 'StringLiteral':
        exportAs = prop.key.value
        break
      default:
        return unexpected(`ObjectExpression property key type ${prop.key.type}`)
    }
    switch (prop.value.type) {
      case 'Identifier':
        local = prop.value.name
        break
      case 'AssignmentPattern':
      case 'ObjectPattern':
      case 'RestElement': // TODO Maybe we want to support this?
      case 'ArrayPattern':
        return unexpected(
          `ObjectExpression property value type ${prop.value.type}`,
        )
      default: {
        local = t.identifier(_.uniqueId('__export')).name
        path.parentPath.insertBefore([
          registerValue(fileName, local, prop.value),
        ])
      }
    }
    return { local, exportAs }
  })
  return bindings
}
