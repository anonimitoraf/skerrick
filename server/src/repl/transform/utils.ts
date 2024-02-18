import * as t from '@babel/types'

type IdentifierMember = t.MemberExpression & { object: t.Identifier }
type MemberMember = t.MemberExpression & { object: IdentifierMember }

export function isIdentifierMember(lhs: t.LVal): lhs is IdentifierMember {
  return lhs.type === 'MemberExpression' && lhs.object.type === 'Identifier'
}

/**
 * For example, `module.exports.c` is: lhs.object = MemberExpression and
 * lhs.object.object = Identifier
 */
export function isMemberMemberIdentifier(lhs: t.LVal): lhs is MemberMember {
  return (
    lhs.type === 'MemberExpression' &&
    lhs.object.type === 'MemberExpression' &&
    lhs.object.object.type === 'Identifier'
  )
}
