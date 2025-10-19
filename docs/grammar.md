# Grammar

## TODOS

- Unpack targets for tuples
- expr control statements (e.g. <expr> catch)

## BNF

```
file
	: stmt*
	;

stmt
	: moduleDecl
	: useDecl
	: declStmt
	: flowStmt
	: block
	;

declStmt
	: fnDecl
	: variableDecl
	: structDecl
	: interfaceDecl
	: typealiasDecl
	;

flowStmt
	: "return" expr?
	: "break"
	: "continue"
	: deferStmt
	: throwStmt
	: forStmt
	: whileStmt
	: loopStmt
	: exprStmt
	;

exprStmt
	: ifStmt
	: whenStmt
	: tryStmt
	: selectStmt
	: expr
	;

block
	: "{" stmt* "}"
	;

moduleDecl
	: "module" modulePathSpec
	;

modulePathSpec
	: id ("::" id)*
	;

useDecl
	: "use" usePathSpec ("as" id)?
	;

usePathSpec
	: typeExpr usePathSpecWildcard?
	;

usePathSpecWildcard
	: "::" *
	;

declModifier
	: "abstract"
	: "override"
	: "final"
	: "private"
	: "operator"
	: "builtin"
	: "export"
	;

structDecl
	: declModifier* "struct" typeExprGenericPart? id structMemberList? structBody?
	;

structMemberList
	: "(" structMemberPart ("," structMemberPart)* ")"
	;

structMemberPart
	: variableDecl
	: fnParam
	;

structBody
	: "{" structBodyStmt* "}"
	;

structBodyStmt
	: variableDecl
	: fnDecl
	: structInitBlock
	: structDeclModifierBlock
	: structCompanionObjectDecl
	;

structInitBlock
	: "init" block
	;

structDeclModifierBlock
	: declModifier* structBody
	;

structCompanionObjectDecl
	: "companion" "object" structBody
	;

interfaceDecl
	: declModifier* "interface" typeExprGenericPart? id interfaceBody?
	;

interfaceBody
	: fnDecl
	;

typealiasDecl
	: "typealias" id typeExprGenericPart? "=" typeExpr
	;

fnDecl
	: declModifier* "fn" typeExprGenericPart? fnName fnParamList? fnReturnDecl? fnBody
	;

fnName
	: id
	;

fnParamList
	: "(" fnParam ("," fnParam)* ")"
	;

fnParam
	: fnSimpleParam
	: fnSpreadParam
	;

fnSimpleParam
	: id typeAnnotation?
	;

fnSpreadParam
	: "..." id typeAnnotation?
	;

fnReturnDecl
	: "->" typeExpr
	;

fnBody
	: fnExprBody
	: fnBlockBody
	;

fnExprBody
	: "=" exprStmt
	;

fnBlockBody
	: block
	;

variableDecl
	: declModifier* ("let" | "const") id typeAnnotation? ("=" exprStmt)
	;

ifStmt
	: "if" expr blockOrFlowStmt elseStmt?
	;

elseStmt
	: "else" blockOrFlowStmt
	;

ifPostfixExpr
	: "if" expr elseStmt
	;

whileStmt
	: "while" expr blockOrFlowStmt
	;

forStmt
	: "for" id "in" expr blockOrFlowStmt
	;

forPostfixExpr
	: "for" id "in" expr
	;

loopStmt
	: "loop" blockOrFlowStmt
	;

whenStmt
	: "when" expr? whenBody
	;

whenBody
	: whenCondition "->" blockOrFlowStmt
	;

whenCondition
	: whenExprCondition
	: whenIsCondition
	: whenElseCondition
	;

whenExprCondition
	: expr
	;

whenIsCondition
	: "is" typeExpr whenIsUnpackPart? whenSubCondition?
	;

whenIsUnpackPart
	: "(" id ("," id)* ")"
	;

whenElseCondition
	: "else" whenSubCondition?
	;

whenSubCondition
	: "if" expr
	;

throwStmt
	: "throw" expr
	;

tryStmt
	: "try" blockOrExpr catchStmt* finallyStmt?
	;

catchStmt
	: "catch" fnParamList? blockOrFlowStmt
	;

catchPostfixExpr
	: "catch" fnParamList? blockOrFlowStmt
	;

finallyStmt
	: "finally" blockOrFlowStmt
	;

selectStmt
	: "select" "{" selectCase* "}"
	;

selectCase
	: selectExprCase
	: selectElseCase
	;

selectExprCase
	: expr "->" (lambda | flowExpr)
	;

selectElseCase
	: "else" "->" (lambda | flowExpr)
	;

deferStmt
	: "defer" blockOrFlowStmt
	;

blockOrFlowStmt
	: (block | flowStmt)
	;

blockOrExprStmt
	: (block | exprStmt)
	;

blockOrExpr
	: (block | expr)
	;

# operator and expression precedence not modeled in grammar yet
expr
	: literal
	: binaryOp
	: unaryOp
	: postfixOp
	: callExpr
	: indexExpr
	: "(" expr ")"
	;

binaryOp
	: expr <operator> expr
	;

unaryOp
	: <operator> expr
	;

postfixOp
	: expr postfixExpr
	;

postfixExpr
	: ifPostfixExpr
	: catchPostfixExpr
	: forPostfixExpr
	;

callExpr
	: expr ("(" (expr ("," expr)*)? ")")? lambda?
	;

indexExpr
	: expr "[" expr ("," expr)* "]"
	;

literal
	: integer
	: float
	: string
	: fstring
	: "true" | "false"
	: "null"
	: id
	: lambda
	: tuple
	: list
	: map
	;

fstring
	: fstringStart (expr | string)* fstringEnd
	;

lambda
	: "{" lambdaParameterDecl? flowStmt* "}"
	;

lambdaParameterDecl
	: fnParam ("," fnParam)* "->"
	;

tuple
	: "(" expr "," ")"
	: "(" expr ("," expr)* ")"
	;

list
	: "[" expr ("," expr)* "]"
	;

map
	: "." "{" mapPart ("," mapPart)* "}"
	;

mapPart
	: mapPartKey  ":" expr
	;

mapPartKey
	: string
	: fstring
	;

typeAnnotation
	: ":" typeExpr
	;

typeExpr
	: "::"? typeExprPart ("::" typeExprPart)* "?"?
	;

typeExprPart
	: id typeExprGenericPart?
	;

typeExprGenericPart
	: "<" typeExpr ("," typeExpr)* ">"
	;

```
