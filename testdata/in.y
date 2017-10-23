%{
// Copyright 2015 The gc Authors. All rights reserved.  Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

// This is a derived work, the original is published at
//
//        https://github.com/golang/go/blob/release-branch.go1.4/src/cmd/gc/go.y
//
// The original work is
//
// Copyright 2009 The Go Authors. All rights reserved.  Use of this source code
// is governed by a BSD-style license that can be found in the GO-LICENSE file.

package gc

import (
        "github.com/cznic/xc"
)
%}

%union {
        node    Node
        Token   xc.Token
}

%token
        /*yy:token "'%c'"   */  CHAR_LIT        "rune literal"
        /*yy:token "1.%d"   */  FLOAT_LIT       "floating-point literal"
        /*yy:token "%c"     */  IDENTIFIER      "identifier"
        /*yy:token "%di"    */  IMAG_LIT        "imaginary literal"
        /*yy:token "%d"     */  INT_LIT         "integer literal"
        /*yy:token "\"%c\"" */  STRING_LIT      "string literal"

%token
        ADD_ASSIGN      "+="
        ANDAND          "&&"
        ANDNOT          "&^"
        ANDNOT_ASSIGN   "&^="
        AND_ASSIGN      "&="
        BODY            "{"
        BREAK           "break"
        CASE            "case"
        CHAN            "chan"
        CHANCOMM        "chan<-"
        COLAS           ":="
        COMM            "<-"
        COMMCHAN        "<-chan"
        CONST           "const"
        CONTINUE        "continue"
        DDD             "..."
        DEC             "--"
        DEFAULT         "default"
        DEFER           "defer"
        DIV_ASSIGN      "/="
        ELSE            "else"
        EQ              "=="
        ERRCHECK
        ERROR
        FALLTHROUGH     "fallthrough"
        FOR             "for"
        FUNC            "func"
        GEQ             ">="
        GO              "go"
        GOTO            "goto"
        IF              "if"
        IMPORT          "import"
        INC             "++"
        INTERFACE       "interface"
        LEQ             "<="
        LSH             "<<"
        LSH_ASSIGN      "<<="
        MAP             "map"
        MOD_ASSIGN      "%="
        MUL_ASSIGN      "*="
        NEQ             "!="
        NO_PACKAGE
        OROR            "||"
        OR_ASSIGN       "|="
        PACKAGE         "package"
        RANGE           "range"
        RETURN          "return"
        RSH             ">>"
        RSH_ASSIGN      ">>="
        SELECT          "select"
        STRUCT          "struct"
        SUB_ASSIGN      "-="
        SWITCH          "switch"
        TYPE            "type"
        VAR             "var"
        XOR_ASSIGN      "^="

%left   COMM

%left   OROR
%left   ANDAND
%left   EQ NEQ '<' LEQ '>' GEQ
%left   '+' '-' '|' '^'
%left   '*' '/' '%' LSH RSH '&' ANDNOT

%precedence     NO_RESULT
%precedence     '('

%precedence     TYPE
%precedence     ')'

%start  File

%%

File:
        Prologue TopLevelDeclList

Argument:
        Expression
|       TypeLiteral

ArgumentList:
        Argument
|       ArgumentList ',' Argument

ArrayType:
        '[' "..." ']' Type
|       '[' Expression ']' Type

Assignment:
        ExpressionList '='   ExpressionList
|       ExpressionList "+="  ExpressionList
|       ExpressionList "&^=" ExpressionList
|       ExpressionList "&="  ExpressionList
|       ExpressionList "/="  ExpressionList
|       ExpressionList "<<=" ExpressionList
|       ExpressionList "%="  ExpressionList
|       ExpressionList "*="  ExpressionList
|       ExpressionList "|="  ExpressionList
|       ExpressionList ">>=" ExpressionList
|       ExpressionList "-="  ExpressionList
|       ExpressionList "^="  ExpressionList

BasicLiteral:
        CHAR_LIT
|       FLOAT_LIT
|       IMAG_LIT
|       INT_LIT
|       STRING_LIT

//yy:field      scope   *Scope
Block:
        '{'
        {
                lx.beginScope()
        }
        StatementList '}'
        {
                lhs.scope = lx.scope
                lx.endScope()
        }

BlockOpt:
|        Block

//yy:field      scope   *Scope
Body:
        BODY
        {
                lx.beginScope()
        }
        StatementList '}'
        {
                lhs.scope = lx.scope
                lx.endScope()
        }

Call:
        '(' ')'
|       '(' ArgumentList CommaOpt ')'
|       '(' ArgumentList "..." CommaOpt ')'

ChanType:
        "chan" Type
|       "chan<-" Type
|       "<-chan" Type

CommaOpt:
|       ','

CompLitItem:
        Expression
|       CompLitValue
|       Expression ':' Expression
|       Expression ':' CompLitValue

CompLitItemList:
        CompLitItem
|       CompLitItemList ',' CompLitItem

CompLitType:
        ArrayType
|       MapType
|       SliceType
|       StructType

CompLitValue:
        '{' '}'
|       '{' CompLitItemList CommaOpt '}'

ConstDecl:
        "const" '(' ')'
|       "const" '(' ConstSpecList SemicolonOpt ')'
|       "const" ConstSpec

//yy:field      Predeclared     int
//yy:field      ref
ConstSpec:
        IdentifierList
        {
                lhs.declare(lx.scope)
        }
|       IdentifierList '=' ExpressionList
        {
                lhs.declare(lx.scope)
        }
|       IdentifierList Type '=' ExpressionList
        {
                lhs.declare(lx.scope)
        }

ConstSpecList:
        ConstSpec
|       ConstSpecList ';' ConstSpec

Elif:
        "else" "if"
        {
                lx.beginScope()
        }
        IfHeader Body
        {
                lx.endScope()
        }

ElifList:
|       ElifList Elif

Else:
|       "else" Block

/*yy:case Unary */	Expression:
			        UnaryExpression
/*yy:case Mod */	|       Expression '%' Expression
/*yy:case And */	|       Expression '&' Expression
/*yy:case Mul */	|       Expression '*' Expression
/*yy:case Add */	|       Expression '+' Expression
/*yy:case Sub */	|       Expression '-' Expression
/*yy:case Div */	|       Expression '/' Expression
/*yy:case Lt */		|       Expression '<' Expression
/*yy:case Gt */		|       Expression '>' Expression
/*yy:case Xor */	|       Expression '^' Expression
/*yy:case Or */		|       Expression '|' Expression
/* no directive */	|       Expression "&&" Expression
/*yy:case AndNot */	|       Expression "&^" Expression
/*yy:case Eq */		|       Expression "==" Expression
/*yy:case Ge */		|       Expression ">=" Expression
/*yy:case Le */		|       Expression "<=" Expression
/*yy:case Lsh */	|       Expression "<<" Expression
/*yy:case Ne */		|       Expression "!=" Expression
/*yy:case LOr */	|       Expression "||" Expression
/*yy:case Rsh */	|       Expression ">>" Expression
/*yy:case Rx */		|       Expression "<-" Expression

ExpressionOpt:
|       Expression

ExpressionList:
        Expression
|       ExpressionList ',' Expression

ExpressionListOpt:
|       ExpressionList

ForHeader:
        Range
|       SimpleStatementOpt ';' SimpleStatementOpt ';' SimpleStatementOpt
|       SimpleStatementOpt

ForStatement:
        "for"
        {
                lx.beginScope()
        }
        ForHeader Body
        {
                lx.endScope()
        }

FuncDecl:
        FuncOrMethod BlockOpt
        {
                if b := lhs.BlockOpt; b != nil {
                        lhs.FuncOrMethod.Block = b.Block
                }
                if lx.injectScope != nil {
                        lx.endScope()
                }
        }

//yy:field      Block   *Block
//yy:field      ref
FuncOrMethod:
        "func" ReceiverOpt IDENTIFIER Signature
        {
                pkgScope := lx.scope.parent
                fnScope := lx.beginScope()
                lhs.declare(pkgScope, fnScope)
                lx.injectScope = fnScope
        }

//yy:example	"package a ; var b func ( ) ;"
FuncType:
        "func" Signature

IdentifierOpt:
|       IDENTIFIER

IdentifierList:
        IDENTIFIER
|       IdentifierList ',' IDENTIFIER

//yy:example	"package a ; func b ( ) { switch c := 1 {"
IfHeader:
        SimpleStatementOpt
|       SimpleStatementOpt ';' SimpleStatementOpt

IfStatement:
        "if"
        {
                lx.beginScope()
        }
        IfHeader Body ElifList Else
        {
                lx.endScope()
        }

ImportDecl:
        "import" '(' ')'
|       "import" '(' ImportSpecList SemicolonOpt ')'
|       "import" ImportSpec

//yy:field      once    *xc.Once
ImportPath:
        BasicLiteral
        {
                lhs.process(lx)
        }

ImportSpec:
        '.' ImportPath
|       IdentifierOpt ImportPath

ImportSpecList:
        ImportSpec
|       ImportSpecList ';' ImportSpec

ImportList:
|       ImportList ImportDecl ';'

InterfaceType:
        "interface" LBrace '}'
|       "interface" LBrace InterfaceMethodDeclList SemicolonOpt '}'

InterfaceMethodDecl:
        IDENTIFIER Signature
|       QualifiedIdent

InterfaceMethodDeclList:
        InterfaceMethodDecl
|       InterfaceMethodDeclList ';' InterfaceMethodDecl

//yy:example	"package a ; func b ( ) { if interface { } ( nil ) { } }"
LBrace:
        BODY
        {
                lx.fixLBR()
        }
|       '{'

LBraceCompLitItem:
        Expression
|       Expression ':' Expression
|       Expression ':' LBraceCompLitValue
|       LBraceCompLitValue

LBraceCompLitItemList:
        LBraceCompLitItem
|       LBraceCompLitItemList ',' LBraceCompLitItem

LBraceCompLitValue:
        LBrace '}'
|       LBrace LBraceCompLitItemList CommaOpt '}'

MapType:
        "map" '[' Type ']' Type

QualifiedIdent:
        IDENTIFIER
|       IDENTIFIER '.' IDENTIFIER

Operand:
        '(' Expression ')'
|       '(' TypeLiteral ')'
|       BasicLiteral
|       FuncType LBrace
        {
                lx.beginScope()
        }
        StatementList '}'
        {
                lx.endScope()
        }
|       FuncType error
|       IDENTIFIER

//yy:field      ref
ParameterDecl:
        "..." Type
|       IDENTIFIER "..." Type
|       IDENTIFIER Type
|       Type %prec TYPE

ParameterDeclList:
        ParameterDecl
|       ParameterDeclList ',' ParameterDecl

Parameters:
        '(' ')'
|       '(' ParameterDeclList CommaOpt ')'

PrimaryExpression:
        Operand
|       CompLitType LBraceCompLitValue
|       PrimaryExpression '.' '(' "type" ')'
|       PrimaryExpression '.' '(' Type ')'
|       PrimaryExpression '.' IDENTIFIER
|       PrimaryExpression '[' Expression ']'
|       PrimaryExpression '[' ExpressionOpt ':' ExpressionOpt ':' ExpressionOpt ']'
|       PrimaryExpression '[' ExpressionOpt ':' ExpressionOpt ']'
|       PrimaryExpression Call
|       PrimaryExpression CompLitValue
|       TypeLiteral '(' Expression CommaOpt ')'

Prologue:
        "package" IDENTIFIER ';'
        {
                if !lx.build { // Build constraints not satisfied.
                        goto ret0
                }

                if lx.main && $2.Val != idMain {
                        errTok($2, "cannot compile non-main package")
                        goto ret1
                }

                if lx.pkgName == 0 {
                        lx.pkgName = $2.Val
                        break
                }

                if $2.Val != lx.pkgName {
                        errTok($2, "expected package %%s", dict.S(lx.pkgName))
                        goto ret1
                }
        }
        ImportList
        {
                lhs.declare(lx.scope)
        }
|       error
        {
                return -NO_PACKAGE
        }

Range:
        ExpressionList '=' "range" Expression
|       ExpressionList ":=" "range" Expression
|       "range" Expression

ReceiverOpt:
|       Parameters

ResultOpt:
        /* empty */ %prec NO_RESULT
|       Parameters
|       Type      

SelectStatement:
        "select" SwitchBody

SemicolonOpt:
|       ';'

//yy:example "package a ; func b ( ) ;"
Signature:
        Parameters ResultOpt

SimpleStatement:
        Assignment
|       Expression
|       Expression "--"
|       Expression "++"
|       ExpressionList ":=" ExpressionList

SimpleStatementOpt:
|       SimpleStatement

SliceType:
        '[' ']' Type

Statement:
        /* empty */
|       Block
|       ConstDecl
|       TypeDecl
|       VarDecl
|       StatementNonDecl

//yy:example	"package a ; func b ( ) { return }"
StatementList:
        Statement
|       StatementList ';' Statement

StatementNonDecl:
        "break" IdentifierOpt
|       "continue" IdentifierOpt
|       "defer" Expression
|       "fallthrough"
|       ForStatement
|       "go" Expression
|       "goto" IDENTIFIER
|       IDENTIFIER ':' Statement
|       IfStatement
|       "return" ExpressionListOpt
|       SelectStatement
|       SimpleStatement
|       SwitchStatement
|       error

StringLitOpt:
|       STRING_LIT

StructFieldDecl:
        '*' QualifiedIdent StringLitOpt
|       IdentifierList Type StringLitOpt
|       QualifiedIdent StringLitOpt
|       '(' QualifiedIdent ')' StringLitOpt
        {
                errTok(lhs.QualifiedIdent.firstTok(), "cannot parenthesize embedded type")
        }
|       '(' '*' QualifiedIdent ')' StringLitOpt
        {
                errTok(lhs.QualifiedIdent.firstTok(), "cannot parenthesize embedded type")
        }
|       '*' '(' QualifiedIdent ')' StringLitOpt
        {
                errTok(lhs.QualifiedIdent.firstTok(), "cannot parenthesize embedded type")
        }

StructFieldDeclList:
        StructFieldDecl
|       StructFieldDeclList ';' StructFieldDecl

StructType:
        "struct" LBrace '}'
|       "struct" LBrace StructFieldDeclList SemicolonOpt '}'

SwitchBody:
        BODY '}'
|       BODY SwitchCaseList '}'

SwitchCase:
        "case" ArgumentList ':'
|       "case" ArgumentList '=' Expression ':'
|       "case" ArgumentList ":=" Expression ':'
|       "default" ':'

SwitchCaseBlock:
        SwitchCase
        {
                lx.beginScope()
        }
        StatementList
        {
                lx.endScope()
        }

SwitchCaseList:
        SwitchCaseBlock
|       SwitchCaseList SwitchCaseBlock

SwitchStatement:
        "switch"
        {
                lx.beginScope()
        }
        IfHeader SwitchBody
        {
                lx.endScope()
        }

TopLevelDecl:
        ConstDecl
|       FuncDecl
|       TypeDecl
|       VarDecl
|       error

TopLevelDeclList:
|       TopLevelDeclList TopLevelDecl ';'

//yy:field      Predeclared     int
//yy:field      UnderlyingType  *Type
//yy:field      scope           *Scope
Type:
        '(' Type ')'
|       '*' Type
|       ArrayType
|       ChanType
/*yy:example "package a ; var b func ( ) ;" */
|       FuncType
|       InterfaceType
|       MapType
|       QualifiedIdent
|       SliceType
|       StructType

TypeDecl:
        "type" '(' ')'
|       "type" '(' TypeSpecList SemicolonOpt ')'
|       "type" TypeSpec

TypeLiteral:
        '*' TypeLiteral
|       ArrayType
|       ChanType
/*yy:example "package a ; var b = ( func ( ) ) ( c )" */
|       FuncType
|       InterfaceType
|       MapType
|       SliceType
|       StructType

//yy:field      ref
//yy:field      typeScope       *Scope
TypeSpec:
        IDENTIFIER Type
        {
                lhs.declare(lx.scope)
        }

TypeSpecList:
        TypeSpec
|       TypeSpecList ';' TypeSpec

UnaryExpression:
        '!' UnaryExpression
|       '&' UnaryExpression
|       '*' UnaryExpression
|       '+' UnaryExpression
|       '-' UnaryExpression
|       '^' UnaryExpression
|       "<-" UnaryExpression
|       PrimaryExpression

VarDecl:
        "var" '(' ')'
|       "var" '(' VarSpecList SemicolonOpt ')'
|       "var" VarSpec

//yy:field      ref
VarSpec:
        IdentifierList '=' ExpressionList
        {
                lhs.declare(lx.scope)
        }
|       IdentifierList Type
        {
                lhs.declare(lx.scope)
        }
|       IdentifierList Type '=' ExpressionList
        {
                lhs.declare(lx.scope)
        }

VarSpecList:
        VarSpec
|       VarSpecList ';' VarSpec

