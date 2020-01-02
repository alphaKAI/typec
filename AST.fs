namespace TypeC

module AST =

    type TypeCProgram = Code of TopLevelDecl list

    and TopLevelDecl =
        | ImportDecl of ImportDecl
        | FunctionDef of FunctionDef
        | GlobalLetDef of GlobalLetDef
        | TypeDef of TypeDef

    and ImportDecl = ImportModulePath of string list

    and FunctionDef =
        { FuncName: string
          OptTemplateArgumentDef: TemplateArguments option
          Parameters: ParameterList
          ReturnType: TypeSpec
          FuncCode: Block }

    and GlobalLetDef = LetExpr

    and TypeDef =
        | RecordTypeDecl of RecordTypeDecl
        | ADTDecl of ADTDecl

    and RecordTypeDecl =
        { TypeName: Symbol
          RecordFields: RecordField list }

    and RecordField =
        { FieldName: Symbol
          FieldType: TypeSpec }

    and ADTDecl =
        { ADTBaseConstructor: ADTBaseConstructor
          ADTConstructors: ADTConstructorDecl list }

    and ADTBaseConstructor =
        | ADTTypeNameWithArgs of ADTTypeNameWithArgs
        | ADTTypeNameWithoutArgs of ADTTypeNameWithoutArgs

    and ADTTypeNameWithArgs =
        { ADTTypeName: ADTTypeName
          ADTParameterList: ADTParameterList }

    and ADTTypeNameWithoutArgs =
        { ADTTypeName: ADTTypeName }

    and ADTConstructorDecl =
        | ADTConstructorWithField of ADTConstructorWithField
        | ADTConstructorWithoutField of ADTConstructorWithoutField

    and ADTConstructorWithField =
        { TypeName: ADTTypeName
          Fields: ADTField list }

    and ADTConstructorWithoutField =
        { TypeName: ADTTypeName }

    and ADTField =
        | ADTFieldOfFunctionType of ADTFieldOfFunctionType
        | ADTFieldWithArgs of ADTFieldWithArgs
        | ADTFieldName of ADTFieldName

    and ADTFieldOfFunctionType =
        { SrcField: ADTField
          DstField: ADTField }

    and ADTFieldWithArgs =
        { ADTFieldName: ADTFieldName
          ADTFieldArgs: ADTField list }

    and ADTFieldName =
        { ADTFieldName: string }

    and ADTTypeName = string

    and ADTParameterList = ADTTypeName list

    and TemplateArguments = TemplateArgument list

    and TemplateArgument = Expr

    and ParameterList = Parameter list

    and Parameter =
        { symbol: Symbol
          typeSpec: TypeSpec }

    and Block = Expr list

    and Symbol = string

    and TypeSpec =
        | ArrowType of TypeSpec * TypeSpec
        | BasicType of BasicType
        | UserDefinedType of Symbol

    and BasicType = IntType of IntType

    and IntType =
        | SizedInt of int64
        | DefaultInt

    and Expr =
        | ReturnExpr of Expr
        | LetExpr of LetExpr
        | IfExpr of IfExpr
        | ForExpr of ForExpr
        | WhileExpr of WhileExpr
        | LoopExpr of LoopExpr
        | CallExpr of CallExpr
        | MathExpr of MathExpr
        | Variable of Variable
        | Literal of Literal

    and LetExpr =
        | LetMutExpr of LetExprRecord
        | LetImmExpr of LetExprRecord

    and LetExprRecord =
        { symbol: Symbol
          typeSpec: TypeSpec option
          value: Expr
          expr: Expr }

    and IfExpr =
        | IfThen of Expr * Expr
        | IfThenElse of Expr * Expr * Expr

    and ForExpr =
        { Begin: Expr
          End: Expr
          ForCode: Expr }

    and WhileExpr =
        { Condition: Expr
          WhileCode: Expr }

    and LoopExpr =
        { LoopCode: Expr }

    and CallExpr =
        | TemplateFunctionCall of TemplateFunctionCall
        | FunctionCall of FunctionCall

    and TemplateFunctionCall =
        { FuncName: Symbol
          TemplateArguments: TemplateArguments
          FunctionCallArguments: FunctionCallArguments }

    and FunctionCall =
        { FuncName: Symbol
          FunctionCallArguments: FunctionCallArguments }

    and MathExpr =
        | AddExpr of Expr * Expr
        | SubExpr of Expr * Expr
        | MulExpr of Expr * Expr
        | DivExpr of Expr * Expr
        | ModExpr of Expr * Expr
        | PowerExpr of Expr * Expr

    and FunctionCallArguments = Expr list

    and Variable = string

    and Literal =
        | InetegerLiteral of int64
        | StringLiteral of string
