namespace TypeC

module AST =

    type TypeCProgram = TopLevelDecl list

    and TopLevelDecl =
        | ImportDecl of ImportDecl
        | FunctionDef of FunctionDef
        | GlobalLetDef of GlobalLetDef
        | TypeDef of TypeDef

    and ImportDecl = ImportModulePath of string list

    and FunctionDef =
        { FuncName: string
          OptTemplateParameterDef: TemplateParameterList option
          ParameterList: ParameterList
          ReturnType: TypeSpec
          FuncCode: Block }

    and GlobalLetDef =
        | LetMutDef of LetDef
        | LetImmDef of LetDef

    and LetDef =
        { Symbol: Symbol
          TypeSpec: TypeSpec option
          Value: Expr }

    and TypeDef =
        | RecordTypeDecl of RecordTypeDecl
        | ADTDecl of ADTDecl

    and RecordTypeDecl =
        { TypeName: Symbol
          TypeParameterList: Symbol list option
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

    and TemplateParameterList = TemplateTypeParameter list

    and ParameterList = Parameter list

    and Parameter =
        { Symbol: Symbol
          TypeSpec: TypeSpec }

    and Block = Expr list

    and Symbol = string

    and TypeSpec =
        | ArrowType of TypeSpec * TypeSpec
        | BasicType of BasicType
        | UserDefinedType of UserDefinedType
        | TemplateType of string * TemplateTypeParameter list

    and BasicType =
        | IntType of IntType
        | UIntType of UIntType
        | ListType of ListType
        | ArrayType of ArrayType
        | StringType
        | VoidType

    and IntType =
        | SizedInt of SizedInt
        | DefaultInt

    and SizedInt =
        | Int8
        | Int16
        | Int32
        | Int64

    and UIntType =
        | SizedUInt of SizedUInt
        | DefaultUInt

    and SizedUInt =
        | UInt8
        | UInt16
        | UInt32
        | UInt64

    and ListType =
        { TypeSpec: TypeSpec }

    and ArrayType =
        { TypeSpec: TypeSpec
          ArraySize: uint64 }

    and UserDefinedType = Symbol

    and TemplateTypeParameter =
        | TTPExpr of Expr
        | TTPTypeSpec of TypeSpec

    and Expr =
        | ReturnExpr of Expr
        | LetExpr of LetExpr
        | IfExpr of IfExpr
        | ForExpr of ForExpr
        | WhileExpr of WhileExpr
        | LoopExpr of LoopExpr
        | CallExpr of CallExpr
        | MathExpr of MathExpr
        | LogicExpr of LogicExpr
        | CompareExpr of CompareExpr
        | BitwiseExpr of BitwiseExpr
        | AssignExpr of AssignExpr
        | ControlFlowExpr of ControlFlowExpr
        | Variable of Variable
        | ExprSequence of Block
        | Literal of Literal

    and LetExpr =
        | LetMutExpr of LetExprRecord
        | LetImmExpr of LetExprRecord

    and LetExprRecord =
        { Symbol: Symbol
          TypeSpec: TypeSpec option
          Value: Expr
          Expr: Expr }

    and IfExpr =
        { Cond: Expr
          TrueExpr: Expr
          FalseExpr: Expr option }

    and ForExpr =
        { Symbol: Symbol
          Begin: Expr
          End: Expr
          Block: Block }

    and WhileExpr =
        { Cond: Expr
          Block: Block }

    and LoopExpr =
        { LoopCode: Expr }

    and CallExpr =
        | TemplateFunctionCall of TemplateFunctionCall
        | FunctionCall of FunctionCall

    and TemplateFunctionCall =
        { FuncName: Symbol
          TemplateParameterList: TemplateParameterList
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

    and LogicExpr =
        | AndExpr of Expr * Expr
        | OrExpr of Expr * Expr
        | XorExpr of Expr * Expr
        | NotExpr of Expr

    and BitwiseExpr =
        | LAndExpr of Expr * Expr
        | LOrExpr of Expr * Expr
        | LXorExpr of Expr * Expr
        | LNotExpr of Expr
        | LLeftShift of Expr * Expr
        | LRightShift of Expr * Expr

    and CompareExpr =
        | EqualExpr of Expr * Expr
        | NotEqualExpr of Expr * Expr
        | LessExpr of Expr * Expr
        | GreaterExpr of Expr * Expr
        | LessThanExpr of Expr * Expr
        | GreaterThanExpr of Expr * Expr

    and AssignExpr =
        { DstSymbol: Symbol
          SrcExpr: Expr }

    and ControlFlowExpr =
        | LabelExpr of Symbol
        | BreakExpr of Symbol option
        | ContinueExpr
        | GotoExpr of Symbol

    and FunctionCallArguments = Expr list

    and Variable = string

    and Literal =
        | IntegerLiteral of int64
        | StringLiteral of string
        | VoidLiteral
        | ListLiteral of Expr list
        | ArrayLiteral of Expr list
        | FunctionLiteral of FunctionLiteral
        | RecordLiteral of RecordLiteral

    and FunctionLiteral =
        { ParameterList: ParameterList
          Expr: Expr }

    and RecordLiteral =
        { RecordType: TypeSpec
          RecordLiteralFields: RecordLiteralField list }

    and RecordLiteralField =
        { FieldName: Symbol
          FieldExpr: Expr }
