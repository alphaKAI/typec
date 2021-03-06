namespace TypeC

module Parser =

    open System.Text
    open FParsec
    open AST

    exception ParseException

    let ws = spaces
    let isAsciiIdStart c = isAsciiLetter c
    let isAsciiIdContinue c = isAsciiLetter c || isDigit c

    let ident =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2 isIdentifierFirstChar isIdentifierChar

    let parseSymbol = ws >>. ident .>> ws
    let parseString sym = ws >>. pstring sym .>> ws
    let parseChar chr = ws >>. pchar chr .>> ws

    let parseImportDecl =
        sepBy ident (parseChar '.')
        |> between (parseString "import") (parseChar ';')
        |>> ImportModulePath |>> ImportDecl

    let parseExpr, parseExprR = createParserForwardedToRef()

    let parseDefaultIntType = parseString "int" |>> fun _ -> BasicType(IntType(DefaultInt))
    let parseDefaultUIntType = parseString "uint" |>> fun _ -> BasicType(UIntType(DefaultUInt))
    let parseStringType = parseString "string" |>> fun _ -> BasicType(StringType)
    let parseVoidType = parseString "void" |>> fun _ -> BasicType(VoidType)

    let parseBasicType =
        choice
            [ attempt parseDefaultIntType
              attempt parseDefaultUIntType
              attempt parseStringType
              parseVoidType ]

    let parseUserDefinedType = parseSymbol |>> TypeSpec.UserDefinedType

    let parseType, parseTypeR = createParserForwardedToRef()

    let parseTemplateParameter =
        attempt (parseType |>> TTPTypeSpec) <|> (parseExpr |>> TTPExpr)
    let parseTemplateType =
        (parseSymbol .>> parseChar '!') .>>.
        (sepBy parseTemplateParameter (parseChar ',')
        |> between (parseChar '(') (parseChar ')'))
        |>> fun (baseTy, tmeplateArgs) -> TemplateType (baseTy, tmeplateArgs)

    let parseArrowType =
        let parseOtherType = attempt parseTemplateType <|> parseBasicType <|> parseUserDefinedType 
        let opp = new OperatorPrecedenceParser<TypeSpec, unit, unit>()

        opp.TermParser <- parseOtherType <|> (parseType |> between (parseChar '(') (parseChar ')'))
        opp.AddOperator(InfixOperator("->", ws, 1, Associativity.Right, (fun x y -> ArrowType(x, y))))
        opp.ExpressionParser

    parseTypeR := parseArrowType

    let parseTypeSpec = parseType

    let parseTypeAnnotation = parseChar ':' >>. parseTypeSpec

    let parseParameter =
        parseSymbol .>> parseChar ':' .>>. parseTypeSpec |>> (fun (symbol, typeSpec) ->
        { Symbol = symbol
          TypeSpec = typeSpec })

    let parseParameterList =
        sepBy parseParameter (parseChar ',') |> between (ws .>> parseChar '(' .>> ws) (ws .>> parseChar ')' .>> ws)


    let parseTemplateParameterList =
        sepBy parseTemplateParameter (parseChar ',') |> between (parseChar '<') (parseChar '>')

    let parseBlock: Parser<Expr list, unit> =
        ws >>. many parseExpr .>> ws |> between (parseChar '{') (parseChar '}')

    let parseReturnExpr = parseString "return" >>. parseExpr |>> ReturnExpr

    let parseLetExpr =
        let parseLetMutExpr =
            ((parseString "let" .>> parseString "mut" >>. parseSymbol .>>. opt parseTypeAnnotation) .>>. (parseChar '=' >>. parseExpr)) .>>. (parseString "in" >>. parseExpr)
            |>> (fun (((sym, typeSpec), value), expr) ->
                LetExpr <| LetMutExpr {
                    Symbol = sym
                    TypeSpec = typeSpec
                    Value = value
                    Expr = expr
                })
        let parseLetImmExpr =
            ((parseString "let" >>. parseSymbol .>>. opt parseTypeAnnotation) .>>. (parseChar '=' >>. parseExpr)) .>>. (parseString "in" >>. parseExpr)
            |>> (fun (((sym, typeSpec), value), expr) ->
                LetExpr <| LetImmExpr {
                    Symbol = sym
                    TypeSpec = typeSpec
                    Value = value
                    Expr = expr
                })
        attempt parseLetMutExpr <|> parseLetImmExpr

    let parseIfExpr =
        (parseString "if" >>. parseExpr) .>>. (parseString "then" >>. parseExpr) .>>. opt (parseString "else" >>. parseExpr)
        |>> (fun ((cond, trueExpr), falseExpr) ->
            IfExpr {
                Cond = cond
                TrueExpr = trueExpr
                FalseExpr = falseExpr
            })

    let parseForExpr =
        (((parseString "for" >>. parseSymbol) .>> parseChar '=') .>>. parseExpr) .>>. (parseString "to" >>. parseExpr) .>>. parseBlock
        |>> (fun (((sym, begin'), end'), block) ->
            ForExpr {
                Symbol = sym
                Begin = begin'
                End = end'
                Block = block
            })

    let parseWhileExpr =
        (parseString "while" >>. parseExpr) .>>. parseBlock
        |>> (fun (cond, block) ->
            WhileExpr {
                Cond = cond
                Block = block
            })

    // Parser for Literal
    let parseIntLiteral: Parser<Literal, unit> = pint64 |>> IntegerLiteral
    let parseStringLiteral: Parser<Literal, unit> =
        manyChars (noneOf [ '"' ])
        |> between (parseChar '"') (parseChar '"')
        |>> StringLiteral
    let parseVoidLiteral = parseString "()" |>> fun _ -> VoidLiteral
    let parseArrayLiteral =
        sepBy parseExpr (parseChar ',')
        |> between (parseString "[|") (parseString "|]")
        |>> ArrayLiteral
    let parseListLiteral =
        sepBy parseExpr (parseChar ',')
        |> between (parseChar '[') (parseChar ']')
        |>> ListLiteral

    let parseFunctionLiteral =
        parseString "fun" >>. parseParameterList .>>. (parseString "->" >>. parseExpr)
        |>> (fun (ps, e) ->
            FunctionLiteral
                { ParameterList = ps
                  Expr = e })

    let parseRecordLiteral =
        let parseRecordLiteralFields =
            let parseRecordLiteralField =
                (parseSymbol .>> parseChar '=') .>>. parseExpr
                |>> fun (fieldName, fieldExpr) -> { FieldName = fieldName; FieldExpr = fieldExpr }
            (many1 parseRecordLiteralField) |> between (parseChar '{') (parseChar '}')
        parseType .>>. parseRecordLiteralFields
        |>> fun (recordType, recordFields) ->
             RecordLiteral { RecordType = recordType
                             RecordLiteralFields = recordFields }

    let parseLiteral =
        choice
            [ parseIntLiteral; parseStringLiteral; parseVoidLiteral; parseArrayLiteral; parseListLiteral; parseFunctionLiteral; parseRecordLiteral ]
        |>> Literal |> between ws ws

    let parseVariable = parseSymbol |>> Expr.Variable


    let parseCallExpr =
        let parseTemplateFunctionCallParameters =
            parseChar '!' >>. (sepBy parseTemplateParameter (parseChar ',') |> between (parseChar '(') (parseChar ')'))
        let parseFunctionCallArguments =
            sepBy parseExpr (parseChar ',') |> between (parseString "(") (parseString ")")
        let parseTemplateFunctionCall =
            parseSymbol .>>. parseTemplateFunctionCallParameters .>>. parseFunctionCallArguments
            |>> fun ((funcName, templateParameters), args) ->
                CallExpr (TemplateFunctionCall {
                            FuncName = funcName
                            TemplateParameterList = templateParameters
                            FunctionCallArguments = args })
        let parseFunctionCall = parseSymbol .>>. parseFunctionCallArguments
                              |>> fun (funcName, args) ->
                                    CallExpr (FunctionCall
                                                { FuncName = funcName
                                                  FunctionCallArguments = args })
        attempt parseTemplateFunctionCall <|> parseFunctionCall

    let parseBinaryOperatorExpr =
        let opp = new OperatorPrecedenceParser<Expr, unit, unit>()
        opp.TermParser <-
            (choice
                [ attempt parseCallExpr
                  attempt parseVariable
                  attempt parseLiteral ])
            <|> (between (parseChar '(') (parseChar ')') parseExpr)
        opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, (fun x y -> AddExpr(x, y) |> MathExpr)))
        opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, (fun x y -> SubExpr(x, y) |> MathExpr)))
        opp.AddOperator(InfixOperator("&&", ws, 1, Associativity.Left, (fun x y -> AndExpr(x, y) |> LogicExpr)))
        opp.AddOperator(InfixOperator("||", ws, 1, Associativity.Left, (fun x y -> OrExpr(x, y) |> LogicExpr)))
        opp.AddOperator(InfixOperator("&", ws, 1, Associativity.Left, (fun x y -> LAndExpr(x, y) |> BitwiseExpr)))
        opp.AddOperator(InfixOperator("|", ws, 1, Associativity.Left, (fun x y -> LOrExpr(x, y) |> BitwiseExpr)))
        opp.AddOperator(InfixOperator("^", ws, 1, Associativity.Left, (fun x y -> LXorExpr(x, y) |> BitwiseExpr)))
        opp.AddOperator(InfixOperator("<<", ws, 1, Associativity.Left, (fun x y -> LLeftShift(x, y) |> BitwiseExpr)))
        opp.AddOperator(InfixOperator(">>", ws, 1, Associativity.Left, (fun x y -> LRightShift(x, y) |> BitwiseExpr)))
        opp.AddOperator(InfixOperator("==", ws, 1, Associativity.Left, (fun x y -> EqualExpr(x, y) |> CompareExpr)))
        opp.AddOperator(InfixOperator("!=", ws, 1, Associativity.Left, (fun x y -> NotEqualExpr(x, y) |> CompareExpr)))
        opp.AddOperator(InfixOperator("<", ws, 1, Associativity.Left, (fun x y -> LessExpr(x, y) |> CompareExpr)))
        opp.AddOperator(InfixOperator(">", ws, 1, Associativity.Left, (fun x y -> GreaterExpr(x, y) |> CompareExpr)))
        opp.AddOperator(InfixOperator("<=", ws, 1, Associativity.Left, (fun x y -> LessThanExpr(x, y) |> CompareExpr)))
        opp.AddOperator(InfixOperator(">=", ws, 1, Associativity.Left, (fun x y -> GreaterThanExpr(x, y) |> CompareExpr)))
        opp.AddOperator(InfixOperator("<-", ws, 1, Associativity.Left, (fun x y ->
                                                                            match x with
                                                                            | Expr.Variable(sym) -> AssignExpr { DstSymbol = sym ; SrcExpr = y}
                                                                            | _ -> raise ParseException)))
        opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, (fun x y -> MulExpr(x, y) |> MathExpr)))
        opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, (fun x y -> DivExpr(x, y) |> MathExpr)))
        opp.AddOperator(InfixOperator("%", ws, 2, Associativity.Left, (fun x y -> ModExpr(x, y) |> MathExpr)))
        opp.AddOperator
            (PrefixOperator
                ("-", ws, 4, true,
                 (fun x ->
                     match x with
                     | Literal(IntegerLiteral(v)) -> IntegerLiteral(-v) |> Literal
                     | _ -> MulExpr(Literal(IntegerLiteral(-1L)), x) |> MathExpr)))
        opp.AddOperator(PrefixOperator("!", ws, 4, true, NotExpr >> LogicExpr))
        opp.AddOperator(PrefixOperator("~", ws, 4, true, LNotExpr >> BitwiseExpr))
        opp.ExpressionParser

    let parseControlFlowExpr =
        let parseLabelExpr =
            parseSymbol .>> parseChar ':'
            |>> LabelExpr

        let parseBreakExpr =
            (parseString "break") >>. opt (parseChar '(' >>. parseSymbol .>> parseChar ')')
            |>> BreakExpr

        let parseContinueExpr =
            parseString "continue"
            |>> fun _ -> ContinueExpr

        let parseGotoExpr =
            parseString "goto" >>. parseSymbol
            |>> GotoExpr

        choice [
            attempt parseLabelExpr
            attempt parseBreakExpr
            attempt parseContinueExpr
            parseGotoExpr
        ] |>> ControlFlowExpr

    parseExprR := choice
                  [ attempt parseReturnExpr
                    attempt parseControlFlowExpr
                    attempt parseBlock |>> ExprSequence
                    attempt parseLetExpr
                    attempt parseIfExpr
                    attempt parseForExpr
                    attempt parseWhileExpr
                    attempt parseBinaryOperatorExpr
                    attempt parseCallExpr
                    attempt parseVariable
                    parseLiteral ]
                  .>> opt (parseChar ';')

    let parseFunctionDef =
        let parseTemplateFunctionDef =
            parseString "fn" >>. parseSymbol .>>. parseTemplateParameterList .>>. parseParameterList
            .>>. parseTypeAnnotation .>>. parseBlock
            |>> (fun ((((funcName, templateParameterList), parameterList), retType), block) ->
                FunctionDef
                    { FuncName = funcName
                      OptTemplateParameterDef = Some templateParameterList
                      ParameterList = parameterList
                      ReturnType = retType
                      FuncCode = block })

        let parseNomalFunctionDef =
            parseString "fn" >>. parseSymbol .>>. parseParameterList .>>. parseTypeAnnotation .>>. parseBlock
            |>> (fun (((funcName, parameterList), retType), block) ->
                 FunctionDef
                    { FuncName = funcName
                      OptTemplateParameterDef = None
                      ParameterList = parameterList
                      ReturnType = retType
                      FuncCode = block })

        attempt parseTemplateFunctionDef <|> parseNomalFunctionDef

    let parseGlobalLetDef =
        let parseLetMutDef =
            (parseString "let" .>> parseString "mut" >>. parseSymbol .>>. opt parseTypeAnnotation) .>>. (parseChar '=' >>. parseExpr)
            |>> (fun ((sym, typeSpec), value)->
                LetMutDef {
                    Symbol = sym
                    TypeSpec = typeSpec
                    Value = value
                } |> GlobalLetDef)
        let parseLetImmDef =
            (parseString "let" >>. parseSymbol .>>. opt parseTypeAnnotation) .>>. (parseChar '=' >>. parseExpr)
            |>> (fun ((sym, typeSpec), value) ->
                LetImmDef {
                    Symbol = sym
                    TypeSpec = typeSpec
                    Value = value
                } |> GlobalLetDef)
        attempt parseLetMutDef <|> parseLetImmDef

    let parseTypeDef =
        let parseTypeParameter =
            sepBy parseSymbol (parseChar ',')
            |> between (parseChar '(') (parseChar ')')
        let parseRecordTypeDecl =
            let parseRecordField =
                (parseSymbol .>> parseChar ':') .>>. (parseType) .>> parseChar ';'
                |>> fun (fieldName, fieldType) -> { FieldName = fieldName; FieldType = fieldType }
            (parseString "type" >>. parseSymbol .>>. opt parseTypeParameter .>> parseChar '=') .>>.
            ((many1 parseRecordField) |> between (parseChar '{') (parseChar '}'))
            |>> fun ((typeName, typeParameterList), recordFields) ->
                TypeDef <| RecordTypeDecl { TypeName = typeName; TypeParameterList = typeParameterList; RecordFields = recordFields }
        parseRecordTypeDecl

    let parseTopLevel =
        many1 (choice [
            parseImportDecl
            parseGlobalLetDef
            parseFunctionDef
            parseTypeDef
        ] |> between ws ws)

    let parseBy p str =
        // run関数はFParsecが用意している、パーサーを実行するための関数
        match run p str with
        | Success(res, _, _) -> res
        | Failure(msg, _, _) -> failwithf "parse error: %s" msg
