namespace TypeC

module Parser =

    open System.Text
    open FParsec
    open AST

    let ws = spaces
    let isAsciiIdStart c = isAsciiLetter c
    let isAsciiIdContinue c = isAsciiLetter c || isDigit c

    let identOpts =
        IdentifierOptions
            (isAsciiIdStart = isAsciiIdStart, isAsciiIdContinue = isAsciiIdContinue,
             normalization = NormalizationForm.FormKC, normalizeBeforeValidation = true)
    let ident = identifier identOpts

    let parseSymbol = ws >>. ident .>> ws
    let parseString sym = ws >>. pstring sym .>> ws
    let parseChar chr = ws >>. pchar chr .>> ws

    let parseImportDecl: Parser<ImportDecl, unit> =
        sepBy ident (parseChar '.')
        |> between (parseString "import") (parseChar ';')
        |>> ImportModulePath

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

    let parseArrowType =
        let parseOtherType = parseBasicType <|> parseUserDefinedType
        let opp = new OperatorPrecedenceParser<TypeSpec, unit, unit>()

        opp.TermParser <- parseOtherType <|> (parseType |> between (parseString "(") (parseString ")"))
        opp.AddOperator(InfixOperator("->", ws, 1, Associativity.Right, (fun x y -> ArrowType(x, y))))
        opp.ExpressionParser

    parseTypeR := choice
                      [ attempt parseArrowType
                        attempt parseBasicType
                        attempt parseDefaultUIntType ]

    let parseTypeSpec = parseType

    let parseTypeAnnotation = parseString ":" >>. parseTypeSpec

    let parseParameter =
        parseSymbol .>> parseString ":" .>>. parseTypeSpec |>> (fun (symbol, typeSpec) ->
        { Symbol = symbol
          TypeSpec = typeSpec })

    let parseParameterList =
        sepBy parseParameter (parseChar ',') |> between (ws .>> parseChar '(' .>> ws) (ws .>> parseChar ')' .>> ws)

    let parseExpr, parseExprR = createParserForwardedToRef()

    let parseTemplateParameter =
        let parseGenericParameter = parseSymbol |>> GenericParameter
        parseGenericParameter

    let parseTemplateParameterList =
        sepBy parseTemplateParameter (parseChar ',') |> between (parseChar '<') (parseChar '>')


    let parseReturnExpr = parseString "return" >>. parseExpr |>> ReturnExpr

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

    let parseLiteral =
        choice [ parseIntLiteral; parseStringLiteral; parseVoidLiteral; parseArrayLiteral; parseListLiteral ]
        |>> Literal |> between ws ws

    let parseVariable = parseSymbol |>> Expr.Variable


    let parseCallExpr =
        let parseFunctionCallArguments =
            sepBy parseExpr (parseChar ',') |> between (parseString "(") (parseString ")")
        let parseFunctionCall = parseSymbol .>>. parseFunctionCallArguments
        parseFunctionCall |>> (fun (funcName, args) ->
        CallExpr
            (FunctionCall
                { FuncName = funcName
                  FunctionCallArguments = args }))

    let parseMathExpr =
        let opp = new OperatorPrecedenceParser<Expr, unit, unit>()

        opp.TermParser <-
            (choice
                [ attempt parseCallExpr
                  attempt parseVariable
                  attempt parseLiteral ])
            <|> (between (parseString "(") (parseString ")") parseExpr)

        opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, (fun x y -> AddExpr(x, y) |> MathExpr)))
        opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, (fun x y -> SubExpr(x, y) |> MathExpr)))
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
        opp.ExpressionParser

    let parseBlock: Parser<Expr list, unit> =
        ws >>. many parseExpr .>> ws |> between (parseChar '{') (parseChar '}')

    parseExprR := choice
                      [ attempt parseReturnExpr
                        attempt parseMathExpr
                        attempt parseCallExpr
                        attempt parseVariable
                        parseLiteral ]
                  .>> opt (parseString ";")

    let parseFunctionDef: Parser<FunctionDef, unit> =
        let parseTemplateFunctionDef =
            parseString "fn" >>. parseSymbol .>>. parseTemplateParameterList .>>. parseParameterList
            .>>. parseTypeAnnotation .>>. parseBlock
            |>> (fun ((((funcName, templateParameters), parameters), retType), block) ->
                { FuncName = funcName
                  OptTemplateParameterDef = Some templateParameters
                  Parameters = parameters
                  ReturnType = retType
                  FuncCode = block })

        let parseNomalFunctionDef =
            parseString "fn" >>. parseSymbol .>>. parseParameterList .>>. parseTypeAnnotation .>>. parseBlock
            |>> (fun (((funcName, parameters), retType), block) ->
                { FuncName = funcName
                  OptTemplateParameterDef = None
                  Parameters = parameters
                  ReturnType = retType
                  FuncCode = block })

        attempt parseTemplateFunctionDef <|> parseNomalFunctionDef

    let parseTopLevel =
        many1 (ws >>. (attempt (parseImportDecl |>> ImportDecl) <|> (parseFunctionDef |>> FunctionDef)) .>> ws)

    let parseBy p str =
        // run関数はFParsecが用意している、パーサーを実行するための関数
        match run p str with
        | Success(res, _, _) -> res
        | Failure(msg, _, _) -> failwithf "parse error: %s" msg


    let example1() =
        FunctionDef
            ({ FuncName = "main"
               OptTemplateParameterDef = None
               Parameters = []
               ReturnType = BasicType(IntType(DefaultInt))
               FuncCode =
                   [ CallExpr
                       (FunctionCall
                           ({ FuncName = "println"
                              FunctionCallArguments = [ Literal(StringLiteral("Hello, world")) ] }))
                     ReturnExpr(Literal(IntegerLiteral(0L))) ] })
