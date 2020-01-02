namespace TypeC

module Parser =

    open FParsec
    open AST

    //let parseTopLevel

    let parse _ = printfn "OK"

    let example1() =
        FunctionDef
            ({ FuncName = "main"
               OptTemplateArgumentDef = None
               Parameters = []
               ReturnType = BasicType(IntType(DefaultInt))
               FuncCode =
                   [ CallExpr
                       (FunctionCall
                           ({ FuncName = "println"
                              FunctionCallArguments = [ Literal(StringLiteral("Hello, world")) ] }))
                     ReturnExpr(Literal(InetegerLiteral(0L))) ] })
