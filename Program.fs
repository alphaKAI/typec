// Learn more about F# at http://fsharp.org

open System
open FSharpPlus

open TypeC

let parserTest testName parseFunc testCases =
    printfn "[Test for %20s]" testName
    List.iter (fun (test_case, expact) ->
        printfn "test case: %s" test_case
        let actual = Parser.parseBy parseFunc test_case
        printfn "  expacted: %A" expact
        printfn "  actual: %A" actual
        assert (expact = actual)
        printfn "-> OK") testCases
    printfn "[Test for %20s -> All OK]" testName

open AST

let doParserTest () =
    parserTest
      "ImportDecl"
      Parser.parseImportDecl
      [("import std.stdio;", ImportModulePath ["std"; "stdio"]); ("  import std.net.curl; ", ImportModulePath ["std"; "net"; "curl"])]

    parserTest
      "TypeSpec"
      Parser.parseTypeSpec
      [
        ("int -> int", ArrowType(BasicType(IntType(DefaultInt)), BasicType(IntType(DefaultInt))));
        ("int -> string -> uint",
          ArrowType(
              BasicType(IntType(DefaultInt)),
              ArrowType(
                BasicType(StringType),
                BasicType(UIntType(DefaultUInt)))));
        ("int -> string -> uint -> int",
          ArrowType(
              BasicType(IntType(DefaultInt)),
              ArrowType(
                BasicType(StringType),
                ArrowType(
                  BasicType(UIntType(DefaultUInt)),
                  BasicType(IntType(DefaultInt))
                ))))
        ("int -> (string -> uint)",
          ArrowType(
            BasicType(IntType(DefaultInt)),
            ArrowType(
                BasicType(StringType),
                BasicType(UIntType(DefaultUInt)))))]

    parserTest
      "ParameterList"
      Parser.parseParameterList
      [
        ("(a: int)",
          [{ Symbol = "a"; TypeSpec = BasicType(IntType(DefaultInt)) }]);
        ("(b : string)",
          [{ Symbol = "b"; TypeSpec = BasicType(StringType) }]);
        ("(c:uint)",
          [{ Symbol = "c"; TypeSpec = BasicType(UIntType(DefaultUInt)) }]);
        ("(d: int -> uint)",
          [{ Symbol = "d"; TypeSpec = ArrowType(BasicType(IntType(DefaultInt)), BasicType(UIntType(DefaultUInt))) }]);
        ("(a: int, b: string)",
          [{ Symbol = "a"; TypeSpec = BasicType(IntType(DefaultInt)) }
           { Symbol = "b"; TypeSpec = BasicType(StringType) }])]

    parserTest
      "Literal"
      Parser.parseLiteral
      [
        ("1234", Literal(IntegerLiteral 1234L))
        ("\"Hello, world!\"", Literal(StringLiteral "Hello, world!"))
        ("()", VoidLiteral |> Literal)
        ("[||]]", ArrayLiteral([]) |> Literal)
        ("[|1, 2, 3|]", ArrayLiteral([
          IntegerLiteral(1L) |> Literal
          IntegerLiteral(2L) |> Literal
          IntegerLiteral(3L) |> Literal
        ]) |> Literal)
        ("[]", ListLiteral([]) |> Literal)
        ("[1, 2, 3]", ListLiteral([
          IntegerLiteral(1L) |> Literal
          IntegerLiteral(2L) |> Literal
          IntegerLiteral(3L) |> Literal
        ]) |> Literal)
        ("fun () -> ()",
          FunctionLiteral {
            ParameterList = []
            Expr = Literal(VoidLiteral)
          } |> Literal)
        ("fun (x: int, y: int) -> x + y",
          FunctionLiteral {
            ParameterList = [
              { Symbol = "x"
                TypeSpec = BasicType(IntType(DefaultInt)) }
              { Symbol = "y"
                TypeSpec = BasicType(IntType(DefaultInt)) }
            ]
            Expr = AddExpr(Expr.Variable "x", Expr.Variable "y") |> MathExpr
          } |> Literal)
      ]

    parserTest
      "ReturnExpr"
      Parser.parseReturnExpr
      [
        ("return 1234;", ReturnExpr (Literal(IntegerLiteral 1234L)))
        ("return \"ABCDEF\";", ReturnExpr (Literal(StringLiteral "ABCDEF")))
      ]

    parserTest
      "MathExpr"
      Parser.parseMathExpr
      [
        ("1 + 2", AddExpr(Literal(IntegerLiteral(1L)), Literal(IntegerLiteral(2L))) |> MathExpr)
        ("1 - 2", SubExpr(Literal(IntegerLiteral(1L)), Literal(IntegerLiteral(2L))) |> MathExpr)
        ("1 * 2", MulExpr(Literal(IntegerLiteral(1L)), Literal(IntegerLiteral(2L))) |> MathExpr)
        ("1 / 2", DivExpr(Literal(IntegerLiteral(1L)), Literal(IntegerLiteral(2L))) |> MathExpr)
        ("1 % 2", ModExpr(Literal(IntegerLiteral(1L)), Literal(IntegerLiteral(2L))) |> MathExpr)
        ("1 * (2 + 6) / (10 - 6)",
          DivExpr(
            MulExpr(
              Literal(IntegerLiteral(1L)),
              AddExpr(Literal(IntegerLiteral(2L)), Literal(IntegerLiteral(6L))) |> MathExpr) |> MathExpr,
            SubExpr(Literal(IntegerLiteral(10L)), Literal(IntegerLiteral(6L))) |> MathExpr) |> MathExpr)
      ]

    parserTest
      "CallExpr"
      Parser.parseCallExpr
      [
        ("println(\"Hello, world\")",
          CallExpr (FunctionCall {
            FuncName = "println"
            FunctionCallArguments = [
              Literal(StringLiteral("Hello, world"))
            ]
          }))
        ("printfln(\"%s\", \"abc\")",
          CallExpr (FunctionCall {
            FuncName = "printfln"
            FunctionCallArguments = [
              Literal(StringLiteral("%s"))
              Literal(StringLiteral("abc"))
            ]
          }))
      ]

    parserTest
      "Expr"
      Parser.parseExpr
      [
        ("return 1234", ReturnExpr (Literal(IntegerLiteral 1234L)))
        ("return 1234;", ReturnExpr (Literal(IntegerLiteral 1234L)))
        ("1 + 2", AddExpr(Literal(IntegerLiteral(1L)), Literal(IntegerLiteral(2L))) |> MathExpr)
        ("1 + 2;", AddExpr(Literal(IntegerLiteral(1L)), Literal(IntegerLiteral(2L))) |> MathExpr)
        ("1234",  (Literal(IntegerLiteral 1234L)))
        ("1234;", (Literal(IntegerLiteral 1234L)))
      ]

    parserTest
      "FunctionDef"
      Parser.parseFunctionDef
      [
        ("fn main(): void { println(\"Hello, world\") }",
          {
            FuncName = "main";
            OptTemplateParameterDef = None
            ParameterList = []
            ReturnType = BasicType(VoidType)
            FuncCode = [
              CallExpr (FunctionCall {
                FuncName = "println"
                FunctionCallArguments = [
                  Literal(StringLiteral("Hello, world"))
                ]
              })
            ]
          })
      ]

let testParseFile fileName p =
  let src = System.IO.File.ReadAllText fileName
  let r = Parser.parseBy p src
  printfn "[source code (%s)]:" fileName
  printf "%s\n" src
  printfn "[parse Result]:"
  printfn "%A" r

open FParsec

[<EntryPoint>]
let main argv =
    doParserTest ()
    testParseFile "./examples/helloworld.tc" Parser.parseTopLevel
    testParseFile "./examples/template_func.tc" Parser.parseTopLevel
    0 // return an integer exit code
