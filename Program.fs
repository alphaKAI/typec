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
      [("import std.stdio;", ImportModulePath ["std"; "stdio"] |> ImportDecl); ("  import std.net.curl; ", ImportModulePath ["std"; "net"; "curl"] |> ImportDecl)]

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
        ("[||]", ArrayLiteral([]) |> Literal)
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
      "LetExpr"
      Parser.parseLetExpr
      [
        ("let x = 10 in x", LetExpr <| LetImmExpr {
          Symbol = "x"
          TypeSpec = None
          Value = Literal(IntegerLiteral(10L))
          Expr = Expr.Variable "x"
        })
        ("let x = 10 in x * x", LetExpr <| LetImmExpr {
          Symbol = "x"
          TypeSpec = None
          Value = Literal(IntegerLiteral(10L))
          Expr = MathExpr <| MulExpr(Expr.Variable "x", Expr.Variable "x")
        })
        ("let x = \"Hello, world\" in printfln(\"%s\", x)", LetExpr <| LetImmExpr {
          Symbol = "x"
          TypeSpec = None
          Value = Literal(StringLiteral("Hello, world"))
          Expr = CallExpr (FunctionCall {
            FuncName = "printfln"
            FunctionCallArguments = [
              Literal(StringLiteral("%s"))
              Expr.Variable "x"
            ]
          })
        })
        ("let x: int = 10 in x", LetExpr <| LetImmExpr {
          Symbol = "x"
          TypeSpec = Some (BasicType (IntType (DefaultInt)))
          Value = Literal(IntegerLiteral(10L))
          Expr = Expr.Variable "x"
        })
        ("let x: int = 10 in x * x", LetExpr <| LetImmExpr {
          Symbol = "x"
          TypeSpec = Some (BasicType (IntType (DefaultInt)))
          Value = Literal(IntegerLiteral(10L))
          Expr = MathExpr <| MulExpr(Expr.Variable "x", Expr.Variable "x")
        })
        ("let x: string = \"Hello, world\" in printfln(\"%s\", x)", LetExpr <| LetImmExpr {
          Symbol = "x"
          TypeSpec = Some (BasicType(StringType))
          Value = Literal(StringLiteral("Hello, world"))
          Expr = CallExpr (FunctionCall {
            FuncName = "printfln"
            FunctionCallArguments = [
              Literal(StringLiteral("%s"))
              Expr.Variable "x"
            ]
          })
        })
        ("let mut x = 10 in x", LetExpr <| LetMutExpr {
          Symbol = "x"
          TypeSpec = None
          Value = Literal(IntegerLiteral(10L))
          Expr = Expr.Variable "x"
        })
        ("let mut x = 10 in x * x", LetExpr <| LetMutExpr {
          Symbol = "x"
          TypeSpec = None
          Value = Literal(IntegerLiteral(10L))
          Expr = MathExpr <| MulExpr(Expr.Variable "x", Expr.Variable "x")
        })
        ("let mut x = \"Hello, world\" in printfln(\"%s\", x)", LetExpr <| LetMutExpr {
          Symbol = "x"
          TypeSpec = None
          Value = Literal(StringLiteral("Hello, world"))
          Expr = CallExpr (FunctionCall {
            FuncName = "printfln"
            FunctionCallArguments = [
              Literal(StringLiteral("%s"))
              Expr.Variable "x"
            ]
          })
        })
        ("let mut x: int = 10 in x", LetExpr <| LetMutExpr {
          Symbol = "x"
          TypeSpec = Some(BasicType(IntType(DefaultInt)))
          Value = Literal(IntegerLiteral(10L))
          Expr = Expr.Variable "x"
        })
        ("let mut x: int = 10 in x * x", LetExpr <| LetMutExpr {
          Symbol = "x"
          TypeSpec = Some(BasicType(IntType(DefaultInt)))
          Value = Literal(IntegerLiteral(10L))
          Expr = MathExpr <| MulExpr(Expr.Variable "x", Expr.Variable "x")
        })
        ("let mut x: string = \"Hello, world\" in printfln(\"%s\", x)", LetExpr <| LetMutExpr {
          Symbol = "x"
          TypeSpec = Some(BasicType(StringType))
          Value = Literal(StringLiteral("Hello, world"))
          Expr = CallExpr (FunctionCall {
            FuncName = "printfln"
            FunctionCallArguments = [
              Literal(StringLiteral("%s"))
              Expr.Variable "x"
            ]
          })
        })
      ]

    parserTest
      "IfExpr"
      Parser.parseIfExpr
      [
        ("if a == 1 then println(\"a == 1\")", IfExpr {
          Cond = CompareExpr(EqualExpr(Expr.Variable "a", Literal(IntegerLiteral(1L))))
          TrueExpr = CallExpr (FunctionCall {
            FuncName = "println"
            FunctionCallArguments = [
              Literal(StringLiteral("a == 1"))
            ]
          })
          FalseExpr = None
        })
      ]
    
    parserTest
      "ForExpr"
      Parser.parseForExpr
      [
        ("for i = 0 to 10 { printfln(\"i = %d\", i) }",
          ForExpr {
            Symbol = "i"
            Begin = Literal(IntegerLiteral(0L))
            End = Literal(IntegerLiteral(10L))
            Block = [
                CallExpr (FunctionCall {
                FuncName = "printfln"
                FunctionCallArguments = [
                  Literal(StringLiteral("i = %d"))
                  Expr.Variable "i"
                ]
              })
            ]
          }
        )
      ]
      
    parserTest
      "WhileExpr"
      Parser.parseWhileExpr
      [
        ("while some_cond { printfln(\"Loop\") }",
          WhileExpr {
            Cond = Expr.Variable "some_cond"
            Block = [
                CallExpr (FunctionCall {
                FuncName = "printfln"
                FunctionCallArguments = [
                  Literal(StringLiteral("Loop"))
                ]
              })
            ]
          }
        )
      ]

    parserTest
      "BinaryOperatorExpr"
      Parser.parseBinaryOperatorExpr
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
    
        ("a && b", AndExpr(Expr.Variable "a", Expr.Variable "b") |> LogicExpr)
        ("a || b", OrExpr(Expr.Variable "a", Expr.Variable "b") |> LogicExpr)
        ("!a", NotExpr(Expr.Variable "a") |> LogicExpr)
        ("!(a && b)", NotExpr(AndExpr(Expr.Variable "a", Expr.Variable "b") |> LogicExpr) |> LogicExpr)
        ("!(!a || b)", NotExpr(OrExpr(NotExpr (Expr.Variable "a") |> LogicExpr, Expr.Variable "b") |> LogicExpr) |> LogicExpr)
        ("(a && b) || (c && !d)",
          OrExpr(
            AndExpr(Expr.Variable "a", Expr.Variable "b") |> LogicExpr,
            AndExpr(Expr.Variable "c", NotExpr (Expr.Variable "d") |> LogicExpr) |> LogicExpr
          ) |> LogicExpr)

        ("a & b", LAndExpr(Expr.Variable "a", Expr.Variable "b") |> BitwiseExpr)
        ("a | b", LOrExpr(Expr.Variable "a", Expr.Variable "b") |> BitwiseExpr)
        ("a << b", LLeftShift(Expr.Variable "a", Expr.Variable "b") |> BitwiseExpr)
        ("a >> b", LRightShift(Expr.Variable "a", Expr.Variable "b") |> BitwiseExpr)
        ("~a", LNotExpr(Expr.Variable "a") |> BitwiseExpr)
        ("~(a & b)", LNotExpr(LAndExpr(Expr.Variable "a", Expr.Variable "b") |> BitwiseExpr) |> BitwiseExpr)
        ("~(~a | b)", LNotExpr(LOrExpr(LNotExpr (Expr.Variable "a") |> BitwiseExpr, Expr.Variable "b") |> BitwiseExpr) |> BitwiseExpr)
        ("(a & b) | (c & ~d)",
          LOrExpr(
            LAndExpr(Expr.Variable "a", Expr.Variable "b") |> BitwiseExpr,
            LAndExpr(Expr.Variable "c", LNotExpr (Expr.Variable "d") |> BitwiseExpr) |> BitwiseExpr
          ) |> BitwiseExpr)
 
        ("a == b", EqualExpr(Expr.Variable "a", Expr.Variable "b") |> CompareExpr)
        ("a != b", NotEqualExpr(Expr.Variable "a", Expr.Variable "b") |> CompareExpr)
        ("a < b", LessExpr(Expr.Variable "a", Expr.Variable "b") |> CompareExpr)
        ("a > b", GreaterExpr(Expr.Variable "a", Expr.Variable "b") |> CompareExpr)
        ("a <= b", LessThanExpr(Expr.Variable "a", Expr.Variable "b") |> CompareExpr)
        ("a >= b", GreaterThanExpr(Expr.Variable "a", Expr.Variable "b") |> CompareExpr)
 
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
          FunctionDef {
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
    testParseFile "./examples/let.tc" Parser.parseTopLevel
    testParseFile "./examples/if.tc" Parser.parseTopLevel
    testParseFile "./examples/for.tc" Parser.parseTopLevel
    testParseFile "./examples/while.tc" Parser.parseTopLevel
    0 // return an integer exit code
