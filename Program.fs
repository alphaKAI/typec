// Learn more about F# at http://fsharp.org

open System
open FSharpPlus

open TypeC

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    printfn "ex1: %A" (Parser.example1 ())
    0 // return an integer exit code
