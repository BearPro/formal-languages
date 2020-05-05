namespace FormalLanguages
#nowarn "0025"
open System
open CommandLine

module Program =

    let argparser = CommandLine.Parser.Default

    [<EntryPoint>]
    let main argv =
        let result = argparser.ParseArguments<Task1.Options, Task2.Assert.Options, Task2.StateNumber.Options> argv
        match result with
        | :? Parsed<obj> as command ->
            match command.Value with
            | :? Task1.Options             as o -> Task1.run o
            | :? Task2.Assert.Options      as o -> Task2.Assert.run o
            | :? Task2.StateNumber.Options as o -> Task2.StateNumber.run o
        | _ -> ignore()
        0
