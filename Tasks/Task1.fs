namespace FormalLanguages

open System
open System.IO
open System.Text.RegularExpressions
open CommandLine
open Grammar

module Task1 =
    type Options =
        { [<Option('g', "grammar", Required = true, HelpText = "Grammar description file")>]
          grammar: string
          [<Option('w', "words", Required = true, HelpText = "Source word to check")>]
          words: string seq }

    module Parser =
        let private parseRuleRight terminals nonterminals (input: string) =
            input.Split(' ')
            |> List.ofArray
            |> List.map (function
                | char when List.contains char terminals -> Terminal char
                | char when List.contains char nonterminals -> Nonterminal char
                | x -> failwith (sprintf "Symbol '%s' not registered" x))

        let private parseRule terminals nonterminals (input: string) =
            match input.Trim().Split "->" with
            | [| l; r |] -> Nonterminal l --> parseRuleRight terminals nonterminals r
            | other      -> failwith (sprintf "Not valid rule format: '%A'" other)

        let grammar string: Grammar<string, string> =
            let pairs =
                Regex.Matches(string, @"(?<param>[IVWP])=(?<value>.*);")
                |> Seq.map (fun m -> (m.Groups.["param"].Value, m.Groups.["value"].Value))
                |> List.ofSeq

            let valueof name =
                pairs
                |> List.pick (function
                    | (p, v) when p = name -> Some v
                    | _                    -> None)

            let i = (valueof "I") |> Nonterminal
            let v = (valueof "V").Split(",") |> List.ofArray
            let w = (valueof "W").Split(",") |> List.ofArray
            let p = (valueof "P").Split(",") |> List.ofArray
                                             |> List.map (parseRule v w)

            { Axiom        = i
              Rules        = p
              Terminals    = v
              Nonterminals = w }


    let fixInput (input: string) = input.Split(";") |> Array.reduce (fun acc line -> (acc + ";\n" + line + ";\n"))

    let main argv =
        let result = CommandLine.Parser.Default.ParseArguments<Options>(argv)
        match result with
        | :? (Parsed<Options>) as parsed ->
            let grammar =
                File.ReadAllLines parsed.Value.grammar
                |> Array.reduce (+)
                |> fixInput
                |> Parser.grammar
            for word in parsed.Value.words do
                printfn "%s - %b" word (Type2.check (Type2.word (Seq.map string word)) grammar)
            ()
        | _ -> ignore()
