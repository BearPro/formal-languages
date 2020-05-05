namespace FormalLanguages

open System.IO
open CommandLine
open FormalLanguages.Character
open System.Collections.Generic

/// Определяет решение для лабораторной работы №1.
module Task1 =
    [<Verb("task1")>]
    type Options =
        { [<Option('g', "grammar", Required = true, HelpText = "Grammar description file")>]
          grammar: string
          [<Option('w', "words", Required = false, HelpText = "Source word to check")>]
          words: string seq
          [<Option('t', "tree", Default = false, HelpText = "If set - displays syntax tree of words.")>]
          displayTree: bool
          [<Option("display-grammar", Default = false, HelpText = "If set - displays parsed grammar.")>]
          displayGrammar: bool
          [<Option('p', "path", Required = false, HelpText = "Path to source file to check")>]
          paths: string seq }

    /// Возвращает истину, если корень синтаксимческого дерева соответствует аксиоме указанной
    /// грамматики.
    let checkTree grammar =
        function
        | [{Node=n}] when n = grammar.Axiom -> true
        | _ -> false

    let tryParseWord grammar word =
        try
            Some <| Grammar.parseWord grammar word
        with
        | :? KeyNotFoundException ->
            printfn "Some symbols of word %A not defined in grammar. Skip." word
            None
    /// Основная функция, демонстрирующая работу программы.
    let checkWords options =
        let grammar = Grammar.Parser.parseFile options.grammar
        let check = checkTree grammar
        let parse = Syntax.LRParser.tree grammar
        if options.displayGrammar then
            printfn "Grammar:\n%A" grammar

        let words =
            options.paths
            |> Seq.map File.ReadAllText
            |> Seq.append options.words
            |> Seq.map (tryParseWord grammar)
            |> Seq.where Option.isSome
            |> Seq.map Option.get

        if options.displayTree then
            for word in words do
                let tree = parse word
                printfn "Tree of %s: \n%A" (toString word) tree

        for word in words do
            let tree = parse word
            printfn "%A\t%b" (toString word) (check tree)

    let run options =
        checkWords options