namespace FormalLanguages

open System.IO
open CommandLine
open FormalLanguages.Character

/// Определяет решение для лабораторной работы №1.
module Task1 =
    type Options =
        { [<Option('g', "grammar", Required = true, HelpText = "Grammar description file")>]
          grammar: string
          [<Option('w', "words", SetName = "read-line", Required = true, HelpText = "Source word to check")>]
          words: string seq
          [<Option('t', "tree", Default = false, HelpText = "If set - displays syntax tree of words.")>]
          displayTree: bool
          [<Option("display-grammar", Default = false, HelpText = "If set - displays parsed grammar.")>]
          displayGrammar: bool
          [<Option('p', "path", SetName = "read-file", Required = true, HelpText = "Path to source file to check")>]
          paths: string seq }

    /// Возвращает истину, если корень синтаксимческого дерева соответствует аксиоме указанной
    /// грамматики.
    let checkTree grammar =
        function
        | [{Node=n}] when n = grammar.Axiom -> true
        | _ -> false

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
            |> Seq.map (Grammar.parseWord grammar)

        if options.displayTree then
            for word in words do
                let tree = parse word
                printfn "Tree of %s: \n%A" (toString word) tree

        for word in words do
            let tree = parse word
            printfn "%A\t%b" (toString word) (check tree)

    let main argv =
        let result = CommandLine.Parser.Default.ParseArguments<Options>(argv)
        match result with
        | :? Parsed<Options> as options -> checkWords options.Value
        | _ -> ignore()
