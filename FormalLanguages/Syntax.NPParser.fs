namespace FormalLanguages.Syntax

open FormalLanguages
open FormalLanguages.Utils
open FormalLanguages.Rule
open FormalLanguages.Grammar

/// Операции, которые нужны для разбора синтаксического дерева слов примерно
/// чуть больше, чем за факториальное время.
module NPParser =
    /// Возвращает все возможные способы сложить синтаксическое дерево в порядке, обратном
    /// заданному правилом.
    let private allFolds syntax rule =
        let indexes = subseqIndexes rule.Left (ofWord syntax)
        seq { for start in indexes ->
                let ``end`` = start + rule.Left.Length
                let childs = syntax |> List.skip start |> List.take rule.Left.Length
                let node = { Node = rule.Right
                             Childs = childs }
                (List.take start syntax) @ (node :: List.skip (``end``) syntax) }

    let rec private parse grammar (syntax: Syntax<_> list) =
        let folds =
            grammar.Rules
            |> Seq.collect (allFolds syntax)
            |> Seq.sortByDescending (List.length)
        let isRoot = function
                     | [{Node=n}] when n = grammar.Axiom -> true
                     | _ -> false
        Seq.append
            (folds |> Seq.where isRoot)
            (folds |> Seq.where (not<<isRoot)
                   |> Seq.collect (parse grammar))

    /// Строит синтаксическое дерево данного слова в заданной грамматике
    let tree grammar word =
        surfaceWord word
        |> parse grammar
        |> Seq.tryPick (function
        | [root] when root.Node = grammar.Axiom -> Some root
        | _                                     -> None)
