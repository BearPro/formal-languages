namespace FormalLanguages.Syntax

open FormalLanguages
open FormalLanguages.Utils

module LRParser =
    /// Возвращает результат первого возможный складывания синтаксическое дерево в порядке,
    /// обратном заданному правилом.
    let private fold syntax rule =
        let start = subseqIndex rule.Left (ofWord syntax)
        let ``end`` = start + rule.Left.Length
        let childs = syntax
                     |> List.skip start
                     |> List.take rule.Left.Length
        let node = { Node = rule.Right
                     Childs = childs }
        (List.take start syntax) @ (node :: List.skip ``end`` syntax)

    /// Возвращает новые значения для стека и входной последовательности,
    /// таким образом чтобы на стеке находилось k - 1 терминальных символов
    /// перед последним нетерминальным символом.
    let private moveStack k (input, stack) =
        let rec exactlyMove number (word, stack) =
            if number = 0
            then (word, stack)
            else exactlyMove (number - 1) (word |> List.tail, stack @ [word.Head])
        let isNonterm syntaxNode =
            match syntaxNode.Node with
            | Nonterminal _ -> true
            | _ -> false
        let countAfterLastNonterm =
            match List.tryFindIndexBack isNonterm stack with
            | Some index -> (Seq.length stack) - index - 1
            | None -> 0
        let needPushCount = if k - countAfterLastNonterm >= 0 then k - countAfterLastNonterm else 0
        let ablePushCount = input |> List.length
        match needPushCount, ablePushCount with
        | _   , 0                     -> input, stack
        | 0   , able when able > 0    -> input, stack
        | need, able when able < need -> exactlyMove able (input, stack)
        | need, _                     -> exactlyMove need (input, stack)

    let rec private parse grammar (input, stack) =
        let k = 4
        grammar.Rules
        |> Seq.tryFind (fun rule -> hasSubseq rule.Left (ofWord stack))
        |> function
        | Some rule -> Some (fold stack rule)
        | None -> None
        |> function
        | None       -> if List.isEmpty input
                        then stack
                        else parse grammar (moveStack k (input, stack))
        | Some stack -> parse grammar (moveStack k (input, stack))

    /// Возвращает синтаксическое дерево указанного слова в указанной грамматике.
    /// Если построить дерево не удалось - возвращает полный бред.
    let tree grammar word =
        parse grammar (surfaceWord word, [])
