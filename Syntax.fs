namespace FormalLanguages

open FormalLanguages.Utils
open FormalLanguages

/// Определяет синтаксическое дерево.
type Syntax<'a> =
    { Node: 'a
      Childs: Syntax<'a> list }

/// Определяет некоторые операции над синтаксическим деревом.
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Syntax =

    /// Возвращает слово, состоящее из верхних символов, записанном в корнях списка
    /// синтаксических деревьев.
    let private ofWord syntax = syntax |> List.map ( fun {Node = s} -> s)

    /// Возвращает список вырожденных деревьев, обладающих нулевой глубиной и корнями,
    /// в соответствии со списком на входе.
    let private surfaceWord word = word |> List.map ( fun c -> { Node = c; Childs = []})

    /// Возвращает поддерево, значение корня которого равно nodeValue.
    /// Если такого поддерева не найдено - возвращает None.
    let rec tryGetNode nodeValue tree =
        match tree with
        | { Node = value} when value = nodeValue -> Some tree
        | { Childs = [] } -> None
        | { Childs = childs } -> childs |> List.tryPick (tryGetNode nodeValue)

    /// Возвращает поддерево, значение корня которого равно nodeValue.
    /// Корень дерева должен быть расположен не глубже чем на глубине maxDepth.
    /// Если такого поддерева не найдено - возвращает None.
    let rec tryGetNodeLim nodeValue maxDepth tree =
        if maxDepth < 0
        then None
        else match tree with
             | { Node = value} when value = nodeValue -> Some tree
             | { Childs = [] } -> None
             | { Childs = childs } -> childs
                                   |> List.tryPick (tryGetNodeLim nodeValue (maxDepth - 1))

    /// Возвращает поддерево, значение корня которого равно nodeValue.
    /// Если такого поддерева не найдено - выбрасывает исключение.
    let getNode nodeValue tree =
        tryGetNode nodeValue tree
        |> function
        | Some value -> value
        | None -> failwith (sprintf "Не найдено поддерева %A" nodeValue)

    /// Возвращает все узлы дерева, в значениях которых находится nodeValue и которые не имеют
    /// дочерних узлов со значением nodeValue.
    let rec getLeafNodes nodeValue tree =
        match tree with
        | { Node = node; Childs = []} when node <> nodeValue -> []
        | { Node = node; Childs = childs} when node = nodeValue ->
            let validChilds =
                childs
                |> List.collect (getLeafNodes nodeValue)
            match validChilds with
            | [] -> [ tree ]
            | some -> validChilds
        | { Childs = childs } -> childs |> List.collect (getLeafNodes nodeValue)

    /// Возвращает поддерево, значение корня которого равно nodeValue.
    /// Корень дерева должен быть расположен не глубже чем на глубине maxDepth.
    /// Если такого поддерева не найдено - выбрасывает исключение.
    let getNodeLim nodeValue maxDepth tree =
        tryGetNodeLim nodeValue maxDepth tree
        |> function
        | Some value -> value
        | None -> failwith "No such value in tree"


    /// Операции, которые нужны для разбора синтаксического дерева слов примерно
    /// чуть больше, чем за факториальное время.
    module NPParser =
        /// Возвращает все возможные способы сложить синтаксическое дерево в порядке, обратном
        /// заданному правилом.
        let private allFolds syntax rule =
            let indexes = subseqIndexes rule.Left (ofWord syntax)
            seq {
                for start in indexes ->
                    let ``end`` = start + rule.Left.Length
                    let childs = syntax |> List.skip start |> List.take rule.Left.Length
                    let node = { Node = rule.Right
                                 Childs = childs }
                    (List.take start syntax) @ (node :: List.skip (``end``) syntax)
                }

        let rec private parse grammar (syntax: Syntax<_> list) =
            let folds =
                grammar.Rules
                |> Seq.collect (allFolds syntax)
                |> Seq.sortByDescending (List.length)
            let isRoot item = match item with
                              | [{Node=n}] when n = grammar.Axiom -> true
                              | _ -> false
            Seq.append
                (folds |> Seq.where isRoot)
                (folds |> Seq.where (not<<isRoot) |> Seq.collect (parse grammar))

        /// Строит синтаксическое дерево данного слова в заданной грамматике
        let tree grammar word =
            let roots = parse grammar (surfaceWord word)
            roots
            |> Seq.tryPick (function
            | [root] when root.Node = grammar.Axiom -> Some root
            | _                                     -> None)

    module LRParser =
        /// Возвращает результат первого возможный складывания синтаксическое дерево в порядке,
        /// обратном заданному правилом.
        let private fold syntax rule =
            let start = (subseqIndex rule.Left (ofWord syntax)).Value
            let ``end`` = start + rule.Left.Length
            let childs = syntax |> List.skip start |> List.take rule.Left.Length
            let node = { Node = rule.Right
                         Childs = childs }
            (List.take start syntax) @ (node :: List.skip (``end``) syntax)

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
            | _   , 0                      -> input, stack
            | 0   , able  when able > 0    -> input, stack //exactlyMove 1 (word, stack)
            | need, able  when able < need -> exactlyMove able (input, stack)
            | need, _                      -> exactlyMove need (input, stack)

        let rec private parse grammar (input, stack) =
            let k = 4//offsetSize grammar
            let appl =
                grammar.Rules
                //|> Seq.sortByDescending (fun r -> List.length r.Left)
                |> Seq.tryFind (fun rule -> hasSubseq rule.Left (ofWord stack))
                |> function
                | Some rule -> Some (fold stack rule)
                | None -> None
            match appl with
            | None -> if input |> List.isEmpty
                      then stack
                      else parse grammar (moveStack k (input, stack))
            | Some stack -> parse grammar (moveStack k (input, stack))

        /// Возвращает синтаксическое дерево указанного слова в указанной грамматике.
        /// Если построить дерево не удалось - возвращает полный бред.
        let tree grammar word =
            parse grammar (surfaceWord word, [])
