namespace FormalLanguages

open System
open System.Linq
open FormalLanguages.Utils
open Character
open Rule

type Syntax<'a> =
    { Node: 'a
      Childs: Syntax<'a> list }

type 'a Alphabet = 'a list

type Grammar<'a, 'b> =
    { Terminals:    'a Alphabet
      Nonterminals: 'b Alphabet
      Rules:        Rule<'a, 'b> list
      Axiom:        Character<'a, 'b> }

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Grammar =
    /// На основе строки возвращает слово, состоящее из терминальныных символов указанной
    /// грамматики.
    let rec parseWord grammar word =
        let firstTerm word terms : (string * int) =
            let term = terms |> Seq.tryFind (fun term -> subseqIndex term word = (Some 0))
            match term with
            | Some term -> (term, (subseqIndex term word).Value)
            | None -> failwith (sprintf
                                    "Не удалось распознать символ начиная с \n%s"
                                    (String.Concat word)
                                )
        if Seq.isEmpty word
        then []
        else match firstTerm word grammar.Terminals with
             | (term, index) -> t term :: parseWord grammar (Seq.skip (index + term.Length) word)

    module Type2 =

        let private makeWord syntax = syntax |> List.map ( fun {Node = s} -> s)

        let private makeSyntax word = word |> List.map ( fun c -> { Node = c; Childs = []})

        /// Возвращает все возможные способы сложить синтаксиечкое дерево в порядке, обратном
        /// заданному правилом.
        let private allFolds syntax rule =
            let indexes = subseqIndexes rule.Left (makeWord syntax)
            seq {
                for start in indexes ->
                    let ``end`` = start + rule.Left.Length
                    let childs = syntax |> List.skip start |> List.take rule.Left.Length
                    let node = { Node = rule.Right
                                 Childs = childs }
                    (List.take start syntax) @ (node :: List.skip (``end``) syntax)
                }

        /// Возвращает результат первого возможный складывания синтаксическое дерево в порядке,
        /// обратном заданному правилом.
        let private fold syntax rule =
            let start = (subseqIndex rule.Left (makeWord syntax)).Value
            let ``end`` = start + rule.Left.Length
            let childs = syntax |> List.skip start |> List.take rule.Left.Length
            let node = { Node = rule.Right
                         Childs = childs }
            (List.take start syntax) @ (node :: List.skip (``end``) syntax)

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


        module NPParser =
            /// Строит синтаксическое дерево данного слова в заданной грамматике
            let tree grammar word =
                let roots = parse grammar (makeSyntax word)
                roots
                |> Seq.tryPick (function
                | [root] when root.Node = grammar.Axiom -> Some root
                | _                                     -> None)

        module LRParser =
            /// Возвращет число символов, которое нужно прочитать наперёд чтобы точно разобрать
            /// самое длинное правило в грамматике.
            let private offsetSize { Rules = rules} =
                let maxLength =
                    rules
                    |> List.map (fun r -> List.length r.Left)
                    |> List.max
                maxLength - 1

            /// Возвращает новые значения для стека и слова, таким образом чтобы на стеке находилось
            /// k - 1 терминальных символов перед последним нетерминальным.
            let moveStack k (word, stack) =
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
                let ablePushCount = word |> List.length
                match needPushCount, ablePushCount with
                | _   , 0                      -> word, stack
                | 0   , able  when able > 0    -> word, stack //exactlyMove 1 (word, stack)
                | need, able  when able < need -> exactlyMove able (word, stack)
                | need, _                      -> exactlyMove need (word, stack)

            let rec private parse grammar (word, stack) =
                let k = 4//offsetSize grammar
                let appl =
                    grammar.Rules
                    //|> Seq.sortByDescending (fun r -> List.length r.Left)
                    |> Seq.tryFind (fun rule -> hasSubseq rule.Left (makeWord stack))
                    |> function
                    | Some rule -> Some (fold stack rule)
                    | None -> None
                match appl with
                | None -> if word |> List.isEmpty
                          then stack
                          else parse grammar (moveStack k (word, stack))
                | Some stack -> parse grammar (moveStack k (word, stack))

            let tree grammar word =
                parse grammar (makeSyntax word, [])
