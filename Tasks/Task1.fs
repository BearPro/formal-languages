namespace FormalLanguages

open System
open System.IO
open System.Text.RegularExpressions
open CommandLine
//open Grammar

module Task1 =
    type Options =
        { [<Option('g', "grammar", Required = true, HelpText = "Grammar description file")>]
          grammar: string
          [<Option('w', "words", SetName = "read-line", Required = true, HelpText = "Source word to check")>]
          words: string seq
          [<Option('p', "path", SetName = "read-file", Required = true, HelpText = "Path to source file to check")>]
          paths: string seq }

    module Parser =
        open FormalLanguages.Character
        open FormalLanguages.Rule

        let private lowercaseLiterals = [for i in 97..122 -> (string<<char) i]
        let private uppercaseLietrals = [for i in 65..90 -> (string<<char) i]
        let private numberLiterals = [for i in 48..57 -> (string<<char) i]
        let private whitepsaces = [ " "; "\n"; "\t"; "\r"; "\n\r" ]

        /// Грамматика, которая описывает формальный язык описания грамматик.
        let grammarGrammar =
            let w = n "Whitespace";
            { Axiom = Nonterminal "Root"
              Terminals = [ "->"; " "; "="; ";"; ","; "("; ")";
                            "!V"; "!I"; "!P"; "!W" ]
                        @ lowercaseLiterals
                        @ uppercaseLietrals
                        @ whitepsaces
              Nonterminals = [ "Whitespace"
                               "Root"
                               "Rules"
                               "RuleDef"
                               "Axiom"
                               "TermSeq"
                               "SingleTerm"
                               "RuleRight"
                               "Nonterm"
                               "Section"
                               "Nonterminals" ]
              Rules =
                [
                  // Одиночный терминальный символ
                  for char in lowercaseLiterals -> n "SingleTerm" --> [ t char; ]
                ] @ [
                  // Пробельныые символы
                  for char in whitepsaces -> w --> [ t char; ]
                ] @ [
                  // Одиночный нетерминальный символ
                  for char in uppercaseLietrals -> n "Nonterm" --> [ t char; ]
                ] @ [
                  w               --> [ w; w ]
                  // Блок терминальных символов
                  n "Terminals"   --> [ t "!V"; w; t "="; w; ]
                  n "Terminals"   --> [ n "Terminals"; n "SingleTerm"; t ","; w; ]
                  n "Terminals"   --> [ n "Terminals"; n "SingleTerm"; t ";"; w; ]
                  // Блок нетерминальных символов
                  n "Nonterminals"--> [ t "!W"; w; t "="; w; ]
                  n "Nonterminals"--> [ n "Nonterminals"; n "Nonterm"; t ","; w; ]
                  n "Nonterminals"--> [ n "Nonterminals"; n "Nonterm"; t ";"; w; ]
                  // Блок аксиомы
                  n "Axiom"       --> [ t "!I"; w; t "="; w; n "Nonterm"; ]
                  n "Axiom"       --> [ n "Axiom"; t ";"; w; ]
                  // Блок правил
                  n "Rules"       --> [ t "!P"; w; t "="; w; n "RuleDef"; ]
                  n "Rules"       --> [ n "Rules"; w; n "RuleDef"; ]
                  // Левая часть правила
                  n "RuleLeft"    --> [ n "Nonterm"; t "->"; ]
                  // Правая часть парвила
                  n "RuleRight"  --> [ n "RuleRight"; n "SingleTerm"; ]
                  n "RuleRight"  --> [ n "RuleRight"; n "TermSeq"; ]
                  n "RuleRight"  --> [ n "RuleRight"; n "Nonterm"; ]
                  n "RuleRight"  --> [ n "Nonterm" ]
                  n "RuleRight"  --> [ n "SingleTerm" ]
                  n "RuleRight"  --> [ n "TermSeq" ]
                  n "RuleRight"  --> [ n "RuleRight"; n "RuleRight"; ]
                  // Определение правила
                  n "RuleDef"     --> [ n "RuleLeft"; n "RuleRight"; t ";" ]
                  n "RuleDef"     --> [ n "RuleLeft"; n "RuleRight"; t "," ]
                  n "RuleDef"     --> [ n "RuleDef"; w; n "RuleDef" ]
                  // Корневые элементы
                  n "Root"        --> [ n "Root"; n "Root" ]
                  n "Root"        --> [ n "Section"; ]
                  n "Section"     --> [ n "Section"; w; ]
                  n "Section"     --> [ n "Rules" ]
                  n "Section"     --> [ n "Axiom" ]
                  n "Section"     --> [ n "Terminals" ]
                  n "Section"     --> [ n "Nonterminals" ]
                  // Посдеовательность терминальных символов
                  n "TermSeq"     --> [ n "SingleTerm"; n "SingleTerm"; ]
                  n "TermSeq"     --> [ n "TermSeq"; n "SingleTerm"; ]
                ] }

        let rec tryGetNode nodeValue tree =
            match tree with
            | { Node = value} when value = nodeValue -> Some tree
            | { Childs = [] } -> None
            | { Childs = childs } -> childs |> List.tryPick (tryGetNode nodeValue)

        /// Ищет узел в дереве, но не глубже чем на глубине maxDepth.
        let rec tryGetNodeLim nodeValue maxDepth tree =
            if maxDepth < 0
            then None
            else match tree with
                 | { Node = value} when value = nodeValue -> Some tree
                 | { Childs = [] } -> None
                 | { Childs = childs } -> childs
                                       |> List.tryPick (tryGetNodeLim nodeValue (maxDepth - 1))

        let getNode nodeValue tree =
            tryGetNode nodeValue tree
            |> function
            | Some value -> value
            | None -> failwith "No such value in tree"

        /// Возвращает все узлы дерева, в значениях которых находится nodeValue и которые не имеют
        /// дочерних узлов со значением nodeValue
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

        let getNodeLim nodeValue maxDepth tree =
            tryGetNodeLim nodeValue maxDepth tree
            |> function
            | Some value -> value
            | None -> failwith "No such value in tree"

        /// Находит определение аксиомы в указанном синтаксимческом дереве, взвращает её.
        let parseAxiom tree =
            tree
            |> getNode (n "Axiom")
            |> getNode (n "Nonterm")
            |> fun x -> x.Childs.Head.Node
            |> function
            | Terminal x -> Nonterminal x
            | _ -> failwith "Не удалось найти аксиому."

        let rec parseTerminals tree =
            let terminals =
                tree
                |> getNode (n "Terminals")

            let char =
                terminals
                |> tryGetNodeLim (n "SingleTerm") 1
                |> function
                | None -> None
                | Some x -> Some x.Childs.Head.Node
                |> function
                | None -> None
                | Some(Terminal x) -> Some(x)
                | _ -> failwith "Не удалось найти терминальный символ."

            let next =
                terminals.Childs |> List.tryPick (tryGetNodeLim (n "Terminals") 0)

            match char, next with
            | None, _ -> []
            | Some last, None -> [last]
            | Some currnet, Some next -> currnet :: parseTerminals next

        let rec parseNonterminals tree =
            let nonterminals =
                tree
                |> getNode (n "Nonterminals")

            let char =
                nonterminals
                |> tryGetNodeLim (n "Nonterm") 1
                |> function
                | None -> None
                | Some x -> Some x.Childs.Head.Node
                |> function
                | None -> None
                | Some(Terminal x) -> Some(x)
                | _ -> failwith "Не удалось найти нетерминальный символ."

            let next =
                nonterminals.Childs |> List.tryPick (tryGetNodeLim (n "Nonterminals") 0)

            match char, next with
            | None, _ -> []
            | Some last, None -> [last]
            | Some currnet, Some next -> currnet :: parseNonterminals next

        let rec parseRules tree =
            let parseRuleDef tree =
                let rec parseRuleRight (tree : Syntax<Character<string, string>>) =
                    let parseNonterm tree =
                        match tree.Childs.[0].Node with
                        | Terminal x -> Nonterminal x
                        | _ -> failwith "Не удалось получить значение."
                    let parseTerm tree =
                        match tree.Childs.[0].Node with
                        | Terminal x -> Terminal x
                        | _ -> failwith "Не удалось получить значение."
                    let parseTermSeq tree =
                        failwith null
                    let items = tree.Childs |> List.map (fun x -> x.Node)
                    match items with
                    | [ Nonterminal "RuleRight"; Nonterminal "RuleRight"; ]  ->
                        parseRuleRight tree.Childs.[0] @ parseRuleRight tree.Childs.[1]
                    | [ Nonterminal "RuleRight"; Nonterminal "SingleTerm"; ] ->
                        parseRuleRight tree.Childs.[0] @ [ parseTerm tree.Childs.[1] ]
                    | [ Nonterminal "RuleRight"; Nonterminal "TermSeq"; ]    ->
                        parseRuleRight tree.Childs.[0] @ [ parseTermSeq tree.Childs.[1] ]
                    | [ Nonterminal "RuleRight"; Nonterminal "Nonterm"; ]    ->
                        parseRuleRight tree.Childs.[0] @ [ parseNonterm tree.Childs.[1] ]
                    | [ Nonterminal "Nonterm" ]    ->
                        [ parseNonterm tree.Childs.[0] ]
                    | [ Nonterminal "SingleTerm" ] ->
                        [ parseTerm tree.Childs.[0] ]
                    | [ Nonterminal "TermSeq" ]    ->
                        [ parseTermSeq tree.Childs.[0] ]
                    | _ -> failwith "Не удалось разобрать правую часть правила."

                let left = tree
                            |> getNode (n "RuleLeft")
                            |> getNode (n "Nonterm")
                            |> fun x -> x.Childs.Head.Node
                            |> function
                            | Terminal x -> Nonterminal x
                            | _ -> failwith "Не удалось найти правую часть правила."
                let right = tree
                            |> getNodeLim (n "RuleRight") 1
                            |> parseRuleRight
                left --> right

            let rules =
                tree
                |> getNode (n "Rules")

            let ruleDefs = rules |> getLeafNodes (n "RuleDef")
            let parsedRules = ruleDefs |> List.map parseRuleDef
            parsedRules


        let produceGrammar tree =
            let i = parseAxiom tree
            let v = parseTerminals tree |> List.rev
            let w = parseNonterminals tree |> List.rev
            let p = parseRules tree
            { Axiom        = i
              Rules        = p
              Terminals    = v
              Nonterminals = w }


    let fixInput (input: string) = input.Split(";") |> Array.reduce (fun acc line -> (acc + ";\n" + line + ";\n"))

    /// Возвращает грамматику, определённую в данном файле.
    let grammar = File.ReadAllText
               >> Grammar.parseWord Parser.grammarGrammar
               >> Grammar.Type2.LRParser.tree Parser.grammarGrammar
               >> List.exactlyOne
               >> Parser.produceGrammar

    let main argv =
        let result = CommandLine.Parser.Default.ParseArguments<Options>(argv)
        match result with
        | :? Parsed<Options> as parsed ->
            let grammar = grammar parsed.Value.grammar
            let contents =
                parsed.Value.paths
                |> Seq.map File.ReadAllText
                |> Seq.append parsed.Value.words
            let words =
                contents |> Seq.map (Grammar.parseWord grammar)
            printfn "GRAMMAR\n%A" grammar

            for word in words do
                let tree = Grammar.Type2.LRParser.tree grammar word
                let isWordMatch =
                    match tree with
                    | [{Node=n}] when n = grammar.Axiom -> true
                    | _ -> false
                printfn "TREE\n%A" tree
                printfn "INPUT SEQUENCE\n%A\nIS VALID\n%b" (Character.toString word) isWordMatch
        | _ -> ignore()
