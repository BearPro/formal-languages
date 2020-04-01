namespace FormalLanguages.Grammar

open System.IO
open FormalLanguages
open FormalLanguages.Character
open FormalLanguages.Rule
open FormalLanguages.Syntax


/// Определения операций, необходимых для разбора описаний формальных грамматик.
module Parser =
    let private lowercaseLiterals =
        [ for i in 97..122 -> (string<<char) i ]
    let private uppercaseLietrals =
        [ for i in 65..90 -> (string<<char) i ]
    let private numberLiterals =
        [ for i in 48..57 -> (string<<char) i ]
    let private whitepsaces =
        [ " "; "\n"; "\t"; "\r"; "\n\r" ]

    /// Грамматика, которая описывает формальный язык описания грамматик.
    let grammar =
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
              // Пробельные символы
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
              // Последовательность терминальных символов
              n "TermSeq"     --> [ n "SingleTerm"; n "SingleTerm"; ]
              n "TermSeq"     --> [ n "TermSeq"; n "SingleTerm"; ]
            ] }

    /// Находит определение аксиомы в указанном синтаксимческом дереве, возвращает её.
    let parseAxiom tree =
        tree
        |> getNode (n "Axiom")
        |> getNode (n "Nonterm")
        |> fun x -> x.Childs.Head.Node
        |> function
        | Terminal x -> Nonterminal x
        | _ -> failwith "Не удалось найти аксиому."

    /// Находит определение терминалов в указанном синтаксическом дереве, возвращает их.
    let rec parseTerminals tree =
        let char =
            tree
            |> getNode (n "Terminals")
            |> tryGetNodeLim (n "SingleTerm") 1
            |> function
            | None -> None
            | Some x -> Some x.Childs.Head.Node
            |> function
            | None -> None
            | Some(Terminal x) -> Some(x)
            | _ -> failwith "Не удалось найти терминальный символ."

        let next =
            tree
            |> getNode (n "Terminals")
            |> fun { Childs = childs } -> childs
            |> List.tryPick (tryGetNodeLim (n "Terminals") 0)

        match char, next with
        | None        , _         -> []
        | Some last   , None      -> [last]
        | Some currnet, Some next -> currnet :: parseTerminals next

    /// Находит определение нетерминалов в указанном синтаксическом дереве, возвращает их.
    let rec parseNonterminals tree =
        let char =
            tree
            |> getNode (n "Nonterminals")
            |> tryGetNodeLim (n "Nonterm") 1
            |> function
            | None -> None
            | Some x -> Some x.Childs.Head.Node
            |> function
            | None -> None
            | Some(Terminal x) -> Some(x)
            | _ -> failwith "Не удалось найти нетерминальный символ."

        let next =
            tree
            |> getNode (n "Nonterminals")
            |> fun { Childs = childs } -> childs
            |> List.tryPick (tryGetNodeLim (n "Nonterminals") 0)

        match char, next with
        | None        , _         -> []
        | Some last   , None      -> [last]
        | Some currnet, Some next -> currnet :: parseNonterminals next

    /// Находит определение правил в указанном синтаксическом дереве, возвращает их.
    let rec parseRules tree =
        let parseRuleDef tree =
            let parseTerm tree =
                match tree.Childs.[0].Node with
                | Terminal x -> x
                | _ -> failwith "Не удалось получить значение."

            let parseTermSeq tree =
                failwith "Разбор последовательности терминальных символов не реализован."

            let rec parseRuleRight (tree : Syntax<Character<string, string>>) =
                tree.Childs
                |> List.map (fun x -> x.Node)
                |> function
                | [ Nonterminal "RuleRight"; Nonterminal "RuleRight"; ]  ->
                    parseRuleRight tree.Childs.[0] @ parseRuleRight tree.Childs.[1]
                | [ Nonterminal "RuleRight"; Nonterminal "SingleTerm"; ] ->
                    parseRuleRight tree.Childs.[0] @ [ tree.Childs.[1] |> parseTerm |> Terminal ]
                | [ Nonterminal "RuleRight"; Nonterminal "TermSeq"; ]    ->
                    parseRuleRight tree.Childs.[0] @ [ parseTermSeq tree.Childs.[1] ]
                | [ Nonterminal "RuleRight"; Nonterminal "Nonterm"; ]    ->
                    parseRuleRight tree.Childs.[0] @ [ tree.Childs.[1] |> parseTerm |> Nonterminal ]
                | [ Nonterminal "Nonterm" ]    ->
                    [ tree.Childs.[0] |> parseTerm |> Nonterminal ]
                | [ Nonterminal "SingleTerm" ] ->
                    [ tree.Childs.[0] |> parseTerm |> Terminal ]
                | [ Nonterminal "TermSeq" ]    ->
                    [ parseTermSeq tree.Childs.[0] ]
                | _ -> failwith "Не удалось разобрать правую часть правила."

            let left =  tree
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

        tree
        |> getNode (n "Rules")
        |> getLeafNodes (n "RuleDef")
        |> List.map parseRuleDef

    /// Строит грамматику на основе указанного синтаксического дерева.
    let produceGrammar tree =
        let i = parseAxiom tree
        let v = parseTerminals tree |> List.rev
        let w = parseNonterminals tree |> List.rev
        let p = parseRules tree
        { Axiom        = i
          Rules        = p
          Terminals    = v
          Nonterminals = w }

    /// Возвращает грамматику, определённую в данном файле.
    let parse = File.ReadAllText
               >> Grammar.parseWord grammar
               >> Syntax.LRParser.tree grammar
               >> List.exactlyOne
               >> produceGrammar
