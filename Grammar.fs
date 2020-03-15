namespace FormalLanguages

open System

module Grammar =
    type 'a Alphabet = 'a list

    type Character<'a, 'b> =
        | Terminal of 'a
        | Nonterminal of 'b
        | Empty

    type Word<'a, 'b> = Character<'a, 'b>

    type Rule<'a, 'b> =
        { Before: Character<'a, 'b>
          After: Character<'a, 'b> list }

    type Grammar<'a, 'b> =
        { Terminals:    'a Alphabet
          Nonterminals: 'b Alphabet
          Rules:        Rule<'a, 'b> list
          Axiom:        Character<'a, 'b> }

    let (-->) a b =
        { Before = a
          After  = b }

    module Type2 =
        let unapply word rule =
            let index =
                word
                |> List.windowed (rule.After.Length)
                |> List.findIndex (fun x -> x = rule.After)
            List.take index word @ rule.Before :: List.skip (index+rule.After.Length) word

        let rec origins word grammar =
            let applicableRules =
                grammar.Rules
                |> List.filter (fun { Before = b; After = a } ->
                    word
                    |> List.windowed (a.Length)
                    |> List.contains a)
            match applicableRules with
            | [] -> [ word ]
            | rules -> rules
                       |> List.collect (unapply word
                       >> fun (word) -> origins word grammar )



        let word sequence =
            sequence |> Seq.map Terminal |> List.ofSeq

        let check word grammar =
            origins word grammar |> List.contains [grammar.Axiom]

    let grammarSample =
        { Terminals = [ 'c'; 'd' ]
          Nonterminals = [ 'A'; 'T' ]
          Rules =
              [ Nonterminal 'A' --> [ Terminal 'c'; Terminal 'c'; Terminal 'd' ]
                Nonterminal 'A' --> [ Nonterminal 'T' ]
                Nonterminal 'T' --> [ Terminal 'c'; Terminal 'd' ]
                Nonterminal 'A' --> [ Terminal 'c'; Terminal 'c'; Nonterminal 'A'; Terminal 'd' ] ]
          Axiom = Nonterminal 'A' }
