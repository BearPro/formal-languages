module Tests

open System
open Xunit
open FormalLanguages
open FormalLanguages.Character
open FormalLanguages.Grammar
open FormalLanguages.Rule

let rawGrammar = @"!I = X;
                   !W = X;
                   !V = c, d;
                   !P = X->ccd,
                        X->ccXd;"

let parsedGrammar = { Axiom        = n "X"
                      Nonterminals = [ "X" ]
                      Terminals    = [ "c"; "d" ]
                      Rules        = [ n "X" --> [ t "c"; t "c"; t "d" ]
                                       n "X" --> [ t "c"; t "c"; n "X"; t "d" ] ] }

let emptyGrammar : Grammar<string, string> =
    { Axiom          = n ""
      Nonterminals = [ ]
      Terminals    = [ ]
      Rules        = [ ] }

[<Fact>]
let ``Разбор грамматики выполняется без исключений.`` () =
    let grammar = FormalLanguages.Grammar.Parser.parse rawGrammar
    ()

[<Fact>]
let ``Грамматика разбирается корректно.`` () =
    let grammar = FormalLanguages.Grammar.Parser.parse rawGrammar
    Assert.Equal(grammar, parsedGrammar)
    ()

[<Fact>]
let ``Проверка вхождения слова в порождаемый язык работает.`` () =
    let tree1 = Syntax.LRParser.tree parsedGrammar [ t "c"; t"c"; t "d" ]
    let tree2 = Syntax.LRParser.tree parsedGrammar [ t "c"; t"c"; t "c"; t"c"; t "d"; t "d" ]
    let tree3 = Syntax.LRParser.tree parsedGrammar [ t "c"; t"c"; t "c"; t "d"; t "d" ]
    Assert.True(Task1.checkTree parsedGrammar tree1)
    Assert.True(Task1.checkTree parsedGrammar tree2)
    Assert.False(Task1.checkTree parsedGrammar tree3)
    ()

