namespace FormalLanguages

open System
open System.Linq
open FormalLanguages.Utils
open Character
open Rule

/// Алфавит - алиас для списка.
type 'a Alphabet = 'a list

/// Определяет грамматику в терминах формальных языков.
type Grammar<'a, 'b> =
    { Terminals:    'a Alphabet
      Nonterminals: 'b Alphabet
      Rules:        Rule<'a, 'b> list
      Axiom:        Character<'a, 'b> }

/// Операции над грамматиками.
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Grammar =
    /// На основе строки возвращает слово, состоящее из терминальных символов указанной
    /// грамматики.
    let rec parseWord grammar word  =
        match Seq.length word with
        | 0 -> []
        | _ -> grammar.Terminals
            |> Seq.find (fun term -> (trySubseqIndex term word) = Some 0)
            |> fun term -> Terminal term
                        :: parseWord grammar (Seq.skip (Seq.length term) word)
