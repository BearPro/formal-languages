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
    let rec parseWord grammar word =
        let firstTerm word terms : (string * int) =
            let applicable term =
                subseqIndex term word
                |> function
                | Some 0 -> true
                | _ -> false

            terms
            |> Seq.tryFind applicable
            |> function
            | Some term -> term, Option.get <| subseqIndex term word
            | None -> failwith (sprintf
                                    "Не удалось распознать символ начиная с \n%s"
                                    (String.Concat word) )
        if Seq.isEmpty word
        then []
        else match firstTerm word grammar.Terminals with
             | (term, index) -> t term :: parseWord grammar (Seq.skip (index + term.Length) word)
