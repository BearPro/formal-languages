namespace FormalLanguages

/// Определяет "символ" в терминах формальных языков.
type Character<'a, 'b> =
    | Terminal of 'a
    | Nonterminal of 'b
    | Empty

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
/// Операции над символами.
module Character =
    /// Возвращает терминальный символ с данным значением.
    let t item = Terminal item
    /// Возвращает нетерминальный символ с данным значением.
    let n item = Nonterminal item

    /// Возвращает строковое представление последовательности символов.
    let toString characters =
        characters
        |> Seq.map (function
                        | Terminal x    -> x.ToString ()
                        | Nonterminal x -> x.ToString ()
                        | Empty         -> "ε" )
        |> Seq.reduce (+)
