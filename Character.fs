namespace FormalLanguages

type Character<'a, 'b> =
    | Terminal of 'a
    | Nonterminal of 'b
    | Empty

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Character =
    let t item = Terminal item
    let n item = Nonterminal item

    let toString characters =
        characters
        |> List.map (function
                        | Terminal x    -> x.ToString ()
                        | Nonterminal x -> x.ToString ()
                        | Empty         -> "ε" )
        |> List.reduce (+)
