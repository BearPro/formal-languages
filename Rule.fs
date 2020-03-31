namespace FormalLanguages

type Rule<'a, 'b> =
    { Right: Character<'a, 'b>
      Left: Character<'a, 'b> list }

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Rule =
    let (-->) a b =
        { Right = a
          Left  = b }
