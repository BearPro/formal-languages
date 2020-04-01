namespace FormalLanguages

/// Определяет "правило" в терминах формальных языков.
type Rule<'a, 'b> =
    { Right: Character<'a, 'b>
      Left: Character<'a, 'b> list }

/// Операции над правилами.
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Rule =
    /// Оператор создания правила. Просто удобный синтаксис для определения своих правил.
    let (-->) right left =
        { Right = right
          Left  = left }
