namespace FormalLanguages

/// Определяет синтаксическое дерево.
type Syntax<'a> =
    { Node: 'a
      Childs: Syntax<'a> list }

/// Определяет некоторые операции над синтаксическим деревом.
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Syntax =

    /// Возвращает слово, состоящее из верхних символов, записанном в корнях списка
    /// синтаксических деревьев.
    let ofWord syntax =
        syntax
        |> List.map ( fun {Node = s} -> s)

    /// Возвращает список вырожденных деревьев, обладающих нулевой глубиной и корнями,
    /// в соответствии со списком на входе.
    let surfaceWord word =
        word
        |> List.map ( fun c -> { Node = c; Childs = []})

    /// Возвращает поддерево, значение корня которого равно nodeValue.
    /// Если такого поддерева не найдено - возвращает None.
    let rec tryGetNode nodeValue tree =
        match tree with
        | { Node = value} when value = nodeValue -> Some tree
        | { Childs = [] }                        -> None
        | { Childs = childs }                    -> childs
                                                 |> List.tryPick (tryGetNode nodeValue)

    /// Возвращает поддерево, значение корня которого равно nodeValue.
    /// Корень дерева должен быть расположен не глубже чем на глубине maxDepth.
    /// Если такого поддерева не найдено - возвращает None.
    let rec tryGetNodeLim nodeValue maxDepth tree =
        if maxDepth < 0
        then None
        else match tree with
             | { Node = value} when value = nodeValue -> Some tree
             | { Childs = [] } -> None
             | { Childs = childs } -> childs
                                   |> List.tryPick (tryGetNodeLim nodeValue (maxDepth - 1))

    /// Возвращает поддерево, значение корня которого равно nodeValue.
    /// Если такого поддерева не найдено - выбрасывает исключение.
    let getNode nodeValue tree =
        tree
        |> tryGetNode nodeValue
        |> function
        | Some value -> value
        | None -> failwith (sprintf "Не найдено поддерева %A" nodeValue)

    /// Возвращает все узлы дерева, в значениях которых находится nodeValue и которые не имеют
    /// дочерних узлов со значением nodeValue.
    let rec getLeafNodes nodeValue tree =
        tree
        |> function
        | { Node = node; Childs = []}      when node <> nodeValue -> []
        | { Node = node; Childs = childs } when node <> nodeValue ->
            childs
            |> List.collect (getLeafNodes nodeValue)
        | { Node = nodeValue; Childs = childs } ->
            childs
            |> List.collect (getLeafNodes nodeValue)
            |> function
            | [] -> [ tree ]
            | list -> list

    /// Возвращает поддерево, значение корня которого равно nodeValue.
    /// Корень дерева должен быть расположен не глубже чем на глубине maxDepth.
    /// Если такого поддерева не найдено - выбрасывает исключение.
    let getNodeLim nodeValue maxDepth tree =
        tree
        |> tryGetNodeLim nodeValue maxDepth
        |> function
        | Some value -> value
        | None -> failwith "No such value in tree"
