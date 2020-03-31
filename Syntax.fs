namespace FormalLanguages

/// Определяет синтаксическое дерево.
type Syntax<'a> =
    { Node: 'a
      Childs: Syntax<'a> list }

/// Определяет некоторые операции над синтаксическим деревом.
[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module Syntax =

    /// Возвращает поддерево, значение корня которого равно nodeValue.
    /// Если такого поддерева не найдено - возвращает None.
    let rec tryGetNode nodeValue tree =
        match tree with
        | { Node = value} when value = nodeValue -> Some tree
        | { Childs = [] } -> None
        | { Childs = childs } -> childs |> List.tryPick (tryGetNode nodeValue)

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
        tryGetNode nodeValue tree
        |> function
        | Some value -> value
        | None -> failwith (sprintf "Не найдено поддерева %A" nodeValue)

    /// Возвращает все узлы дерева, в значениях которых находится nodeValue и которые не имеют
    /// дочерних узлов со значением nodeValue.
    let rec getLeafNodes nodeValue tree =
        match tree with
        | { Node = node; Childs = []} when node <> nodeValue -> []
        | { Node = node; Childs = childs} when node = nodeValue ->
            let validChilds =
                childs
                |> List.collect (getLeafNodes nodeValue)
            match validChilds with
            | [] -> [ tree ]
            | some -> validChilds
        | { Childs = childs } -> childs |> List.collect (getLeafNodes nodeValue)

    /// Возвращает поддерево, значение корня которого равно nodeValue.
    /// Корень дерева должен быть расположен не глубже чем на глубине maxDepth.
    /// Если такого поддерева не найдено - выбрасывает исключение.
    let getNodeLim nodeValue maxDepth tree =
        tryGetNodeLim nodeValue maxDepth tree
        |> function
        | Some value -> value
        | None -> failwith "No such value in tree"
