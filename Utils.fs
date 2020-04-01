namespace FormalLanguages

module Utils =

    /// Возвращает первый индекс, с которого начинается данная подпоследовательность в данной
    /// последовательности.
    let trySubseqIndex subseq seq =
        let subseq = Array.ofSeq subseq
        let length = Seq.length subseq
        seq
        |> Seq.windowed length
        |> Seq.tryFindIndex ((=) subseq)

    let subseqIndex subseq seq =
        trySubseqIndex subseq seq
        |> function
        | Some x -> x
        | None -> failwith "Не найдена такая подпоследовательность."

    /// Возвращает индексы начала всех подпоследовательностей, совпадающих с данной, в данной
    /// последовательности.
    let subseqIndexes subseq seq =
        let subseq = Array.ofSeq subseq
        let length = Seq.length subseq
        seq
        |> Seq.windowed length
        |> Seq.indexed
        |> Seq.where (fun (_, window) -> window = subseq)
        |> Seq.map (fun (index, _) -> index)

    /// Возвращает истину, если данная последовательность содержит данную подпоследовательность.
    let hasSubseq subseq seq =
        let subseq = Array.ofSeq subseq
        let length = Seq.length subseq
        seq
        |> Seq.windowed length
        |> Seq.contains subseq
