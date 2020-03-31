namespace FormalLanguages

module Utils =

    /// Возвращает первый индекс, с которого начинается данная подпоследовательность в данной
    /// последовательсности.
    let subseqIndex subseq seq =
        let subseq = Array.ofSeq subseq
        seq
        |> Seq.windowed (subseq |> Seq.length)
        |> Seq.tryFindIndex (fun x -> x = subseq)

    let subseqIndexes subseq seq =
        let subseq = Array.ofSeq subseq
        seq
        |> Seq.windowed (subseq |> Seq.length)
        |> Seq.indexed
        |> Seq.where (fun (i, x) -> x = subseq)
        |> Seq.map (fun (i, _) -> i)

    /// Возвращает истину, если данная последовательность содержит данную подпоследовательность.
    let hasSubseq subseq seq =
        let subseq = Array.ofSeq subseq
        seq
        |> Seq.windowed (subseq |> Seq.length)
        |> Seq.contains subseq
