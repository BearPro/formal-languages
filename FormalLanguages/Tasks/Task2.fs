namespace FormalLanguages.Task2

open FormalLanguages
open CommandLine

module StateNumber =
    [<Verb("state")>]
    type Options =
      { [<Option('s', "state-machine", MetaValue = "JSON PATH", Required = true, HelpText = "Файл описания машины состояний.")>]
        stateMachine: string 
        [<Value(0, MetaValue="WORD", Min=1, HelpText = "Слова, которые требуется проверить.")>]
        words: string seq }

    type 'state CheckResult =
        | Right of int * 'state
        | NotRight of int * 'state

    let checkWord machine word =
        let next = StateMachine.next machine
        let rec r_check current command count =
            match command with
            | [] -> Right(count, current)
            | first :: other -> 
                match next current first with
                | None -> NotRight (count, current)
                | Some state -> r_check state other (count + 1)
        r_check machine.Start word 0

    let run options =
        let machine = StateMachine.loadFromJson(options.stateMachine)
        let checkWord = checkWord machine
        for word in options.words do
            let charList = word |> Seq.map string |> List.ofSeq
            printfn "%s - %A" word (checkWord charList)
        ()

module Assert =
    [<Verb("assert")>]
    type Options =
      { [<Option('s', "state-machine", MetaValue = "JSON PATH", Required = true, HelpText = "Файл описания машины состояний.")>]
        stateMachine: string 
        [<Value(0, MetaValue="WORD", Min=1, HelpText = "Слова, которые требуется проверить.")>]
        words: string seq }
    
    let check machine word =
        let next = StateMachine.next machine
        let rec r_check current command count =
            match command with
            | [] -> List.contains current machine.Final
            | first :: other -> 
                match next current first with
                | None -> false
                | Some state -> r_check state other (count + 1)
        r_check machine.Start word 0

    let run options =
        let machine = StateMachine.loadFromJson(options.stateMachine)
        let checkWord = check machine
        for word in options.words do
            let charList = word |> Seq.map string |> List.ofSeq
            printfn "%s - %A" word (checkWord charList)
        ()