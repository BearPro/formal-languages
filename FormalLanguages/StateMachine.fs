namespace FormalLanguages

open System.IO
open System.Text.Json

/// Определение машины состояний
type ('t, 'v) StateMachine =
  { Alphabet: 'v list
    States: 't seq
    Start: 't
    Final: 't list
    Transitions: ('t * 'v * 't) list }

[<CompilationRepresentationAttribute(CompilationRepresentationFlags.ModuleSuffix)>]
module StateMachine =
    let loadFromJson path =
        let document = JsonDocument.Parse(File.ReadAllText(path))
        let dict =
            document.RootElement.EnumerateObject()
            |> Seq.map (fun x -> x.Name, x.Value)
            |> dict
        let alphabet =
            dict.["alphabet"].EnumerateArray() 
            |> Seq.map(fun x -> x.GetString()) 
            |> List.ofSeq
        let states =
            dict.["states"].EnumerateArray() 
            |> Seq.map(fun x -> x.GetString()) 
            |> List.ofSeq
        let start = dict.["start"].GetString()
        let endStates =
            dict.["end_states"].EnumerateArray() 
            |> Seq.map(fun x -> x.GetString()) 
            |> List.ofSeq
        let transitions =
            dict.["transitions"].EnumerateArray() 
            |> Seq.map(fun x -> x.EnumerateArray() 
                             |> Seq.map(fun x -> x.GetString())
                             |> List.ofSeq
                             |> fun x -> x.[0], x.[1], x.[2]) 
            |> List.ofSeq
        { Alphabet = alphabet
          States = states
          Start = start
          Final = endStates
          Transitions = transitions}

    /// Возвращает новое состояние недетерминированного автомата.
    let next { Transitions = transitions } currentState command =
        transitions
        |> List.tryFind (fun (s, c, _) -> s = currentState && c = command)
        |> Option.map (fun (_, _, n) -> n)