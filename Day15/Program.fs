open System
open System.IO

let initSeq =
    File.ReadAllText("input.txt")
    |> String.filter (fun ch -> ch <> '\n' && ch <> '\r')
    |> _.Split(",")

let hash: string -> int =
    let folder (hashValue: int) (char: char) =
        ((hashValue + int char) * 17) % 256
    _.ToCharArray() >> Array.fold folder 0

let part1 () =
    initSeq |> Array.sumBy hash

printfn "Part 1: %A" (part1 ())

type Operation =
    | Remove
    | Insert of focalLength: int

type Step =
    { Label: string
      Operation: Operation }

let parseStep step =
    let label = step |> String.filter Char.IsAsciiLetter
    let operation =
        match step[label.Length] with
        | '-' -> Remove
        | '=' -> Insert (step |> String.filter Char.IsAsciiDigit |> Int32.Parse)
    { Label = label; Operation = operation }

let steps = Array.map parseStep initSeq

let execStep boxes step =
    let boxId = hash step.Label
    let box = Map.tryFind boxId boxes |> Option.defaultValue []
    let newBox =
        match step.Operation with
        | Remove ->
            box |> List.filter (fun (label, _) -> label <> step.Label)
        | Insert focalLength ->
            if box |> List.exists (fun (label, _) -> label = step.Label)
            then box |> List.map (fun (l, fl) -> if l = step.Label then (l, focalLength) else (l, fl))
            else box @ [ (step.Label, focalLength) ]
    boxes |> Map.add boxId newBox

let focussingPower boxId =
    List.mapi (fun i (_, focalLength) -> (boxId + 1) * (i + 1) * focalLength)
    >> List.sum

let part2 () =
    steps 
    |> Array.fold execStep Map.empty
    |> Seq.sumBy (fun (KeyValue (boxId, box)) -> focussingPower boxId box)

printfn "Part 2: %A" (part2 ())
