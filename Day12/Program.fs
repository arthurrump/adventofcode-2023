open System
open System.IO

type Condition = Operational | Damaged | Unknown

type ConditionRecord =
    { SpringConditions: Condition list
      DamagedGroups: int list }

let parseCondition = function
    | '.' -> Operational
    | '#' -> Damaged
    | '?' -> Unknown
    | _ -> failwith "Invalid condition"

let parseRecord (line: string) =
    let [| conditions; groups |] = line.Split(" ")
    { SpringConditions = conditions.ToCharArray() |> Array.map parseCondition |> Array.toList 
      DamagedGroups = groups.Split(",") |> Array.map Int32.Parse |> Array.toList }

let records =
    File.ReadAllLines("input.txt")
    |> Array.map parseRecord

let countDamagedGroups conditions =
    let groups =
        List.fold (fun (group::groups) condition ->
            match condition with
            | Operational | Unknown when group = 0 ->
                group::groups
            | Operational | Unknown ->
                0::group::groups
            | Damaged -> 
                (group + 1)::groups
        ) [ 0 ] conditions
    match groups with
    | 0::rest -> List.rev rest
    | _ -> List.rev groups

let rec allArrangements conditions =
    match conditions |> List.tryFindIndex (fun c -> c = Unknown) with
    | Some i ->
        seq {
            yield! allArrangements (conditions |> List.updateAt i Operational)
            yield! allArrangements (conditions |> List.updateAt i Damaged)
        }
    | None ->
        Seq.singleton conditions

let validArrangements record =
    allArrangements record.SpringConditions
    |> Seq.filter (fun conditions -> countDamagedGroups conditions = record.DamagedGroups)

let part1 () =
    records
    |> Array.map (validArrangements >> Seq.length)
    |> Array.sum

printfn "Part 1: %A" (part1 ())
