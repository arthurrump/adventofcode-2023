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

module List =
    let rec hasPrefix xs ys =
        match xs, ys with
        | x::xs, y::ys -> x = y && hasPrefix xs ys
        | [], _ -> true
        | _::_, [] -> false

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

let rec validArrangements conditionsValidUpto groupsValidCount record =
    match record.SpringConditions |> List.tryFindIndex (fun c -> c = Unknown) with
    | Some unknownIndex ->
        let lastOperationalIndex = 
            record.SpringConditions[..unknownIndex - 1] 
            |> List.tryFindIndexBack (fun c -> c = Operational)
            |> Option.defaultValue 0
        let newDamagedGroups = 
            countDamagedGroups record.SpringConditions[conditionsValidUpto..lastOperationalIndex - 1]
        if record.DamagedGroups |> List.skip groupsValidCount |> List.hasPrefix newDamagedGroups then
            let groupsValidCount = groupsValidCount + (List.length newDamagedGroups)
            seq {
                yield! validArrangements lastOperationalIndex groupsValidCount { record with SpringConditions = record.SpringConditions |> List.updateAt unknownIndex Operational }
                yield! validArrangements lastOperationalIndex groupsValidCount { record with SpringConditions = record.SpringConditions |> List.updateAt unknownIndex Damaged }
            }
        else
            Seq.empty
    | None ->
        if countDamagedGroups record.SpringConditions = record.DamagedGroups
        then Seq.singleton record.SpringConditions
        else Seq.empty

let part1 () =
    records
    |> Array.map (validArrangements 0 0 >> Seq.length)
    |> Array.sum

printfn "Part 1: %A" (part1 ())

let unfold record =
    { SpringConditions = List.concat (List.replicate 4 (record.SpringConditions @ [ Unknown ])) @ record.SpringConditions
      DamagedGroups = List.concat (List.replicate 5 record.DamagedGroups) }

let tee f x = f x; x

let part2 () =
    records
    |> Array.Parallel.map (unfold >> validArrangements 0 0 >> Seq.length >> tee (printfn "%d") >> bigint)
    |> Array.sum

printfn "Part 2: %A" (part2 ())
