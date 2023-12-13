open System
open System.IO
open System.Collections.Generic

type Condition = Operational | Damaged | Unknown

type ConditionRecord =
    { Springs: Condition list
      Chunks: int list }

let parseCondition = function
    | '.' -> Operational
    | '#' -> Damaged
    | '?' -> Unknown
    | _ -> failwith "Invalid condition"

let parseRecord (line: string) =
    let [| springs; chunks |] = line.Split(" ")
    { Springs = springs.ToCharArray() |> Array.map parseCondition |> Array.toList 
      Chunks = chunks.Split(",") |> Array.map Int32.Parse |> Array.toList }

let records =
    File.ReadAllLines("input.txt")
    |> Array.map parseRecord

// With some help from https://www.youtube.com/watch?v=g3Ms5e7Jdqo
let countArrangements =
    let cache = Dictionary<_, _>()

    let rec count (springs, chunks) =
        match cache.TryGetValue((springs, chunks)) with
        | true, result ->
            result
        | false, _ ->
            let result =
                match springs, chunks with
                | [], [] -> 1I
                | [], _ -> 0I
                | springs, [] when List.forall ((<>) Damaged) springs -> 1I
                | _, [] -> 0I
                | Operational::springs, chunks ->
                    count (springs, chunks)
                | Damaged::_ as springs, n::chunks ->
                    if List.length springs >= n 
                        && List.take n springs |> List.forall ((<>) Operational) 
                        && (List.length springs = n || springs[n] <> Damaged) 
                    then count ((if List.length springs = n then List.empty else List.skip (n + 1) springs), chunks)
                    else 0I
                | Unknown::springs, chunks ->
                    count (Operational::springs, chunks) + count (Damaged::springs, chunks)
            cache.Add((springs, chunks), result)
            result

    fun record -> count (record.Springs, record.Chunks)

let part1 () =
    records
    |> Array.map countArrangements
    |> Array.sum

printfn "Part 1: %A" (part1 ())

let unfold record =
    { Springs = List.concat (List.replicate 4 (record.Springs @ [ Unknown ])) @ record.Springs
      Chunks = List.concat (List.replicate 5 record.Chunks) }

let tee f x = f x; x

let part2 () =
    records
    |> Array.map (unfold >> countArrangements)
    |> Array.sum

printfn "Part 2: %A" (part2 ())
