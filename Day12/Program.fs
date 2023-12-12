open System
open System.Collections.Generic
open System.IO

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
    let [| conditions; chunks |] = line.Split(" ")
    { Springs = conditions.ToCharArray() |> Array.map parseCondition |> Array.toList 
      Chunks = chunks.Split(",") |> Array.map Int32.Parse |> Array.toList }

let records =
    File.ReadAllLines("example.txt")
    |> Array.map parseRecord


// Based on https://www.youtube.com/watch?v=GVcER1GavNQ
let count =
    let cache = new Dictionary<_, _>()

    let rec count = function
        // There are no more springs and we covered all chunks
        | [], [], _
        // There are no more springs and the final chunk zeroed out
        | [], [ 0 ], _ ->
            // So we're done and this is valid
            1

        // There are no more springs, but there are still one or more chunks left
        | [], _, _ ->
            // Then this is invalid
            0

        // There are no more chunks and no Damaged springs left
        | springs, [], _ when springs |> List.forall (fun s -> s <> Damaged) ->
            // Then this is valid
            1
        // No more chunks, but some of the next chunks are Damaged
        | _, [], _ ->
            // Then this is invalid
            0
        
        // The next is operational and the chunk we are in is finished
        | Operational::springs, 0::chunks, true
        // The next is operational and we are not in a chunk
        | Operational::springs, chunks, false ->
            // Then go to the next
            countMemo (springs, chunks, false)
        // The next is operational, but we are in a non-zeroed-out chunk
        | Operational::_, _, true ->
            // Then this is not a valid option
            0
        
        // The next chunk is zero while we are not in a chunk
        | _, 0::_, false ->
            // That should not be possible
            failwith "Impossible case"

        // The next is damaged, we are in a chunk, but the current chunk zeroed out
        | Damaged::_, 0::_, true ->
            // That's not valid, because the current chunk is larger
            0
        // The next is damaged, the current chunk has room to play
        | Damaged::springs, n::chunks, _ ->
            // Then go to the next
            countMemo (springs, n-1::chunks, true)

        // The next is unkown and we are in a zeroed out chunk
        | Unknown::springs, 0::chunks, true ->
            // Then this should be Operational and we can just count on as such
            countMemo (springs, chunks, false)
        // The next spring is unknown and we are in a chunk that is not yet 0
        | Unknown::springs, n::chunks, true ->
            // Then this should be Damaged and we can just count on as such
            countMemo (springs, n-1::chunks, true)
        // The next spring is unknown, and we could start a new chunk
        | Unknown::springs, n::chunks, false ->
            // Then we may choose this as Operational
            countMemo (springs, n::chunks, false)
            // or as Damaged
            + countMemo (springs, n-1::chunks, true)

    and countMemo (springs, chunks, inChunk) = 
        match cache.TryGetValue((springs, chunks, inChunk)) with
        | true, res -> 
            res
        | false, _ ->
            let result = countMemo (springs, chunks, inChunk)
            cache.Add((springs, chunks, inChunk), result)
            result
                
    fun record -> countMemo (record.Springs, record.Chunks, false)

let part1 () =
    records
    |> Array.map count
    |> Array.sum

printfn "Part 1: %A" (part1 ())

let unfold record =
    { Springs = List.concat (List.replicate 4 (record.Springs @ [ Unknown ])) @ record.Springs
      Chunks = List.concat (List.replicate 5 record.Chunks) }

let part2 () =
    records
    |> Array.map (unfold >> count >> bigint)
    |> Array.sum

printfn "Part 2: %A" (part2 ())
