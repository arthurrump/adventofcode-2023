open System
open System.IO

type Race =
    { Time: int64
      Record: int64 }

let races =
    let lines = 
        File.ReadAllLines("input.txt")
        |> Array.map (fun line ->
            line.Substring("Distance:   ".Length)
                .Split(" ", StringSplitOptions.RemoveEmptyEntries + StringSplitOptions.TrimEntries)
            |> Array.map Int32.Parse)
    Seq.zip lines[0] lines[1]
    |> Seq.map (fun (time, record) -> { Time = time; Record = record })
    |> Seq.toList

let distance holdTime raceTime =
    (raceTime - holdTime) * holdTime

let beats holdTime race =
    distance holdTime race.Time > race.Record

let margin race =
    let mutable count = 0
    for holdTime = 0 to int race.Time do
        if beats holdTime race then
            count <- count + 1
    count

let part1 () =
    races |> List.map margin |> List.reduce (*)

printfn "Part 1: %d" (part1 ())

let race =
    let lines =
        File.ReadAllLines("input.txt")
        |> Array.map (String.filter Char.IsDigit >> Int64.Parse)
    { Time = lines[0]; Record = lines[1] }

// (raceTime - holdTime) * holdTime = record
// -holdTime^2 + raceTime * holdTime - record = 0

let quadratic1 a b c =
    (-b + sqrt (pown b 2 - 4. * a * c)) / (2. * a)
let quadratic2 a b c =
    (-b - sqrt (pown b 2 - 4. * a * c)) / (2. * a)

let part2 () =
    let a = -1.
    let b = float race.Time
    let c = float -race.Record

    let q1 = quadratic1 a b c
    let q2 = quadratic2 a b c

    let left = ceil (min q1 q2)
    let right = ceil (max q1 q2)
    int (right - left)

printfn "Part 2: %d" (part2 ())
