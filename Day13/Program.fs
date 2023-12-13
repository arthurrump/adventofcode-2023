open System
open System.IO

module Array =
    let split predicate arr =
        Seq.indexed arr
        |> Seq.filter (snd >> predicate)
        |> Seq.map fst
        |> Seq.insertAt 0 -1
        |> fun seq -> Seq.append seq [ Array.length arr ]
        |> Seq.pairwise
        |> Seq.filter (fun (from, to') -> from + 1 < to' - 1)
        |> Seq.map (fun (from, to') -> arr[from + 1 .. to' - 1])
        |> Seq.toArray

    let dist arr1 arr2 =
        Array.zip arr1 arr2
        |> Array.filter (fun (a1, a2) -> a1 <> a2)
        |> Array.length

let patterns =
    File.ReadAllLines("input.txt")
    |> Array.map _.ToCharArray()
    |> Array.split Array.isEmpty

let horizontalReflectionDist (grid: 'a array array) index =
    Seq.init (index + 1) id
    |> Seq.sumBy (fun n ->
        let top = index - n
        let bottom = index + n + 1
        if bottom < Array.length grid 
        then Array.dist grid[top] grid[bottom]
        else 0
    )

let findHorizontalReflectionWithDist dist (grid: 'a array array) =
    Seq.init (Array.length grid - 1) id
    |> Seq.tryFind (fun index -> horizontalReflectionDist grid index = dist)
    |> Option.map (fun index -> index + 1)

let findVerticalReflectionWithDist dist (grid: 'a array array) =
    Array.transpose grid
    |> findHorizontalReflectionWithDist dist

let summarizeReflectionWithDist dist (grid: 'a array array) =
    findVerticalReflectionWithDist dist grid
    |> Option.orElseWith (fun () -> 
        findHorizontalReflectionWithDist dist grid
        |> Option.map (fun res -> res * 100)
    )

let part1 () =
    patterns
    |> Array.choose (summarizeReflectionWithDist 0)
    |> Array.sum

printfn "Part 1: %A" (part1 ())

let part2 () =
    patterns
    |> Array.choose (summarizeReflectionWithDist 1)
    |> Array.sum

printfn "Part 2: %A" (part2 ())
