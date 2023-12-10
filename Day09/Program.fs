open System
open System.IO

let readings =
    File.ReadAllLines("input.txt")
    |> Array.map (fun line -> line.Split(" ") |> Array.map Int32.Parse)

let differences =
    Array.pairwise >> Array.map (fun (left, right) -> right - left)

let rec extrapolate arr =
    let diff = differences arr
    if Array.forall (fun d -> d = 0) diff
    then Array.last arr
    else Array.last arr + (extrapolate diff)

let part1 () =
    readings
    |> Array.map extrapolate
    |> Array.sum

printfn "Part 1: %d" (part1 ())

let part2 () =
    readings
    |> Array.map (Array.rev >> extrapolate)
    |> Array.sum

printfn "Part 2: %d" (part2 ())
