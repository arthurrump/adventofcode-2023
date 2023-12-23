open System
open System.Collections.Generic
open System.IO

let tee f x = f x; x

let trailMap =
    File.ReadAllLines("input.txt")
    |> Array.map _.ToCharArray()
    |> array2D

let maxY = Array2D.length1 trailMap - 1
let maxX = Array2D.length2 trailMap - 1

let startX = [ 0 .. maxX ] |> List.find (fun x -> trailMap[0, x] = '.')
let start = (0, startX)
let destX = [ 0 .. maxX ] |> List.find (fun x -> trailMap[maxY, x] = '.')
let dest = (maxY, destX)

let neighbours (y, x) =
    match trailMap[y, x] with
    | '#' -> []
    | '>' -> [ (y, x + 1) ]
    | '<' -> [ (y, x - 1) ]
    | '^' -> [ (y - 1, x) ]
    | 'v' -> [ (y + 1, x) ]
    | '.' ->
        [ if y > 0 then (y - 1, x)
          if y < maxY then (y + 1, x)
          if x > 0 then (y, x - 1)
          if x < maxX then (y, x + 1) ]
    |> List.filter (fun (y, x) -> trailMap[y, x] <> '#')

let allPaths start dest neighbours =
    let rec walk seen current =
        if current = dest then
            Seq.singleton seen
        else    
            let seen = Set.add current seen
            neighbours current
            |> Seq.filter (fun n -> not (Set.contains n seen))
            |> Seq.collect (fun n -> walk seen n)
    walk Set.empty start

let part1 () =
    allPaths start dest neighbours
    |> Seq.map Set.count
    |> Seq.max

printfn "Part 1: %A" (part1 ())

let neighbours2 (y, x) =
    [ if y > 0 then (y - 1, x)
      if y < maxY then (y + 1, x)
      if x > 0 then (y, x - 1)
      if x < maxX then (y, x + 1) ]
    |> List.filter (fun (y, x) -> trailMap[y, x] <> '#')

// Too slow
let part2 () =
    allPaths start dest neighbours2
    |> Seq.map (Set.count >> tee (printfn "%d"))
    |> Seq.max

printfn "Part 2: %A" (part2 ())
