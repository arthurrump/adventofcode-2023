open System
open System.IO

let map =
    File.ReadAllLines("input.txt")
    |> Array.map _.ToCharArray()
    |> array2D

let maxY = Array2D.length1 map - 1
let maxX = Array2D.length2 map - 1

let step (map: char[,]) (y, x) =
    [ if y > 0 then (y - 1, x)
      if x > 0 then (y, x - 1)
      if y < maxY then (y + 1, x)
      if x < maxX then (y, x + 1) ]
    |> List.filter (fun (y, x) -> map[y, x] <> '#')
    |> Set.ofList

let rec walk map steps positions =
    if steps <= 0 then
        positions
    else
        positions 
        |> Seq.map (step map)
        |> Set.unionMany
        |> walk map (steps - 1)

let start =
    [ for y = 0 to maxY do for x = 0 to maxX do (y, x) ]
    |> List.find (fun (y, x) -> map[y, x] = 'S')

let part1 () =
    walk map 64 (set [ start ])
    |> Set.count

printfn "Part 1: %A" (part1 ())
