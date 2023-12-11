open System
open System.IO

let image =
    File.ReadAllLines("input.txt")
    |> Array.map _.ToCharArray()
    |> array2D

let maxY = Array2D.length1 image - 1
let maxX = Array2D.length2 image - 1

let galaxies =
    [| for y = 0 to maxY do
        for x = 0 to maxX do
            if image[y, x] = '#' then
                (bigint y, bigint x) |]

let emptyRows =
    [ for y = 0 to maxY do
        if image[y, *] |> Array.forall (fun ch -> ch = '.') then
            bigint y ]
    |> Set.ofList

let emptyColumns =
    [ for x = 0 to maxX do
        if image[*, x] |> Array.forall (fun ch -> ch = '.') then
            bigint x ]
    |> Set.ofList

let expandCoordinate expansionFactor expandRows expandColumns (y, x) =
    let y = y + bigint (expandRows |> Set.filter (fun row -> row < y) |> Set.count) * (expansionFactor - 1I)
    let x = x + bigint (expandColumns |> Set.filter (fun col -> col < x) |> Set.count) * (expansionFactor - 1I)
    (y, x)

let part1 () =
    let expanded = galaxies |> Array.map (expandCoordinate 2I emptyRows emptyColumns)
    [ for i = 0 to Array.length expanded - 1 do
        for j = i + 1 to Array.length expanded - 1 do
            let (iy, ix) = expanded[i]
            let (jy, jx) = expanded[j]
            abs (iy - jy) + abs (ix - jx) ]
    |> List.sum

printfn "Part 1: %O" (part1 ())

let part2 () =
    let expanded = galaxies |> Array.map (expandCoordinate 1_000_000I emptyRows emptyColumns)
    [ for i = 0 to Array.length expanded - 1 do
        for j = i + 1 to Array.length expanded - 1 do
            let (iy, ix) = expanded[i]
            let (jy, jx) = expanded[j]
            abs (iy - jy) + abs (ix - jx) ]
    |> List.sum

printfn "Part 2: %O" (part2 ())
