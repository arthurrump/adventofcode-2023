open System
open System.Collections.Generic
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

let walk map steps start =
    let result = HashSet()
    let seen = HashSet([ start ])
    let frontier = Queue([ (start, steps) ])
    while frontier.Count > 0 do
        let pos, steps = frontier.Dequeue()
        if steps % 2 = 0 then
            result.Add(pos) |> ignore
        if steps <> 0 then
            for next in step map pos do
                if seen.Add(next) then
                    frontier.Enqueue((next, steps - 1))
    result.Count

let start =
    [ for y = 0 to maxY do for x = 0 to maxX do (y, x) ]
    |> List.find (fun (y, x) -> map[y, x] = 'S')

let part1 () =
    walk map 64 start

printfn "Part 1: %A" (part1 ())

// Thanks to https://www.youtube.com/watch?v=9UOMZSL0JTg

assert (maxY = maxX)
assert (fst start = maxY / 2)
assert (snd start = maxX / 2)
assert (map[0, *] |> Array.forall (fun cell -> cell = '.'))
assert (map[maxY, *] |> Array.forall (fun cell -> cell = '.'))
assert (map[*, 0] |> Array.forall (fun cell -> cell = '.'))
assert (map[*, maxX] |> Array.forall (fun cell -> cell = '.'))
assert (map[fst start, *] |> Array.forall (fun cell -> cell = '.' || cell = 'S'))
assert (map[*, snd start] |> Array.forall (fun cell -> cell = '.' || cell = 'S'))

let size = Array2D.length1 map

let part2 () =
    let steps = 26501365
    assert (steps % size = size / 2)

    let tilesWidth = steps / size - 1
    let oddTiles = pown (bigint (tilesWidth / 2 * 2 + 1)) 2
    let evenTiles = pown (bigint ((tilesWidth + 1) / 2 * 2)) 2

    let oddPoints = walk map (size * 2 + 1) start
    let evenPoints = walk map (size * 2) start

    let innerPoints = oddTiles * bigint oddPoints + evenTiles * bigint evenPoints

    let cornerTopPoints = walk map (size - 1) (maxY, snd start)
    let cornerRightPoints = walk map (size - 1) (fst start, 0)
    let cornerBottomPoints = walk map (size - 1) (0, snd start)
    let cornerLeftPoints = walk map (size - 1) (fst start, maxX)

    let centerPoints = 
        bigint cornerTopPoints + bigint cornerRightPoints
        + bigint cornerBottomPoints + bigint cornerLeftPoints

    let smallTiles = tilesWidth + 1
    let smallTopRightPoints = walk map (size / 2 - 1) (maxY, 0)
    let smallBottomRightPoints = walk map (size / 2 - 1) (0, 0)
    let smallTopLeftPoints = walk map (size / 2 - 1) (maxY, maxX)
    let smallBottomLeftPoints = walk map (size / 2 - 1) (0, maxX)

    let smallPoints =
        bigint smallTiles * bigint (smallTopRightPoints + smallBottomRightPoints + smallTopLeftPoints + smallBottomLeftPoints)

    let largeTiles = tilesWidth
    let largeTopRightPoints = walk map ((3 * size) / 2 - 1) (maxY, 0)
    let largeBottomRightPoints = walk map ((3 * size) / 2 - 1) (0, 0)
    let largeTopLeftPoints = walk map ((3 * size) / 2 - 1) (maxY, maxX)
    let largeBottomLeftPoints = walk map ((3 * size) / 2 - 1) (0, maxX)

    let largePoints =
        bigint largeTiles * bigint (largeTopRightPoints + largeBottomRightPoints + largeTopLeftPoints + largeBottomLeftPoints)
    
    innerPoints + centerPoints + smallPoints + largePoints

printfn "Part 2: %A" (part2 ())
