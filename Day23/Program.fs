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

// Opposite of Dijkstra's: longest simple path
let artskjid start dest neighbours =
    let unvisited = PriorityQueue<'n, int>({ new IComparer<int> with member _.Compare(a, b) = -1 * compare a b })
    let distances = Dictionary<'n, int>()
    let seen = Dictionary<'n, Set<'n>>()
    let mutable current = start
    distances.Add(start, 0)
    unvisited.Enqueue(start, 0)
    seen.Add(start, Set.empty)
    while unvisited.Count > 0 do
        current <- unvisited.Dequeue()
        let dist' = distances[current] + 1
        let seen' = Set.add current seen[current]
        let neighbours = 
            neighbours current
            |> List.filter (fun n -> not (Set.contains n seen'))
        for neighbour in neighbours do
            match distances.TryGetValue neighbour with
            | true, d ->
                if dist' > d then
                    distances[neighbour] <- dist'
                    seen[neighbour] <- seen'
                    unvisited.Enqueue(neighbour, dist')
            | false, _ ->
                distances[neighbour] <- dist'
                seen[neighbour] <- seen'
                unvisited.Enqueue(neighbour, dist')
    distances[dest], Set.add dest seen[dest]

let printPath neighbours seen =
    for y = 0 to maxY do
        for x = 0 to maxX do
            if trailMap[y, x] <> '#' && neighbours (y, x) |> List.length > 2 then
                Console.BackgroundColor <- ConsoleColor.Red
            elif seen |> Set.contains (y, x) then
                Console.BackgroundColor <- ConsoleColor.Green

            printf "%c" trailMap[y, x]

            Console.ResetColor()
        printfn ""

let part1 () =
    artskjid start dest neighbours
    |> tee (snd >> printPath neighbours)
    |> fst

printfn "Part 1: %A" (part1 ())

let neighbours2 (y, x) =
    [ if y > 0 then (y - 1, x)
      if y < maxY then (y + 1, x)
      if x > 0 then (y, x - 1)
      if x < maxX then (y, x + 1) ]
    |> List.filter (fun (y, x) -> trailMap[y, x] <> '#')

let part2 () =
    artskjid start dest neighbours2
    |> tee (snd >> printPath neighbours2)
    |> fst

printfn "Part 2: %A" (part2 ())
