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

let knotGraph start dest neighbours =
    let rec walkToKnotOrDest steps previous current =
        if current = dest then
            Some (current, steps)
        else
            match neighbours current |> List.filter ((<>) previous) with
            | [] -> None
            | [ next ] -> walkToKnotOrDest (steps + 1) current next
            | _ -> Some (current, steps)

    let seen = HashSet()
    let graph = Dictionary()
    let queue = Queue([ start ])

    while queue.Count > 0 do
        let current = queue.Dequeue()
        let edges =
            neighbours current 
            |> List.choose (walkToKnotOrDest 1 current)
        
        graph.Add(current, edges)
        assert seen.Add(current)
        for n, _ in edges do
            if not (seen.Contains n || queue.Contains n) then
                queue.Enqueue(n)

    graph

let allPaths start dest neighbours =
    let rec walk dist seen current =
        if current = dest then
            Seq.singleton (dist, seen)
        else    
            let seen = Set.add current seen
            neighbours current
            |> Seq.filter (fun (n, _) -> not (Set.contains n seen))
            |> Seq.collect (fun (n, d) -> walk (dist + d) seen n)
    walk 0 Set.empty start

let printPath neighbours seen =
    printf " "
    for x = 0 to maxX do
        printf "%d" (x % 10)
    printfn ""
    for y = 0 to maxY do
        printf "%d" (y % 10)
        for x = 0 to maxX do
            if trailMap[y, x] <> '#' && neighbours (y, x) |> List.length > 2 then
                Console.BackgroundColor <- ConsoleColor.Red
            elif seen |> Set.contains (y, x) then
                Console.BackgroundColor <- ConsoleColor.Green
            printf "%c" trailMap[y, x]
            Console.ResetColor()
        printfn ""

let part1 () =
    let kng = knotGraph start dest neighbours
    allPaths start dest (fun pos -> kng[pos])
    |> Seq.map fst
    |> Seq.max

printfn "Part 1: %A" (part1 ())

let neighbours2 (y, x) =
    match trailMap[y, x] with
    | '#' -> []
    | _ ->
        [ if y > 0 then (y - 1, x)
          if y < maxY then (y + 1, x)
          if x > 0 then (y, x - 1)
          if x < maxX then (y, x + 1) ]
    |> List.filter (fun (y, x) -> trailMap[y, x] <> '#')

let part2 () =
    let kng = knotGraph start dest neighbours2
    allPaths start dest (fun pos -> kng[pos])
    |> Seq.map fst
    |> Seq.max

printfn "Part 2: %A" (part2 ())
