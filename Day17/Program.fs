open System
open System.Collections.Generic
open System.IO

let city =
    File.ReadAllLines("input.txt")
    |> Array.map (_.ToCharArray() >> Array.map (fun ch -> int ch - int '0'))
    |> array2D

let maxY = Array2D.length1 city - 1
let maxX = Array2D.length2 city - 1

type Direction = North | East | South | West

module Direction =
    let turns = function
        | North | South -> [ East; West ]
        | East | West -> [ North; South ]

    let straightOrTurns dir = 
        dir :: turns dir

    let move (y, x) = function
        | North -> (y - 1, x)
        | East -> (y, x + 1)
        | South -> (y + 1, x)
        | West -> (y, x - 1)

type State =
    { Location: int * int
      Direction: Direction
      StraightMoves: int }

let neighbours state =
    let newDirections =
        if state.StraightMoves < 3
        then Direction.straightOrTurns state.Direction
        else Direction.turns state.Direction
    [ for dir in newDirections do
        let (y, x) = Direction.move state.Location dir
        if 0 <= y && y <= maxY && 0 <= x && x <= maxX then
            { Location = (y, x)
              Direction = dir
              StraightMoves = if dir = state.Direction then state.StraightMoves + 1 else 1 } ]

let dijkstra starts isDest neighbours cost =
    let unvisited = PriorityQueue<'n, int>()
    let distances = Dictionary<'n, int>()
    let paths = Dictionary<'n, 'n list>()
    let mutable current = starts |> List.head
    for start in starts do
        distances.Add(start, 0)
        unvisited.Enqueue(start, 0)
        paths.Add(start, [])
    while not (isDest current) do
        current <- unvisited.Dequeue()
        let dist = distances[current]
        for neighbour in neighbours current do
            let dist = dist + cost neighbour
            match distances.TryGetValue(neighbour) with
            | true, d ->
                if dist < d then
                    distances[neighbour] <- dist
                    paths[neighbour] <- current::paths[current]
                    unvisited.Enqueue(neighbour, dist)
            | false, _ ->
                distances[neighbour] <- dist
                paths[neighbour] <- current::paths[current]
                unvisited.Enqueue(neighbour, dist)
                
    distances[current], List.rev (current::paths[current])

let printPath path =
    let pathMap = 
        path 
        |> List.map (fun state -> state.Location, state.Direction) 
        |> Map.ofList
    
    let colors = [| ConsoleColor.White; ConsoleColor.Yellow; ConsoleColor.DarkYellow; ConsoleColor.Magenta; ConsoleColor.Red |]
    for y = 0 to maxY do
        for x = 0 to maxX do
            let v = city[y, x]
            let color = colors[v / 2]
            match Map.tryFind (y, x) pathMap with
            | None -> 
                Console.ForegroundColor <- color
                printf "%d" v
            | Some dir ->
                Console.BackgroundColor <- color
                Console.ForegroundColor <- ConsoleColor.Black
                match dir with
                | North -> '^'
                | East -> '>'
                | West -> '<'
                | South -> 'v'
                |> printf "%c"
            Console.ResetColor()
        printfn ""

let part1 () =
    let heatLoss, path = 
        dijkstra 
            [ for dir in [ East; South ] -> { Location = (0, 0); Direction = dir; StraightMoves = 1 } ]
            (fun state -> state.Location = (maxY, maxX))
            neighbours
            (fun state -> let (y, x) = state.Location in city[y, x])

    printPath path

    heatLoss

printfn "Part 1: %A" (part1 ())

let ultraNeighbours state =
    let newDirections =
        if state.StraightMoves < 4
        then [ state.Direction ]
        elif state.StraightMoves < 10
        then Direction.straightOrTurns state.Direction
        else Direction.turns state.Direction
    [ for dir in newDirections do
        let (y, x) = Direction.move state.Location dir
        if 0 <= y && y <= maxY && 0 <= x && x <= maxX then
            { Location = (y, x)
              Direction = dir
              StraightMoves = if dir = state.Direction then state.StraightMoves + 1 else 1 } ]

let part2 () =
    let heatloss, path =
        dijkstra
            [ for dir in [ East; South ] -> { Location = (0, 0); Direction = dir; StraightMoves = 1 } ]
            (fun state -> state.Location = (maxY, maxX) && state.StraightMoves >= 4)
            ultraNeighbours
            (fun state -> let (y, x) = state.Location in city[y, x])
    
    printPath path

    heatloss

printfn "Part 2: %A" (part2 ())
