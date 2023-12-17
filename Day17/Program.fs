open System
open System.Collections.Generic
open System.IO

let city =
    File.ReadAllLines("input.txt")
    |> Array.map (_.ToCharArray() >> Array.map (fun ch -> int ch - int '0'))
    |> array2D

let maxY = Array2D.length1 city - 1
let maxX = Array2D.length2 city - 1

let cityGraph =
    [ for y = 0 to maxY do
        for x = 0 to maxX do
            let edges =
                [ if y > 0 then (y - 1, x)
                  if y < maxY then (y + 1, x)
                  if x > 0 then (y, x - 1)
                  if x < maxX then (y, x + 1) ]
                |> List.map (fun (ny, nx) -> city[ny, nx], (ny, nx)) 
            (y, x), edges ]
    |> Map.ofList

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
        if state.StraightMoves < 2
        then Direction.straightOrTurns state.Direction
        else Direction.turns state.Direction
    [ for dir in newDirections do
        let (y, x) = Direction.move state.Location dir
        if 0 <= y && y <= maxY && 0 <= x && x <= maxX then
            { Location = (y, x)
              Direction = dir
              StraightMoves = if dir = state.Direction then state.StraightMoves + 1 else 0 } ]

let dijkstra start dest (city: int[,]) =
    let unvisited = PriorityQueue<State, int>()
    let distances = Dictionary<State, int>()
    let paths = Dictionary<State, State list>()
    let mutable current = { Location = start; Direction = East; StraightMoves = 0 }
    distances.Add(current, 0)
    unvisited.Enqueue(current, 0)
    paths.Add(current, [])
    while current.Location <> dest do
        current <- unvisited.Dequeue()
        let dist = distances[current]
        for neighbour in neighbours current do
            let (ny, nx) = neighbour.Location
            let edge = city[ny, nx]
            let dist = dist + edge
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

let part1 () =
    let heatLoss, path = dijkstra (0, 0) (maxY, maxX) city

    // let pathMap = path |> List.map (fun state -> state.Location, state.Direction) |> Map.ofList
    // for y = 0 to maxY do
    //     for x = 0 to maxX do
    //         Map.tryFind (y, x) pathMap
    //         |> function
    //            | None -> char city[y, x] + '0'
    //            | Some North -> '^'
    //            | Some East -> '>'
    //            | Some West -> '<'
    //            | Some South -> 'v'
    //         |> printf "%c"
    //     printfn ""

    heatLoss

printfn "Part 1: %A" (part1 ())
