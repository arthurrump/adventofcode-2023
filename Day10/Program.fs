open System
open System.IO

type Direction = North | East | South | West

type Tile =
    | Pipe of Set<Direction>
    | Ground
    | Start

let parseTile = function
    | '|' -> Pipe (set [ North; South ])
    | '-' -> Pipe (set [ East; West ])
    | 'L' -> Pipe (set [ North; East ])
    | 'J' -> Pipe (set [ North; West ])
    | '7' -> Pipe (set [ South; West ])
    | 'F' -> Pipe (set [ South; East ])
    | '.' -> Ground
    | 'S' -> Start
    | char -> failwithf "Invalid tile: %c" char

let isPipeWith direction = 
    function
    | Pipe directions when Set.contains direction directions -> true
    | _ -> false

let grid =
    File.ReadAllLines("example4.txt")
    |> Array.map (_.ToCharArray() >> Array.map parseTile)
    |> array2D

let getStart grid =
    seq {
        for y in 0 .. Array2D.length1 grid - 1 do
            for x in 0 .. Array2D.length2 grid - 1 do
                if grid[y, x] = Start then
                    yield y, x
    } |> Seq.head

let getStartConnections (grid: Tile[,]) (y, x) =
    seq {
        if y > 0 && grid[y - 1, x] |> isPipeWith South 
        then South, (y - 1, x)
        if y < Array2D.length1 grid - 1 && grid[y + 1, x] |> isPipeWith North 
        then North, (y + 1, x)
        if x > 0 && grid[y, x - 1] |> isPipeWith East 
        then East, (y, x - 1)
        if x < Array2D.length2 grid - 1 && grid[y, x + 1] |> isPipeWith West 
        then West, (y, x + 1)
    }

let getStartTile grid (y, x) =
    getStartConnections grid (y, x)
    |> Seq.map (fun (dir, _) ->
        match dir with
        | North -> South
        | East -> West
        | South -> North
        | West -> East
    )
    |> Set.ofSeq
    |> Pipe

let getNextConnection (grid: Tile[,]) incomingDirection (y, x) =
    match grid[y, x] with
    | Pipe directions ->
        let outgoingDirection =
            Set.remove incomingDirection directions 
            |> Set.toSeq
            |> Seq.exactlyOne
        match outgoingDirection with
        | North -> South, (y - 1, x)
        | East -> West, (y, x + 1)
        | South -> North, (y + 1, x)
        | West -> East, (y, x - 1)
    | _ ->
        failwithf $"Tile at (%d{y}, %d{x}) is not a pipe"

let getPipeCoords (grid: Tile[,]) =
    let start = getStart grid
    let nextIncomingDirection, next = 
        getStartConnections grid start
        |> Seq.head

    let rec walk visited incomingDirection (y, x) =
        if grid[y, x] = Start then 
            visited
        else 
            let visited = (y, x) :: visited
            let incomingDirection, (y, x) = getNextConnection grid incomingDirection (y, x)
            walk visited incomingDirection (y, x)

    walk [ start ] nextIncomingDirection next

let part1 () =
    List.length (getPipeCoords grid) / 2

printfn "Part 1: %A" (part1 ())
