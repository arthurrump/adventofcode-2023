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

let isPipeWith direction = function
    | Pipe directions when Set.contains direction directions -> true
    | _ -> false

let isStraightPipe = function
    | Pipe dirs when dirs = set [ North; South ] -> true
    | Pipe dirs when dirs = set [ East; West ] -> true
    | _ -> false

let grid =
    File.ReadAllLines("input.txt")
    |> Array.map (_.ToCharArray() >> Array.map parseTile)
    |> array2D

let maxY = Array2D.length1 grid - 1
let maxX = Array2D.length2 grid - 1

let getStart (grid: Tile[,]) =
    seq {
        for y = 0 to maxY do
            for x = 0 to maxX do
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

let oppositeDirection = function
    | North -> South
    | East -> West
    | South -> North
    | West -> East

let updateCoords direction (y, x) =
    match direction with
    | North -> (y - 1, x)
    | East -> (y, x + 1)
    | South -> (y + 1, x)
    | West -> (y, x - 1)

let getStartTile grid (y, x) =
    getStartConnections grid (y, x)
    |> Seq.map (fun (dir, _) -> oppositeDirection dir)
    |> Set.ofSeq
    |> Pipe

let getNextConnection (grid: Tile[,]) incomingDirection (y, x) =
    match grid[y, x] with
    | Pipe directions ->
        let outgoingDirection =
            Set.remove incomingDirection directions 
            |> Set.toSeq
            |> Seq.exactlyOne
        oppositeDirection outgoingDirection, updateCoords outgoingDirection (y, x)
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

let dirDeg = function
    | North -> 0
    | East -> 90
    | South -> 180
    | West -> 270

let degDir = function
    | 0 -> North
    | 90 -> East
    | 180 -> South
    | 270 -> West
    | other -> failwithf "Not a valid direction: %d" other

let (%%) x m =
    (x % m + m) % m

let fill (grid: Tile[,]) (pipe: (int * int) list) fillDirection =
    let (startY, startX) = getStart grid
    let startTile = getStartTile grid (startY, startX)
    let grid = Array2D.copy grid
    grid[startY, startX] <- startTile
    let isPipe =
        let pipeSet = set pipe
        fun (y, x) -> pipeSet |> Set.contains (y, x)
    
    let getFill direction (y, x) =
        let fillRay =
            match direction with
            | North -> List.allPairs [y-1..-1..0] [x]
            | East -> List.allPairs [y] [x+1..maxX]
            | South -> List.allPairs [y+1..maxY] [x]
            | West -> List.allPairs [y] [x-1..-1..0]
        fillRay 
        |> List.takeWhile (not << isPipe)
        |> Set.ofList

    let rec walk filled travelDirection fillDirection (y, x) =
        if (y, x) = (startY, startX) then
            filled
        else
            let filled = getFill fillDirection (y, x) |> Set.union filled
            let newTravelDirection =
                match grid[y, x] with
                | Pipe directions ->
                    directions 
                    |> Set.remove (oppositeDirection travelDirection)
                    |> Set.toSeq
                    |> Seq.exactlyOne
                | _ ->
                    failwithf $"Tile at (%d{y}, %d{x}) is not a pipe"
            let newFillDirection = (dirDeg newTravelDirection - dirDeg travelDirection + dirDeg fillDirection) %% 360 |> degDir
            let newCoords = updateCoords newTravelDirection (y, x)
            let filled = getFill newFillDirection (y, x) |> Set.union filled
            walk filled newTravelDirection newFillDirection newCoords

    let nextDirection, initCoords =
        getStartConnections grid (startY, startX)
        |> Seq.head
    let initDirection = oppositeDirection nextDirection

    walk Set.empty initDirection (degDir ((dirDeg initDirection + fillDirection) %% 360)) initCoords
            
let part2 () =
    let pipe = getPipeCoords grid
    let rightFill = fill grid pipe 90
    if rightFill |> Set.exists (fun (y, x) -> y = 0 || y = maxY || x = 0 || x = maxX) 
    then fill grid pipe -90
    else rightFill
    |> Set.count

printfn "Part 2: %d" (part2 ())
