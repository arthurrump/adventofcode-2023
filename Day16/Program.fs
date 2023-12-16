open System
open System.IO

type Direction = North | East | South | West

let grid =
    File.ReadAllLines("input.txt")
    |> Array.map _.ToCharArray()
    |> array2D

let nextDirection (currentDirection: Direction) tile =
    match tile with
    | '.' ->
        seq { currentDirection }
    | '/' ->
        match currentDirection with
        | North -> seq { East }
        | East -> seq { North }
        | South -> seq { West }
        | West -> seq { South }
    | '\\' ->
        match currentDirection with
        | North -> seq { West }
        | East -> seq { South }
        | South -> seq { East }
        | West -> seq { North }
    | '|' ->
        match currentDirection with
        | East | West -> seq { North; South }
        | _ -> seq { currentDirection }
    | '-' ->
        match currentDirection with
        | North | South -> seq { East; West }
        | _ -> seq { currentDirection }
    | _ ->
        failwithf "Invalid tile '%c'" tile

let move (y, x) direction =
    match direction with
    | North -> (y - 1, x)
    | East -> (y, x + 1)
    | South -> (y + 1, x)
    | West -> (y, x - 1)

let rec walk (grid: char[,]) steps ((y, x), direction) =
    if steps |> Set.contains ((y, x), direction) then
        steps
    else
        nextDirection direction grid[y, x]
        |> Seq.map (fun direction -> move (y, x) direction, direction)
        |> Seq.filter (fun ((y, x), _) -> 0 <= y && y < Array2D.length1 grid && 0 <= x && x < Array2D.length2 grid)
        |> Seq.fold (walk grid) (Set.add ((y, x), direction) steps)

let countEnergized grid start =
    walk grid Set.empty start
    |> Set.map fst
    |> Set.count

let part1 () =
    countEnergized grid ((0, 0), East)

printfn "Part 1: %d" (part1 ())

let part2 () =
    let maxY = Array2D.length1 grid - 1
    let maxX = Array2D.length2 grid - 1
    let starts =
        seq {
            for y = 0 to maxY do 
                yield (y, 0), East
                yield (y, maxX), West
            for x = 0 to maxX do
                yield (0, x), South
                yield (maxY, x), North
        }
    starts
    |> Seq.map (countEnergized grid)
    |> Seq.max

printfn "Part 2: %d" (part2 ())
