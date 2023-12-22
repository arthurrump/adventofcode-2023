open System
open System.IO

type Point = { X: int; Y: int; Z: int }
type Brick = { Low: Point; High: Point }

module Brick =
    let fallTo z brick =
        let fall = brick.Low.Z - z
        { brick with Low.Z = z; High.Z = brick.High.Z - fall }

    let overlapsXY b1 b2 =
        b1.Low.X <= b2.High.X && b1.High.X >= b2.Low.X &&
        b1.Low.Y <= b2.High.Y && b1.High.Y >= b2.Low.Y

let parsePoint (point: string) =
    let [| x; y; z |] = point.Split(",")
    { X = Int32.Parse x; Y = Int32.Parse y; Z = Int32.Parse z }

let parseBrick (line: string) =
    let [| low; high |] = line.Split("~")
    { Low = parsePoint low; High = parsePoint high }

let bricks =
    File.ReadAllLines("input.txt")
    |> Array.map parseBrick
    |> Array.sortBy _.Low.Z

let minX = bricks |> Array.map _.Low.X |> Array.min
let maxX = bricks |> Array.map _.High.X |> Array.max
let minY = bricks |> Array.map _.Low.Y |> Array.min
let maxY = bricks |> Array.map _.High.Y |> Array.max

let fall bricks =
    let fallBrick (zMap: int[,]) brick =
        let z = 
            [ for x = brick.Low.X to brick.High.X do 
                for y = brick.Low.Y to brick.High.Y do 
                    zMap[x, y] ]
            |> List.max
        let box = brick |> Brick.fallTo (z + 1)
        let zMap = 
            Array2D.mapi (fun x y z -> 
                if box.Low.X <= x && x <= box.High.X && box.Low.Y <= y && y <= box.High.Y 
                then box.High.Z
                else z
            ) zMap
        box, zMap
    let zMap = Array2D.createBased minX minY (maxX - minX + 1) (maxY - minY + 1) 0
    Array.mapFold fallBrick zMap bricks
    |> fst

let getSupported bricks =
    let byLowZ = bricks |> Array.groupBy _.Low.Z |> Map.ofArray
    fun brick -> 
        Map.tryFind (brick.High.Z + 1) byLowZ
        |> Option.defaultValue Array.empty
        |> Array.filter (Brick.overlapsXY brick)

let getSupports bricks =
    let byHighZ = bricks |> Array.groupBy _.High.Z |> Map.ofArray
    fun brick ->
        Map.tryFind (brick.Low.Z - 1) byHighZ
        |> Option.defaultValue Array.empty
        |> Array.filter (Brick.overlapsXY brick)

let findDisintegratable bricks =
    let getSupported = getSupported bricks
    let getSupports = getSupports bricks
    [ for brick in bricks do
        let supported = getSupported brick
        if Array.isEmpty supported then
            yield brick
        elif supported |> Array.forall (fun br -> getSupports br |> Array.length > 1) then
            yield brick ]

let part1 () =
    fall bricks
    |> findDisintegratable
    |> List.length

printfn "Part 1: %A" (part1 ())

let wouldFall bricks =
    let getSupports = getSupports bricks
    fun disintegrated ->
        bricks
        |> Array.fold (fun fallen brick -> 
            let supports = set (getSupports brick)
            if not (Set.isEmpty supports) &&  Set.isSubset supports fallen
            then Set.add brick fallen
            else fallen
        ) (Set.singleton disintegrated)
        |> Set.remove disintegrated

let part2 () =
    let bricks = fall bricks
    Array.sumBy (wouldFall bricks >> Set.count) bricks

printfn "Part 2: %A" (part2 ())
