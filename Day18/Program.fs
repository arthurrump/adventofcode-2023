open System
open System.IO

type Point = { X: int; Y: int }
type Direction = North | East | South | West

module Direction =
    let move direction distance point =
        match direction with
        | North -> { point with Y = point.Y - distance }
        | East -> { point with X = point.X + distance }
        | South -> { point with Y = point.Y + distance }
        | West -> { point with X = point.X - distance }

    let step direction point =
        move direction 1 point

type Dig =
    { Direction: Direction
      Distance: int }

let parseLine1 (line: string) =
    let [| dir; dist; _ |] = line.Split(" ")
    { Direction =
        match dir with
        | "U" -> North
        | "R" -> East
        | "D" -> South
        | "L" -> West
      Distance =
        Int32.Parse dist }

let lines =
    File.ReadAllLines("input.txt")

let digPlan1 =
    Array.map parseLine1 lines

type Space<'t> = 
    { Low: Point
      High: Point
      Value: 't }

module Space =
    let universe value =
        { Low = { X = Int32.MinValue; Y = Int32.MinValue }
          High = { X = Int32.MaxValue;Y = Int32.MaxValue }
          Value = value }

    // s2 contains s1
    let contains s1 s2 =
        s2.Low.X <= s1.Low.X && s1.High.X <= s2.High.X &&
        s2.Low.Y <= s1.Low.Y && s1.High.Y <= s2.High.Y

    let size space =
        bigint (space.High.X - space.Low.X + 1) * bigint (space.High.Y - space.Low.Y + 1)

    let excavate excavation spaces =
        // A | B | C
        // D | E | F -> excavate E, splits into 9 spaces
        // G | H | I
        let splitVerticals space =
            if excavation.Low.X <= space.High.X && excavation.High.X >= space.Low.X then
                [ // ADG
                  if space.Low.X < excavation.Low.X then
                    { space with High.X = excavation.Low.X - 1 }
                  // BEH
                  { space with 
                        Low.X = max excavation.Low.X space.Low.X
                        High.X = min excavation.High.X space.High.X }
                  // CFI
                  if space.High.X > excavation.High.X then
                    { space with Low.X = excavation.High.X + 1 } ]
            else
                [ space ]
        let splitHorizontals space =
            if excavation.Low.Y <= space.High.Y && excavation.High.Y >= space.Low.Y then
                [ // ABC
                  if space.Low.Y < excavation.Low.Y then
                    { space with High.Y = excavation.Low.Y - 1 }
                  // DEF
                  { space with
                        Low.Y = max excavation.Low.Y space.Low.Y
                        High.Y = min excavation.High.Y space.High.Y }
                  // GHI
                  if space.High.Y > excavation.High.Y then
                    { space with Low.Y = excavation.High.Y + 1 } ]
            else
                [ space ]
        spaces
        |> List.collect splitVerticals 
        |> List.collect splitHorizontals
        |> List.map (fun space -> 
            if excavation |> contains space
            then { space with Value = excavation.Value }
            else space
        )
        

type Terrain = Trench | Ground

let trench (digPlan: Dig[]) =
    Array.fold (fun (point, spaces) dig ->
        let startPoint = Direction.step dig.Direction point
        let endPoint = Direction.move dig.Direction dig.Distance point
        let trench =
            { Low = { X = min startPoint.X endPoint.X; Y = min startPoint.Y endPoint.Y }
              High = { X = max startPoint.X endPoint.X; Y = max startPoint.Y endPoint.Y }
              Value = Trench }
        endPoint, spaces |> Space.excavate trench
    ) ({ X = 0; Y = 0 }, [ Space.universe Ground ]) digPlan
    |> snd

let countInterior (spaces: Space<Terrain> list) =
    let spaceMap =
        spaces 
        |> Array.ofList
        |> Array.groupBy _.Low.Y
        |> Array.map (snd >> Array.sortBy _.Low.X)

    let rec countRow y' count x' isInside =
        let row = spaceMap[y']
        if x' >= Array.length row then
            count
        elif row[x'].Value = Trench then
            let edge = 
                row[x'..]
                |> Array.takeWhile (fun space -> space.Value = Trench)
            let edgeWidth =
                edge |> Array.sumBy (fun space -> space.High.X - space.Low.X + 1)
            let edgeSize =
                edge |> Array.sumBy Space.size
            if edgeWidth = 1 then
                countRow y' (count + edgeSize) (x' + Array.length edge) (not isInside)
            else
                let switchInside =
                    (spaceMap[y' - 1][x']).Value = Trench && (spaceMap[y' + 1][x' + Array.length edge - 1]).Value = Trench
                    || (spaceMap[y' + 1][x']).Value = Trench && (spaceMap[y' - 1][x' + Array.length edge - 1]).Value = Trench
                let isInside = if switchInside then not isInside else isInside
                countRow y' (count + edgeSize) (x' + Array.length edge) isInside
        elif isInside then
            countRow y' (count + Space.size row[x']) (x' + 1) isInside
        else
            countRow y' count (x' + 1) isInside

    [ for y' = 0 to Array.length spaceMap - 1 do
        countRow y' 0I 0 false ]
    |> List.sum

let printSpaces (spaces: Space<'t> list) =
    for _, row in List.groupBy (_.Low.Y) spaces do
        printf $"{row.Head.Low.Y} - {row.Head.High.Y}: "
        for i, space in List.sortBy (_.Low.X) row |> List.indexed do
            printf $"{space.Low.X}~{space.High.X} %A{space.Value}"
            if i <> List.length row then
                printf " | "
        printfn ""

let tee f = fun x -> f x; x

let part1 () =
    trench digPlan1
    |> countInterior

printfn "Part 1: %A" (part1 ())

let parseLine2 (line: string) =
    let [| _; _; color |] = line.Split(" ")
    let color = color.Substring("(#".Length, "123456".Length)
    let distance = "0" + color[0..4]
    { Direction =
        match color[5] with
        | '0' -> East
        | '1' -> South
        | '2' -> West
        | '3' -> North
      Distance =
        Int32.Parse(distance, Globalization.NumberStyles.AllowHexSpecifier) }

let digPlan2 =
    Array.map parseLine2 lines

let part2 () =
    trench digPlan2
    |> countInterior

printfn "Part 2: %A" (part2 ())
