open System
open System.IO

type Direction = North | East | South | West

module Direction =
    let move direction distance (y, x) =
        match direction with
        | North -> (y - distance, x)
        | East -> (y, x + distance)
        | South -> (y + distance, x)
        | West -> (y, x - distance)

    let step direction (y, x) =
        move direction 1 (y, x)

type Dig =
    { Direction: Direction
      Distance: int
      Color: string }

let parseLine (line: string) =
    let [| dir; dist; color |] = line.Split(" ")
    { Direction =
        match dir with
        | "U" -> North
        | "R" -> East
        | "D" -> South
        | "L" -> West
      Distance =
        Int32.Parse dist
      Color =
        color.Substring("(#".Length, "123456".Length) }

let digPlan =
    File.ReadAllLines("input.txt")
    |> Array.map parseLine

let trench (digPlan: Dig[]) =
    Array.mapFold (fun (y, x) dig ->
        let locations = set [ for i in 1 .. dig.Distance -> Direction.move dig.Direction i (y, x) ]
        locations, Direction.move dig.Direction dig.Distance (y, x)
    ) (0, 0) digPlan
    |> fst
    |> Set.unionMany

let fieldBoundaries trench =
    let ys = Set.map fst trench
    let minY = Set.minElement ys
    let maxY = Set.maxElement ys
    let xs = Set.map snd trench
    let minX = Set.minElement xs
    let maxX = Set.maxElement xs
    (minY, minX), (maxY, maxX)

let countInterior trench =
    let rec countRow y maxX count isInside x =
        if x > maxX then
            count
        elif Set.contains (y, x) trench then
            let edge = 
                Seq.initInfinite (fun i -> i + x)
                |> Seq.takeWhile (fun x -> Set.contains (y, x) trench)
                |> Seq.length
            if edge = 1 then
                countRow y maxX (count + 1) (not isInside) (x + 1)
            else
                let switchInside =
                    (Set.contains (y - 1, x) trench && Set.contains (y + 1, x + edge - 1) trench)
                    || (Set.contains (y + 1, x) trench && Set.contains (y - 1, x + edge - 1) trench)
                let isInside = if switchInside then not isInside else isInside
                countRow y maxX (count + edge) isInside (x + edge)
        elif isInside then
            countRow y maxX (count + 1) isInside (x + 1)
        else
            countRow y maxX count isInside (x + 1)

    let (minY, minX), (maxY, maxX) = fieldBoundaries trench
    [ for y = minY to maxY do countRow y maxX 0 false minX ]
    |> List.sum

let printTrenchMap trench =
    let (minY, minX), (maxY, maxX) = fieldBoundaries trench
    for y = minY to maxY do
        for x = minX to maxX do
            if Set.contains (y, x) trench
            then printf "#"
            else printf "."
        printfn ""

let tee f = fun x -> f x; x

let part1 () =
    trench digPlan
    |> tee printTrenchMap
    |> countInterior

printfn "Part 1: %A" (part1 ())
