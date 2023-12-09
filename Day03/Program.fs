open System
open System.IO

let map =
    File.ReadAllLines("input.txt")
    |> Array.map _.ToCharArray()
    |> array2D

type Number =
    { Value: int
      Y: int
      StartX: int
      EndX: int }

let charToNum char =
    if not (Char.IsDigit char)
    then raise (ArgumentException("Character is not a digit", nameof char))
    else int (char - '0')

let numbers =
    [ for y = 0 to Array2D.length1 map - 1 do
        yield!
            map[y, *] 
            |> Array.indexed 
            |> Array.filter (fun (_, char) -> Char.IsDigit char)
            |> Array.fold (fun numbers (x, char) -> 
                match numbers with
                | [] -> 
                    [ { Value = charToNum char; Y = y; StartX = x; EndX = x } ]
                | num::rest when num.EndX = x - 1 ->
                    { num with Value = num.Value * 10 + charToNum char; EndX = x }
                    :: rest
                | rest ->
                    { Value = charToNum char; Y = y; StartX = x; EndX = x }
                    :: rest
            ) [] ]

let partNumbers =
    numbers
    |> List.filter (fun number ->
        [ for y = max 0 (number.Y - 1) to min (Array2D.length1 map - 1) (number.Y + 1) do
            for x = max 0 (number.StartX - 1) to min (Array2D.length2 map - 1) (number.EndX + 1) do
                yield (y, x) ]
        |> List.exists (fun (y, x) -> map[y, x] <> '.' && not (Char.IsDigit map[y, x]))
    )

let part1 () =
    partNumbers
    |> List.sumBy _.Value

printfn "Part 1: %d" (part1 ())

let adjacentNumbers (y, x) =
    numbers
    |> List.filter (fun number ->
        number.Y - 1 <= y && y  <= number.Y + 1
        && number.StartX - 1 <= x && x <= number.EndX + 1
    )

let part2 () =
    [ for y = 0 to Array2D.length1 map - 1 do
        for x = 0 to Array2D.length2 map - 1 do
            if map[y, x] = '*' then yield (y, x) ]
    |> List.map adjacentNumbers
    |> List.filter (fun numbers -> List.length numbers = 2)
    |> List.map (List.map _.Value >> List.reduce (*))
    |> List.sum

printfn "Part 2: %d" (part2 ())