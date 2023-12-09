open System
open System.IO

type Game =
    { Id: int
      Shows: Show list }

and Show =
    { Red: int
      Green: int
      Blue: int }

let parseShow (str: string) =
    str.Split(", ")
    |> Array.fold (fun show str ->
        let [| count; color |] = str.Split(" ")
        let count = Int32.Parse count
        match color with
        | "red" -> { show with Red = count }
        | "green" -> { show with Green = count }
        | "blue" -> { show with Blue = count }
    ) { Red = 0; Green = 0; Blue = 0 }

let parseGame (line: string) =
    let [| game; shows |] = line.Split(": ")
    { Id = 
        game.Substring("Game ".Length) 
        |> Int32.Parse
      Shows = 
        shows.Split("; ")
        |> Array.map parseShow
        |> Array.toList }

let games =
    File.ReadAllLines("input.txt")
    |> Array.map parseGame
    |> Array.toList

let part1 () =
    games
    |> List.filter (fun game ->
        game.Shows
        |> List.forall (fun show ->
            show.Red <= 12 &&
            show.Green <= 13 &&
            show.Blue <= 14
        )
    )
    |> List.sumBy _.Id

printfn "Part 1: %d" (part1 ())

let maxShow game =
    game.Shows
    |> List.reduce (fun maxShow show ->
        { Red = max maxShow.Red show.Red
          Green = max maxShow.Green show.Green
          Blue = max maxShow.Blue show.Blue }
    )

let minPower game =
    let maxShow = maxShow game
    maxShow.Red * maxShow.Green * maxShow.Blue

let part2 () =
    games
    |> List.map minPower
    |> List.sum

printfn "Part 2: %d" (part2 ())
