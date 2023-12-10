open System
open System.IO

type Mapping =
    { DestinationRangeStart: int64
      SourceRangeStart: int64
      RangeLength: int64 }

module Mapping =
    let parse (line: string) =
        let [| dest; src; len |] = line.Split(" ")
        { DestinationRangeStart = Int64.Parse dest
          SourceRangeStart = Int64.Parse src
          RangeLength = Int64.Parse len }

    let tryMap mapping source =
        if mapping.SourceRangeStart <= source && source < mapping.SourceRangeStart + mapping.RangeLength
        then Some (source - mapping.SourceRangeStart + mapping.DestinationRangeStart)
        else None

type Map =
    Mapping list

module Map =
    let tryMap (map: Map) source =
        List.tryPick (fun mapping -> Mapping.tryMap mapping source) map

    let map map source =
        tryMap map source
        |> Option.defaultValue source

type Almanac =
    { Seeds: int64 list
      Maps: Map list }

let almanac =
    let lines = File.ReadAllLines("input.txt")
    { Seeds = 
        lines[0].Substring("seeds: ".Length).Split(" ") 
        |> Array.map Int64.Parse
        |> Array.toList
      Maps =
        lines
        |> Array.skip 1
        |> Array.fold (fun maps line -> 
            if String.IsNullOrWhiteSpace line then
                maps
            elif line.Contains("map:") then
                []::maps
            else
                let map::maps = maps
                let mapping = Mapping.parse line
                (mapping::map)::maps
        ) []
        |> List.map List.rev
        |> List.rev }

let part1 () =
    almanac.Seeds
    |> List.map (fun seed -> almanac.Maps |> List.fold (fun n map -> Map.map map n) seed)
    |> List.min

printfn "Part 1: %d" (part1 ())

// Way to slow...
// let part2 () =
//     almanac.Seeds 
//     |> List.chunkBySize 2
//     |> List.collect (fun [ start; len ] -> List.init (int len) (fun i -> start + int64 i))
//     |> List.map (fun seed -> almanac.Maps |> List.fold (fun n map -> Map.map map n) seed)
//     |> List.min

// printfn "Part 2: %d" (part2 ())
