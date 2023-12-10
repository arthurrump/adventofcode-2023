open System
open System.IO

type Range =
    { /// Inclusive start-index
      Start: int64
      /// Exclusive end-index
      End: int64 }

module Range =
    let containsValue x range =
        range.Start <= x && x < range.End

    let contains range1 range2 =
        range2.Start <= range1.Start && range1.End <= range2.End
    
    let overlaps range1 range2 =
        not (range1.Start >= range2.End) && not (range1.End <= range2.Start)

    let tryIntersect range1 range2 =
        if overlaps range1 range2 
        then Some { Start = max range1.Start range2.Start
                    End = min range1.End range2.End }
        else None

    let shift x range =
        { Start = range.Start + x
          End = range.End + x }

type Ranges =
    Range list

// module Ranges =
//     let simplify ranges =
//         ranges
//         |> List.sortBy _.Start
//         |> List.fold ()

type Mapping =
    { Source: Range
      Shift: int64 }

module Mapping =
    let parse (line: string) =
        let [| dest; src; len |] = line.Split(" ")
        let dest = Int64.Parse dest
        let src = Int64.Parse src
        let len = Int64.Parse len
        { Source = { Start = src; End = src + len }
          Shift = dest - src }

    let tryMap mapping source =                                                                                                                                                       
        if mapping.Source |> Range.containsValue source
        then Some (source + mapping.Shift)
        else None

    let mapRange mapping source =
        match Range.tryIntersect mapping.Source source with
        | Some intersection ->
            let unmapped = 
                [ if intersection.Start > source.Start then
                    { Start = source.Start; End = intersection.Start }
                  if intersection.End < source.End then
                    { Start = intersection.End; End = source.End } ]
            Some (Range.shift mapping.Shift intersection), unmapped
        | None ->
            None, [ source ]

type Map =
    Mapping list

module Map =
    let tryMap (map: Map) source =
        List.tryPick (fun mapping -> Mapping.tryMap mapping source) map

    let map map source =
        tryMap map source
        |> Option.defaultValue source

    let mapRanges (map: Map) (sources: Ranges) =
        List.fold (fun (mapped, unmapped) mapping -> 
            let newMapped, newUnmapped =
                unmapped 
                |> List.map (Mapping.mapRange mapping)
                |> List.unzip
            (mapped @ List.choose id newMapped, List.concat newUnmapped)
        ) ([], sources) map
        ||> List.append

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

let part2 () =
    almanac.Seeds 
    |> List.chunkBySize 2
    |> List.map (fun [ start; len ] -> { Start = start; End = start + len })
    |> fun seeds -> List.fold (fun n map -> Map.mapRanges map n) seeds almanac.Maps
    |> List.map _.Start
    |> List.min

printfn "Part 2: %A" (part2 ())
