open System
open System.IO

let parseNumbers (str: string) =
    str.Split(' ', StringSplitOptions.RemoveEmptyEntries + StringSplitOptions.TrimEntries)
    |> Array.map Int32.Parse
    |> Set.ofArray

let parseCard (line: string) =
    let [| card; numbers |] = line.Split(": ")
    let [| winning; youHave |] = numbers.Split(" | ") |> Array.map parseNumbers
    Set.intersect winning youHave |> Set.count

let cards =
    File.ReadAllLines("input.txt")
    |> Array.map parseCard

let points card =
    if card = 0 then 0 else pown 2 (card - 1)

let part1 () =
    cards
    |> Array.map points
    |> Array.sum

printfn "Part 1: %d" (part1 ())

let updateCounts counts (index, card) =
    Seq.init card (fun i -> index + 1 + i)
    |> Seq.fold (fun counts i -> Array.updateAt i (counts[i] + counts[index]) counts) counts

let getCounts cards =
    Array.indexed cards
    |> Array.fold updateCounts (Array.create (Array.length cards) 1)

let part2 () =
    getCounts cards |> Array.sum

printfn "Part 2: %d" (part2 ())
