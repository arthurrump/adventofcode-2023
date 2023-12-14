open System
open System.IO

module Array =
    let split predicate arr =
        Seq.indexed arr
        |> Seq.filter (snd >> predicate)
        |> Seq.map fst
        |> Seq.insertAt 0 -1
        |> fun seq -> Seq.append seq [ Array.length arr ]
        |> Seq.pairwise
        |> Seq.map (fun (from, to') -> arr[from + 1 .. to' - 1])
        |> Seq.toArray

let dish =
    File.ReadAllLines("input.txt")
    |> Array.map _.ToCharArray()

let slideForwardLoad arr =
    Array.mapi (fun i tile -> bigint (Array.length arr - i), tile) arr
    |> Array.split (fun (_, tile) -> tile = '#')
    |> Array.sumBy (fun tiles -> 
        let count = tiles |> Array.filter (fun (_, tile) -> tile = 'O') |> Array.length
        tiles |> Array.take count |> Array.sumBy fst)

let part1 () =
    Array.transpose dish
    |> Array.sumBy slideForwardLoad

printfn "Part 1: %A" (part1 ())
