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

    let join joiner arrays =
        [| for i, arr in Array.indexed arrays do
            if i <> 0 then yield joiner
            yield! arr |]

let dish =
    File.ReadAllLines("input.txt")
    |> Array.map _.ToCharArray()

let slide comparer line =
    Array.split ((=) '#') line
    |> Array.map (Array.sortWith comparer)
    |> Array.join '#'

let slideLeft dish = dish |> Array.map (slide (fun a b -> - (compare a b)))
let slideUp dish = dish |> Array.transpose |> slideLeft |> Array.transpose
let slideRight dish = dish |> Array.map (slide compare)
let slideDown dish = dish |> Array.transpose |> slideRight |> Array.transpose

let columnNorthLoad column =
    column
    |> Seq.mapi (fun i tile -> if tile = 'O' then Seq.length column - i else 0)
    |> Seq.sum

let northLoad dish =
    Seq.transpose dish
    |> Seq.sumBy columnNorthLoad

let part1 () =
    slideUp dish |> northLoad

printfn "Part 1: %A" (part1 ())

let cycle = slideUp >> slideLeft >> slideDown >> slideRight

let list2d arr = Array.map Array.toList arr |> Array.toList

let findCycle =
    let results = Collections.Generic.Dictionary<_, _>()
    let rec find i currentDish =
        match results.TryGetValue (list2d currentDish) with
        | true, cycleStart ->
            cycleStart, i - cycleStart, results
        | false, _ ->
            results.Add (list2d currentDish, i)
            find (i + 1) (cycle currentDish)
    find 0

let part2 () =
    let (cycleStart, cycleLength, dishes) = findCycle dish
    let endIndex = (1_000_000_000 - cycleStart) % cycleLength + cycleStart
    dishes
    |> Seq.pick (fun (KeyValue (dish, index)) -> if index = endIndex then Some dish else None)
    |> northLoad

printfn "Part 2: %A" (part2 ())
