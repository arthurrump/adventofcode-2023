open System
open System.IO

let parseLine (line: string) =
    let [| from; tos |] = line.Split(": ")
    let tos = tos.Split(" ")
    from, tos

let wires = 
    File.ReadAllLines("input.txt")
    |> Array.collect (fun line ->
        let from, tos = parseLine line
        tos |> Array.map (fun to' -> from, to')
    )

// let wireGraph = 
//     [ for KeyValue (from, tos) in wires do
//         for to' in tos do
//             (from, to')
//             (to', from) ]
//     |> List.groupBy fst
//     |> List.map (fun (from, edges) -> from, List.map (fun (_, to') -> to', 1) edges)
//     |> Map.ofList

let dot (name: string) (graph: Map<string, string list>) (tw: TextWriter) = 
    tw.Write("graph "); tw.Write(name); tw.WriteLine(" {")
    for KeyValue (from, tos) in graph do
        for to' in tos do
            tw.Write("  "); tw.Write(from); tw.Write " -- "; tw.Write(to'); tw.WriteLine(";")
    tw.WriteLine("}")
    tw.Flush()

let contractable graph =
    graph
    |> Array.map (fun (a, b) -> Set.singleton a, (a, b), Set.singleton b)

// https://en.wikipedia.org/wiki/Karger%27s_algorithm#Karger%E2%80%93Stein_algorithm
let contractEdge (a, edge, b) graph =
    let node = Set.union a b
    graph
    |> Array.filter (fun (a', _, b') -> not (a = a' && b = b' || a = b' && b = a'))
    |> Array.map (fun (a', edge', b') -> 
        if a' = a || a' = b then node, edge', b'
        elif b' = a || b' = b then a', edge', node
        else a', edge', b'
    )

let nodes graph = 
    seq {
        for a, _, b in graph do
            yield a
            yield b
    } 
    |> Seq.distinct
    |> Seq.length

let contractRandom t graph =
    let n = nodes graph - t
    let rec contractRandomTimes n graph =
        if n <= 0 then
            graph
        else
            let i = Random.Shared.Next(Array.length graph)
            let e = graph[i]
            contractRandomTimes (n - 1) (contractEdge e graph)
    contractRandomTimes n graph

let rec fastmincut graph =
    let nodes = nodes graph
    if nodes <= 6 then
        contractRandom 2 graph
    else
        let t = int (ceil (1. + float nodes / sqrt 2.))
        let g1 = fastmincut (contractRandom t graph)
        let g2 = fastmincut (contractRandom t graph)
        [ g1; g2 ]
        |> List.minBy Array.length

let rec repeatedMincut expectedMincut graph =
    let res = contractRandom 2 graph
    if Array.length res > expectedMincut then 
        printfn "Found minimum cut of %d edges. Trying again." (Array.length res)
        repeatedMincut expectedMincut graph
    else
        res

let part1 () =
    let left, _, right =
        contractable wires
        |> repeatedMincut 3
        |> Array.head
    Set.count left * Set.count right

printfn "Part 1: %A" (part1 ())
