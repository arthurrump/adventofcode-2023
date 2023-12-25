open System
open System.Collections.Generic
open System.IO

let parseLine (line: string) =
    let [| from; tos |] = line.Split(": ")
    let tos = tos.Split(" ")
    from, Array.toList tos

let wires = 
    File.ReadAllLines("input.txt")
    |> Array.map parseLine
    |> Map.ofArray

let wireGraph = 
    [ for KeyValue (from, tos) in wires do
        for to' in tos do
            (from, to')
            (to', from) ]
    |> List.distinct
    |> List.groupBy fst
    |> List.map (fun (from, edges) -> from, List.map snd edges)
    |> Map.ofList

let dot (name: string) (graph: Map<string, string list>) (tw: TextWriter) = 
    tw.Write("graph "); tw.Write(name); tw.WriteLine(" {")
    for KeyValue (from, tos) in graph do
        for to' in tos do
            tw.Write("  "); tw.Write(from); tw.Write " -- "; tw.Write(to'); tw.WriteLine(";")
    tw.WriteLine("}")
    tw.Flush()

let bfsPath (start: 'node) (dest: 'node) (isAvailable: 'node * 'node -> bool) (graph: Map<'node, 'node list>) =
    let frontier = Queue<'node>([ start ])
    let paths = Dictionary<'node, 'node list>([ KeyValuePair (start, []) ])
    while frontier.Count > 0 && not (paths.ContainsKey dest) do
        let current = frontier.Dequeue()
        for neighbour in Map.tryFind current graph |> Option.defaultValue List.empty do
            if not (paths.ContainsKey neighbour) && isAvailable (current, neighbour) then
                frontier.Enqueue(neighbour)
                paths.Add(neighbour, current :: paths[current])
    match paths.TryGetValue(dest) with
    | true, path -> Some (List.rev path)
    | false, _ -> None

let bfsDiscovery (start: 'node) (isAvailable: 'node * 'node -> bool) (graph: Map<'node, 'node list>) =
    let frontier = Queue<'node>([ start ])
    let discovered = HashSet<'node>([ start ])
    while frontier.Count > 0 do
        let current = frontier.Dequeue()
        for neighbour in Map.tryFind current graph |> Option.defaultValue List.empty do
            if not (discovered.Contains neighbour) && isAvailable (current, neighbour) then
                frontier.Enqueue(neighbour)
                assert discovered.Add(neighbour)
    set discovered

let edmondsKarp (source: 'node) (sink: 'node) (capacity: 'node * 'node -> int) (graph: Map<'node, 'node list>) =
    let rec run maxFlow flow =
        let getFlow edge = 
            flow |> Map.tryFind edge |> Option.defaultValue 0
        match bfsPath source sink (fun edge -> getFlow edge < capacity edge) graph with
        | Some augmentingPath ->
            let augmentingEdges = augmentingPath |> List.pairwise
            let addFlow = 
                augmentingEdges
                |> Seq.map (fun edge -> capacity edge - getFlow edge)
                |> Seq.min
            let flow =
                augmentingEdges
                |> List.fold (fun flow edge -> Map.add edge (getFlow edge + addFlow) flow) flow
            run (maxFlow + addFlow) flow
        | None ->
            maxFlow, flow
    run 0 Map.empty

let minCut source sink (graph: Map<'node, 'node list>) =
    let _, flow = edmondsKarp source sink (fun _ -> 1) graph
    let nodes = set (Map.keys graph)
    let sourceComponent = bfsDiscovery source (fun edge -> not (flow |> Map.containsKey edge)) graph
    sourceComponent, nodes - sourceComponent

let part1 () =
    let left, right = minCut "str" "mmj" wireGraph
    Set.count left * Set.count right

printfn "Part 1: %A" (part1 ())
