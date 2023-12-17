open System
open System.Collections.Generic
open System.IO

let city =
    File.ReadAllLines("example.txt")
    |> Array.map (_.ToCharArray() >> Array.map (fun ch -> int ch - int '0'))
    |> array2D

let maxY = Array2D.length1 city - 1
let maxX = Array2D.length2 city - 1

let cityGraph =
    [ for y = 0 to maxY do
        for x = 0 to maxX do
            let edges =
                [ if y > 0 then (y - 1, x)
                  if y < maxY then (y + 1, x)
                  if x > 0 then (y, x - 1)
                  if x < maxX then (y, x + 1) ]
                |> List.map (fun (ny, nx) -> city[ny, nx], (ny, nx)) 
            (y, x), edges ]
    |> Map.ofList

let dijkstra (start: 'n) (dest: 'n) (graph: Map<'n, (int * 'n) list>) =
    let folder (dist: int, path: 'n list) (unvisited: PriorityQueue<'n, int>, distances: Map<'n, int>, paths: Map<'n, 'n list>) (edge: int, neighbour: 'n) =
        let dist = dist + edge
        match distances |> Map.tryFind neighbour with
        | Some d when d > dist ->
            unvisited.Enqueue(neighbour, dist)
            unvisited,
            distances |> Map.add neighbour dist, 
            paths |> Map.add neighbour path
        | Some _ ->
            unvisited, distances, paths
        | None ->
            unvisited.Enqueue(neighbour, dist)
            unvisited,
            distances |> Map.add neighbour dist, 
            paths |> Map.add neighbour path
    
    let rec dijkstra (unvisited: PriorityQueue<'n, int>) (distances: Map<'n, int>) (paths: Map<'n, 'n list>) =
        let current = unvisited.Dequeue()
        if current = dest then 
            distances[current], List.rev paths[current]
        else
            let currentDist = distances[current]
            let path = current :: paths[current]
            let unvisited, distances, paths =
                graph[current] 
                |> List.fold (folder (currentDist, path)) (unvisited, distances, paths)   
            dijkstra unvisited distances paths
    
    let unvisited = PriorityQueue()
    unvisited.Enqueue(start, 0)
    let distances = Map.ofList [ start, 0 ]
    let paths = Map.ofList [ start, [] ]
    dijkstra unvisited distances paths


// The shortest path problem with forbidden paths
// https://doi.org/10.1016/j.ejor.2004.01.032
let martinsTransform t path (graph: Map<'n, (int * 'n) list>) =
    [ for (i, from), (j, to') in path |> List.indexed |> List.pairwise do
        if i = 0 then
            yield Map.add from (graph[from] |> List.map (fun (edge, neighbour) -> edge, if neighbour = to' then t neighbour else neighbour))
        elif j = List.length path - 1 then
            yield Map.add (t from) (graph[from] |> List.filter (fun (_, neighbour) -> neighbour <> to'))
        else
            let edge = graph[from] |> List.pick (fun (edge, neighbour) -> if neighbour = to' then Some edge else None)
            yield Map.add (t from) ((edge, t to') :: (graph[from] |> List.filter (fun (_, neighbour) -> neighbour <> to'))) ]
    |> List.fold (fun graph f -> f graph) graph

let searchValidPath start dest isValid transformNode transformNode' graph =
    let rec search i currentGraph =
        let dist, path = dijkstra start dest currentGraph
        let path' = List.map transformNode' path
        if isValid path'
        then dist, path'
        else 
            printfn $"%d{i}: %d{dist} %A{path'}"
            search (i + 1) (martinsTransform transformNode path currentGraph)
    search 0 graph

let transformNode (y, x) =
    (y + maxY + 1, x + maxX + 1)

let transformNode' (y, x) =
    (y % (maxY + 1), x % (maxX + 1))

let isValidPath path =
    path 
    |> List.windowed 4 
    |> List.forall (fun window -> 
        List.map fst window |> List.distinct |> List.length <> 1
        && List.map snd window |> List.distinct |> List.length <> 1)

let part1 () =
    searchValidPath (0, 0) (maxY, maxX) isValidPath transformNode transformNode' cityGraph

printfn "Part 1: %A" (part1 ())
