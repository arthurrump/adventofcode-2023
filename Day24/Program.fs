open MathNet.Numerics.LinearAlgebra
open System
open System.IO

module Array =
    let trianglePairs arr =
        [| for i = 0 to Array.length arr - 1 do
            for j = i + 1 to Array.length arr - 1 do
                (arr[i], arr[j]) |]

let parseHailstone (line: string) =
    let [| position; velocity |] = line.Split(" @ ")
    let position = position.Split(", ") |> Array.map bigint.Parse
    let velocity = velocity.Split(", ") |> Array.map bigint.Parse
    position, velocity

let hailstones =
    File.ReadAllLines("input.txt")
    |> Array.map parseHailstone

// p1 + t*v1 = p2 + s*v2
// t*v1 - s*v2 = p2 - p1
// [ [ v1[0]; -v2[0] ]   
//   [   .  ;    .   ]   [ t; s ] = [ p2[0] - p1[0]; ...; p2[n] - p1[n] ]
//   [ v1[n]; -v2[n] ] ]
let intersect dimensions (p1: bigint[], v1: bigint[]) (p2: bigint[], v2: bigint[]) =
    let A = matrix [ for i = 0 to dimensions - 1 do [ float v1[i]; float -v2[i] ] ]
    let b = vector [ for i = 0 to dimensions - 1 do float (p2[i] - p1[i]) ]
    let ts = A.Solve(b)
    ts[0], ts[1]

let getPoint (p: bigint[], v: bigint[]) (t: float) =
    vector (Array.map float p) + (vector (Array.map float v)).Multiply t
    |> Vector.toArray

let intersectingPairsWithin dimensions minCoord maxCoord hailstones =
    Array.trianglePairs hailstones
    |> Array.filter (fun (h1, h2) -> 
        let t, s = intersect dimensions h1 h2
        if t >= 0 && s >= 0 && t <> infinity && s <> infinity then
            getPoint h1 t
            |> Array.take dimensions
            |> Array.forall (fun c -> minCoord <= c && c <= maxCoord)
        else
            false
    )

let part1 () =
    // Example:
    // intersectingPairsWithin 2 7. 27. hailstones
    intersectingPairsWithin 2 200000000000000. 400000000000000. hailstones
    |> Array.length

printfn "Part 1: %A" (part1 ())
