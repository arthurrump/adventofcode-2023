open System
open System.IO

let input =
    File.ReadAllLines("input.txt")

let directions = input[0].ToCharArray()

let parseNetworkLine (line: string) =
    let [| from; to' |] = line.Split(" = ")
    let [| left; right |] = to'.Substring(1, to'.Length - 2).Split(", ")
    from, (left, right)

let network =
    input[2..]
    |> Array.map parseNetworkLine
    |> Map.ofArray

let nextLocation (network: Map<string, string * string>) direction location =
    if direction = 'L'
    then fst network[location]
    else snd network[location]

let walk start isEnd =
    let rec walk location dirIndex stepCount =
        if isEnd location then
            stepCount
        else
            let location = nextLocation network directions[dirIndex] location
            let dirIndex = (dirIndex + 1) % Array.length directions
            walk location dirIndex (stepCount + 1)
    walk start 0 0

let part1 () =
    walk "AAA" (fun loc -> loc = "ZZZ")

printfn "Part 1: %d" (part1 ())

let rec gcd (a: bigint) (b: bigint) =
    if b = 0I
    then a 
    else gcd b (a % b)

let lcm a b =
    (a / gcd a b) * b

let part2 () =
    Map.keys network 
    |> Seq.filter (_.EndsWith("A")) 
    |> Seq.map (fun start -> walk start (_.EndsWith("Z")) |> bigint)
    |> Seq.reduce lcm

printfn "Part 2: %A" (part2 ())
