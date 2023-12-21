open System
open System.IO

type Module =
    | Broadcaster
    | FlipFlop of state: bool
    | Conjunction of history: Map<string, bool>

type Network = 
    { Connections: Map<string, string list>
      Modules: Map<string, Module> }

let network: Network =
    let lines = File.ReadAllLines("input.txt")
    let connections =
        lines
        |> Array.map (fun line -> 
            let [| send; receive |] = line.Split(" -> ")
            let receive = receive.Split(", ")
            send.TrimStart('%', '&'), Array.toList receive
        )
        |> Map.ofArray
    let modules = 
        lines
        |> Array.map (fun line ->
            let [| fullName; _ |] = line.Split(" -> ")
            let name = fullName.TrimStart('%', '&')
            let module' =
                if fullName.StartsWith("%") then 
                    FlipFlop false
                elif fullName.StartsWith("&") then 
                    connections 
                    |> Seq.choose (fun (KeyValue (from, tos)) -> 
                        if List.contains name tos
                        then Some (from, false)
                        else None
                    )
                    |> Map.ofSeq
                    |> Conjunction
                else Broadcaster
            name, module'
        )
        |> Map.ofArray
    { Connections = connections; Modules = modules }

type Pulse = Pulse of from: string * value: bool * to': string

let handlePulse network (Pulse (fromModule, value, currentModule)) =
    if Map.containsKey currentModule network.Modules then
        let nextModuleState, nextPulseValue =
            match network.Modules[currentModule] with
            | Broadcaster -> 
                Broadcaster, [ value ]
            | FlipFlop state ->
                if value
                then FlipFlop state, []
                else FlipFlop (not state), [ not state ]
            | Conjunction history ->
                let history = Map.add fromModule value history
                Conjunction history, [ not (Map.forall (fun _ value -> value) history) ]
        let network = { network with Modules = Map.add currentModule nextModuleState network.Modules }
        let pulses =
            Map.tryFind currentModule network.Connections
            |> Option.defaultValue List.empty
            |> List.allPairs nextPulseValue
            |> List.map (fun (value, to') -> Pulse (currentModule, value, to'))
        pulses, network
    else
        [], network

let startPulse = Pulse ("button", false, "broadcaster")

let executePulse network pulse =
    let rec run allPulses network pulseList =
        let nextPulses, network = List.mapFold handlePulse network pulseList
        let nextPulses = List.concat nextPulses
        if List.isEmpty nextPulses
        then allPulses @ pulseList, network
        else run (allPulses @ pulseList) network nextPulses
    run [] network [ pulse ]

let countPulses network pulse =
    let pulses, network = executePulse network pulse
    let pulseCounts = pulses |> List.countBy (fun (Pulse (_, value, _)) -> value)
    let lowCount = pulseCounts |> List.tryFind (not << fst) |> Option.map snd |> Option.defaultValue 0
    let highCount = pulseCounts |> List.tryFind fst |> Option.map snd |> Option.defaultValue 0
    (lowCount, highCount), network

let countPulsesMemo =
    let cache = Collections.Generic.Dictionary()
    fun network pulse ->
        match cache.TryGetValue((network, pulse)) with
        | true, result ->
            result
        | false, _ ->
            let result = countPulses network pulse
            cache.Add((network, pulse), result)
            result

let part1 () =
    let (lowCount, highCount), _ =
        Seq.init 1000 id
        |> Seq.fold (fun ((lowCount, highCount), network) _ -> 
            let ((lc, hc), network) = countPulsesMemo network startPulse
            (lowCount + bigint lc, highCount + bigint hc), network
        ) ((0I, 0I), network)
    lowCount * highCount

printfn "Part 1: %A" (part1 ())

let part2() =
    // TODO: This is too slow
    let rec run count network =
        let pulses, network = executePulse network startPulse
        printfn "%d" count
        if pulses |> List.exists (fun (Pulse (_, value, to')) -> not value && to' = "rx")
        then count
        else run (count + 1) network
    run 1 network

printfn "Part 2: %A" (part2 ())
