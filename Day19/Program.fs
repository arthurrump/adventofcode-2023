open System
open System.IO

type WorkflowName = string

type RuleOutput =
    | Accept
    | Reject
    | Redirect of WorkflowName

type RuleCondition =
    | Comparison of property: char * operation: char * value: int
    | Wildcard

type Rule =
    { Condition: RuleCondition
      Output: RuleOutput }

type Workflows = Map<WorkflowName, Rule list> 
type Part = Map<char, int>

let parseOutput = function
    | "A" -> Accept
    | "R" -> Reject
    | workflow -> Redirect workflow

let parseRule (rule: string) =
    if rule.Contains(":") then
        let value = 
            rule[2..] 
            |> String.filter Char.IsDigit
            |> Int32.Parse
        { Condition =
            Comparison (rule[0], rule[1], value)
          Output =
            let [| _; output |] = rule.Split(":")
            parseOutput output }
    else
        { Condition = Wildcard
          Output = parseOutput rule }

let parseWorkflow (line: string) =
    let [| name; rules |] = line.Split([| "{"; "}" |], StringSplitOptions.RemoveEmptyEntries)
    let rules = rules.Split(",") |> Array.map parseRule |> Array.toList
    name, rules

let parsePart (line: string) =
    line.Trim('{', '}').Split(",")
    |> Array.map (fun field -> field[0], Int32.Parse field[2..])
    |> Map.ofArray

let workflows, parts =
    let lines = File.ReadAllLines("input.txt")
    let splitIndex = lines |> Array.findIndex String.IsNullOrEmpty
    let workflows =
        lines[0..splitIndex - 1]
        |> Array.map parseWorkflow
        |> Map.ofArray
    let parts =
        lines[splitIndex + 1..]
        |> Array.map parsePart
    workflows, parts

let applyRule (rule: Rule) (part: Part) =
    match rule.Condition with
    | Comparison (field, '<', value) ->
        if part[field] < value
        then Some rule.Output
        else None
    | Comparison (field, '>', value) ->
        if part[field] > value
        then Some rule.Output
        else None
    | Wildcard ->
        Some rule.Output

let runWorkflow (rules: Rule list) (part: Part) =
    rules |> List.pick (fun rule -> applyRule rule part)

let isAccepted (workflows: Workflows) (part: Part) =
    let rec run current =
        match runWorkflow workflows[current] part with
        | Accept -> true
        | Reject -> false
        | Redirect workflow -> run workflow
    run "in"

let totalRating (part: Part) = 
    Map.values part |> Seq.sum

let part1 () =
    parts 
    |> Array.sumBy (fun part -> 
        if isAccepted workflows part 
        then totalRating part
        else 0
    )

printfn "Part 1: %A" (part1 ())

type Range =
    { Low: int
      High: int }
type RangePart = Map<char, Range>

let applyRuleRange (rule: Rule) (rangePart: RangePart) =
    match rule.Condition with
    | Comparison (field, '<', value) ->
        if rangePart[field].High < value then 
            Some (rangePart, rule.Output), None
        elif rangePart[field].Low > value then
            None, Some rangePart
        else
            let matched = rangePart |> Map.add field { rangePart[field] with High = value - 1 }
            let fallthrough = rangePart |> Map.add field { rangePart[field] with Low = value }
            Some (matched, rule.Output), Some fallthrough
    | Comparison (field, '>', value) ->
        if rangePart[field].Low > value then 
            Some (rangePart, rule.Output), None
        elif rangePart[field].High < value then
            None, Some rangePart
        else
            let matched = rangePart |> Map.add field { rangePart[field] with Low = value + 1 }
            let fallthrough = rangePart |> Map.add field { rangePart[field] with High = value }
            Some (matched, rule.Output), Some fallthrough
    | Comparison (_, _, _) ->
        None, Some rangePart
    | Wildcard ->
        Some (rangePart, rule.Output), None

let runWorkflowRange (rules: Rule list) (rangePart: RangePart) =
    let rec run matches (rule::rules) rangePart  =
        let match', falltrough = applyRuleRange rule rangePart
        let matches = 
            match match' with
            | Some m -> m::matches
            | None -> matches
        match falltrough with
        | Some fallthroughRange ->
            run matches rules fallthroughRange
        | None ->
            matches
    run [] rules rangePart

let findAccepted (workflows: Workflows) (rangePart: RangePart) =
    let rec run current range =
        [ for (range, output) in runWorkflowRange workflows[current] range do
            match output with
            | Accept -> yield range
            | Reject -> ()
            | Redirect workflow -> yield! run workflow range ]
    run "in" rangePart

let part2 () =
    let rangePart = Map.ofList [ for field in "xmas" -> field, { Low = 1; High = 4000 } ]
    findAccepted workflows rangePart
    |> List.map (Map.fold (fun res _ range -> res * bigint (range.High - range.Low + 1)) 1I)
    |> List.sum

printfn "Part 2: %A" (part2 ())
