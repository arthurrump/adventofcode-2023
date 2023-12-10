open System
open System.IO

type Label = A | K | Q | J | T | Nine | Eight | Seven | Six | Five | Four | Three | Two

type Type =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard

type Hand = Hand of Label[] 

let parseLabel = function
    | 'A' -> A 
    | 'K' -> K 
    | 'Q' -> Q 
    | 'J' -> J 
    | 'T' -> T 
    | '9' -> Nine 
    | '8' -> Eight 
    | '7' -> Seven 
    | '6' -> Six 
    | '5' -> Five 
    | '4' -> Four 
    | '3' -> Three 
    | '2' -> Two
    | label -> failwithf "Invalid card: %c" label

let parseHand (str: string) =
    str.ToCharArray() |> Array.map parseLabel |> Hand

type Line =
    { Hand: Hand
      Bid: int }

let parseLine (str: string) = 
    let [| hand; bid |] = str.Split(" ")
    { Hand = parseHand hand
      Bid = Int32.Parse bid }

let lines =
    File.ReadAllLines("input.txt")
    |> Array.map parseLine
    |> Array.toList

let type' (Hand hand) =
    match Array.countBy id hand |> Array.sortBy snd with
    | [| (_, 5) |] -> FiveOfAKind
    | [| (_, 1); (_, 4) |] -> FourOfAKind
    | [| (_, 2); (_, 3) |] -> FullHouse
    | [| (_, 1); (_, 1); (_, 3) |] -> ThreeOfAKind
    | [| (_, 1); (_, 2); (_, 2) |] -> TwoPair
    | [| (_, 1); (_, 1); (_, 1); (_, 2) |] -> OnePair
    | [| (_, 1); (_, 1); (_, 1); (_, 1); (_, 1) |] -> HighCard
    | _ -> failwith "Invalid hand"

let comparer hand1 hand2 =
    if type' hand1 <> type' hand2 then
        compare (type' hand1) (type' hand2)
    else
        let (Hand arr1) = hand1
        let (Hand arr2) = hand2
        compare arr1 arr2

let part1 () =
    lines 
    |> List.sortWith (fun line1 line2 -> -1 * (comparer line1.Hand line2.Hand))
    |> List.mapi (fun i line -> (i + 1) * line.Bid)
    |> List.sum

printfn "Part 1: %d" (part1 ())

let jokerType (Hand hand) =
    if hand = [| J; J; J; J; J |] then
        FiveOfAKind
    else
        let majority = 
            hand
            |> Array.filter (fun l -> l <> J)
            |> Array.countBy id 
            |> Array.maxBy snd 
            |> fst
        let bestHand = Array.map (fun l -> if l = J then majority else l) hand
        type' (Hand bestHand)

let jokerLabelComparer l1 l2 =
    if l1 = l2 then 0
    elif l1 = J then 1
    elif l2 = J then -1
    else compare l1 l2

let jokerComparer hand1 hand2 =
    let t1 = jokerType hand1
    let t2 = jokerType hand2
    if t1 <> t2 then
        compare t1 t2
    else
        let (Hand arr1) = hand1
        let (Hand arr2) = hand2
        Array.compareWith jokerLabelComparer arr1 arr2

let part2 () =
    lines 
    |> List.sortWith (fun line1 line2 -> -1 * (jokerComparer line1.Hand line2.Hand))
    |> List.mapi (fun i line -> (i + 1) * line.Bid)
    |> List.sum
    // |> List.iteri (fun i line -> printfn "%d: %A" i line.Hand)

printfn "Part 2: %A" (part2 ())
