open System
open System.IO
open System.Text.RegularExpressions

let getCalibrationValue =
    let digitPattern = Regex("""\d""", RegexOptions.Compiled)
    fun line ->
        let digits = digitPattern.Matches(line)
        Int32.Parse((Seq.head digits).Value + (Seq.last digits).Value)

let part1 () =
    File.ReadAllLines("input.txt")
    |> Array.map getCalibrationValue
    |> Array.sum

printfn "Part 1: %d" (part1 ())

let digitReplacements =
    [ "one", "1"
      "two", "2"
      "three", "3"
      "four", "4"
      "five", "5"
      "six", "6"
      "seven", "7"
      "eight", "8"
      "nine", "9" ]
    |> Map.ofList

let getCalibrationValue2 =
    let digitPattern = Regex("""(?=(\d|one|two|three|four|five|six|seven|eight|nine))""", RegexOptions.Compiled)
    let getDigit (m: Match) =
        let str = m.Groups[1].Value
        match Map.tryFind str digitReplacements with
        | Some digit -> digit
        | None -> str
    fun line ->
        let digits = digitPattern.Matches(line) |> Seq.map getDigit
        Int32.Parse((Seq.head digits) + (Seq.last digits))

let part2 () =
    File.ReadAllLines("input.txt")
    |> Array.map getCalibrationValue2
    |> Array.sum

printfn "Part 2: %d" (part2 ())
