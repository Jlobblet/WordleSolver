open System
open System.Diagnostics
open System.IO
open FSharpx.Collections
open XPlot.Plotly

let (<&&>) f g = fun x -> f x && g x

let tap a x =
    a x
    x

let rng = Random()

type LetterResult =
    | NotPresent // Black square
    | DifferentPlace // Yellow square
    | Correct // Green square
    override this.ToString() =
        match this with
        | NotPresent -> "⬛"
        | DifferentPlace -> "🟨"
        | Correct -> "🟩"

    static member CreateFilter index (character: char, result) =
        fun (word: string) ->
            match result with
            | NotPresent -> not <| word.Contains character
            | DifferentPlace ->
                word.Contains character
                && word[index] <> character
            | Correct -> word[index] = character

    static member FromGuess (word: string) index (character: char) =
        if word[index] = character then
            Correct
        elif word.Contains character then
            DifferentPlace
        else
            NotPresent

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type GuessResults =
    { Word: string
      LetterResults: LetterResult []
      InitialLength: int }
    member this.StructuredFormatDisplay =
        let blocks =
            String.concat "" (Array.map (fun lr -> lr.ToString()) this.LetterResults)

        $"%s{this.Word} | %s{blocks} (from %i{this.InitialLength} options)"

    static member CreateFilter
        { Word = word
          LetterResults = letterResults }
        =
        Array.zip (word.ToCharArray()) letterResults
        |> Array.mapi LetterResult.CreateFilter
        |> Array.reduce (<&&>)

type PlyResult =
    | Failed
    | Success of string
    | Ongoing of string []

let makeGuess (picker: string [] -> string) (words: string []) (answer: string) =
    let guess = picker words

    let letterResults =
        guess.ToCharArray()
        |> Array.mapi (LetterResult.FromGuess answer)

    { Word = guess
      LetterResults = letterResults
      InitialLength = words.Length }

let ply picker answer words =
    let results =
        makeGuess picker words answer
//        |> tap (printfn "%A")

    if Array.forall ((=) Correct) results.LetterResults then
        Success results.Word
    else
        let filter = GuessResults.CreateFilter results
        Array.filter filter words |> Ongoing

let solve picker words answer =
    let rec inner words' acc =
        let result = ply picker answer words'

        match result with
        | Ongoing remainingWords -> inner remainingWords (result :: acc)
        | s -> s :: acc

    inner words [] |> List.rev

let randomWord (words: string []) = words[rng.Next words.Length]

let mostCommonLetters (words: string []) =
    let mapping =
        words
        |> Seq.collect id
        |> Seq.countBy id
        |> Map.ofSeq

    words
    |> Array.maxBy (Seq.sumBy (fun c -> mapping[c]))

let leastCommonLetters (words: string []) =
    let mapping =
        words
        |> Seq.collect id
        |> Seq.countBy id
        |> Map.ofSeq

    words
    |> Array.minBy (Seq.sumBy (fun c -> mapping[c]))

let consonants =
    [|'b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'j'; 'k'; 'l'; 'm'; 'n'; 'p'; 'q'; 'r'; 's'; 't'; 'v'; 'w'; 'x'; 'y'; 'z'|]
    |> Set.ofArray

let consonantPreference (words: string[]) =
    let strings = 
        words
        |> Array.groupBy (Seq.sumBy (fun c -> if consonants.Contains c then 1 else 0))
        |> Array.maxBy fst
        |> snd
    strings[rng.Next strings.Length]
    
let vowelPreference (words: string[]) =
    let strings = 
        words
        |> Array.groupBy (Seq.sumBy (fun c -> if consonants.Contains c then 1 else 0))
        |> Array.minBy fst
        |> snd
    strings[rng.Next strings.Length]
    
let combined (words: string[]) =
    if words.Length > 1000 then
        consonantPreference words
    elif words.Length > 10 then
        mostCommonLetters words
    else
        randomWord words
        
let tree (words: string[]) =
    let words = 
        [|0..4|]
        |> Array.fold (fun (remainingWords: string[]) i ->
            let letter = 
                remainingWords
                |> Array.countBy (fun w -> w[i])
                |> Array.maxBy snd
                |> fst
            remainingWords
            |> Array.filter (fun w -> w[i] = letter)) words
    words[rng.Next words.Length]

let words =
    File.ReadAllLines "wordle-words.txt"
    |> Array.Parallel.map (fun s -> s.ToLower())
    |> Array.distinct

let charts () =
    let strategies: (string * (string [] -> string)) [] =
        [| "random", randomWord
//           "first", Array.head
           "most common letters", mostCommonLetters
           "least common letters", leastCommonLetters
           "consonant preference", consonantPreference
//           "vowel preference", vowelPreference
//           "combined", combined
           "tree", tree |]

    let n_runs = 10_000

    let doRun (label, picker) =
        let sw = Stopwatch.StartNew()
        let xs, ys =
            [| 1..n_runs |]
            |> Array.Parallel.map (fun _ ->
                words[rng.Next words.Length]
                |> solve picker words
                |> List.length)
            |> Array.countBy id
            |> Array.unzip

        sw.Stop()
        let successful = Array.zip xs ys |> Array.filter (fst >> (>=) 6) |> Array.sumBy snd |> float
        let percentage = $"%.1f{100.0 * successful / (float n_runs)}%%"
        Console.WriteLine $"%s{label} in %i{sw.ElapsedMilliseconds}ms, %s{percentage}"
        Bar(x = xs, y = ys, name = $"%s{label} (%s{percentage})")

    let sw = Stopwatch.StartNew()

    strategies
    |> Array.Parallel.map doRun
    |> Chart.Plot
    |> Chart.WithLayout(Layout(barmode = "group"))
    |> Chart.WithTitle "Moves to complete"
    |> Chart.WithXTitle "Number of moves"
    |> Chart.WithYTitle "Frequency"
    |> Chart.Show

    sw.Stop()
    printfn $"%i{sw.ElapsedMilliseconds}ms"

charts ()

//let answer = "prick"
//
//solve randomWord words answer |> ignore
//|> List.iter (printfn "%A")
