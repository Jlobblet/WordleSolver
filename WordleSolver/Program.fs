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
    let results = makeGuess picker words answer
    //    printfn $"%A{results}"

    if Array.forall ((=) Correct) results.LetterResults then
        Success results.Word
    else
        let filter = GuessResults.CreateFilter results
        Array.filter filter words |> Ongoing

let solve picker words answer =
    let rec inner words' i =
        match i with
        //        | 6 -> Failed
        | _ ->
            match ply picker answer words' with
            | Ongoing remainingWords -> inner remainingWords (i + 1)
            | s -> i, s

    inner words 0

let words =
    File.ReadAllLines "words.txt"
    |> Array.Parallel.map (fun s -> s.ToLower())
    |> Array.distinct

let randomWord (words: string []) = words[rng.Next words.Length]

let mostCommonLetters (words: string []) =
    let mapping =
        words
        |> Seq.collect id
        |> Seq.countBy id
        |> Map.ofSeq

    words
    |> Array.sortByDescending (Seq.sumBy (fun c -> mapping[c]))
    |> Array.head
    
let leastCommonLetters (words: string []) =
    let mapping =
        words
        |> Seq.collect id
        |> Seq.countBy id
        |> Map.ofSeq

    words
    |> Array.sortBy (Seq.sumBy (fun c -> mapping[c]))
    |> Array.head
    


let strategies: (string * (string [] -> string)) [] =
    [| "random", randomWord
       "first", Array.head
       "most common letters", mostCommonLetters
       "least common letters", leastCommonLetters |]

let n_runs = 1_000

let doRun (label, picker) =
    let xs, ys =
        [| 1 .. n_runs |]
        |> Array.Parallel.map
            (fun _ ->
                words[rng.Next words.Length]
                |> solve picker words
                |> fst)
        |> Array.countBy id
        |> Array.unzip

    Bar(x = xs, y = ys, name = label)

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
