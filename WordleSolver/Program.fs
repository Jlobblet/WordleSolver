open System
open System.Diagnostics
open System.IO

let (<&&>) f g = fun x -> f x && g x
let tap a x = a x; x

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
            | DifferentPlace -> word.Contains character && word[index] <> character
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
      LetterResults: LetterResult[] }
    member this.StructuredFormatDisplay =
        let blocks = String.concat "" (Array.map (fun lr -> lr.ToString()) this.LetterResults)
        $"%s{this.Word} | %s{blocks}"
    static member CreateFilter { Word = word; LetterResults = letterResults } =
        Array.zip (word.ToCharArray()) letterResults
        |> Array.mapi LetterResult.CreateFilter
        |> Array.reduce (<&&>)

type PlyResult =
    | Failed
    | Success of string
    | Ongoing of string[]
        
let makeGuess (words: string[]) (answer: string) =
    let guess = words[rng.NextInt64 words.LongLength |> int]
    let letterResults =
        guess.ToCharArray()
        |> Array.mapi (LetterResult.FromGuess answer)
        
    { Word = guess
      LetterResults = letterResults }

let ply answer words =
    let results = makeGuess words answer
    printfn $"%A{results}"
    
    if Array.forall ((=) Correct) results.LetterResults then
        Success results.Word
    else
        let filter = GuessResults.CreateFilter results
        Array.filter filter words
        |> Ongoing
    
let solve answer words =
    let rec inner words' i =
        match i with
        | 6 -> Failed
        | _ ->
            match ply answer words' with
            | Ongoing remainingWords -> inner remainingWords (i + 1)
            | s -> s
        
    inner words 0

let words =
    File.ReadAllLines "words.txt"
    |> Array.map (fun s -> s.ToLower())
    |> Array.distinct

let answer = words[rng.NextInt64 words.LongLength |> int]
printfn $"Answer is: %s{answer}"
let sw = Stopwatch.StartNew()
solve answer words |> ignore
sw.Stop()
printfn $"Solved in %i{sw.ElapsedMilliseconds}ms"

