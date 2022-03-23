open CloudImageWriter
open CodeSourceParser


[<EntryPoint>]
let main(args :string[]) =
    
    let parseIntoCloud = GetWordOccurence >> WriteCloudImage "output.png"
    let displayWord (word :string, count:int) = printfn $"%s{word} - %d{count}"
    args.[0]
    |> parseIntoCloud
//    |> GetWordOccurence
//    |> Seq.take 100
//    |> Seq.iter displayWord
    0