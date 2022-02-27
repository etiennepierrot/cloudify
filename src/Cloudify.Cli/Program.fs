open CloudImageWriter
open CodeSourceParser


[<EntryPoint>]
let main(args :string[]) =
    args.[0]
    |> (GetWordOccurence >> WriteCloudImage "output.png")
    0