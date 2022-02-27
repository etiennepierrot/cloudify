module CodeSourceParser
open System
open System.IO

//http://www.fssnip.net/pi/title/Recursively-find-all-files-from-a-sequence-of-directories
let rec allFiles dirs =
    let isSourceFile (filename:string) = filename.EndsWith(".cs")
    if Seq.isEmpty dirs then Seq.empty else
        seq { 
                yield! dirs |> Seq.collect (Directory.EnumerateFiles >> Seq.filter isSourceFile )
                yield! dirs |> Seq.collect Directory.EnumerateDirectories |> allFiles 
            } 
let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let readFile path =
    let isLineToParse (line :string)  =
        match line.TrimStart() with 
        | "" -> false
        | Prefix @"//" rest -> false
        | Prefix "using" rest -> false
        | _ -> true  

    let isExcludeWord word = 
        [|"public" ;"private"; "protected"; 
        "new"; "return"; "readonly"; "await"; "async"; "get"; "set"; "class"; "using"; "namespace"; "var"; "static"; "void";
        "task"; "cancellationtoken"; 
        "if"; "bool"; 
        "string"; "ulong"; "dynamic"|] 
        |> Array.contains word

    let splitLineIntoWords (line :string) = 
        line.Split([|' ';'\n';'\t';',';'.';'/';'\\';'|';':';';';'{';'}'; '(';')'; '['; ']';'='; '<'; '>'; '"'|], StringSplitOptions.RemoveEmptyEntries)

    File.ReadAllLines(path) 
    |> Seq.map( fun x -> x.ToLowerInvariant())
    |> Seq.filter isLineToParse
    |> Seq.collect splitLineIntoWords
    |> Seq.filter(isExcludeWord >> not )

let GetWordOccurence path = allFiles [|path|] 
                            |> Seq.collect readFile
                            |> Seq.countBy id
