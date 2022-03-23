module CodeSourceParser
open System
open System.IO

//http://www.fssnip.net/pi/title/Recursively-find-all-files-from-a-sequence-of-directories
let rec allFiles dirs =
    let isSourceFile (filename:string) = filename.EndsWith(".cs")
    if Seq.isEmpty dirs then Seq.empty else
        seq { 
                yield! dirs |> Seq.collect (Directory.EnumerateFiles >> Seq.filter isSourceFile )
                yield! dirs |> Seq.collect Directory.EnumerateDirectories |> Seq.filter (fun d -> d.ToLowerInvariant().Contains("dynamodb") |> not) |> allFiles 
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

    let isExcludeWord (word :string) =
        if word.Length < 2 then
            true
        else
            
            let builtInWord = [ "tostring";  "task"; "cancellationtoken"; "argumentnullexception";"dictionary"; "iservicollection"; "configuration"; "addsingleton"; "addtransient"; "addscoped"; "assembly"; "list"; "array"; "system"; "ienumerable"; "exception"; "reflection" ]
            let infraWord = [ "dynamodb"; "document"  ]
            let miscWord = [ "ex";  ]
            let toolingWord = [ "metrics"; "imetrics"; "logger"; "ilogger"; "result"; "mediator"; "imediator"; "oneof"; "policyresult"; "throwifnull"; "must" ]
            let dotnetKeyWord =  ["public" ;"private"; "protected"; "internal"; "new"; "return"; "try"; "catch"; "interface"; "readonly"; "await"; "async"; "get"; "set"; "class"; "using"; "namespace"; "var"; "static"; "void";  "if"; "else"; "bool"; "null"; "throw"; "is";  "override"
               ; "object";  "int"; "const";  "out"; "decimal"; "long"; "string"; "ulong"; "dynamic"; "nameof"; "true"; "false";  "this"; "in";  "for"; "case"; "when"; "default"]
            builtInWord @
            toolingWord @
            infraWord @
            miscWord @
            dotnetKeyWord
            |> List.contains (word.ToLowerInvariant())

    let splitLineIntoWords (line :string) = 
        line.Split([|' ';'\n';'\t';',';'.';'/';'\\';'|';':';';';'{';'}'; '(';')'; '['; ']';'='; '<'; '>'; '"'; '?'; '_'; '!'; '&'; '$'|], StringSplitOptions.RemoveEmptyEntries)

    File.ReadAllLines(path) 
    |> Seq.filter isLineToParse
    |> Seq.collect splitLineIntoWords
    |> Seq.filter(isExcludeWord >> not )

let GetWordOccurence path = allFiles [|path|] 
                            |> Seq.collect readFile
                            |> Seq.countBy id
                            |> Seq.sortByDescending snd