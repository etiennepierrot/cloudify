module CloudImageWriter

open KnowledgePicker.WordCloud
open KnowledgePicker.WordCloud.Primitives
open KnowledgePicker.WordCloud.Sizers
open KnowledgePicker.WordCloud.Drawing
open KnowledgePicker.WordCloud.Layouts
open SkiaSharp
open System.IO

let WriteCloudImage outputPath (occurences : seq<string * int>)  =
    let wordEntries = occurences
                        |> Seq.map (fun (word, count ) -> new WordCloudEntry(word, count))
    let wordCloud = new WordCloudInput(wordEntries)
    wordCloud.Width <- 1024
    wordCloud.Height <- 1024
    wordCloud.MinFontSize <- 1
    wordCloud.MaxFontSize <- 32
    let sizer = new LogSizer(wordCloud)
    use engine = new SkGraphicEngine(sizer, wordCloud)
    let layout = new SpiralLayout(wordCloud)
    let wcg = new WordCloudGenerator<SKBitmap>(wordCloud, engine, layout)
    use bitmap = new SKBitmap(wordCloud.Width, wordCloud.Height)
    use canvas = new SKCanvas(bitmap)
    canvas.Clear(SKColors.White)
    canvas.DrawBitmap(wcg.Draw(), 0F, 0F)
    use data = bitmap.Encode(SKEncodedImageFormat.Png, 100);
    use writer = File.Create outputPath
    data.SaveTo(writer);