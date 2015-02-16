(*[omit:(Opening declarations)]*)
open System.Drawing
open System.Windows.Forms
#r "System.Windows.Forms.DataVisualization"
open System.Windows.Forms.DataVisualization.Charting
(*[/omit]*)

// Based on a code by James Hugard, available here: 
// http://stackoverflow.com/questions/3276357/how-do-i-plot-a-data-series-in-f 

// Create a window with Chart control and initializes
// the chart control using a given function
type Visualiser(title, style, init_fun:Chart->unit) =
  inherit Form( Text=title )
  let chart = new Chart(Dock=DockStyle.Fill)
  let area = new ChartArea(Name=title)
  let series = new Series()
  do series.ChartType <- style
  do series.ChartArea <- title
  do chart.Series.Add(series)
  do chart.ChartAreas.Add(area)
  do init_fun chart
  do base.Controls.Add(chart)
  
// Visualises a series of float values
// Returns a Form
let SeqVisualiser title style (xs: float seq) = 
  new Visualiser(title,style,fun chart -> xs |> Seq.iter (chart.Series.[0].Points.Add >> ignore))

// Visualizes a series of float pairs treated as (x,y) coordinats
// Returns a Form
let PointVisualiser title style xs =
  new Visualiser(title,style,
                  fun chart -> 
                   xs |> Seq.iter (fun (x,y) -> chart.Series.[0].Points.AddXY(x,y)|>ignore))

// Visualizes a series of float values with labels
let LabelVisualiser3D title style xs =
  new Visualiser(title,style,
   fun chart -> 
       chart.ChartAreas.[0].Area3DStyle.Enable3D <- true
       chart.ChartAreas.[0].Area3DStyle.Perspective <- 10
       xs |> Seq.iter (fun (y:string,x:float) -> 
                         let pt = chart.Series.[0].Points.Add(x)
                         pt.Label <- y))

let domain = seq {-6.0..0.01..6.0}

// Plotting sin function
let plot f =
  let vis = SeqVisualiser "Data" (SeriesChartType.Line) (domain |> Seq.map f)
  vis.Show()

let V1 = SeqVisualiser "Data" (SeriesChartType.Line) (Seq.zip domain domain |> Seq.map sin)
V1.Show()

// Plotting bars with random values
let V2 = SeqVisualiser "Data" SeriesChartType.Bar 
           (seq { let R = new System.Random()
                 for i in [1..10] -> R.NextDouble()*3.0 })
V2.Show()
 
// Plotting a number or random (x,y)-points
let V3 = PointVisualiser "Data" SeriesChartType.Bubble
           (seq { let R = new System.Random()
                  for i in [1..10] -> (R.NextDouble()*3.0,R.NextDouble()*3.0) })
V3.Show()

// Plotting some statistics about occurence of strings in HTML page
(*[omit:(Opening declarations)]*)
open System.IO
open System.Text.RegularExpressions
(*[/omit]*)

let http(url: string) = (*[omit:(fetch HTML from given URL)]*)
    let req    = System.Net.WebRequest.Create(url) 
    use resp   = req.GetResponse()         // note 'use' = C# 'using'
    use stream = resp.GetResponseStream() 
    use reader = new StreamReader(stream) 
    let html   = reader.ReadToEnd()
    html
(*[/omit]*)
let page = http "http://osys.ru/"
let count s page = Regex.Matches(page,s).Count
let os = ["Windows";"UNIX";"Linux";"DOS"] |> Seq.map (fun s -> (s,float(count s page)))

let V4 = LabelVisualiser3D "OS" SeriesChartType.Pie os
V4.Show()
