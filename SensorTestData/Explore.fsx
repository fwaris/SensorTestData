#r @"..\packages\FSharp.Data.2.1.0\lib\net40\FSharp.Data.dll"
#load @"..\packages\FSharp.Charting.0.90.9\Fsharp.Charting.fsx"

open FSharp.Data
open System.IO

let Accelerometer       = 1
let Gravity             = 9
let Gyroscope           = 4
let LinearAcceleration  = 10
let RotationVector      = 11

let driving = @"C:\ws\sensorLogs\driving_20141230_204949.csv"
let twist   = @"C:\ws\sensorLogs\twist_20141231_144058.csv"
[<Literal>] 
let schema = "Ticks(int64), Sensor(int), Count(int), X (float), Y(float), Z(float), Omega (float option)"


type TwistGame= CsvProvider<Schema=schema, HasHeaders=false>
let f = TwistGame.Load(twist)
f.Rows |> Seq.take 1

f.Rows |> Seq.map (fun x -> x.Sensor) |> Seq.distinct |> Seq.toArray

let showSensor sensor = f.Rows |> Seq.filter (fun r -> r.Sensor = sensor) |> Seq.iter (printfn "%A")
let countSensors() = f.Rows |> Seq.countBy (fun r->r.Sensor) |> Seq.iter (printfn "%A")

(*
showSensor Gravity
showSensor Accelerometer
showSensor LinearAcceleration
showSensor RotationVector
showSensor Gyroscope
showSensor Orientation
*)

let showTimeDiffs sensor =
    f.Rows
    |> Seq.filter (fun r -> r.Sensor = sensor)
    |> Seq.windowed 2 
    |> Seq.map (fun xs -> xs.[1].Ticks - xs.[0].Ticks)
    |> Seq.map (fun x -> float x / 1000000.)
    |> Seq.iter (printfn "%0.2f")

(*
showTimeDiffs Gravity
showTimeDiffs Accelerometer
showTimeDiffs LinearAcceleration
showTimeDiffs RotationVector
showTimeDiffs Gyroscope
showTimeDiffs Orientation
*)

open FSharp.Charting
(*
*)
let plot3Y title (xs:(int64*float)seq) (ys:(int64*float)seq) (zs:(int64*float)seq) =
    Chart.Combine 
        [
            Chart.Line(xs,Name="X")
            Chart.Line(ys,Name="Y")
            Chart.Line(zs,Name="Z")
        ]
    |> Chart.WithTitle title
    |> Chart.WithLegend (Enabled=true)

let plot4Y 
    title 
    (xs:(int64*float)seq) 
    (ys:(int64*float)seq) 
    (zs:(int64*float)seq)
    (ws:(int64*float)seq) =    
    Chart.Combine 
        [
            Chart.Line(xs,Name="X")
            Chart.Line(ys,Name="Y")
            Chart.Line(zs,Name="Z")
            Chart.Line(ws,Name="w")
        ]
    |> Chart.WithTitle title
    |> Chart.WithLegend (Enabled=true)

let plot sensor title (data:TwistGame) =
    let d2 = data.Rows |> Seq.filter (fun r -> r.Sensor = sensor)
    let xs = d2 |> Seq.map (fun r->r.Ticks,r.X) |> Seq.toArray
    let ys = d2 |> Seq.map (fun r->r.Ticks,r.Y) |> Seq.toArray
    let zs = d2 |> Seq.map (fun r->r.Ticks,r.Z) |> Seq.toArray
    plot3Y title xs ys zs

let plotW sensor title (data:TwistGame) =
    let d2 = data.Rows |> Seq.filter (fun r -> r.Sensor = sensor)
    let xs = d2 |> Seq.map (fun r->r.Ticks,r.X) |> Seq.toArray
    let ys = d2 |> Seq.map (fun r->r.Ticks,r.Y) |> Seq.toArray
    let zs = d2 |> Seq.map (fun r->r.Ticks,r.Z) |> Seq.toArray
    let ws = d2 |> Seq.map (fun r->r.Ticks,r.Omega.Value) |> Seq.toArray
    plot4Y title xs ys zs ws

(*
plot Gravity "Gravity" f
plot Accelerometer "Accelerometer" f
plot LinearAcceleration "Linear Acceleration" f
plotW RotationVector "Rotation Vector" f
plot RotationVector "Rotation Vector" f
plot Gyroscope "Gyroscope" f
*)

