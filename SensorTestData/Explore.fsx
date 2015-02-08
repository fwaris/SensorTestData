#r @"..\packages\FSharp.Data.2.1.0\lib\net40\FSharp.Data.dll"
#load @"..\packages\FSharp.Charting.0.90.9\Fsharp.Charting.fsx"
#load "KMeansClustering.fs"
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
let rng = System.Random()
type Rec = {Sensor:int; X:float; Y:float; Z:float; Omega:float option}
let X x = x.X
let Y x = x.Y
let Z x = x.Z
let O x = x.Omega.Value

let distance3 (s1:Rec) (s2:Rec) =
    abs(s1.X - s2.X)
    + abs(s1.Y - s2.Y)
    + abs(s1.Z - s2.Z)

let distance4 (s1:Rec) (s2:Rec) =
    abs(s1.X - s2.X)
    + abs(s1.Y - s2.Y)
    + abs(s1.Z - s2.Z)
    + abs(s1.Omega.Value - s2.Omega.Value)

let records sensor = 
     f.Rows
     |> Seq.filter (fun r->r.Sensor=sensor) 
     |> Seq.map (fun (x:TwistGame.Row)->{Sensor=x.Sensor; X=x.X; Y=x.Y;Z=x.Z;Omega=x.Omega})

let avgCentroid3 current sample =
    let size = Seq.length sample |> float
    match size with
    | 0. -> current
    | _ ->
        sample
        |> Seq.reduce (fun v1 v2 -> {v1 with X=v1.X+v2.X; Y=v1.Y+v2.Y; Z=v1.Z+v2.Z})
        |> fun e -> {e with X= float e.X/size;Y=e.Y/size;Z=e.Z/size}

let avgCentroid4 current sample =
    let size = Seq.length sample |> float
    match size with
    | 0. -> current
    | _ ->
        sample
        |> Seq.reduce (fun v1 v2 -> {v1 with X=v1.X+v2.X; Y=v1.Y+v2.Y; Z=v1.Z+v2.Z; Omega=Some (v1.Omega.Value + v2.Omega.Value)})
        |> fun e -> {e with X= float e.X/size;Y=e.Y/size;Z=e.Z/size; Omega=Some(e.Omega.Value/size)}

let cluster3 numCentroids sensor (data:TwistGame)= 
    let factory = KMeans.randomCentroids<Rec> rng
    let d2 = records sensor
    let centroids, classifier = KMeans.kmeans distance3 factory avgCentroid3 d2 numCentroids
    centroids, classifier

let cluster4 numCentroids sensor (data:TwistGame)= 
    let factory = KMeans.randomCentroids<Rec> rng
    let d2 = records sensor
    let centroids, classifier = KMeans.kmeans distance4 factory avgCentroid4 d2 numCentroids
    centroids, classifier

let plotCluster title centroids data fx fy =
    let data = data |> Seq.map (fun x -> fx x, fy x)
    let centroids = centroids |> Seq.map (fun x -> fx x, fy x)
    Chart.Combine 
        [
            Chart.Point(data,Name="Data")
            Chart.Point(centroids,Name="Centroids",MarkerColor=System.Drawing.Color.IndianRed,MarkerSize=10)
        ]
    |> Chart.WithTitle title
    |> Chart.WithLegend (Enabled=true)


(*
let csGyro,_ = cluster3 3 Gyroscope f
plotCluster "Gyroscope X-Y" csGyro (records Gyroscope) X Y
plotCluster "Gyroscope Y-Z" csGyro (records Gyroscope) Y Z
plotCluster "Gyroscope Z-X" csGyro (records Gyroscope) Z X
*)
(*
let csGravity,_ = cluster3 3 Gravity f
plotCluster "Gravity X-Y" csGravity (records Gravity) X Y
plotCluster "Gravity Y-Z" csGravity (records Gravity) Y Z
plotCluster "Gravity Z-X" csGravity (records Gravity) Z X
*)
(*
let csRV,_ = cluster4 3 RotationVector f
plotCluster "RotationVector X-Y" csRV (records RotationVector) X Y
plotCluster "RotationVector Y-Z" csRV (records RotationVector) Y Z
plotCluster "RotationVector Z-X" csRV (records RotationVector) Z X
plotCluster "RotationVector X-O" csRV (records RotationVector) X O
plotCluster "RotationVector Y-O" csRV (records RotationVector) Y O
plotCluster "RotationVector Z-O" csRV (records RotationVector) Z O
*)

