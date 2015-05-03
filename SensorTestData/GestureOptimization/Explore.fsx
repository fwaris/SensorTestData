#r @"..\..\packages\FSharp.Data.2.1.1\lib\net40\FSharp.Data.dll"
#load @"..\..\packages\FSharp.Charting.0.90.9\Fsharp.Charting.fsx"
#load "..\KMeansClustering.fs"
open FSharp.Data
open System.IO

[<Literal>] 
let schema = "Ticks(int64), Sensor(int), Count(int), X (float), Y(float), Z(float), Omega (float option)"

let Accelerometer       = 1
let Gravity             = 9
let Gyroscope           = 4
let LinearAcceleration  = 10
let RotationVector      = 11

//let driving = @"C:\ws\sensorLogs\driving_20141230_204949.csv"
//let twist   = @"C:\ws\sensorLogs\twist_20141231_144058.csv"
let tap = @"c:/ws\sensorLogs\training_sony\tap.csv"
let tapSm = @"c:/ws\sensorLogs\training_samsung\tap.csv"
let swipe = @"c:/ws\sensorLogs\training_sony\swipe.csv"
let swipeSm = @"c:/ws\sensorLogs\training_samsung\swipe.csv"
let twist = @"c:/ws\sensorLogs\training_sony\twist.csv"
let twistsm = @"c:/ws\sensorLogs\training_samsung\twist.csv"
let left = @"c:/ws\sensorLogs\training_sony\left.csv"
let leftSm = @"c:/ws\sensorLogs\training_samsung\left.csv"
let right = @"c:/ws\sensorLogs\training_sony\right.csv"
let rightSm = @"c:/ws\sensorLogs\training_samsung\right.csv"

type SensorInput= CsvProvider<Schema=schema, HasHeaders=false>
//let f = SensorInput.Load(tap1)
//f.Rows |> Seq.take 1
//
//f.Rows |> Seq.map (fun x -> x.Sensor) |> Seq.distinct |> Seq.toArray

let showSensor sensor (f:SensorInput)  = f.Rows |> Seq.filter (fun r -> r.Sensor = sensor) |> Seq.iter (printfn "%A")
let countSensors (f:SensorInput) = f.Rows |> Seq.countBy (fun r->r.Sensor) |> Seq.iter (printfn "%A")

let rng = System.Random()
type Rec = {Sensor:int; Ticks:int64; X:float32; Y:float32; Z:float32; Omega:float32 option}
let X x = x.X
let Y x = x.Y
let Z x = x.Z
let O x = x.Omega.Value

let toRec (x:SensorInput.Row) = {Sensor=x.Sensor; Ticks=x.Ticks; X=float32 x.X; Y=float32 x.Y;Z=float32 x.Z;Omega= x.Omega |> Option.map (fun x -> float32 x)}

let records sensor (f:SensorInput) = 
     f.Rows
     |> Seq.filter (fun r->r.Sensor=sensor) 
     |> Seq.map toRec

(*
let d = SensorInput.Load(swipe)
let d = SensorInput.Load(swipeSm)
let d = SensorInput.Load(twist)
let d = SensorInput.Load(twistSm)
let d = SensorInput.Load(tap)
let d = SensorInput.Load(tapSm)
let d = SensorInput.Load(left)
let d = SensorInput.Load(leftSm)
let d = SensorInput.Load(right)
let d = SensorInput.Load(rightSm)

showSensor Gravity d
showSensor Accelerometer d
showSensor LinearAcceleration d
showSensor RotationVector d
showSensor Gyroscope d
*)

let showTimeDiffs (f:SensorInput) sensor =
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
*)

open FSharp.Charting
(*
*)
let plot3Y title (xs:(int64*float32)seq) (ys:(int64*float32)seq) (zs:(int64*float32)seq) =
    Chart.Combine 
        [
            Chart.Point(xs,Name="X")
            Chart.Point(ys,Name="Y")
            Chart.Point(zs,Name="Z")
        ]
    |> Chart.WithTitle title
    |> Chart.WithLegend (Enabled=true)

let plot3YL title (xs:(int64*float32)seq) (ys:(int64*float32)seq) (zs:(int64*float32)seq) =
    Chart.Combine 
        [
            Chart.Line(xs,Name="X")
            Chart.Line(ys,Name="Y")
            Chart.Line(zs,Name="Z")
        ]
    |> Chart.WithTitle title
    |> Chart.WithXAxis(MinorGrid=ChartTypes.Grid(Enabled=true))
    |> Chart.WithLegend (Enabled=true)

let plot4Y
    title 
    (xs:(int64*float32)seq) 
    (ys:(int64*float32)seq) 
    (zs:(int64*float32)seq)
    (ws:(int64*float32)seq) =    
    Chart.Combine 
        [
            Chart.Point(xs,Name="X")
            Chart.Point(ys,Name="Y")
            Chart.Point(zs,Name="Z")
            Chart.Point(ws,Name="w")
        ]
    |> Chart.WithTitle title
    |> Chart.WithLegend (Enabled=true)

let plot sensor title (data:SensorInput) =
    let d2 = records sensor data
    let ft = (d2 |> Seq.head).Ticks
    let xs = d2 |> Seq.map (fun r->r.Ticks - ft,r.X) |> Seq.toArray
    let ys = d2 |> Seq.map (fun r->r.Ticks - ft,r.Y) |> Seq.toArray
    let zs = d2 |> Seq.map (fun r->r.Ticks - ft,r.Z) |> Seq.toArray
    plot3Y title xs ys zs

let plotL sensor title (data:SensorInput) =
    let d2 = records sensor data
    let ft = (d2 |> Seq.head).Ticks
    let xs = d2 |> Seq.map (fun r->r.Ticks - ft,r.X) |> Seq.toArray
    let ys = d2 |> Seq.map (fun r->r.Ticks - ft,r.Y) |> Seq.toArray
    let zs = d2 |> Seq.map (fun r->r.Ticks - ft,r.Z) |> Seq.toArray
    plot3YL title xs ys zs

let plotW sensor title (data:SensorInput) =
    let d2 = records sensor data
    let ft = (d2 |> Seq.head).Ticks
    let xs = d2 |> Seq.map (fun r->r.Ticks - ft,r.X) |> Seq.toArray
    let ys = d2 |> Seq.map (fun r->r.Ticks - ft,r.Y) |> Seq.toArray
    let zs = d2 |> Seq.map (fun r->r.Ticks - ft,r.Z) |> Seq.toArray
    let ws = d2 |> Seq.map (fun r->r.Ticks - ft,r.Omega.Value) |> Seq.toArray
    plot4Y title xs ys zs ws

(*
let d = SensorInput.Load(swipe)
let d = SensorInput.Load(swipeSm)
let d = SensorInput.Load(twist)
let d = SensorInput.Load(twistSm)
let d = SensorInput.Load(tap)
let d = SensorInput.Load(tapSm)
let d = SensorInput.Load(left)
let d = SensorInput.Load(leftSm)
let d = SensorInput.Load(right)
let d = SensorInput.Load(rightSm)

let d = SensorInput.Load(@"c:\ws\sensorLogs\sony\swipe.csv")
let d = SensorInput.Load(@"c:\ws\sensorLogs\sony\tap.csv")

plot Gravity "Gravity"  d
plot Accelerometer "Accelerometer" d
plotL Accelerometer "Accelerometer" d
plot LinearAcceleration "Linear Acceleration" d
plotL LinearAcceleration "Linear Acceleration" d
plotW RotationVector "Rotation Vector" d
plot RotationVector "Rotation Vector" d
plot Gyroscope "Gyroscope" d
*)

let distance3 (s1:Rec) (s2:Rec) =
    abs(s1.X - s2.X)
    + abs(s1.Y - s2.Y)
    + abs(s1.Z - s2.Z) |> float

let distance4 (s1:Rec) (s2:Rec) =
    abs(s1.X - s2.X)
    + abs(s1.Y - s2.Y)
    + abs(s1.Z - s2.Z)
    + abs(s1.Omega.Value - s2.Omega.Value) |> float

let avgCentroid3 current sample =
    let size = Seq.length sample |> float32
    match size with
    | 0.f -> current
    | _ ->
        sample
        |> Seq.reduce (fun v1 v2 -> {v1 with X=v1.X+v2.X; Y=v1.Y+v2.Y; Z=v1.Z+v2.Z})
        |> fun e -> {e with X= float32 e.X/size;Y=e.Y/size;Z=e.Z/size}

let avgCentroid4 current sample =
    let size = Seq.length sample |> float32
    match size with
    | 0.f -> current
    | _ ->
        sample
        |> Seq.reduce (fun v1 v2 -> {v1 with X=v1.X+v2.X; Y=v1.Y+v2.Y; Z=v1.Z+v2.Z; Omega=Some (v1.Omega.Value + v2.Omega.Value)})
        |> fun e -> {e with X= float32 e.X/size;Y=e.Y/size;Z=e.Z/size; Omega=Some(e.Omega.Value/size)}

let cluster3 numCentroids sensor (data:SensorInput)= 
    let factory = KMeans.randomCentroids<Rec> rng
    let d2 = records sensor data
    let centroids, classifier = KMeans.kmeans distance3 factory avgCentroid3 d2 numCentroids
    centroids, classifier

let cluster4 numCentroids sensor (data:SensorInput)= 
    let factory = KMeans.randomCentroids<Rec> rng
    let d2 = records sensor data
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
plotCluster "Gyroscope X-Y" csGyro (records Gyroscope f) X Y
plotCluster "Gyroscope Y-Z" csGyro (records Gyroscope f) Y Z
plotCluster "Gyroscope Z-X" csGyro (records Gyroscope f) Z X
*)
(*
let csGravity,_ = cluster3 3 Gravity f
plotCluster "Gravity X-Y" csGravity (records Gravity f) X Y
plotCluster "Gravity Y-Z" csGravity (records Gravity f) Y Z
plotCluster "Gravity Z-X" csGravity (records Gravity f) Z X
*)
(*
let csRV,_ = cluster4 3 RotationVector f
plotCluster "RotationVector X-Y" csRV (records RotationVector f) X Y
plotCluster "RotationVector Y-Z" csRV (records RotationVector f) Y Z
plotCluster "RotationVector Z-X" csRV (records RotationVector f) Z X
plotCluster "RotationVector X-O" csRV (records RotationVector f) X O
plotCluster "RotationVector Y-O" csRV (records RotationVector f) Y O
plotCluster "RotationVector Z-O" csRV (records RotationVector f) Z O
*)

