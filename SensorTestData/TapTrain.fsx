#load "Explore.fsx"
#load "Recognize.fsx"
open FSharp.Data

//let trainData = @"C:/ws\sensorLogs\training\tap_20150323_205305.csv"
//let trainData = @"C:/ws\sensorLogs\training\left_20150323_205204.csv"
//let trainData = @"c:/ws\sensorLogs\training\right_20150323_205135.csv"
//let trainData = @"C:/ws\sensorLogs\training\swipe_20150323_205232.csv"
//let trainData = @"C:/ws\sensorLogs\training\twist_20150323_205343.csv"
let trainData = @"C:/ws\sensorLogs\training\driving_20150323_222420.csv"
//let trainData = @"C:/ws\sensorLogs\training\twist_20150323_205343.csv"

let toEv (x:Explore.SensorInput.Row) = {Recognition.SnsrEvent.Snsr=x.Sensor; Recognition.SnsrEvent.Ticks=x.Ticks; Recognition.SnsrEvent.X=float32 x.X; Recognition.SnsrEvent.Y=float32 x.Y; Recognition.SnsrEvent.Z=float32 x.Z}

let tap = Explore.SensorInput.Load(trainData).Rows |> Seq.map toEv
open FSM
open Recognition

let navigation  = F(Navigation.initialQuite Navigation.NavigationCfg.Default , None)
let tw = F(Twist.start Twist.TwistCfg.Default, None)

let countEvent e m = match Map.tryFind e m with Some c -> m |> Map.add e (c+1) | _ -> m |> Map.add e 1

let _,m = ((navigation,Map.empty),tap) ||> Seq.fold (fun (F (sm,_),m) e ->
    match sm e with
    | F(sm, Some e) -> F (sm,None), countEvent e m
    | F(sm, None)   -> F (sm,None), m)

