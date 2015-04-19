#load "Explore.fsx"
#load "Recognize.fsx"
open FSharp.Data
open System.IO

let folder = @"c:/ws/sensorLogs/training/"
let swipeData = Path.Combine(folder,"swipe.csv")
let tapData   = Path.Combine(folder,"tap.csv")
let leftData  = Path.Combine(folder,"left.csv")
let rightData = Path.Combine(folder,"right.csv")
let twistData = Path.Combine(folder,"twist.csv")
let drivData  = Path.Combine(folder,"driving.csv")

let toEv (x:Explore.SensorInput.Row) = {Recognition.SnsrEvent.Snsr=x.Sensor; Recognition.SnsrEvent.Ticks=x.Ticks; Recognition.SnsrEvent.X=float32 x.X; Recognition.SnsrEvent.Y=float32 x.Y; Recognition.SnsrEvent.Z=float32 x.Z}

let swipeTrain = Explore.SensorInput.Load(swipeData).Rows   |> Seq.map toEv |> Seq.toArray
let tapTrain   = Explore.SensorInput.Load(tapData).Rows     |> Seq.map toEv |> Seq.toArray
let leftTrain  = Explore.SensorInput.Load(leftData).Rows    |> Seq.map toEv |> Seq.toArray
let rightTrain = Explore.SensorInput.Load(rightData).Rows   |> Seq.map toEv |> Seq.toArray
let twistTrain = Explore.SensorInput.Load(twistData).Rows   |> Seq.map toEv |> Seq.toArray
let drivTrain  = Explore.SensorInput.Load(drivData).Rows    |> Seq.map toEv |> Seq.toArray

open FSM
open Recognition

let navigationSM cfg = F(Navigation.initialQuite cfg , None)
let twistSM cfg = F(Twist.start cfg, None)

let countEvent e m = match Map.tryFind e m with Some c -> m |> Map.add e (c+1) | _ -> m |> Map.add e 1

let evaluate stateMachine trainingSet fitness = 
    let _,m = ((stateMachine,Map.empty),trainingSet) ||> Seq.fold (fun (F (sm,_),m) e ->
        match sm e with
        | F(sm, Some e) -> F (sm,None), countEvent e m
        | F(sm, None)   -> F (sm,None), m)
    fitness m

let fitnessEvent event m =  (0,m)  ||> Map.fold (fun acc k v -> if k = event then 10 - abs(10 - v) else -v)
let fitnessDriving  m    = -((0,m) ||> Map.fold (fun acc _ v -> acc+v))

let evalSwipe cfg = evaluate (navigationSM cfg) swipeTrain (fitnessEvent RE_Swipe)
let evalTap cfg   = evaluate (navigationSM cfg) tapTrain   (fitnessEvent RE_Tap)
let evalLeft cfg  = evaluate (navigationSM cfg) leftTrain  (fitnessEvent RE_Left)
let evalRight cfg = evaluate (navigationSM cfg) rightTrain (fitnessEvent RE_Right)
let evalTwist cfg = evaluate (twistSM cfg)      twistTrain (fitnessEvent RE_Twist)
let evalDriv1 cfg = evaluate (twistSM cfg)      drivTrain  fitnessDriving
let evalDriv2 cfg = evaluate (navigationSM cfg) drivTrain  fitnessDriving

(*
//baseline
let yourself x = x:Map<RecognizedEvents,int>
let cfgN = Navigation.NavigationCfg.Default
let cfgT = Twist.TwistCfg.Default
let baseSwipe = evaluate (navigationSM cfgN) swipeTrain yourself
let baseTap   = evaluate (navigationSM cfgN) tapTrain   yourself
let baseLeft  = evaluate (navigationSM cfgN) leftTrain  yourself
let baseRight = evaluate (navigationSM cfgN) rightTrain yourself
let baseTwist = evaluate (twistSM cfgT)      twistTrain yourself
let baseDriv1 = evaluate (twistSM cfgT)      drivTrain  yourself
let baseDriv2 = evaluate (navigationSM cfgN) drivTrain  yourself
*)

(* testing fitness
evalSwipe   Navigation.NavigationCfg.Default
evalTap     Navigation.NavigationCfg.Default
evalLeft    Navigation.NavigationCfg.Default
evalRight   Navigation.NavigationCfg.Default
evalTwist   Twist.TwistCfg.Default
evalDriv1   Twist.TwistCfg.Default
evalDriv2   Navigation.NavigationCfg.Default
*)
