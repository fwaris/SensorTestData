#load "Explore.fsx"
#load "Recognize.fsx"
open FSharp.Data
open System.IO

//let folder = @"c:/ws/sensorLogs/training_samsung/"
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

(* testing updated fitness
open Navigation
let cfg = 
  {xyz_accel_quite_limit = 1.16620791f;
   TapConfig = {gstr_time_limit = 101890680L;
                y_accel_front_thrld = 2.6929369f;
                y_accel_back_thrld = 1.3310101f;
                y_accel_avg_front_thrld = 4.06539774f;
                y_accel_avg_back_thrld = 3.5772686f;
                xz_accel_tolerance = 0.881871462f;
                avg_over_count = 7.0f;
                xy_rot_tolerance = 1.743325f;
                z_rot_tolerance = 2.33148289f;
                TapCfg = {gstr_time_limit = 163440372L;
                          time_limit_to_reach_low_accel = 469021986L;
                          low_y_accel_limit = 3.9340148f;
                          x_accel_tolerance = 1.4137789f;
                          z_accel_tolerance = 1.44362843f;
                          ret_x_accel_tolerance = 3.54649019f;
                          ret_z_accel_tolerance = 2.69347501f;
                          xy_rot_tolerance = 2.70938659f;
                          z_rot_tolerance = 2.22077751f;};};
   LRConfig = {x_grav_high_limit = 7.36571312f;
               x_grav_low_limit = 2.58273578f;
               z_grav_zero_tolerance = 0.586463332f;
               z_grav_Left_thrsld = -4.76018095f;
               z_grav_right_thrsld = 6.91672707f;};
   SwipeConfig = {z_accel_front_thrsld = 1.70534348f;
                  z_accel_back_thrsld = -2.7296102f;
                  z_accel_avg_front_thrsld = 3.93321824f;
                  z_accel_avg_back_thrsld = -2.67512584f;
                  xy_accel_tolerance = 3.3374567f;
                  avg_over_count = 1.0f;
                  gstr_time_limit = 324103250L;
                  xz_rot_tolerance = 1.05284703f;
                  y_rot_tolerance = 0.395497471f;
                  SwipeCfg = {gstr_time_limit = 554492926L;
                              x_accel_tolerance = 1.42294478f;
                              y_accel_tolerance = 1.26815343f;
                              ret_x_accel_tolerance = 1.97864759f;
                              ret_y_accel_tolerance = 0.903666735f;
                              xz_rot_tolerance = 1.55496728f;
                              y_rot_tolerance = 3.2753396f;
                              low_z_accel_limit = 3.21115351f;};};}

let cfg = 
  {xyz_accel_quite_limit = 0.919411957f;
   TapConfig = {gstr_time_limit = 15163268L;
                y_accel_front_thrld = 0.804343998f;
                y_accel_back_thrld = 3.92195177f;
                y_accel_avg_front_thrld = 2.95477939f;
                y_accel_avg_back_thrld = 0.928464472f;
                xz_accel_tolerance = 0.342303216f;
                avg_over_count = 3.0f;
                xy_rot_tolerance = 4.48053265f;
                z_rot_tolerance = 2.34155917f;
                TapCfg = {gstr_time_limit = 757525849L;
                          time_limit_to_reach_low_accel = 68345562L;
                          low_y_accel_limit = 1.77510369f;
                          x_accel_tolerance = 0.455026031f;
                          z_accel_tolerance = 0.786106646f;
                          ret_x_accel_tolerance = 0.770389616f;
                          ret_z_accel_tolerance = 4.54573059f;
                          xy_rot_tolerance = 2.17461944f;
                          z_rot_tolerance = 3.28895926f;};};
   LRConfig = {x_grav_high_limit = 3.10088658f;
               x_grav_low_limit = 1.48582661f;
               z_grav_zero_tolerance = 4.24221516f;
               z_grav_Left_thrsld = -0.599535942f;
               z_grav_right_thrsld = 0.0866801292f;};
   SwipeConfig = {z_accel_front_thrsld = 3.7412312f;
                  z_accel_back_thrsld = -1.50900674f;
                  z_accel_avg_front_thrsld = 2.47797418f;
                  z_accel_avg_back_thrsld = -1.28357196f;
                  xy_accel_tolerance = 3.8151319f;
                  avg_over_count = 4.0f;
                  gstr_time_limit = 369066073L;
                  xz_rot_tolerance = 1.33671188f;
                  y_rot_tolerance = 4.02579069f;
                  SwipeCfg = {gstr_time_limit = 833597303L;
                              x_accel_tolerance = 2.97079945f;
                              y_accel_tolerance = 1.77923357f;
                              ret_x_accel_tolerance = 2.09107447f;
                              ret_y_accel_tolerance = 1.75464177f;
                              xz_rot_tolerance = 4.90125132f;
                              y_rot_tolerance = 2.22707582f;
                              low_z_accel_limit = 1.15894961f;};};}

evalSwipe   cfg
evalTap     cfg
evalLeft    cfg
evalRight   cfg
evalDriv2   cfg
*)
