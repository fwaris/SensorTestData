#load "CA_SensorMapping.fsx"
open CA
open TapTrain

//http://en.wikipedia.org/wiki/Nonlinear_programming
// maximize x + y
// st. 1 <= x**2 + y**2 <= 2.
let fitness (parms:Parm array) = 
    let cfg = CA_SensorMapping.tocfg parms
    let fL = float <| evalLeft cfg 
    let fR = float <| evalRight cfg
    let fS = float <| evalSwipe cfg
    let fT = float <| evalTap cfg
    let fD = float <| evalDriv2 cfg
    let total = fL + fR + fS + fT + (0.2 * fD)
    let diffs = 
        abs (fL - fR) + abs(fR - fS) + abs (fS - fT) + 
        abs (fL - fS) + abs (fR - fT) + abs (fL - fT)
    3. * total - diffs

let comparator  = CAUtils.Maximize
let beliefSpace = CARunner.defaultBeliefSpace CA_SensorMapping.parms comparator fitness
//let beliefSpace = Leaf (SituationalKS.create comparator 5)
let parms2 = 
  [|F32 (1.4938637f,0.100000001f,2.0f); I64 (583490062L,1000000L,1000000000L);
    F32 (2.70222688f,0.100000001f,5.0f); F32 (1.74108469f,0.100000001f,5.0f);
    F32 (1.24265194f,0.100000001f,5.0f); F32 (0.861593187f,0.100000001f,5.0f);
    I (3,1,10); F32 (1.89575446f,0.100000001f,5.0f);
    F32 (2.42411137f,0.100000001f,5.0f); F32 (2.80611014f,0.100000001f,5.0f);
    I64 (89760664L,1000000L,1000000000L);
    I64 (369118969L,1000000L,1000000000L);
    F32 (0.788991868f,0.100000001f,5.0f); F32 (2.3016386f,0.100000001f,5.0f);
    F32 (3.24254441f,0.100000001f,5.0f); F32 (0.140221655f,0.100000001f,5.0f);
    F32 (3.18078423f,0.100000001f,5.0f); F32 (3.5707233f,0.100000001f,5.0f);
    F32 (2.13042641f,0.100000001f,5.0f);
    F32 (5.49063492f,0.100000001f,9.80000019f);
    F32 (4.17871571f,0.100000001f,5.0f); F32 (4.28368902f,0.100000001f,5.0f);
    F32 (-3.07674098f,-5.0f,-0.100000001f); F32 (6.86025476f,0.0f,8.0f);
    I64 (462654619L,1000000L,1000000000L); F32 (2.21421123f,1.0f,5.0f);
    F32 (-3.44232178f,-5.0f,-1.0f); F32 (2.85102463f,1.0f,5.0f);
    F32 (-1.72562742f,-5.0f,-1.0f); F32 (4.35351038f,0.100000001f,5.0f);
    I (1,1,10); F32 (3.72308302f,0.100000001f,5.0f);
    F32 (3.81876135f,0.100000001f,5.0f); I64 (733474002L,1000000L,1000000000L);
    F32 (4.36390162f,0.100000001f,5.0f); F32 (1.68377984f,0.100000001f,5.0f);
    F32 (1.81328452f,0.100000001f,5.0f); F32 (2.56743193f,0.100000001f,5.0f);
    F32 (2.30504012f,0.100000001f,5.0f); F32 (2.82958937f,0.100000001f,5.0f);
    F32 (1.11323428f,0.100000001f,5.0f)|]
let pop         = CAUtils.createPop parms2 1000 beliefSpace false

let ca =
    {
        Population           = pop
        Network              = CAUtils.l4BestNetwork
        KnowlegeDistribution = CARunner.knowledgeDistribution CARunner.majority
        BeliefSpace          = beliefSpace
        Acceptance           = CARunner.acceptance 5 comparator
        Influence            = CARunner.influence
        Update               = CARunner.update
        Fitness              = fitness
        Comparator           = comparator
    }

let termination step = step.Count > 100

(*
fitness parms2
let r = CARunner.run CARunner.``terminate if no improvement in 5 generations`` 2 ca
let r = CARunner.run termination 2 ca
let cfg = r.Best.[0].Parms |> CA_SensorMapping.tocfg
let bparms = r.Best.[0].Parms
*)

(*
type Fs = {Left:float; Right:float; Tap:float; Swipe:float; Driv:float}
let termIn step = step.Count > 3
let f1 = CA_SensorMapping.tocfg>>evalLeft>>float
let f2 = CA_SensorMapping.tocfg>>evalRight>>float
let f3 = CA_SensorMapping.tocfg>>evalTap>>float
let f4 = CA_SensorMapping.tocfg>>evalSwipe>>float
let f5 = CA_SensorMapping.tocfg>>evalDriv2>>float
let fs = [f1;f2;f3;f4;f5]
let runF f = {ca with Fitness=f} |> CARunner.run termIn 3
let getBest ts=ts.Best.[0].Parms
let bests = fs |> List.map (runF>>getBest) 
let bestsCfg = bests |> List.map CA_SensorMapping.tocfg
let bLeft = bestsCfg.[0]
let bRight = bestsCfg.[1]
let bTap   = bestsCfg.[2]
let bSwipe = bestsCfg.[3]
let bcfg = 
      {
           Navigation.NavigationCfg.xyz_accel_quite_limit = 0.746813118f;
           Navigation.NavigationCfg.TapConfig = bTap.TapConfig;
           Navigation.NavigationCfg.LRConfig = if f1 bests.[0] > f2 bests.[1] then bLeft.LRConfig else bRight.LRConfig
           Navigation.NavigationCfg.SwipeConfig = bSwipe.SwipeConfig;
        }
let popn = bests |> Seq.collect (fun p -> CAUtils.createPop p 200 beliefSpace false) |> Seq.toArray
let caAll = {ca with Population=popn}
let allR = CARunner.run termination 3 caAll
let bestAll = allR.Best.[0].Parms |> CA_SensorMapping.tocfg

let bparms = allR.Best.[0].Parms
let bparms = CA_SensorMapping.toParms bcfg
fs |> List.map (fun f -> f bparms) |> fun xs -> {Left=xs.[0]; Right=xs.[1]; Tap=xs.[2]; Swipe=xs.[3]; Driv=xs.[4]}
*)
