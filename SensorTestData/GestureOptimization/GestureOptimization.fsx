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
    let total = fL + fR + fS + fT + fD
    let diffs = 
        abs (fL - fR) + abs(fR - fS) + abs (fS - fT) + 
        abs (fL - fS) + abs (fR - fT) + abs (fL - fT)
    10.0 * total - diffs

let comparator  = CAUtils.Maximize
let beliefSpace = CARunner.defaultBeliefSpace CA_SensorMapping.parms comparator fitness
//let beliefSpace = Leaf (SituationalKS.create comparator 5)
let parms2 = 
  [|F32 (1.53595424f,0.100000001f,2.0f); I64 (627096163L,1000000L,1000000000L);
    F32 (0.137690037f,0.100000001f,5.0f); F32 (0.937321484f,0.100000001f,5.0f);
    F32 (1.42948413f,0.100000001f,5.0f); F32 (2.57372451f,0.100000001f,5.0f);
    I (7,1,10); F32 (1.57058358f,0.100000001f,5.0f);
    F32 (2.45579791f,0.100000001f,5.0f); F32 (0.63446033f,0.100000001f,5.0f);
    I64 (89182441L,1000000L,1000000000L);
    I64 (368253201L,1000000L,1000000000L);
    F32 (0.914043486f,0.100000001f,5.0f); F32 (2.04115629f,0.100000001f,5.0f);
    F32 (3.29856491f,0.100000001f,5.0f); F32 (2.03625607f,0.100000001f,5.0f);
    F32 (2.95862842f,0.100000001f,5.0f); F32 (3.23148632f,0.100000001f,5.0f);
    F32 (2.97839952f,0.100000001f,5.0f);
    F32 (5.47138548f,0.100000001f,9.80000019f);
    F32 (4.30215931f,0.100000001f,5.0f); F32 (3.89866972f,0.100000001f,5.0f);
    F32 (-2.88945603f,-5.0f,-0.100000001f); F32 (6.48369932f,0.0f,8.0f);
    I64 (469225488L,1000000L,1000000000L); F32 (2.12353539f,1.0f,5.0f);
    F32 (-3.59044194f,-5.0f,-1.0f); F32 (2.90341806f,1.0f,5.0f);
    F32 (-1.64295053f,-5.0f,-1.0f); F32 (4.52377892f,0.100000001f,5.0f);
    I (1,1,10); F32 (2.4141531f,0.100000001f,5.0f);
    F32 (4.12105274f,0.100000001f,5.0f); I64 (738089063L,1000000L,1000000000L);
    F32 (3.93655849f,0.100000001f,5.0f); F32 (1.68081605f,0.100000001f,5.0f);
    F32 (1.9172442f,0.100000001f,5.0f); F32 (2.1677525f,0.100000001f,5.0f);
    F32 (1.23240876f,0.100000001f,5.0f); F32 (1.77168429f,0.100000001f,5.0f);
    F32 (1.79593349f,0.100000001f,5.0f)|]
let pop         = CAUtils.createPop CA_SensorMapping.parms 1000 beliefSpace false

let ca =
    {
        Population           = pop
        Network              = CAUtils.l4BestNetwork
        KnowlegeDistribution = CARunner.knowledgeDistribution CARunner.rouletteDistribution
        BeliefSpace          = beliefSpace
        Acceptance           = CARunner.acceptance 5 comparator
        Influence            = CARunner.influence
        Update               = CARunner.update
        Fitness              = fitness
        Comparator           = comparator
    }

let termination step = step.Count > 20

(*
let r = CARunner.run CARunner.``terminate if no improvement in 5 generations`` 2 ca
let cfg = r.Best.[0].Parms |> CA_SensorMapping.tocfg
let bparms = r.Best.[0].Parms
*)

(*
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
let popn = bests |> Seq.collect (fun p -> CAUtils.createPop p 200 beliefSpace false) |> Seq.toArray
let caAll = {ca with Population=popn}
let allR = CARunner.run termination 3 caAll
let bestAll = allR.Best.[0].Parms |> CA_SensorMapping.tocfg

let bparms = allR.Best.[0].Parms
fs |> List.map (fun f -> f bparms)
*)
