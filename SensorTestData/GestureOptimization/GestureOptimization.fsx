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
    let total = fL + fR + fS + fT + (0.3 * fD)
    let diffs = 
        abs (fL - fR) + abs(fR - fS) + abs (fS - fT) + 
        abs (fL - fS) + abs (fR - fT) + abs (fL - fT)
    3. * total - diffs

let comparator  = CAUtils.Maximize
let beliefSpace = CARunner.defaultBeliefSpace CA_SensorMapping.parms comparator fitness
//let beliefSpace = Leaf (SituationalKS.create comparator 5)
let parms2 = 
  [|F32 (1.54266667f,0.100000001f,2.0f); I64 (627096161L,1000000L,1000000000L);
    F32 (0.278097719f,0.100000001f,5.0f); F32 (0.937309027f,0.100000001f,5.0f);
    F32 (1.42948687f,0.100000001f,5.0f); F32 (2.57358718f,0.100000001f,5.0f);
    I (5,1,10); F32 (1.45854759f,0.100000001f,5.0f);
    F32 (2.45577288f,0.100000001f,5.0f); F32 (0.685640156f,0.100000001f,5.0f);
    I64 (89182438L,1000000L,1000000000L);
    I64 (368253198L,1000000L,1000000000L);
    F32 (0.917704344f,0.100000001f,5.0f); F32 (2.04115486f,0.100000001f,5.0f);
    F32 (3.29855824f,0.100000001f,5.0f); F32 (2.03626728f,0.100000001f,5.0f);
    F32 (2.94672751f,0.100000001f,5.0f); F32 (3.23148394f,0.100000001f,5.0f);
    F32 (2.97842479f,0.100000001f,5.0f);
    F32 (5.66170073f,0.100000001f,9.80000019f);
    F32 (4.30124664f,0.100000001f,5.0f); F32 (4.51463461f,0.100000001f,5.0f);
    F32 (-3.4966228f,-5.0f,-0.100000001f); F32 (6.67922068f,0.0f,8.0f);
    I64 (469225486L,1000000L,1000000000L); F32 (2.11974669f,1.0f,5.0f);
    F32 (-3.58925152f,-5.0f,-1.0f); F32 (2.90714598f,1.0f,5.0f);
    F32 (-2.04377747f,-5.0f,-1.0f); F32 (4.5203743f,0.100000001f,5.0f);
    I (1,1,10); F32 (2.32675743f,0.100000001f,5.0f);
    F32 (3.57043314f,0.100000001f,5.0f); I64 (738089061L,1000000L,1000000000L);
    F32 (3.93707943f,0.100000001f,5.0f); F32 (1.68081212f,0.100000001f,5.0f);
    F32 (1.91229534f,0.100000001f,5.0f); F32 (2.16931653f,0.100000001f,5.0f);
    F32 (1.23249066f,0.100000001f,5.0f); F32 (1.78700745f,0.100000001f,5.0f);
    F32 (1.78635275f,0.100000001f,5.0f)|]
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
