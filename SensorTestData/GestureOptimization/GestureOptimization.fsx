#load "CA_SensorMapping.fsx"
open CA
open TapTrain

//http://en.wikipedia.org/wiki/Nonlinear_programming
// maximize x + y
// st. 1 <= x**2 + y**2 <= 2.
let fitness (parms:Parm array) = 
    let cfg = CA_SensorMapping.tocfg parms
    evalLeft cfg 
    + evalRight cfg
    + evalSwipe cfg
    + evalTap cfg
    + evalDriv2 cfg
    |> float

let comparator  = CAUtils.Maximize
let beliefSpace = CARunner.defaultBeliefSpace CA_SensorMapping.parms comparator fitness
//let beliefSpace = Leaf (SituationalKS.create comparator 5)
let pop         = CAUtils.createPop CA_SensorMapping.parms 1000 beliefSpace true

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

let termination step = step.Count > 50

(*
let r = (CARunner.run ca termination 2)
r.Best.[0].Parms |> CA_SensorMapping.tocfg
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
let popn = bests |> Seq.collect (fun p -> CAUtils.createPop p 100 beliefSpace false) |> Seq.toArray
let caAll = {ca with Population=popn}
let allR = CARunner.run termination 3 caAll
let bestAll = allR.Best.[0].Parms |> CA_SensorMapping.tocfg
*)
