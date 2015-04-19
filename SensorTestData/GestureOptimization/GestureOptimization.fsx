#load "CA_SensorMapping.fsx"
open CA
open TapTrain

//http://en.wikipedia.org/wiki/Nonlinear_programming
// maximize x + y
// st. 1 <= x**2 + y**2 <= 2.
let fitness (parms:Parm array) = 
    let cfg = CA_SensorMapping.tocfg parms
    evalLeft cfg  +
    evalRight cfg +
    evalSwipe cfg +
    evalTap cfg
    |> float

let comparator  = CAUtils.Maximize
let beliefSpace = CARunner.defaultBeliefSpace CA_SensorMapping.parms comparator fitness
//let beliefSpace = Leaf (SituationalKS.create comparator 5)
let pop         = CAUtils.createPop CA_SensorMapping.parms 1000 beliefSpace

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

let termination step = step.Count > 100

(*
let r = (CARunner.run ca termination 2)
r.Best
*)
