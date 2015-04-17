#r @"../../packages/FSharp.Collections.ParallelSeq.1.0.2\lib\net40\FSharp.Collections.ParallelSeq.dll"
#load "../CA.fs"
#load "../CAUtils.fs"
#load "../BeliefSpace/SituationalKS.fs"
#load "../BeliefSpace/NormativeKS.fs"
#load "../BeliefSpace/HistoricalKS.fs"
#load "../BeliefSpace/DomainKS.fs"
#load "../CARunner.fs"
open CA

let parms = 
    [|
        F(0.,0.,10.) // x
        F(0.,0.,10.) // y
    |]

let goal = sqrt (6.**2. + 3.**2.)

let fitness (parms:Parm array) = 
    let x = match parms.[0] with F(x,_,_) -> x | _ -> failwith "no match"
    let y = match parms.[0] with F(y,_,_) -> y | _ -> failwith "no match"
    sqrt (x**2. + y**2.) - goal

let comparator  = CAUtils.Minimize
let beliefSpace = CARunner.defaultBeliefSpace parms comparator fitness

let ca =
    {
        Population           = CAUtils.createPop parms 1000 beliefSpace
        Network              = CAUtils.l4bestNetwork
        KnowlegeDistribution = CARunner.knowledgeDistribution CARunner.rouletteDistribution
        BeliefSpace          = beliefSpace
        Acceptance           = CARunner.acceptance 5 comparator
        Influence            = CARunner.influence
        Update               = CARunner.update
        Fitness              = fitness
        Comparator           = comparator
    }

let termination step = step.Count > 500

let r = CARunner.run ca termination 2