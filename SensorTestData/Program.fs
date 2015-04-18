module Program
// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
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
let pop         = CAUtils.createPop parms 1000 beliefSpace

let ca =
    {
        Population           = pop
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


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let r = CARunner.run ca termination 2
    printfn "%A" r
    0 // return an integer exit code
