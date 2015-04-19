module Program
// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open CA

let parms = 
    [|
        F(0.,0.,10.) // x
        F(0.,0.,10.) // y
    |]

//http://en.wikipedia.org/wiki/Nonlinear_programming
// maximize x + y
// st. 1 <= x**2 + y**2 <= 2.
let fitness (parms:Parm array) = 
    let x = match parms.[0] with F(x,_,_) -> x | _ -> failwith "no match"
    let y = match parms.[1] with F(y,_,_) -> y | _ -> failwith "no match"
    let s = x**2. + y**2.
    if s >= 1. && s <= 2. then
        x + y
    else
        -1. 

let comparator  = CAUtils.Maximize
let beliefSpace = CARunner.defaultBeliefSpace parms comparator fitness
//let beliefSpace = Leaf (SituationalKS.create comparator 5)
let pop         = CAUtils.createPop parms 1000 beliefSpace true

let ca =
    {
        Population           = pop
        Network              = CAUtils.lBestNetwork
        KnowlegeDistribution = CARunner.knowledgeDistribution CARunner.rouletteDistribution
        BeliefSpace          = beliefSpace
        Acceptance           = CARunner.acceptance 5 comparator
        Influence            = CARunner.influence
        Update               = CARunner.update
        Fitness              = fitness
        Comparator           = comparator
    }

let termination step = step.Count > 1000

(*
let r = (CARunner.run ca termination 2)
r.Best
*)


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let r = CARunner.run ca termination 2
    printfn "%A" r
    0 // return an integer exit code
