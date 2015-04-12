module DomainKS
open CA
open CAUtils

type Slope = {Index:int; Magnitude:float; Direction:Dir}

let rateOfImprovement oldFitness newFitness isBetter epsilon =
    let denominator = parmToFloat epsilon
    if oldFitness = newFitness then 
        Flat,0.
    elif isBetter newFitness oldFitness then
        Up, (abs newFitness-oldFitness) / denominator
    else
        Down,(abs newFitness-oldFitness) / denominator

let maxParm isBetter fitness oldFitness parms  =
    let parms    = Array.copy parms
    let epsilons = parms |> Array.map epsilon
    let mutable maxS = Flat,0.
    let mutable maxI = 0
    for i in 0..parms.Length - 1 do
        let p = parms.[i]     //x
        let e = epsilons.[i]  //dx
        let p' = parmAdd p e
        parms.[i] <- p' //x + dx
        let newFitness = fitness parms
        parms.[i] <- p  //reset x
        let imp = rateOfImprovement oldFitness newFitness isBetter e
        match maxS,imp with
        | _    , (Flat,_)           -> ()
        | (_,a), (dir,b) when b > a -> maxS <- dir,b; maxI <- i
        | _                         -> ()
    {Index=maxI; Magnitude=snd maxS; Direction=fst maxS},parms    

let create isBetter fitness maxExemplars =
    let create state fAccept fInfluence : KnowledgeSource =
        {
            Type        = Domain
            Accept      = fAccept fInfluence state
            Influence   = fInfluence state
        }

    let rec acceptance 
        fInfluence 
        (prevExemplars:Individual list, gbestSlope) 
        (inds:Individual array) =
        let runBest = inds.[0] //assume best individual is first
        match prevExemplars with
        | [] -> 
            [|runBest|], create [gbest] acceptance fInfluence
        | prevBest::rest when isBetter ind.Fitness prevBest.Fitness -> 
            let newExemplars = 
                ind::prevExemplars 
                |> List.truncate maxExemplars
            let gbestSlope,_ = maxParm isBetter fitness ind.Fitness ind.Parms
            [|ind|], create (newExemplars,gbestSlope) acceptance fInfluence
        | xs -> [||], create (xs,gbestSlope) acceptance fInfluence
    
    let influence (exemplars,gbestSlope) (ind:Individual) =
        let (slope,parms) = maxParm isBetter fitness ind.Fitness ind.Parms
        let parm = parms.[slope.Index]
        let parm =
            match slope.Direction with
            | Up   -> slideUp parm
            | Down -> slideDown parm
            | Flat -> 
                match gbestSlope.Direction with
                | Up    -> slideUp parms.[slope.Index]
                | Down  -> slideDown parms.[slope.Index]
                | Flat  -> evolveS(parms.[slope.Index])
        parms.[i] <- parm
        {ind with Parms=parms}
       
    create ([],{Index=0; Direction=Flat; Magnitude=0.}) acceptance influence
