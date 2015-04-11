module DomainKS
open CA
open CAUtils

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
    maxI,maxS    

let create isBetter fitness maxExemplars =
    let create exemplars fAccept fInfluence : KnowledgeSource =
        {
            Type        = Domain
            Accept      = fAccept fInfluence exemplars
            Influence   = fInfluence exemplars
        }

    let rec acceptance fInfluence prevExemplars (inds:Individual array) =
        let ind = inds.[0] //assume best individual is first
        match prevExemplars with
        | [] -> [|ind|], create [ind] acceptance fInfluence
        | prevBest::rest when isBetter ind.Fitness prevBest.Fitness -> 
            let newExemplars = 
                ind::prevExemplars 
                |> List.truncate maxExemplars
            [|ind|], create newExemplars acceptance fInfluence
        | xs -> [||], create xs acceptance fInfluence
    
    let influence exemplars (ind:Individual) =
        let (i,(Dir,magnitude)) = maxParm isBetter fitness ind.Fitness ind.Parms
        let best =
            match exemplars with
            | [] -> ind
            | best::_ -> influenceInd  ind best
       
    create [] acceptance influence
