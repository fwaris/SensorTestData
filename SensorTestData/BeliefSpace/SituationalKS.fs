module SituationalKS
open CA
open CAUtils

let create isBetter maxExemplars =
    let create (examplars:Individual list) fAccept fInfluence : KnowledgeSource =
        {
            Type        = Situational
            Accept      = fAccept fInfluence examplars
            Influence   = fInfluence examplars
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
        match exemplars with
        | [] -> ind
        | best::_ -> influenceInd  ind best
       
    create [] acceptance influence
