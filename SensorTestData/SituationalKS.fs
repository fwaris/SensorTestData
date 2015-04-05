module SituationalKS
open CA
open CAUtils

//cross corresponding parameters between a belief space and pop. individual
//according to Situational KS rules
let crossParms = function
    | F(pV,mn,mx),F(iV,_,_) when pV > iV     -> F(randF rnd iV pV,mn,mx)
    | F(pV,mn,mx),F(iV,_,_) when pV < iV     -> F(randF rnd pV iV,mn,mx)
    | F(_),fInd                              -> evolveS rnd fInd

    | F32(pV,mn,mx),F32(iV,_,_) when pV > iV -> F32(randF32 rnd iV pV,mn,mx)
    | F32(pV,mn,mx),F32(iV,_,_) when pV < iV -> F32(randF32 rnd pV iV,mn,mx)
    | F32(_),fInd                            -> evolveS rnd fInd

    | I(pV,mn,mx),I(iV,_,_) when pV > iV     -> I(randI rnd iV pV,mn,mx)
    | I(pV,mn,mx),I(iV,_,_) when pV < iV     -> I(randI rnd pV iV,mn,mx)
    | I(_),fInd                              -> evolveS rnd fInd

    | I64(pV,mn,mx),I64(iV,_,_) when pV > iV -> I64(randI64 rnd iV pV,mn,mx)
    | I64(pV,mn,mx),I64(iV,_,_) when pV < iV -> I64(randI64 rnd pV iV,mn,mx)
    | I64(_),fInd                            -> evolveS rnd fInd

    | a,b -> failwithf "two pop individual parameters not matched %A %A" a b

let cross beliefSpaceInd popInd =
    {popInd with
        Parms = 
            popInd.Parms 
            |> Array.mapi (fun i p ->  
                crossParms (beliefSpaceInd.Parms.[i], p))
    }

let create() =
    let create (examplars:Individual list) fAccept fInfluence : KnowledgeSource =
        {
            Type        = Situational
            Acceptance  = fAccept fInfluence examplars
            Influence   = fInfluence examplars
        }

    let rec acceptance fInfluence prevExemplars (ind:Individual) =
        match prevExemplars with
        | [] -> Some(ind),create [ind] acceptance fInfluence
        | prevBest::rest when ind.Fitness > prevBest.Fitness -> 
            let newExemplars = ind::prevExemplars |> List.truncate CAConstants.max_examplars
            Some(ind), create newExemplars acceptance fInfluence
        | xs -> None, create xs acceptance fInfluence
    
    let influence exemplars (ind:Individual) =
        match exemplars with
        | [] -> ind
        | best::_ -> cross best ind
        
    create [] acceptance influence
