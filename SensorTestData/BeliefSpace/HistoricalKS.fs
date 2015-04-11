module HistoricalKS
open CA
open CAUtils

//determine direction of change
let dir newParm prevParm = 
    match prevParm,newParm with
    | F(prevV,_,_),F(newV,_,_)  when newV > prevV       -> Up
    | F(prevV,_,_),F(newV,_,_)  when newV < prevV       -> Down
    | F(prevV,_,_),F(newV,_,_)                          -> Flat
    | F32(prevV,_,_),F32(newV,_,_)  when newV > prevV   -> Up
    | F32(prevV,_,_),F32(newV,_,_)  when newV < prevV   -> Down
    | F32(prevV,_,_),F32(newV,_,_)                      -> Flat
    | I(prevV,_,_),I(newV,_,_)  when newV > prevV       -> Up
    | I(prevV,_,_),I(newV,_,_)  when newV < prevV       -> Down
    | I(prevV,_,_),I(newV,_,_)                          -> Flat
    | I64(prevV,_,_),I64(newV,_,_)  when newV > prevV   -> Up
    | I64(prevV,_,_),I64(newV,_,_)  when newV < prevV   -> Down
    | I64(prevV,_,_),I64(newV,_,_)                      -> Flat
    | a,b -> failwithf "HistoricalKS: invalid combination of types for dir %A,%A" a b

let parmAvg count = function
    | F(v,_,_)      -> float v / float count
    | F32(v,_,_)    -> float v / float count
    | I(v,_,_)      -> float v / float count
    | I64(v,_,_)    -> float v / float count

type ChangeEvent = {Best:Individual; Direction:Dir array}
type History = 
    {
        Window      : int
        Distance    : Parm array
        Direction   : Dir array
        Events      : ChangeEvent list
    }

let create isBetter window =
    let create history fAccept fInfluence : KnowledgeSource =
        {
            Type        = Historical
            Accept      = fAccept fInfluence history
            Influence   = fInfluence history
        }

    let rec acceptance 
        fInfluence 
        ({Window=win; Events=events} as history)
        (inds:Individual array) =
        let newBest = inds.[0] //assume best individual is first
        let prevBest = match events with [] -> newBest | b::_ -> b.Best
        if isBetter newBest.Fitness prevBest.Fitness then
            let eventDirection  = (prevBest.Parms,newBest.Parms) ||> Array.map2 dir
            let changeEvent     = {Best=newBest; Direction=eventDirection}
            let events          = changeEvent::events |> List.truncate win
            let earliestEvent = events.[events.Length - 1]
            let distance      = (newBest.Parms,earliestEvent.Best.Parms) ||> Array.map2 parmDiff
            let direction     = (newBest.Parms,earliestEvent.Best.Parms) ||> Array.map2 dir
            let updatedHistory =
                {
                    Window              = win
                    Distance            = distance
                    Direction           = direction
                    Events              = events
                }
            [|newBest|],create updatedHistory acceptance fInfluence
        else
            [||],create history acceptance fInfluence
    
    let influence {Events=events} (ind:Individual) =
        let ev = events.[rnd.Next(0,events.Length-1)]
        if isBetter ev.Best.Fitness ind.Fitness then
            ev.Best |> influenceInd ind
        else
            evolveInd ind

    let initialHistory = {Window=window; Distance=[||]; Direction=[||]; Events=[]}
       
    create initialHistory acceptance influence


