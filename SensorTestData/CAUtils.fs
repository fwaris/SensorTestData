module CAUtils
open CA
let rnd = System.Random()

let knowledgeSources beliefSpace =
    let rec loop acc = function
        | []                      -> List.rev acc
        | Leaf(ks)::rest          -> loop (ks::acc) rest
        | Node(ks,children)::rest -> loop (loop (ks::acc) children) rest
    loop [] beliefSpace
        
let randI min max = rnd.Next(min,max)
let randF32 (min:float32) (max:float32) =  min + (float32 ((rnd.NextDouble()) * float (max - min)))
let randF  min max = min + (rnd.NextDouble() * (max - min))
let randI64 min max =  min + (int64 ((rnd.NextDouble()) * float (max - min)))
(*
let rnd = System.Random()
[for i in 1..100 -> randI 1 1000]
[for i in 1..100 -> randF32 1.f 1000.f]
[for i in 1..100 -> randF 1000. 1000000.]
[for i in 1..100 -> randI64 1000L 1000000L]
*)   

//Box-Muller method
let gaussian mean sigma = 
    let mutable v1 = 0.
    let mutable v2 = 0.
    let mutable r2 = 2.
    while r2 >= 1. || r2 = 0. do
        v1 <- 2. * rnd.NextDouble() - 1.
        v2 <- 2. * rnd.NextDouble() - 1.
        r2 <- v1 * v1 + v2 * v2
    let polar = sqrt(-2.*log(r2) / r2)
    v1*polar*sigma + mean
(*
let rnd = System.Random()
[for i in 0..100 -> gaussian (float 50.) 1.]
*)

let randomize = function
    | F (v,mn,mx)   -> F (randF mn mx, mn, mx)
    | F32 (v,mn,mx) -> F32 (randF32 mn mx, mn, mx)
    | I (v,mn,mx)   -> I(randI mn mx, mn, mx)
    | I64 (v,mn,mx) -> I64(randI64 mn mx, mn, mx)

let slideUp = function
    | F (v,mn,mx)   -> F (randF v mx, mn, mx)
    | F32 (v,mn,mx) -> F32 (randF32 v mx, mn, mx)
    | I (v,mn,mx)   -> I(randI v mx, mn, mx)
    | I64 (v,mn,mx) -> I64(randI64 v mx, mn, mx)

let slideDown = function
    | F (v,mn,mx)   -> F (randF mn v, mn, mx)
    | F32 (v,mn,mx) -> F32 (randF32 mn v, mn, mx)
    | I (v,mn,mx)   -> I(randI mn v, mn, mx)
    | I64 (v,mn,mx) -> I64(randI64 mn v, mn, mx)

let clamp mn mx x = max (min x mx) mn

let evolveS = function
    | F (v,mn,mx)    -> F   (gaussian v 1.                      |> clamp mn mx, mn, mx)
    | F32 (v,mn,mx)  -> F32 (gaussian (float v) 1. |> float32   |> clamp mn mx, mn, mx)
    | I (v,mn,mx)    -> I   (gaussian (float v) 1. |> int       |> clamp mn mx, mn, mx)
    | I64 (v,mn,mx)  -> I64 (gaussian (float v) 1. |> int64     |> clamp mn mx, mn, mx)

///Use values from the 2nd parm to influence 1st parm
///(randomly move towards 2nd parm value)
let influenceParm influenced influencer =
    match influencer,influenced with
    | F(pV,mn,mx),F(iV,_,_) when pV > iV     -> F(randF iV pV,mn,mx)
    | F(pV,mn,mx),F(iV,_,_) when pV < iV     -> F(randF pV iV,mn,mx)
    | F(_),fInd                              -> evolveS fInd

    | F32(pV,mn,mx),F32(iV,_,_) when pV > iV -> F32(randF32 iV pV,mn,mx)
    | F32(pV,mn,mx),F32(iV,_,_) when pV < iV -> F32(randF32 pV iV,mn,mx)
    | F32(_),fInd                            -> evolveS fInd

    | I(pV,mn,mx),I(iV,_,_) when pV > iV     -> I(randI iV pV,mn,mx)
    | I(pV,mn,mx),I(iV,_,_) when pV < iV     -> I(randI pV iV,mn,mx)
    | I(_),fInd                              -> evolveS fInd

    | I64(pV,mn,mx),I64(iV,_,_) when pV > iV -> I64(randI64 iV pV,mn,mx)
    | I64(pV,mn,mx),I64(iV,_,_) when pV < iV -> I64(randI64 pV iV,mn,mx)
    | I64(_),fInd                            -> evolveS fInd

    | a,b -> failwithf "two pop individual parameters not matched %A %A" a b

///influenced indivual's parameters are modified 
///to move them towards the influencer's parameters
let influenceInd influenced influencer =
    {influenced with
        Parms = (influenced.Parms,influencer.Parms) ||> Array.map2 influenceParm
    }

///influenced indivual's parameters are modified 
///to move them towards the influencer's parameters
let evolveInd individual =
    {individual with
        Parms = individual.Parms |> Array.map evolveS
    }

let createPop parms size kss =
    let kss = Seq.toArray kss
    let rnd = System.Random()
    [|
        for i in 0..size-1 do
            let rndParms = parms |> Array.map randomize
            yield
                {
                    Id      = i
                    Parms   = rndParms
                    Fitness = System.Double.MinValue
                    KS      = kss.[kss.Length%i]

                }
    |]

type Dir = Up | Down | Flat

//add two parms
let parmAdd p1 p2 = 
    match p1, p2 with
    | F(prevV,_,_),F(newV,mn,mx)        -> F(prevV + newV   ,mn,mx)
    | F32(prevV,_,_),F32(newV,mn,mx)    -> F32(prevV + newV ,mn,mx)
    | I(prevV,_,_),I(newV,mn,mx)        -> I(prevV + newV   ,mn,mx)
    | I64(prevV,_,_),I64(newV,mn,mx)    -> I64(prevV + newV ,mn,mx)
    | a,b -> failwithf "CAUtils: invalid combination of types for parmSum %A,%A" a b

//Effectively newParm - oldParm
let parmDiff newParm oldParm  = 
    match oldParm, newParm with
    | F(prevV,_,_),F(newV,mn,mx)        -> F(newV - prevV   ,mn,mx)
    | F32(prevV,_,_),F32(newV,mn,mx)    -> F32(newV - prevV ,mn,mx)
    | I(prevV,_,_),I(newV,mn,mx)        -> I(newV - prevV   ,mn,mx)
    | I64(prevV,_,_),I64(newV,mn,mx)    -> I64(newV - prevV ,mn,mx)
    | a,b -> failwithf "CAUtils: invalid combination of types for parmDiff %A,%A" a b

//Effectively numerator / denominator
let parmDiv numerator denominator  = 
    try
        let r = 
            match numerator, denominator with
            | F(n,_,_),F(d,_,_)        -> n / d
            | F32(n,_,_),F32(d,_,_)    -> float n / float d
            | I(n,_,_),I(d,_,_)        -> float n / float d
            | I64(n,_,_),I64(d,_,_)    -> float n / float d
            | a,b -> failwithf "CAUtils: invalid combination of types for parmDiv %A,%A" a b
        Some r
    with :? System.DivideByZeroException ->
        None

//Effectively numerator / denominator
let parmToFloat = function
    | F(v,_,_)   -> v
    | F32(v,_,_) -> float v
    | I(v,_,_)   -> float v
    | I64(v,_,_) -> float v

let epsilon = function
    | F(_,mn,mx)    -> F(System.Double.Epsilon,mn,mx)
    | F32(_,mn,mx)  -> F32(System.Single.Epsilon,mn,mx)
    | I(_,mn,mx)    -> I(1,mn,mx)
    | I64(_,mn,mx)  -> I64(1L,mn,mx)

let l4bestNetwork (pop:Population) id = //return 4 'friends' from the ring
    let m2 = id - 2
    let m2 = if m2 < 0 then pop.Length + m2 else m2
    let m1 = id - 1
    let m1 = if m1 < 0 then pop.Length + m1 else m1
    let p2 = id + 2
    let p2 = if p2 >= pop.Length then p2 - pop.Length else p2
    let p1 = id + 1 
    let p1 = if p1 >= pop.Length then p1 - pop.Length else p1
//    printfn "id=%d, m1=%d, m2=%d, p1=%d, p2=%d" id m1 m2 p1 p2
    [|pop.[m2]; pop.[m1]; pop.[p1]; pop.[p2] |]
(*
#load "CA.fs"
open CA
let parms = [|F(1.,1.,10.)|]
let pop= [|for i in 1..100 -> {Id=i;Parms=parms;Fitness=0.;KS=Normative}|]
let net = pop |> Array.mapi (fun i _ -> lbestNetwork pop i)       
*)

