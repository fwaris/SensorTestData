module CARunner
open CA

let knowledgeSources beliefSpace =
    let rec loop acc = function
        | []                        -> List.rev acc
        | Leaf(ks)::rest            -> loop (ks::acc) rest
        | Node(ks,children)::rest   -> loop ( loop (ks::acc) children ) rest
    loop [] beliefSpace
        
let randI (rnd:System.Random) min max = rnd.Next(min,max)
let randF32 (rnd:System.Random) (min:float32) (max:float32) =  min + (float32 ((rnd.NextDouble()) * float (max - min)))
let randF (rnd:System.Random)  min max = min + (rnd.NextDouble() * (max - min))
let randI64 (rnd:System.Random) min max =  min + (int64 ((rnd.NextDouble()) * float (max - min)))
(*
let rnd = System.Random()
[for i in 1..100 -> randI rnd 1 1000]
[for i in 1..100 -> randF32 rnd 1.f 1000.f]
[for i in 1..100 -> randF rnd 1000. 1000000.]
[for i in 1..100 -> randI64 rnd 1000L 1000000L]
*)   

let randomize (rnd:System.Random) = function
    | F (v,min,max)     -> F (randF rnd min max, min, max)
    | F32 (v,min,max)   -> F32 (randF32 rnd min max, min, max)
    | I (v,min,max)     -> I(randI rnd min max, min, max)
    | I64 (v,min,max)   -> I64(randI64 rnd min max, min, max)

let createPop parms size kss =
    let kss = Seq.toArray kss
    let rnd = System.Random()
    [|
        for i in 0..size-1 do
            let rndParms = parms |> Array.map (randomize rnd)
            yield
                {
                    Id      = i
                    Parms   = rndParms
                    Fitness = System.Double.MinValue
                    KS      = kss.[kss.Length%i]

                }
    |]

let squareNetwork (pop:Population,id) = 
    let m2 = id - 2
    let m2 = if m2 < 0 then pop.Length - m2 - 1 else m2
    let m1 = id - 1
    let m1 = if m1 < 0 then pop.Length - m1 - 1 else m1
    let p2 = id + 2
    let p2 = if p2 >= pop.Length then p2 - pop.Length - 1 else p2
    let p1 = id + 1 
    let p1 = if p1 >= pop.Length then p1 - pop.Length - 1 else p1
    [|pop.[m2]; pop.[m1]; pop.[p1]; pop.[p2] |]
        
let createNetwork = function
    | Global -> {Network=(fun (pop,id)->pop);   Distribute=fun (a,b)->a}
    | Square -> {Network=squareNetwork;         Distribute=fun (a,b)->a}

let defaultBeliefSpace =
    [Normative]
    