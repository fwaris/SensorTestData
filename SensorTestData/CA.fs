module CA

type Tree<'a> = Leaf of 'a | Node of 'a * Tree<'a> list

type Parm = 
    | F of      v:float     * min:float     * max:float 
    | F32 of    v:float32   * min:float32   * max:float32
    | I of      v:int       * min:int       * max:int
    | I64 of    v:int64     * min:int64     * max:int64

type ParmTrend = {Parm:Parm; Slope:float}
type Id = int
type Topology = Square | Global
type Individual = {Id:Id; Parms:Parm array; Fitness:float; KS:KnowledgeSource}
and Fitness     = Parm array -> float
and Population  = Individual array
and KnowledgeSource = 
    | Situational       of Individual list 
    | Historical        of ParmTrend array
    | Normative         of Parm array
    | Topographical     of Tree<Parm>
    | Domain            of Domain
and Domain = {Acceptance:Individual->Domain; Influence:Individual->Individual}
and KnowledgeDistribution = {Network:Population*Id->Individual array; Distribute:Population*BeliefSpace->Population}
and BeliefSpace = Tree<KnowledgeSource> list
and Acceptance  = Population * BeliefSpace -> Individual list
and Influence   = Population * BeliefSpace -> Population
and Update      = Individual list * BeliefSpace -> BeliefSpace

type CA =
    {
        Population              : Population
        KnowlegeDistribution    : KnowledgeDistribution
        BeliefSpace             : BeliefSpace
        Acceptance              : Acceptance
        Influence               : Influence
        Fitness                 : Fitness
    }

type TimeStep = {CA:CA ; Best:Individual list; Count:int}
type TerminationCondition = TimeStep -> bool

            
