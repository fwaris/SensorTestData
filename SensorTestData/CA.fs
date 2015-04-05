module CA

type Tree<'a> = Leaf of 'a | Node of 'a * Tree<'a> list

type Parm = 
    | F of      v:float     * min:float     * max:float 
    | F32 of    v:float32   * min:float32   * max:float32
    | I of      v:int       * min:int       * max:int
    | I64 of    v:int64     * min:int64     * max:int64

type Id = int
type Topology   = LBest | Global
type Knowledge  = Situational | Historical | Normative | Topgraphical | Domain | Other of string
type Individual = {Id:Id; Parms:Parm array; Fitness:float; KS:Knowledge}
and Fitness     = Parm array -> float
and Population  = Individual array
and Network     = Population -> Id -> Individual array
and BeliefSpace = KnowledgeSource Tree list
and Acceptance  = BeliefSpace -> Population -> Individual list
and Influence   = BeliefSpace -> Population -> Population
and Update      = BeliefSpace -> Individual list -> BeliefSpace
and KnowledgeDistribution   = BeliefSpace->Population->Population
and KnowledgeSource         = {
                                Type:Knowledge
                                Acceptance:Individual->Individual option*KnowledgeSource
                                Influence:Individual->Individual
                               }

type CA =
    {
        Population              : Population
        Network                 : Network
        KnowlegeDistribution    : KnowledgeDistribution
        BeliefSpace             : BeliefSpace
        Acceptance              : Acceptance
        Influence               : Influence
        Fitness                 : Fitness
    }

type TimeStep = {CA:CA ; Best:Individual list; Count:int}
type TerminationCondition = TimeStep -> bool

            
