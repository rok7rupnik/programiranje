(**************************************** SET **********************************)
signature SET = sig
  type ''a set
  val insert : (''a * ''a set) -> ''a set
  val member : (''a * ''a set) -> bool
  val empty : ''a set
  val toList : ''a set -> ''a list
end

structure ListSet :> SET =
struct 
    type ''a set = ''a list
    val rec member = fn (el, set) =>
        case set of
            [] => false
            | x::xs => x=el orelse member (el,xs)
    val insert = fn (el, set) =>
        if member (el, set)
        then set
        else el::set
    val empty = []
    val toList = fn (set) => set
end

(* Uporaba *)
val set = ListSet.empty
val set = ListSet.insert(10, set)
val set = ListSet.insert(1, set)

(************************************* ORDER **************************************)
signature ORD = sig
  type t
  val cmp : t * t -> order
end

structure IntOrder :> ORD = 
struct
    type t = int
    val cmp = Int.compare
end

(************************************ ORDERED SET ************************************)
signature ORDSET = sig

  structure Order : ORD

  type elem = Order.t
  type set

  val insert : (elem * set) -> set
  val member : (elem * set) -> bool
  val empty : set
  val toList : set-> elem list
end

functor OrdListSetfn (ordParam : ORD) :> ORDSET
    where type Order.t = ordParam.t = 
    struct
        structure Order = ordParam
        type elem = Order.t
        type set = elem list
        val empty = []
        val rec member = fn (x, set) => 
            case set of
                [] => false
                | y::ys => case Order.cmp (y, x) of
                    LESS => false
                    | EQUAL => true
                    | GREATER => member (x, ys)
        val rec insert = fn (x, set) => 
            case set of
                [] => [x]
                | y::ys => case Order.cmp (x, y) of
                    LESS => y::x::ys
                    | EQUAL => set
                    | GREATER => y::insert (x, ys)
        val toList = fn (set) => set
    end

(* test *)
structure intOLS = OrdListSetfn (IntOrder)
(*val s1 = intOLS.empty
val s11 = intOLS.insert (7, s1)
val s2 = intOLS.insert (11, s11)
val sez = intOLS.toList (s2)
val sez = intOLS.toList (s2) *)

(*********************************************** TREE MAP *************************************)
signature ORDMAP =
sig
  structure Key : ORD
  type 'a map

  val empty : 'a map
  val find : Key.t * 'a map -> 'a option
  val insert : Key.t * 'a * 'a map -> 'a map
end

functor TreeMapFn (ordParam: ORD) :> ORDMAP
    where type Key.t = ordParam.t =
    struct 
        structure Key = ordParam
        datatype 'a map = Empty
            | Node of Key.t * 'a * 'a map * 'a map
        val empty = Empty
        fun find (key, map) =
            case map of
                Empty => NONE
                | Node (k, v, l, r) => case Key.cmp (key, k) of
                    LESS => find (key, l)
                    | EQUAL => SOME v
                    | GREATER => find  (k, r)
        fun insert (key: Key.t, value, map) =
            case map of
                Empty => Node (key, value, Empty, Empty)
                | Node (k, v, l, r) => case Key.cmp (key, k) of
                    LESS => Node (k, v, insert (key, value, l), r)
                    | EQUAL => Node (k, value, l, r)
                    | GREATER => Node (k, v, l, insert (key, value, r))
    end

structure intTM = TreeMapFn (IntOrder)
(* val s1 = intTM.empty
val s11 = intTM.insert (7, "sedem", s1)
val s2 = intTM.insert (2, "dva", s11)
val sedem = intTM.find (7, s2)
val pet = intTM.find (5, s2) *)

(**************************************** GRAPH ************************************)
(*signature GRAPH = sig
  type node
  type edge
  type graph

  val addNode : node*graph -> graph
  val addEdge : node*edge*graph -> graph
  val getEdges : node*graph -> edge list
  val empty : graph
end

functor TreeGraphFn (ordParam: ORD) :> GRAPH =
    struct 
        datatype node = Empty
            | Node of ordParam.t * node list
        datatype edge = Edge of ordParam.t * Empty
        datatype graph = Empty
            | Root of ordParam.t * Empty
        fun find (key, map) =
            case map of
                Empty => NONE
                | Node (k, v, l, r) => case Key.cmp (key, k) of
                    LESS => find (key, l)
                    | EQUAL => SOME v
                    | GREATER => find  (k, r)
        fun insert (key: Key.t, value, map) =
            case map of
                Empty => Node (key, value, Empty, Empty)
                | Node (k, v, l, r) => case Key.cmp (key, k) of
                    LESS => Node (k, v, insert (key, value, l), r)
                    | EQUAL => Node (k, value, l, r)
                    | GREATER => Node (k, v, l, insert (key, value, r))
    end *)


