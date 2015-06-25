namespace Mal
namespace Types

type [<StructuralComparisonAttribute; StructuralEqualityAttribute>] t =
    | Number of int
    | List of t list
    | Symbol of string
    | Lambda of fun_t
    | Keyword of string
    | Vector of t array
    | Hash of Map<t, t>
    | Atom of t
and [<CustomEquality; CustomComparison>] fun_t = 
    { f : (t list -> t) }
    override x.Equals(yobj) =  
        match yobj with 
        | :? fun_t as y -> System.Collections.Generic.Comparer.Default.Compare(x, y) = 0
        | _ -> false 
    override x.GetHashCode() = x.GetHashCode() // TODO: hash values stored in leaf/branch
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with 
            | :? fun_t as y -> System.Collections.Generic.Comparer.Default.Compare(x, y)
            | _ -> x.GetHashCode() - yobj.GetHashCode()
