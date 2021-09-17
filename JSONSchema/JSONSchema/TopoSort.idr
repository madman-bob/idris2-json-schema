module JSONSchema.TopoSort

import Data.List
import public Data.List1

import public Libraries.Data.SortedMap
import public Libraries.Data.SortedSet
import public Libraries.Data.Graph

public export
Graph : Type -> Type
Graph a = SortedMap a (SortedSet a)

export
topoSort : Ord a => Graph a -> List (List1 a)
topoSort = reverse . tarjan
