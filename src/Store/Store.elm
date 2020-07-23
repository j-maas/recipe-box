module Store.Store exposing (Id, Store)

import Db exposing (Db)
import Id
import Store.Path exposing (Path)


type Store item
    = Store (Db item)


type Id item
    = Id (Id.Id item)



{-
   insert : Path -> item -> Store item -> Store item
   insert path item store =
-}
