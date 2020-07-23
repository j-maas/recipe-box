module Store.Path exposing (Path)

import Store.PathComponent exposing (PathComponent)


type alias Path =
    { folder : List PathComponent
    , name : PathComponent
    , extension : ( PathComponent, List PathComponent )
    }
