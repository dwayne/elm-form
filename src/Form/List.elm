module Form.List exposing
    ( Forms
    , Id
    , append
    , empty
    , fromList
    , get
    , idToString
    , modify
    , prepend
    , remove
    , toList
    , validate
    )

import Form3 as Form exposing (Accessor, Form)
import Validation as V exposing (Validation)


type Forms a
    = Forms
        { id : Int
        , elements : List ( Id, a )
        }


type Id
    = Id Int


idToString : Id -> String
idToString (Id id) =
    String.fromInt id


empty : Forms a
empty =
    Forms
        { id = 0
        , elements = []
        }


fromList : List a -> Forms a
fromList list =
    let
        ( id, elements ) =
            fromListHelper 0 [] list
    in
    Forms
        { id = id
        , elements = elements
        }


fromListHelper : Int -> List ( Id, a ) -> List a -> ( Int, List ( Id, a ) )
fromListHelper i elements list =
    case list of
        [] ->
            ( i, List.reverse elements )

        x :: xs ->
            fromListHelper (i + 1) (( Id i, x ) :: elements) xs


get : Id -> (accessors -> Accessor state a) -> a -> Forms (Form state accessors error output) -> a
get id toAccessor default (Forms { elements }) =
    elements
        |> List.filter (Tuple.first >> (==) id)
        |> List.head
        |> Maybe.map (Tuple.second >> Form.get toAccessor)
        |> Maybe.withDefault default


modify : Id -> (accessors -> Accessor state a) -> (a -> a) -> Forms (Form state accessors error output) -> Forms (Form state accessors error output)
modify id toAccessor f (Forms c) =
    Forms
        { c
            | elements =
                List.map
                    (\( currentId, element ) ->
                        ( currentId
                        , if currentId == id then
                            Form.modify toAccessor f element

                          else
                            element
                        )
                    )
                    c.elements
        }


prepend : a -> Forms a -> Forms a
prepend x (Forms c) =
    Forms { c | id = c.id + 1, elements = ( Id c.id, x ) :: c.elements }


append : a -> Forms a -> Forms a
append x (Forms c) =
    Forms { c | id = c.id + 1, elements = c.elements ++ [ ( Id c.id, x ) ] }


remove : Id -> Forms a -> Forms a
remove id (Forms c) =
    Forms { c | elements = List.filter (Tuple.first >> (/=) id) c.elements }


validate : (x -> y) -> Forms (Form state accessors x output) -> Validation y (List output)
validate f (Forms c) =
    List.foldr
        (\( _, form ) -> V.map2 (::) (Form.validate form |> V.mapError f))
        (V.succeed [])
        c.elements


toList : Forms a -> List ( Id, a )
toList (Forms { elements }) =
    elements
