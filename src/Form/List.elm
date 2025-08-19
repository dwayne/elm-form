module Form.List exposing
    ( Forms
    , empty, fromList
    , get, modify
    , prepend, append, remove
    , validate
    , toList
    )

{-|


# Forms

@docs Forms


# Construct

@docs empty, fromList


# Accessors

@docs get, modify


# Change

@docs prepend, append, remove


# Validate

@docs validate


# Convert

@docs toList

-}

import Form exposing (Accessor, Form)
import Validation as V exposing (Validation)



-- FORMS


{-| -}
type Forms a
    = Forms
        { id : Int
        , elements : List ( Int, a )
        }



-- CONSTRUCT


{-| -}
empty : Forms a
empty =
    Forms
        { id = 0
        , elements = []
        }


{-| -}
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


fromListHelper : Int -> List ( Int, a ) -> List a -> ( Int, List ( Int, a ) )
fromListHelper i elements list =
    case list of
        [] ->
            ( i, List.reverse elements )

        x :: xs ->
            fromListHelper (i + 1) (( i, x ) :: elements) xs


{-| -}
get : Int -> (accessors -> Accessor state a) -> a -> Forms (Form state accessors error output) -> a
get id toAccessor default (Forms { elements }) =
    elements
        |> List.filter (Tuple.first >> (==) id)
        |> List.head
        |> Maybe.map (Tuple.second >> Form.get toAccessor)
        |> Maybe.withDefault default


{-| -}
modify : Int -> (accessors -> Accessor state a) -> (a -> a) -> Forms (Form state accessors error output) -> Forms (Form state accessors error output)
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



-- CHANGE


{-| -}
prepend : a -> Forms a -> Forms a
prepend x (Forms c) =
    Forms { c | id = c.id + 1, elements = ( c.id, x ) :: c.elements }


{-| -}
append : a -> Forms a -> Forms a
append x (Forms c) =
    Forms { c | id = c.id + 1, elements = c.elements ++ [ ( c.id, x ) ] }


{-| -}
remove : Int -> Forms a -> Forms a
remove id (Forms c) =
    Forms { c | elements = List.filter (Tuple.first >> (/=) id) c.elements }



--VALIDATE


{-| -}
validate : (x -> y) -> Forms (Form state accessors x output) -> Validation y (List output)
validate f (Forms c) =
    List.foldr
        (\( _, form ) -> V.map2 (::) (Form.validate form |> V.mapError f))
        (V.succeed [])
        c.elements



--
-- What about validateResult and validateMaybe?
--
-- CONVERT


{-| -}
toList : Forms a -> List ( Int, a )
toList (Forms { elements }) =
    elements
