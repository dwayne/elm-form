module Form.List exposing
    ( Forms, Id
    , empty, fromList
    , get, set, modify, update
    , prepend, append, remove
    , validate, validateAsMaybe, validateAsResult
    , toList
    )

{-| Provides a way to manage an ordered list of uniquely identifiable forms.


# Forms

@docs Forms, Id


# Construct

@docs empty, fromList


# Accessors

@docs get, set, modify, update


# Change

@docs prepend, append, remove


# Validate

@docs validate, validateAsMaybe, validateAsResult


# Convert

@docs toList

-}

import Form exposing (Accessor, Form)
import Validation as V exposing (Validation)



-- FORMS


{-| An ordered list of uniquely identifiable forms. The identifiers are automatically assigned and managed.
-}
type Forms form
    = Forms
        { id : Id
        , elements : List ( Id, form )
        }


{-| An identifier is represented by a non-negative integer.
-}
type alias Id =
    Int



-- CONSTRUCT


{-| Create an empty list of forms.
-}
empty : Forms form
empty =
    Forms
        { id = 0
        , elements = []
        }


{-| Create an ordered list of uniquely identifiable forms derived from the given list of forms.
-}
fromList : List form -> Forms form
fromList list =
    let
        ( id, elements ) =
            fromListHelper 0 [] list
    in
    Forms
        { id = id
        , elements = elements
        }


fromListHelper : Id -> List ( Id, form ) -> List form -> ( Id, List ( Id, form ) )
fromListHelper id elements list =
    case list of
        [] ->
            ( id, List.reverse elements )

        x :: xs ->
            fromListHelper (id + 1) (( id, x ) :: elements) xs



-- ACCESSORS


{-| Get the value of a property associated with the form identified by the given identifier.

If the form with the given identifier cannot be found then `Nothing` is returned.

-}
get : Id -> (accessors -> Accessor state a) -> Forms (Form state accessors error output) -> Maybe a
get id toAccessor (Forms { elements }) =
    elements
        |> List.filter (Tuple.first >> (==) id)
        |> List.head
        |> Maybe.map (Tuple.second >> Form.get toAccessor)


{-| Set the value of a property associated with the form identified by the given identifier.

If the form with the given identifier cannot be found then no change is made.

-}
set : Id -> (accessors -> Accessor state a) -> a -> Forms (Form state accessors error output) -> Forms (Form state accessors error output)
set id toAccessor x =
    modify id toAccessor (always x)


{-| Modify the value of a property associated with the form identified by the given identifier.

If the form with the given identifier cannot be found then no change is made.

-}
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


{-| Update the value of a property associated with the form identified by the given identifier.

If the form with the given identifier cannot be found then no change is made.

-}
update : Id -> (accessors -> state -> state) -> Forms (Form state accessors error output) -> Forms (Form state accessors error output)
update id f (Forms c) =
    Forms
        { c
            | elements =
                List.map
                    (\( currentId, element ) ->
                        ( currentId
                        , if currentId == id then
                            Form.update f element

                          else
                            element
                        )
                    )
                    c.elements
        }



-- CHANGE


{-| Add a form to the start of the list.
-}
prepend : form -> Forms form -> Forms form
prepend x (Forms c) =
    Forms { c | id = c.id + 1, elements = ( c.id, x ) :: c.elements }


{-| Add a form to the end of the list.
-}
append : form -> Forms form -> Forms form
append x (Forms c) =
    Forms { c | id = c.id + 1, elements = c.elements ++ [ ( c.id, x ) ] }


{-| Remove the form identified by the given identifier.

If the form with the given identifier cannot be found then no change is made.

-}
remove : Id -> Forms form -> Forms form
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


{-| -}
validateAsMaybe : (x -> y) -> Forms (Form state accessors x output) -> Maybe (List output)
validateAsMaybe f =
    validate f >> V.toMaybe


{-| -}
validateAsResult : (x -> y) -> Forms (Form state accessors x output) -> Result (List y) (List output)
validateAsResult f =
    validate f >> V.toResult



-- CONVERT


{-| -}
toList : Forms form -> List ( Id, form )
toList (Forms { elements }) =
    elements
