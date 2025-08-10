module DynamicForm.Publication exposing
    ( Error
    , Publication(..)
    , fieldType
    , fromString
    , toString
    )

import Field.Advanced as F


type Publication
    = Post
    | Question


type alias Error =
    F.Error Never


fromString : String -> Result Error Publication
fromString =
    F.trim
        (\s ->
            case s of
                "post" ->
                    Ok Post

                "question" ->
                    Ok Question

                _ ->
                    Err (F.validationError s)
        )


toString : Publication -> String
toString p =
    case p of
        Post ->
            "post"

        Question ->
            "question"


fieldType : F.Type Error Publication
fieldType =
    F.customType
        { fromString = fromString
        , toString = toString
        }
