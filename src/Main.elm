module Main exposing (view)

import Browser
import Html exposing (Attribute, Html, button, div, h1, input, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import List exposing (indexedMap)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias TodoItem =
    { label : String
    , done : Bool
    }


type alias Model =
    { items : List TodoItem
    , inputValue : String
    }


init : Model
init =
    { items = []
    , inputValue = ""
    }


type Msg
    = ClearItems
    | AddItem
    | InputUpdated String
    | ToggleDone Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClearItems ->
            { model | inputValue = "", items = [] }

        AddItem ->
            if (model.inputValue |> String.length) > 0 then
                { model | items = [ { label = model.inputValue, done = False } ] |> List.append model.items, inputValue = "" }

            else
                model

        InputUpdated value ->
            { model | inputValue = value }

        ToggleDone index ->
            { model
                | items =
                    model.items
                        |> indexedMap
                            (\i item ->
                                if i == index then
                                    { item | done = item.done == False }

                                else
                                    item
                            )
            }


robButton : List (Attribute msg) -> List (Html msg) -> Html msg
robButton attributes children =
    button
        ([ style "background-color" "steelblue"
         , style "color" "white"
         , style "padding" "1rem"
         , style "margin" "1rem 0"
         ]
            ++ attributes
        )
        children


doneStylesAttr : Bool -> List (Attribute msg)
doneStylesAttr isDone =
    if isDone then
        [ style "text-decoration" "underline"
        , style "color" "green"
        ]

    else
        []


view : Model -> Html Msg
view model =
    div [ style "margin" "1rem" ]
        [ h1 []
            [ text "Hello elm todo list " ]
        , input [ onInput InputUpdated, value model.inputValue ] []
        , robButton
            [ onClick AddItem, style "display" "block", style "min-width" "300px" ]
            [ text "Add" ]
        , robButton
            [ onClick ClearItems, style "display" "block", style "min-width" "300px" ]
            [ text "Clear List" ]
        , model.items
            |> indexedMap
                (\i item ->
                    div
                        (doneStylesAttr
                            item.done
                            ++ [ onClick (ToggleDone i), style "padding" "1rem", style "font-size" "1.25rem" ]
                        )
                        [ text ((i + 1 |> String.fromInt) ++ " " ++ item.label) ]
                )
            |> div []
        ]
