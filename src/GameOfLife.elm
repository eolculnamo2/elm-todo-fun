module GameOfLife exposing (columns, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Random.List
import Task
import Time


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


interval : Float
interval =
    toFloat 100


rows : Int
rows =
    25


columns : Int
columns =
    25


totalCells : Int
totalCells =
    rows * columns


cellPxSize =
    20


initList : Array Cell
initList =
    Array.repeat totalCells { isAlive = True, id = 0 }
        |> Array.indexedMap (\i cell -> { cell | id = i })


type alias Cell =
    { isAlive : Bool
    , id : Int
    }


type alias Model =
    { cells : Array Cell
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cells = initList
      }
      -- this is bad/inefficient, but not sure how better to do it atm
      -- https://medium.com/elm-shorts/how-to-turn-a-msg-into-a-cmd-msg-in-elm-5dd095175d84
    , Task.succeed RandomizeCells |> Task.perform identity
    )


type Msg
    = Start (List Bool)
    | RandomizeCells
    | Tick Time.Posix
    | CellClicked Int


halfTotalCells : Int
halfTotalCells =
    (rows * columns |> toFloat) / 2 |> floor


generateRandomList : Random.Generator (List Bool)
generateRandomList =
    Random.List.shuffle (List.repeat halfTotalCells False ++ List.repeat halfTotalCells True)


getAlive : Array Cell -> Cell -> Int -> Bool
getAlive cells currentCell idToGet =
    case cells |> Array.get idToGet of
        Just cell ->
            cell.isAlive

        Nothing ->
            False


getTotalLiveNeighbors : Array Cell -> Cell -> Int
getTotalLiveNeighbors cells currentCell =
    let
        getAliveForCurrentCell =
            getAlive cells currentCell
    in
    List.foldl
        (\idToGet total ->
            if getAliveForCurrentCell idToGet then
                -- top left
                total + 1

            else if getAliveForCurrentCell idToGet then
                -- top
                total + 1

            else if getAliveForCurrentCell idToGet then
                -- top right
                total + 1

            else if getAliveForCurrentCell idToGet then
                -- left
                total + 1

            else if getAliveForCurrentCell idToGet then
                -- right
                total + 1

            else if getAliveForCurrentCell idToGet then
                -- bottom left
                total + 1

            else if getAliveForCurrentCell idToGet then
                -- bottom
                total + 1

            else if getAliveForCurrentCell idToGet then
                -- bottom right
                total + 1

            else
                total
        )
        0
        [ currentCell.id - rows - 1
        , currentCell.id - rows
        , currentCell.id - rows + 1
        , currentCell.id - 1
        , currentCell.id + 1
        , currentCell.id + rows - 1
        , currentCell.id + rows
        , currentCell.id + rows + 1
        ]


doesLiveCellSurvive : Int -> Bool
doesLiveCellSurvive totalLiveNeighbors =
    if totalLiveNeighbors == 2 || totalLiveNeighbors == 3 then
        True

    else
        False


doesDeadRegenerate : Int -> Bool
doesDeadRegenerate totalLiveNeighbors =
    if totalLiveNeighbors == 3 then
        True

    else
        False


handleCellIteration : Array Cell -> Cell -> Cell
handleCellIteration cells currentCell =
    let
        handleLiveCell =
            getTotalLiveNeighbors cells >> doesLiveCellSurvive

        handleDeadCell =
            getTotalLiveNeighbors cells >> doesDeadRegenerate
    in
    { currentCell
        | isAlive =
            currentCell
                |> (if currentCell.isAlive then
                        handleLiveCell

                    else
                        handleDeadCell
                   )
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start booleans ->
            -- probably need to break these up?
            ( { model
                | cells =
                    model.cells
                        |> Array.indexedMap
                            (\i cell ->
                                { cell
                                    | isAlive =
                                        case Array.get i (booleans |> Array.fromList) of
                                            Just booleanValue ->
                                                booleanValue

                                            Nothing ->
                                                False
                                }
                            )
              }
            , Cmd.none
            )

        RandomizeCells ->
            ( model
            , Random.generate Start generateRandomList
            )

        CellClicked id ->
            ( { model
                | cells =
                    model.cells
                        |> Array.map
                            (\cell ->
                                if cell.id == id then
                                    { cell | isAlive = cell.isAlive == False }

                                else
                                    cell
                            )
              }
            , Cmd.none
            )

        Tick time ->
            -- need to handle cell rules here
            ( { model | cells = model.cells |> Array.map (\cell -> handleCellIteration model.cells cell) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every interval Tick


mappedCells : Array Cell -> List (Html Msg)
mappedCells cells =
    cells
        |> Array.map
            (\cell ->
                div
                    [ CellClicked cell.id |> onClick
                    , style "border" "1px solid #333"
                    , style "height" (String.fromInt cellPxSize ++ "px")
                    , style "width" (String.fromInt cellPxSize ++ "px")
                    , style "background-color"
                        (if cell.isAlive then
                            "green"

                         else
                            "gray"
                        )
                    ]
                    []
            )
        |> Array.toList


view : Model -> Html Msg
view model =
    div []
        [ h1 [ style "text-align" "center" ] [ text "Game of Life" ]
        , button [ onClick RandomizeCells ] [ text "Start" ]
        , div [ style "display" "grid", style "place-items" "center" ]
            [ div
                [ style "display" "grid"
                , style "grid-template-columns" ("repeat(" ++ (columns |> String.fromInt) ++ ", " ++ (String.fromInt cellPxSize ++ "px"))
                ]
                -- []
                (mappedCells model.cells)
            ]
        ]
