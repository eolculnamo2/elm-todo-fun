module Pokedex exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, input, p, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, int, map2, string)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias PokemonResponse =
    { name : String
    , weight : Int
    }



-- In a real project, there's some things I'd change about the shape of the model


type alias Model =
    { pokemonName : Maybe String
    , pokemonWeight : Maybe Int
    , searchInputValue : String
    , errorMsg : Maybe String
    , loadingRequest : Bool
    }


type Msg
    = SearchInputChanged String
    | InitSearch
    | SearchComplete (Result Http.Error PokemonResponse)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pokemonName = Nothing
      , pokemonWeight = Nothing
      , searchInputValue = ""
      , errorMsg = Nothing
      , loadingRequest = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInputChanged newValue ->
            ( { model | searchInputValue = newValue }, Cmd.none )

        InitSearch ->
            ( { model | searchInputValue = "", errorMsg = Nothing, loadingRequest = True }, getPokemonDetails model.searchInputValue )

        SearchComplete result ->
            case result of
                Ok pokemonData ->
                    ( { model
                        | pokemonName = Just pokemonData.name
                        , pokemonWeight = Just pokemonData.weight
                        , loadingRequest = False
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | pokemonName = Nothing
                        , pokemonWeight = Nothing
                        , errorMsg = Just "Something went wrong with your search. Check your spelling and try again."
                        , loadingRequest = False
                      }
                    , Cmd.none
                    )


pokemonResultDecoder : Decoder PokemonResponse
pokemonResultDecoder =
    map2 PokemonResponse (field "name" string) (field "weight" int)


getPokemonDetails : String -> Cmd Msg
getPokemonDetails name =
    Http.get
        { url = "https://pokeapi.co/api/v2/" ++ "pokemon/" ++ name
        , expect = Http.expectJson SearchComplete pokemonResultDecoder
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Pokedex" ]
        , case ( model.errorMsg, model.loadingRequest ) of
            ( Just msg, False ) ->
                p [ style "color" "red", style "font-size" ".875rem" ] [ text msg ]

            ( _, True ) ->
                p [ style "color" "green", style "font-size" ".875rem" ] [ text "Loading..." ]

            _ ->
                text ""
        , input [ onInput SearchInputChanged, value model.searchInputValue ] []
        , button [ onClick InitSearch ] [ text "Search" ]
        , p []
            [ case model.pokemonName of
                Just name ->
                    text <| "Name: " ++ name

                Nothing ->
                    text "Search for a pokemon"
            ]
        , case model.pokemonWeight of
            Just w ->
                p []
                    [ text <| "Weight: " ++ String.fromInt w
                    ]

            Nothing ->
                text ""
        ]
