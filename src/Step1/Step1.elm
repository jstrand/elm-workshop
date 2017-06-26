module Step1.Step1 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop
import Markdown


-- Model


type Position
    = Before
    | After


type alias Insert =
    ( Position, Int )


type alias Card =
    { id : Int
    , label : String
    }


type alias Model =
    { cards : List Card
    , dragDrop : DragDrop.Model Int Insert
    }


{-| This doesn't look right!
List.Extra can come in handy, for example using splitWhen, that is already imported (!)
<http://package.elm-lang.org/packages/elm-community/list-extra/6.1.0/List-Extra#splitWhen>

Try using elm-repl in the command line to test out your implementation

> cd src
> elm-repl
> import Step1.Step1 exposing (..)
> Expected behavior would be
> insertBefore 5 (\x -> x == 7) [3,12,7,4] == [3,12,5,7,4]

-}
insertBefore : a -> (a -> Bool) -> List a -> List a
insertBefore insert when into =
    insert :: into


moveCard : Int -> Insert -> List Card -> List Card
moveCard cardIdToMove insert cards =
    let
        ( otherCards, movedCards ) =
            List.partition (\card -> card.id /= cardIdToMove) cards
    in
        case movedCards of
            movedCard :: _ ->
                case insert of
                    ( Before, id ) ->
                        insertBefore movedCard (\card -> card.id == id) otherCards

                    ( After, id ) ->
                        cards

            -- ignore this for now
            [] ->
                cards


cardIds : List Card -> List Int
cardIds =
    List.map .id


model : Model
model =
    { cards = [ Card 1 "A card (1)", Card 2 "Another card (2)", Card 3 "Yet another card (3)" ]
    , dragDrop = DragDrop.init
    }



-- Update


type Msg
    = DragDropMsg (DragDrop.Msg Int Insert)


doDragDrop : DragDrop.Msg Int Insert -> Model -> ( Model, Cmd Msg )
doDragDrop msg model =
    let
        ( dragModel, result ) =
            DragDrop.update msg model.dragDrop

        dragId =
            DragDrop.getDragId model.dragDrop
    in
        { model
            | dragDrop = dragModel
            , cards =
                case result of
                    Nothing ->
                        model.cards

                    Just ( _, insert ) ->
                        case dragId of
                            Nothing ->
                                model.cards

                            Just dragId ->
                                moveCard dragId insert model.cards
        }
            ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragDropMsg dragMsg ->
            doDragDrop dragMsg model



-- View


cardStyle : Attribute Msg
cardStyle =
    style
        [ ( "background", "white" )
        , ( "box-shadow", "0.5px 0.5px 3px #00000080" )
        , ( "height", "40px" )
        , ( "margin", "10px" )
        , ( "padding", "10px" )
        , ( "border-radius", "2px" )
        ]


columnStyle : Attribute Msg
columnStyle =
    style
        [ ( "background", "#B8C3F0" )
        , ( "margin", "10px" )
        , ( "padding", "10px" )
        , ( "border-radius", "4px" )
        , ( "width", "250px" )
        ]


dropStyle : Attribute Msg
dropStyle =
    style
        [ ( "top", "50%" )
        , ( "margin", "10px" )
        , ( "height", "10px" )
        , ( "border", "2px dashed black" )
        ]


instructionStyle : Attribute Msg
instructionStyle =
    style
        [ ( "margin", "10px" )
        ]


instructions : Html Msg
instructions =
    Markdown.toHtml [ instructionStyle ] """
# Step 1 - Dropping in

Try dragging and dropping!

Let's fix the first problem; make the drag drop move the cards where we expect them.
Look for insertBefore in the code.

Concepts include passing functions as parameters, pattern matching and working with
lists
(tip: [splitWhen](http://package.elm-lang.org/packages/elm-community/list-extra/6.1.0/List-Extra#splitWhen))

[Step 2](../Step2/Step2.elm)
"""


dropZone : Insert -> Html Msg
dropZone insert =
    div
        (dropStyle :: (DragDrop.droppable DragDropMsg insert))
        []


viewCard : Card -> Bool -> Html Msg
viewCard card withDropZones =
    let
        draggableAttributes =
            DragDrop.draggable DragDropMsg card.id

        attributes =
            cardStyle :: draggableAttributes

        handleDropZone element =
            if withDropZones then
                (dropZone ( Before, card.id ) :: element :: [])
            else
                [ element ]

        cardElement =
            div attributes [ text card.label ]
    in
        div [] (handleDropZone cardElement)


view : Model -> Html Msg
view model =
    let
        dragId =
            DragDrop.getDragId model.dragDrop

        allCardIds =
            cardIds model.cards

        isDraggingACard =
            case dragId of
                Just _ ->
                    True

                Nothing ->
                    False

        viewCards =
            List.map (\card -> viewCard card isDraggingACard) model.cards
    in
        div [] [ div [ columnStyle ] viewCards, instructions ]


main : Program Never Model Msg
main =
    program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
