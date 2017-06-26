module Step2.Step2 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop
import List.Extra exposing (splitWhen)
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


insertBefore : a -> (a -> Bool) -> List a -> List a
insertBefore insert when into =
    case splitWhen when into of
        Just ( before, after ) ->
            before ++ insert :: after

        Nothing ->
            insert :: into


insertLast : a -> List a -> List a
insertLast new xs =
    xs ++ [ new ]


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
                        insertLast movedCard otherCards

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
# Step 2 - Model change

Now that insertBefore works drag and drop should work.

However, we would like to be able to drag cards to the end of the column.
A new drop zone has now been added to the column after all cards.
It works, but now the model is a bit outdated, it requires an ID for a card
even though we just want it at the end of the column.

The type Position is currently ```Before | After```, but let's try deleting the
Position type altogether and changing the type Insert to be:

```
type Insert = Before Int | Last
```

See what the compiler starts complaining about, follow the compiler errors until it works again, then move on to step 3.

[Step 1](../Step1/Step1.elm) [Step 3](../Step3/Step3.elm)
"""


dropZone : Insert -> Html Msg
dropZone insert =
    div
        (dropStyle :: (DragDrop.droppable DragDropMsg insert))
        []


viewCard : Bool -> Card -> Html Msg
viewCard withDropZones card =
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


hasValue : Maybe a -> Bool
hasValue =
    (Maybe.withDefault False) << (Maybe.map (\_ -> True))


view : Model -> Html Msg
view model =
    let
        isDraggingACard =
            hasValue <| DragDrop.getDragId model.dragDrop

        viewCards =
            List.map (viewCard isDraggingACard) model.cards

        fakeLastCardId =
            0

        lastDropZone =
            if isDraggingACard then
                [ dropZone ( After, fakeLastCardId ) ]
            else
                []
    in
        div [] [ div [ columnStyle ] (viewCards ++ lastDropZone), instructions ]


main : Program Never Model Msg
main =
    program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
