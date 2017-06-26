module Step3.Step3 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop
import List.Extra exposing (splitWhen, last)
import Markdown


-- Model


type Insert
    = Before Int
    | Last


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
            into


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
            movedCard :: rest ->
                case insert of
                    Before id ->
                        insertBefore movedCard (\card -> card.id == id) otherCards

                    Last ->
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
                (dropZone (Before card.id) :: element :: [])
            else
                [ element ]

        cardElement =
            div attributes [ text card.label ]
    in
        div [] (handleDropZone cardElement)


isOneBeforeTheOther : a -> a -> List a -> Bool
isOneBeforeTheOther one other list =
    case list of
        first :: second :: rest ->
            if first == one && second == other then
                True
            else
                isOneBeforeTheOther one other rest

        _ ->
            False


instructions : Html Msg
instructions =
    Markdown.toHtml [ instructionStyle ] """
# Step 3 - Fixing a bug

In this step we only want to be able to drop cards where it makes sense.
This means that there should be no drop zone before or after the card being dragged.

Code has already been added to do this, feel free to look it over. Does it work though?

When it works, move on!

[Step 2](../Step2/Step2.elm) [Step 4](../Step4/Step4.elm)
"""


view : Model -> Html Msg
view model =
    let
        dragId =
            DragDrop.getDragId model.dragDrop

        allCardIds =
            cardIds model.cards

        isLastCardDragged =
            Maybe.map2 (\draggedId theLastCard -> draggedId == theLastCard.id) dragId (last model.cards)
                |> Maybe.withDefault False

        -- Here is an alternative to 'case' when converting from a Maybe to a Bool,
        -- dragId is a Maybe because there might not be any dragging going on
        isCardBeforeDragged cardId =
            Maybe.map (\draggedId -> isOneBeforeTheOther draggedId cardId allCardIds) dragId
                |> Maybe.withDefault False

        -- Here we use "cases" to act differently depending on if there is a drag operation active
        -- Checking the last card follows the same pattern
        showZones cardId =
            case dragId of
                Just id ->
                    cardId /= id && not (isCardBeforeDragged cardId)

                Nothing ->
                    False

        lastDropZone =
            case dragId of
                Just id ->
                    if isLastCardDragged then
                        []
                    else
                        [ dropZone Last ]

                Nothing ->
                    []

        viewCards =
            List.map (\card -> viewCard card (showZones card.id)) model.cards
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
