module Step1 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop
import List.Extra exposing (splitWhen)


-- Model


type Position =
    Before | After

type alias Insert = (Position, Int)

type alias Card =
  { id: Int
  , label: String
  }


type alias Model =
    { cards : List Card
    , dragDrop : DragDrop.Model Int Insert
    }


insertBefore : a -> (a -> Bool) -> List a -> List a
insertBefore insert when into =
  case splitWhen when into of
    Just (before, after) -> before ++ insert :: after
    Nothing -> into


insertLast : a -> List a -> List a
insertLast new xs = xs ++ [new]


moveCard : Int -> Insert -> List Card -> List Card
moveCard cardIdToMove insert cards =
  let
    (otherCards, movedCards) = List.partition (\card -> card.id /= cardIdToMove) cards
  in
    case movedCards of
      movedCard :: _ ->
        case insert of
          (Before, id) -> insertBefore movedCard (\card -> card.id == id) otherCards
          (After, id) -> insertAfter movedCard (\card -> card.id == id) otherCards
      [] -> cards


cardIds cards = List.map (\x -> x.id) cards


model =
    { cards = [ Card 1 "A card (1)", Card 2 "Another card (2)", Card 3 "Yet another card (3)" ]
    , dragDrop = DragDrop.init
    }


-- Update


type Msg
    = DragDropMsg (DragDrop.Msg Int Insert)


doDragDrop msg model =
  let
    ( dragModel, result ) = DragDrop.update msg model.dragDrop
    dragId = DragDrop.getDragId model.dragDrop
  in
    { model
    | dragDrop = dragModel
    , cards =
      case result of
        Nothing ->
          model.cards

        Just (_, insert) ->
          case dragId of
            Nothing -> model.cards
            Just dragId -> moveCard dragId insert model.cards
    } ! []


update msg model =
    case msg of
        DragDropMsg dragMsg -> doDragDrop dragMsg model


-- View


cardStyle = style
  [ ("background", "white")
  , ("box-shadow", "0.5px 0.5px 3px #00000080")
  , ("height", "40px")
  , ("margin", "10px")
  , ("padding", "10px")
  , ("border-radius", "2px")
  ]


columnStyle = style
  [ ("background", "#8A86FA")
  , ("margin", "10px")
  , ("padding", "10px")
  , ("border-radius", "4px")
  , ("width", "250px")
  ]


dropStyle = style
  [ ("top", "50%")
  , ("margin", "10px")
  , ("height", "10px")
  , ("border", "2px dashed black")
  ]


instructionStyle = style
  [ ("margin", "10px")
  ]


instructions = div [instructionStyle]
  [ h1 [] [ text "Step 1" ]
  , p [] [ text "Try dragging and dropping!" ]
  , p [] [ text "Let's fix the first problem; make the drag drop move the cards where we expect them. Look for insertBefore and insertAfter in the code." ]
  , p [] [ text "Concepts include passing functions as parameters, pattern matching and list concatenation." ]
  , p [] [ text "Another problem is that we can show drop zones that don't make sense (the card is already there), but let's fix that later." ]
  ]


dropZone insert =
  div
    ( dropStyle :: (DragDrop.droppable DragDropMsg insert) )
    []


viewCard card withDropZones =
  let
    draggableAttributes = DragDrop.draggable DragDropMsg card.id
    attributes = cardStyle :: draggableAttributes
    handleDropZone element = if withDropZones then (dropZone (Before, card.id) :: element :: []) else [element]
    cardElement = div attributes [ text card.label ]
  in
    div [] (handleDropZone cardElement)

view model =
    let
        dragId = DragDrop.getDragId model.dragDrop
        allCardIds = cardIds model.cards

        isDraggingACard =
          case dragId of
            Just _ -> True
            Nothing -> False

        viewCards = List.map (\card -> viewCard card isDraggingACard) model.cards
    in
        div [] [ div [columnStyle] viewCards, instructions ]


main =
    program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

