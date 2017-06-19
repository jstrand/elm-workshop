module Step2 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop
import List.Extra exposing (splitWhen)


-- Model


type Position = Before | After


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
    Nothing -> insert :: into


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
          (After, id) -> insertLast movedCard otherCards
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


instruction t = p [] [ text t ]


instructions = div [instructionStyle]
  [ h1 [] [ text "Step 2" ]
  , instruction "Now that insertBefore works drag and drop should work."
  , instruction "However, we would like to be able to drag cards to the end of the column. A new drop zone has now been added to the column after all cards and it works, but the model is a bit outdated, it requires an ID for a card even though we just want it at the end of the column."
  , instruction "The type Position is currently Before | After, but let's try deleting the Position type altogether and changing the type Insert to be:"
  , pre [] [text "type Insert = Before Int | Last"]
  , instruction "See what the compiler starts complaining about, follow the compiler errors until it works again, then move on to step 3."
  , a [href "../step3/Step3.elm"] [text "Step 3"]
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

        fakeLastCardId = 0
        lastDropZone =
          case isDraggingACard of
            True -> [dropZone (After, fakeLastCardId)]
            False -> []
    in
        div [] [ div [columnStyle] (viewCards ++ lastDropZone), instructions ]


main =
    program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

