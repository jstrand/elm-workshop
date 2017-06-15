module Program exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop
import List.Extra exposing (splitWhen)


-- Model


type Relative = Before | After


type alias Position =
  { id: Int,
    relative: Relative
  }


type alias Card =
  { id: Int
  , label: String
  }


type alias Model =
    { cards : List Card
    , dragDrop : DragDrop.Model Int Position
    }


insertBefore : a -> (a -> Bool) -> List a -> List a
insertBefore new predicate xs =
  case splitWhen predicate xs of
    Just (before, after) -> before ++ new :: after
    Nothing -> xs


insertAfter : a -> (a -> Bool) -> List a -> List a
insertAfter new predicate xs =
  case splitWhen predicate xs of
    Just (before, headAfter::restAfter) -> before ++ headAfter :: new :: restAfter
    Just (before, []) -> before
    Nothing -> xs


moveCard : Int -> Position -> List Card -> List Card
moveCard cardIdToMove {id, relative} cards =
  let
    (otherCards, movedCards) = List.partition (\card -> card.id /= cardIdToMove) cards
  in
    case movedCards of
      movedCard :: rest ->
        case relative of
          Before -> insertBefore movedCard (\card -> card.id == id) otherCards
          After -> insertAfter movedCard (\card -> card.id == id) otherCards
      [] -> cards


cardIds cards = List.map (\x -> x.id) cards


model =
    { cards = [ Card 1 "A card (1)", Card 2 "Another card (2)", Card 3 "Yet another card (3)" ]
    , dragDrop = DragDrop.init
    }


-- Update


type Msg
    = DragDropMsg (DragDrop.Msg Int Position)


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

        Just (_, position) ->
          case dragId of
            Nothing -> model.cards
            Just dragId -> moveCard dragId position model.cards
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


dropZone cardId pos =
  div
    ( dropStyle :: (DragDrop.droppable DragDropMsg (Position cardId pos)) )
    []


viewCard card withDropZones =
  let
    draggableAttributes = DragDrop.draggable DragDropMsg card.id
    attributes = cardStyle :: draggableAttributes
    handleDropZone element = if withDropZones then (dropZone card.id Before :: element :: []) else [element]
    cardElement = div attributes [ text card.label ]
  in
    div [] (handleDropZone cardElement)


isXBeforeY x y xs =
  case xs of
    [] -> False
    x :: [] -> False
    x1 :: x2 :: xs ->
      if x1 == x && x2 == y then
        True
      else
        isXBeforeY x y (x2::xs)


view model =
    let
        dragId = DragDrop.getDragId model.dragDrop
        allCardIds = cardIds model.cards

        lastCard =
          model.cards
          |> List.reverse
          |> List.head

        isLastCardDragged =
          case dragId of
            Just id ->
              case lastCard of
                Just theLastCardIsReal -> theLastCardIsReal.id == id
                Nothing -> False
            Nothing -> False

        isCardBeforeDragged cardId =
          dragId
          |> Maybe.map (\id -> isXBeforeY id cardId allCardIds)
          |> Maybe.withDefault False

        showZones cardId =
          case dragId of
            Just id -> cardId /= id && not (isCardBeforeDragged cardId)
            Nothing -> False

        lastDropZone =
          case dragId of
            Just id ->
              case lastCard of
                Just theLastCardIsReal -> [dropZone theLastCardIsReal.id After]
                Nothing -> []
            Nothing -> []

        viewCards = List.map (\card -> viewCard card (showZones card.id)) model.cards
    in
        div [columnStyle] (viewCards ++ lastDropZone)


main =
    program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

