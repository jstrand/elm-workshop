module Step4.Step4 exposing (..)

import Html exposing (..)
import Html.Events as Events
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop
import List.Extra exposing (splitWhen, last)


-- Model


type Insert =
    Before Int
  | Last


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
      movedCard :: rest ->
        case insert of
          Before id -> insertBefore movedCard (\card -> card.id == id) otherCards
          Last -> insertLast movedCard otherCards
      [] -> cards


cardIds cards = List.map (\x -> x.id) cards


-- Help me!
-- Given a list of cards I should return a new number that is not already an id amongst the given cards
-- List.maximum could come in handy to implement me, so could cardIds
-- http://package.elm-lang.org/packages/elm-lang/core/5.1.1/List#maximum
nextCardId : List Card -> Int
nextCardId cards = 0


-- This could be something to use in an update message to add cards
-- (How is nextCardId implemented though?)
addCard : List Card -> String -> List Card
addCard cards name =
  let
    newCard = Card (nextCardId cards) name
  in
    newCard :: cards


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


inputCardStyle = style
  [ ("margin", "10px")
  ]


instructionStyle = style
  [ ("margin", "10px")
  ]


dropZone insert =
  div
    ( dropStyle :: (DragDrop.droppable DragDropMsg insert) )
    []


viewCard card withDropZones =
  let
    draggableAttributes = DragDrop.draggable DragDropMsg card.id
    attributes = cardStyle :: draggableAttributes
    handleDropZone element = if withDropZones then (dropZone (Before card.id) :: element :: []) else [element]
    cardElement = div attributes [ text card.label ]
  in
    div [] (handleDropZone cardElement)


isOneBeforeTheOther : a -> a -> List a -> Bool
isOneBeforeTheOther one other list =
  case list of
    first :: second :: rest ->
      if first == one && second == other then
        True
      else
        isOneBeforeTheOther one other (second :: rest)
    _ -> False


instruction t = p [] [ text t ]


instructions = div [inputCardStyle]
  [ h1 [] [ text "Step 4 - The Text Field" ]
  , instruction "It's time to add cards. An input field is there along with an alluring button."
  , instruction "The input field needs some events, the model needs to hold more data and there should be a message to add cards and handle input of the new card name."
  , a [href "../Step5/Step5.elm"] [text "Then it's time to add columns in step 5"]
  ]


-- Help me!
-- See the example in http://elm-lang.org/examples/buttons on how to act on button events.
-- And http://elm-lang.org/examples/field on how to act on events from input field
viewCardInput nameSoFar = div [cardStyle]
  [ input [size 14] []
  , button [] [text "Add"]
  ]


view model =
    let
        dragId = DragDrop.getDragId model.dragDrop
        allCardIds = cardIds model.cards

        isLastCardDragged =
          Maybe.map2 (\draggedId theLastCard -> draggedId == theLastCard.id) dragId (last model.cards)
          |> Maybe.withDefault False

        isCardBeforeDragged cardId =
          dragId
          |> Maybe.map (\draggedId -> isOneBeforeTheOther draggedId cardId allCardIds)
          |> Maybe.withDefault False

        showZones cardId =
          case dragId of
            Just id -> cardId /= id && not (isCardBeforeDragged cardId)
            Nothing -> False

        lastDropZone =
          case dragId of
            Just id -> if isLastCardDragged then [] else [dropZone Last]
            Nothing -> []

        viewCards = List.map (\card -> viewCard card (showZones card.id)) model.cards
    in
        div [] [ div [columnStyle] ((viewCardInput "New card") :: viewCards ++ lastDropZone), instructions ]


main =
    program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

