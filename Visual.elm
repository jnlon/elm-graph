--import Html exposing (..)
import GraphicSVG exposing (..)
import Array
import String
import Data
import Time

type alias BarData = (Int,String)
type alias Bounds =  
  { top : Int,
    left : Int,
    bottom: Int,
    right : Int }

-- Utilities 
range from to step =  -- inclusive
  let genRange i lst = 
      if i <= to 
      then (genRange (i+step) (i :: lst))
      else lst 
  in
      List.reverse <| genRange from []

highestCommonDivisorMax max n =
  let r i = 
      if n `rem` i == 0 && i < max
      then i
      else if (i < 0) then 1
      else r (i - 1)
  in
      r n

getBarColour ith = 
  let colours = Array.fromList [red,blue,green,yellow,purple] in
  Maybe.withDefault red <| 
    Array.get (ith % Array.length colours ) <| colours

smallText t = filled black <| size 9 t 

-- TODO: use elm-http?
data = List.map (\j -> ((toFloat j.availableBikes), j.stationName)) Data.data
--data = [(1,"test1"), (3,"test3")]

-- Size information
b = {left = -450.0, right = 450.0, top = 225.0, bottom = -150.0}
barWidth = graphWidth/(toFloat (List.length data))
graphHeight = (abs b.bottom) + b.top
graphWidth =  (abs b.left) + b.right
maxHeight = (fst (Maybe.withDefault (0,"") (List.maximum data)))
numBars = List.length data

-- These are magic
toHeight n = ((n/maxHeight)*graphHeight)
toXPosition i = ((i/(toFloat numBars))*graphWidth) + (b.left + barWidth/2)

-- assumes minimum is zero
makeGraph yMax = 
  let leftLine = line (b.left,b.bottom) (b.left,b.top)
      bottomLine = line (b.left,b.bottom) (b.right,b.bottom)
      labelStyle = filled black
      yMakeGrid y = line (b.left,y) (b.right,y) |> outlined (solid 1) grey
      yMakeLine y = line (b.left,y) (b.right,y) |> outlined (solid 1) black
      yLabels = toFloat <| highestCommonDivisorMax 10 (round maxHeight)
      formatLabel n = (String.left 3 (toString n))
      yMakeLabel i y = 
        group 
          [ yMakeLine y
          , text (formatLabel ((toFloat i)*(yMax/yLabels))) |> smallText |> move ((b.left-25),y)]
      yLabelLines =  
        List.indexedMap yMakeLabel (range b.bottom b.top (graphHeight/yLabels))
      yGrid = 
        List.map yMakeGrid (range b.bottom b.top (graphHeight/yLabels/5))
  in 
  group 
    [ group yGrid
    , group yLabelLines
    , leftLine |> outlined (solid 1) black 
    , bottomLine |> outlined (solid 1) black ]

makeGraphAnnotation datas t =
  let i = (round t) % numBars
      data = Maybe.withDefault (0, "") (List.head (List.drop i datas))
      annotLine = line ((toXPosition (toFloat i)), b.bottom) (b.left+15,b.bottom-15)
      annotDot = circle (barWidth/50) |> filled black |> move ((toXPosition (toFloat i)), b.bottom)
      columnText = text (snd data) |> size 20 |> filled black |> move (b.left,b.bottom-40)
      valueText = text (toString (fst data)) |> size 18 |> filled black |> move (b.left,b.bottom-70)
  in
      group 
        [ annotLine |> outlined (solid 3.0) black
        , annotDot
        , columnText
        , valueText ]

makeBar barIndex barValue = 
  let barHeight = (toHeight barValue)
      barXPosition = toXPosition (toFloat barIndex)
      barSizeLabel = text (String.left 5 (toString barValue)) |> filled black
  in
  group 
    [ rect barWidth barHeight
        |> filled (getBarColour barIndex) 
        |> move (barXPosition, (b.bottom+(barHeight/2))) ]

makeBars data maxHeight = 
  group <| List.indexedMap (\i d -> makeBar i (fst d)) data

graph t = 
  let 
   -- nothing = Debug.log (toString t) []
      title = text "Available Bikes at Toronto Rideshare Stations" 
                |> size 20 |> fixedwidth |> filled black |> move (-250,b.top+10)
      annotation = makeGraphAnnotation data t
  in
  group [  title
         , annotation
         , makeBars data maxHeight
         , makeGraph maxHeight ]

type Message = Tick Float GetKeyState

main =
  gameApp Tick
    { model = { t = 0 }
    , view = view
    , update = update }

update message model =
  case message of
    Tick tick _ ->
      { model | t = tick }

view model = collage 1000 500 [graph model.t] 
