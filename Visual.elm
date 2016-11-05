--import Html exposing (..)
import GraphicSVG exposing (..)
import Array
import String
import Data

type alias BarData = (Int,String)
type alias Bounds =  
  { top : Int,
    left : Int,
    bottom: Int,
    right : Int }

-- inclusive
range from to step = 
  let genRange i lst = 
      if i <= to 
      then (genRange (i+step) (i :: lst))
      else lst 
  in
      List.reverse <| genRange from []

getBarColour ith = 
  let colours = Array.fromList [red,blue,green] in
  Maybe.withDefault red <| 
    Array.get (ith % Array.length colours ) <| colours

-- boundaries
b = {left = -450.0, right = 450.0, top = 250.0, bottom = -190.0}
graphHeight = (abs b.bottom) + b.top
graphWidth =  (abs b.left) + b.right
smallText t = filled black <| size 9 t 

-- assumes minimum is zero
makeGraph yMax = 
  let leftLine = line (b.left,b.bottom) (b.left,b.top)
      bottomLine = line (b.left,b.bottom) (b.right,b.bottom)
      labelStyle = filled black
      yMakeGrid y = line (b.left,y) (b.right,y) |> outlined (solid 1) grey
      yMakeLine y = line (b.left,y) (b.right,y) |> outlined (solid 2) black
      yLabels = 10 
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
    , leftLine |> outlined (solid 5) black 
    , bottomLine |> outlined (solid 5) black ]


barDataOfJsonData json = 
  (toFloat json.availableBikes, json.stationName)

makeBars data maxHeight numBars = 
  let barWidth = graphWidth/(toFloat (List.length data))
      toHeight n = ((n/maxHeight)*graphHeight)
      toXPosition i =  -- I DON't KNOW HOW THIS WORKS
        ((i/numBars)*graphWidth) + (b.left + barWidth/2)
      makeBar i d = 
        let barHeight = (toHeight (fst d))
            barXPosition = toXPosition (toFloat i)
            xLabel = text (snd d) |> filled black 
            barSizeLabel = text (String.left 5 (toString (fst d))) |> filled black
            xAxisTextX = barXPosition-10
            xAxisTextPosition = if (i % 2) == 0 
            then (xAxisTextX, b.bottom-20) 
            else (xAxisTextX, b.bottom-45)
        in
        group 
          [ rect barWidth barHeight
              |> filled (getBarColour i) 
              |> move (barXPosition, (b.bottom+(barHeight/2))) 
          , xLabel |> move xAxisTextPosition
          , barSizeLabel |> move xAxisTextPosition |> move (8,-12)]
      in
      group <| List.indexedMap makeBar data

graph = 
  let data = List.map barDataOfJsonData Data.data
      maxHeight = (fst (Maybe.withDefault (0,"") (List.maximum data)))
  in
  group [  makeBars data maxHeight (toFloat (List.length data))
         , makeGraph maxHeight ]

--main = Html.text (toString (range 10 30 2)) 
main = graphicsApp { view = collage 1000 500 [graph] }
