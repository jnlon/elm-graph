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

-- inclusive
range from to step = 
  let genRange i lst = 
      if i <= to 
      then (genRange (i+step) (i :: lst))
      else lst 
  in
      List.reverse <| genRange from []

getBarColour ith = 
  let colours = Array.fromList [red,blue,green,yellow,purple] in
  Maybe.withDefault red <| 
    Array.get (ith % Array.length colours ) <| colours

-- boundaries
b = {left = -450.0, right = 450.0, top = 230.0, bottom = -120.0}
graphHeight = (abs b.bottom) + b.top
graphWidth =  (abs b.left) + b.right
smallText t = filled black <| size 9 t 

-- assumes minimum is zero
makeGraph yMax = 
  let leftLine = line (b.left,b.bottom) (b.left,b.top)
      bottomLine = line (b.left,b.bottom) (b.right,b.bottom)
      labelStyle = filled black
      yMakeGrid y = line (b.left,y) (b.right,y) |> outlined (solid 1) grey
      yMakeLine y = line (b.left,y) (b.right,y) |> outlined (solid 1) black
      yLabels = 5 
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
            xLabel = text (snd d) |> size 8 |> filled black 
            barSizeLabel = text (String.left 5 (toString (fst d))) |> filled black
            xAxisTextX = barXPosition-10
            xAxisText = 
              case (i % 2) of
                1 ->  xLabel |> move (xAxisTextX, b.bottom-20) |> rotate (degrees 40)
                o -> xLabel |> move (xAxisTextX+(barWidth*2), b.top/2)  |> rotate (degrees 90)
        in
        group 
          [ rect barWidth barHeight
              |> filled (getBarColour i) 
              |> move (barXPosition, (b.bottom+(barHeight/2))) 
          , xAxisText ]
          --, barSizeLabel |> move xAxisTextPosition |> move (8,-12)]
      in
      group <| List.indexedMap makeBar data

graph t = 
  let data = List.map barDataOfJsonData Data.data
  --let data = [(5,"test1"), (10,"test2")] 
      maxHeight = (fst (Maybe.withDefault (0,"") (List.maximum data)))
  in
  group [  text "Number of Available Bikes at Toronto Rideshare Stations" |> size 20 |> fixedwidth |> filled black |> move (-330,b.top+5)
         , makeBars data maxHeight (toFloat (List.length data))
         , makeGraph maxHeight ]


-- main = graphicsApp { view = view, model = { t = 0 }, update = update }

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
