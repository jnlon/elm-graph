--import Html exposing (..)
import GraphicSVG exposing (..)
import Array
--import Data

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
b = {left = -450.0, right = 450.0, top = 250.0, bottom = -250.0}
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
      yMakeLabel i y = 
        group 
          [ yMakeLine y
          , text (toString ((toFloat i)*(yMax/yLabels))) |> smallText |> move ((b.left-35),(y+1))]
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


makeBars data maxHeight numBars = 
  let 
      barWidth = graphWidth/(toFloat (List.length data))
      toHeight n = (n/maxHeight)*graphHeight
      -- I DON't KNOW HOW THIS WORKS
      toXPosition i = ((i/numBars)*graphWidth) + (b.left + barWidth/2)
      makeBar i d = 
        rect barWidth (toHeight (fst d))
          |> filled (getBarColour i) 
          |> move ((toXPosition (toFloat i)), b.bottom) 
  in
      group <| List.indexedMap makeBar data

graph = 
  let data = [(10, "test"), (2, "two"), (5, "two")]
      maxHeight = (fst (Maybe.withDefault (0,"") (List.maximum data)))
  in
  group [  makeBars data maxHeight (toFloat (List.length data))
         , makeGraph 10 ]

--main = Html.text (toString (range 10 30 2)) 
main = graphicsApp { view = collage 1000 500 [graph] }
