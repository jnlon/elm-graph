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

getBarColor ith = 
  let colours = Array.fromList [red,blue,green] in
  Maybe.withDefault red <| 
    Array.get (ith % Array.length colours ) <| colours

-- boundaries
b =  {left = -450, right = 450, top = 250, bottom = -250}
makeBars barEntry = 1
smallText t = filled black <| size 9 t 

-- assumes minimum is zero
makeGraph yMax = 
  let leftLine = line (b.left,b.bottom) (b.left,b.top)
      bottomLine = line (b.left,b.bottom) (b.right,b.bottom)
      labelStyle = filled black
      yMakeGrid y = line (b.left,y) (b.right,y) |> outlined (solid 1) grey
      yMakeLine y = line (b.left,y) (b.right,y) |> outlined (solid 2) black
      yLabels = 10 
      height = (abs(b.bottom)+b.top)
      yMakeLabel i y = 
        group 
          [ yMakeLine y
          , text (toString ((toFloat i)*(yMax/yLabels))) |> smallText |> move ((b.left-35),(y+1))]
      yLabelLines =  
        List.indexedMap yMakeLabel (range b.bottom b.top (height/yLabels))
      yGrid = 
        List.map yMakeGrid (range b.bottom b.top (height/yLabels/5))
  in 
  group 
    [ group yGrid
    , group yLabelLines
    , leftLine |> outlined (solid 5) black 
    , bottomLine |> outlined (solid 5) black ]

graph = 
  makeGraph 500

--main = Html.text (toString (range 10 30 2)) 
main = graphicsApp { view = collage 1000 500 [graph] }
