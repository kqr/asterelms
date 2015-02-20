import Graphics.Collage (..)
import Graphics.Element (container, middle)
import Color (black, white)
import Keyboard
import Signal
import Signal ((<~), Signal)
import Time
import List

main = view <~ gameState

gameState : Signal Spaceship
gameState =
  let input = Signal.sampleOn (Time.fps 30) Keyboard.arrows
  in  Signal.foldp update {x = 0, y = 0, dir = 0} input

view {x, y, dir} =
  let player = sprSpaceship |> move (x, y) |> rotate dir
      playingArea = collage 640 480 [filled black (rect 640 480), player]
  in  container 640 480 middle playingArea

update : {x : Int, y : Int} -> Spaceship -> Spaceship
update arrows player =
  let newdir = player.dir - toFloat arrows.x / 4
      newx   = player.x + 8 * toFloat arrows.y * cos newdir
      newy   = player.y + 8 * toFloat arrows.y * sin newdir
  in  { x = newx, y = newy, dir = newdir }

type alias Spaceship = { x : Float, y : Float, dir : Float }

sprSpaceship = rotate (-pi / 2) <| outlined (solid white) <| polygon
  [(0,15), (-10,-15), (0,-8), (10,-15)]

sprRock = polygon
  [(-4,32), (-14, 23), (-29, 18), (-25, 0)
  ,(-30, -16), (-16, -29), (-3, -25), (8, -32)
  ,(27, -22), (30, 4), (18, 14), (19, 30)]
