import Graphics.Collage (..)
import Graphics.Element (Element, container, middle)
import Color (black, white)
import Keyboard
import Signal
import Signal ((<~), Signal, foldp)
import Time
import List

-- SIGNALS --

main : Signal Element
main = view <~ gameState

gameState : Signal Space
gameState = foldp update { player = mkSpaceship } input

input : Signal Arrowkeys
input = Signal.sampleOn (Time.fps 30) Keyboard.arrows

type alias Arrowkeys = { x : Int, y : Int }


-- MODEL --

type alias Space =
  { player : Entity
  }

type alias Entity =
  { pos : Position
  , vel : Velocity
  , spr : Form
  }

mkSpaceship : Entity
mkSpaceship =
  { pos = { x = 0, y = 0 }
  , vel = { dir = 0, speed = 0 }
  , spr = sprSpaceship
  }

type alias Position = { x : Float, y : Float }
type alias Velocity = { dir : Float, speed : Float }


-- UPDATE --

update : Arrowkeys -> Space -> Space
update arrows space =
  let newdir = space.player.dir - toFloat arrows.x / 4
      newx   = wrap -320 320 (space.player.x + 8 * toFloat arrows.y * cos newdir)
      newy   = wrap -240 240 (space.player.y + 8 * toFloat arrows.y * sin newdir)
  in  { player = { space.player | position <- {newx, newy}, vel <= { newdir, 0 } } }

wrap min max value =
  if | value < min -> wrap min max (value + (max - min))
     | value > max -> wrap min max (value - (max - min))
     | otherwise   -> value


-- VIEW --

view : Space -> Element
view space =
  let player = space.player.spr
                 |> move (space.player.pos.x, space.player.pos.y)
                 |> rotate space.player.vel.dir
      playingArea = collage 640 480 [filled black (rect 640 480), player]
  in  container 640 480 middle playingArea


sprSpaceship : Form
sprSpaceship = rotate (-pi / 2) <| outlined (solid white) <| polygon
  [(0, 15), (-10, -15), (0, -8), (10, -15)]

sprRock : Form
sprRock = outlined (solid white) <| polygon
   [ ( -4,  32),   (-14,  23),   (-29,  18),   (-25,   0)
   , (-30, -16),   (-16, -29),   ( -3, -25),   (  8, -32)
   , ( 27, -22),   ( 30,   4),   ( 18,  14),   ( 19,  30)
   ]

