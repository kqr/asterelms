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
  { player : Spaceship
  }

type alias Spaceship = Entity { dir : Float, form : Form }

-- Shortcut for making a spaceship
mkSpaceship : Spaceship
mkSpaceship =
  { posx = 0, posy = 0, velx = 0, vely = 0
  , dir = 0, form = sprSpaceship
  }

-- Anything suffering under classical mechanics is an entity
type alias Entity a =
  { a | posx : Float, posy : Float, velx : Float, vely : Float }


-- UPDATE --

update : Arrowkeys -> Space -> Space
update arrows space =
  { player = wrap (mechanics (thrust arrows space.player)) }

thrust : Arrowkeys -> Spaceship -> Spaceship
thrust arrows spaceship =
  { spaceship |
    velx <- spaceship.velx + 2 * toFloat arrows.y * cos spaceship.dir
  , vely <- spaceship.vely + 2 * toFloat arrows.y * sin spaceship.dir
  , dir <- spaceship.dir - toFloat arrows.x / 4
  }

mechanics : Entity a -> Entity a
mechanics ety =
  { ety |
    posx <- ety.posx + ety.velx
  , posy <- ety.posy + ety.vely
  , velx <- ety.velx * 0.95
  , vely <- ety.vely * 0.95
  }


wrap : Entity a -> Entity a
wrap ety =
  let cp = clippingPoints ety
  in  if | ety.posx >  320 -> (if | cp.left_y   >  240 -> { ety | posx <-    cp.top_x, posy <-        240 }
                                  | cp.left_y   < -240 -> { ety | posx <- cp.bottom_x, posy <-       -240 }
                                  | otherwise          -> { ety | posx <-        -320, posy <-  cp.left_y })
         | ety.posy >  240 -> (if | cp.bottom_x >  320 -> { ety | posx <-         320, posy <- cp.right_y }
                                  | cp.bottom_x < -320 -> { ety | posx <-        -320, posy <-  cp.left_y }
                                  | otherwise          -> { ety | posx <- cp.bottom_x, posy <-       -240 })
         | ety.posx < -320 -> (if | cp.right_y  >  240 -> { ety | posx <-    cp.top_x, posy <-        240 }
                                  | cp.right_y  < -240 -> { ety | posx <- cp.bottom_x, posy <-       -240 }
                                  | otherwise          -> { ety | posx <-        -320, posy <- cp.right_y })
         | ety.posy < -240 -> (if | cp.top_x    >  320 -> { ety | posx <-         320, posy <- cp.right_y }
                                  | cp.top_x    < -320 -> { ety | posx <-        -320, posy <-  cp.left_y }
                                  | otherwise          -> { ety | posx <-    cp.top_x, posy <-        240 })
         | otherwise       -> ety


type alias ClippingPoints =
  { top_x : Float, left_y : Float, bottom_x : Float, right_y : Float }

clippingPoints : Entity a -> ClippingPoints
clippingPoints { posx, posy, velx, vely } =
  { top_x    = posx - safediv velx vely * (posy - 240)
  , left_y   = posy - safediv vely velx * (posx + 320)
  , bottom_x = posx - safediv velx vely * (posy + 240)
  , right_y  = posy - safediv vely velx * (posx - 320)
  }

safediv x y = if y == 0 then 0 else x / y


-- VIEW --

view : Space -> Element
view space =
  let player = space.player.form
                 |> move (space.player.posx, space.player.posy)
                 |> rotate space.player.dir
      cps = clippingPoints space.player
      playingArea = collage 640 480
                      [ filled black (rect 640 480)
                      , player
                      , traced (solid white) (segment (cps.top_x, 240) (cps.top_x, 230))
                      , traced (solid white) (segment (-320, cps.left_y) (-310, cps.left_y))
                      , traced (solid white) (segment (cps.bottom_x, -230) (cps.bottom_x, -240))
                      , traced (solid white) (segment (310, cps.right_y) (320, cps.right_y))
                      ]
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

