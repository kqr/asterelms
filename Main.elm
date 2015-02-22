import Graphics.Collage (..)
import Graphics.Element (Element, container, middle)
import Color (black, white)
import Keyboard
import Signal
import Signal ((<~), Signal, foldp)
import Time
import List
import List ((::), map)
import Random



{-- SIGNALS --}

main : Signal Element
main = view <~ gameState

gameState : Signal Space
gameState = foldp update init input

input : Signal Controls
input = Signal.sampleOn (Time.fps 30) <|
  Signal.map2 (\a s -> { arrows = a, spacebar = s })
    Keyboard.arrows Keyboard.space

type alias Arrowkeys = { x : Int, y : Int }
type alias Controls = { arrows : Arrowkeys, spacebar : Bool }



{-- MODEL --}

-- Global game state
type alias Space =
  { player : Spaceship
  , bullets : List Bullet
  , background : Starfield
  }

-- Anything suffering under classical mechanics is an entity
type alias Entity a =
  { a | posx : Float, posy : Float, velx : Float, vely : Float }


-- Spaceship and a shortcut for making it
type alias Spaceship = Entity { dir : Float, form : Form }

mkSpaceship : Spaceship
mkSpaceship =
  { posx = 0, posy = 0, velx = 0, vely = 0
  , dir = 0, form = sprSpaceship
  }


-- Bullet and a shortcut for making it
type alias Bullet = Entity { form : Form, ttl : Float }

mkBullet : (Float, Float) -> (Float, Float) -> Float -> Bullet
mkBullet (x, y) (source_vx, source_vy) dir =
  { posx = x, posy = y, velx = 16 * cos dir + source_vx, vely = 16 * sin dir + source_vy
  , form = sprBullet, ttl = 2
  }



{-- UPDATE --}

init : Space
init =
  let randfr n = Random.float -n n
      coordGen = Random.list 200 (Random.pair (randfr 320) (randfr 240))
      (coords, _) = Random.generate coordGen (Random.initialSeed 42)
  in  { player = mkSpaceship
      , bullets = []
      , background = { offset = (0,0), stars = coords }
      }

update : Controls -> Space -> Space
update controls space =
  { player  = space.player
                |> thrust controls.arrows
                |> mechanics
  , bullets = space.bullets
                |> List.filter (\b -> b.ttl > 0)
                |> map (\b -> { b | ttl <- b.ttl - 1/30 })
                |> shootBullets controls.spacebar True space.player
                |> map mechanics
  , background = offsetBackground space.background space.player
  }


offsetBackground : Starfield -> Entity a -> Starfield
offsetBackground bg spaceship =
  let vx = spaceship.velx / 8
      vy = spaceship.vely / 8
      ox = fst bg.offset
      oy = snd bg.offset
      newox = -vx
      newoy = -vy
      newstars = map (\(x, y) -> wrapstars (-vx, -vy) (x + ox, y + oy)) bg.stars
  in  { stars = newstars, offset = (newox, newoy) }

wrapstars (vx, vy) (x, y) =
  let fakeEntity = { posx = x, posy = y, velx = vx, vely = vy }
      wrapped = wrap fakeEntity
  in  (wrapped.posx, wrapped.posy)

shootBullets : Bool -> Bool -> Spaceship -> List Bullet -> List Bullet
shootBullets shooting canShoot spaceship bullets =
  let pos = (spaceship.posx, spaceship.posy)
      vel = (spaceship.velx, spaceship.vely)
      newBullet = mkBullet pos vel spaceship.dir
  in  if shooting && canShoot
         then newBullet :: bullets
         else bullets


thrust : Arrowkeys -> Spaceship -> Spaceship
thrust arrows spaceship =
  { spaceship |
    velx <- spaceship.velx + 2 * toFloat arrows.y * cos spaceship.dir
  , vely <- spaceship.vely + 2 * toFloat arrows.y * sin spaceship.dir
  , dir <- spaceship.dir - toFloat arrows.x / 4
  }

mechanics : Entity a -> Entity a
mechanics ety =
  wrap { ety |
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
                                  | otherwise          -> { ety | posx <-         320, posy <- cp.right_y })
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



{-- VIEW --}

view : Space -> Element
view space =
  let player = space.player.form
                 |> move (space.player.posx, space.player.posy)
                 |> rotate space.player.dir
      bullets = map (\b -> move (b.posx, b.posy) b.form) space.bullets
      environment = filled black (rect 640 480)
                      :: map (\pos -> move pos sprStar)
                           (absoluteStarpos space.background)
      playingArea = collage 640 480 <|
                      environment ++ [player] ++ bullets
  in  container 640 480 middle playingArea


absoluteStarpos : Starfield -> List (Float, Float)
absoluteStarpos { offset, stars } =
  map (\(x, y) -> (x + fst offset, y + snd offset)) stars


type alias Starfield =
  { offset : (Float, Float)
  , stars : List (Float, Float)
  }

sprSpaceship : Form
sprSpaceship = 
  let shape = polygon [(0, 15), (-10, -15), (0, -8), (10, -15)]
      body  = filled black shape
      edges = outlined (solid white) shape
  in  rotate (-pi / 2) (group [body, edges])

sprBullet : Form
sprBullet = outlined (solid white) <| circle 2

sprStar : Form
sprStar = filled white <| circle 0.5

sprRock : Form
sprRock = outlined (solid white) <| polygon
   [ ( -4,  32),   (-14,  23),   (-29,  18),   (-25,   0)
   , (-30, -16),   (-16, -29),   ( -3, -25),   (  8, -32)
   , ( 27, -22),   ( 30,   4),   ( 18,  14),   ( 19,  30)
   ]

