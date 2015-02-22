import Graphics.Collage (..)
import Graphics.Element (Element, container, middle)
import Color (black, white)
import Keyboard
import Signal
import Signal ((<~), (~), Signal, foldp)
import Time
import List
import List ((::), map)
import Random
import Char
import Text


-- These are currently constants. Should probably be something better later.
wwidth  = 640
wheight = 480
frames  = 30


{-- SIGNALS --}

main : Signal Element
main = view <~ gameState

gameState : Signal Space
gameState = foldp update init input

input : Signal Controls
input = Signal.sampleOn (Time.fps frames) <|
  Controls <~ Keyboard.arrows ~ Keyboard.isDown (Char.toCode 'x')

type alias Arrowkeys = { x : Int, y : Int }
type alias Controls = { arrows : Arrowkeys, shoot : Bool }



{-- MODEL --}

-- Global game state
type alias Space =
  { player : Spaceship
  , bullets : List Bullet
  , stars : List (Float, Float)
  , cooldown : Float
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
  { posx = x, posy = y
  , velx = 20 * cos dir + source_vx, vely = 20 * sin dir + source_vy
  , form = sprBullet, ttl = 1.2
  }



{-- UPDATE --}

init : Space
init =
  let randfr n = Random.float -n n
      coordGen = Random.list 200 (Random.pair
                   (randfr (wwidth / 2)) (randfr (wheight / 2)))
      (coords, _) = Random.generate coordGen (Random.initialSeed 42)
  in  { player = mkSpaceship
      , bullets = []
      , stars = coords
      , cooldown = 0
      }

update : Controls -> Space -> Space
update controls space =
  { player     = space.player
                   |> thrust controls.arrows
                   |> mechanics
  , bullets    = space.bullets
                   |> List.filter (\b -> b.ttl > 0)
                   |> map (\b -> { b | ttl <- b.ttl - 1/frames })
                   |> shootBullets controls.shoot space.cooldown space.player
                   |> map mechanics
  , stars = offsetBackground space.stars space.player
  , cooldown   = if space.cooldown > 0
                    then space.cooldown - 1/frames
                    else if controls.shoot
                         then 0.4
                         else 0
  }


offsetBackground : List (Float, Float) -> Entity a -> List (Float, Float)
offsetBackground stars spaceship =
  map (wrapstars (spaceship.velx / 4, spaceship.vely / 4)) stars

wrapstars (vx, vy) (x, y) =
  let fakeEntity = { posx = x - vx, posy = y - vy, velx = -vx, vely = -vy }
      wrapped = wrap fakeEntity
  in  (wrapped.posx, wrapped.posy)


shootBullets : Bool -> Float -> Spaceship -> List Bullet -> List Bullet
shootBullets shooting cooldown spaceship bullets =
  let pos = (spaceship.posx, spaceship.posy)
      vel = (spaceship.velx, spaceship.vely)
      newBullet = mkBullet pos vel spaceship.dir
  in  if shooting && cooldown <= 0
         then newBullet :: bullets
         else bullets


thrust : Arrowkeys -> Spaceship -> Spaceship
thrust arrows spaceship =
  { spaceship |
    velx <- spaceship.velx + 0.5 * toFloat arrows.y * cos spaceship.dir
  , vely <- spaceship.vely + 0.5 * toFloat arrows.y * sin spaceship.dir
  , dir <- spaceship.dir - toFloat arrows.x / 4
  }

mechanics : Entity a -> Entity a
mechanics ety =
  wrap { ety |
         posx <- ety.posx + ety.velx
       , posy <- ety.posy + ety.vely
       , velx <- ety.velx * 0.98
       , vely <- ety.vely * 0.98
       }


wrap : Entity a -> Entity a
wrap ety =
  let cp = clippingPoints ety
  in  if | ety.posx >   wwidth / 2 -> (if | cp.left_y   >  wheight / 2 -> { ety | posx <-    cp.top_x, posy <-  wheight / 2 }
                                          | cp.left_y   < -wheight / 2 -> { ety | posx <- cp.bottom_x, posy <- -wheight / 2 }
                                          | otherwise                  -> { ety | posx <- -wwidth / 2, posy <-    cp.left_y })
         | ety.posy >  wheight / 2 -> (if | cp.bottom_x >   wwidth / 2 -> { ety | posx <-  wwidth / 2, posy <-   cp.right_y }
                                          | cp.bottom_x <  -wwidth / 2 -> { ety | posx <- -wwidth / 2, posy <-    cp.left_y }
                                          | otherwise                  -> { ety | posx <- cp.bottom_x, posy <- -wheight / 2 })
         | ety.posx <  -wwidth / 2 -> (if | cp.right_y  >  wheight / 2 -> { ety | posx <-    cp.top_x, posy <-  wheight / 2 }
                                          | cp.right_y  < -wheight / 2 -> { ety | posx <- cp.bottom_x, posy <- -wheight / 2 }
                                          | otherwise                  -> { ety | posx <-  wwidth / 2, posy <-   cp.right_y })
         | ety.posy < -wheight / 2 -> (if | cp.top_x    >   wwidth / 2 -> { ety | posx <-  wwidth / 2, posy <-   cp.right_y }
                                          | cp.top_x    <  -wwidth / 2 -> { ety | posx <- -wwidth / 2, posy <-    cp.left_y }
                                          | otherwise                  -> { ety | posx <-    cp.top_x, posy <-  wheight / 2 })
         | otherwise               -> ety


type alias ClippingPoints =
  { top_x : Float, left_y : Float, bottom_x : Float, right_y : Float }

clippingPoints : Entity a -> ClippingPoints
clippingPoints { posx, posy, velx, vely } =
  { top_x    = posx - safediv velx vely * (posy - wheight / 2)
  , left_y   = posy - safediv vely velx * (posx + wwidth / 2)
  , bottom_x = posx - safediv velx vely * (posy + wheight / 2)
  , right_y  = posy - safediv vely velx * (posx - wwidth / 2)
  }

safediv x y = if y == 0 then 0 else x / y



{-- VIEW --}

view : Space -> Element
view space =
  let player = space.player.form
                 |> move (space.player.posx, space.player.posy)
                 |> rotate space.player.dir
      bullets = map (\b -> move (b.posx, b.posy) b.form) space.bullets
      environment = filled black (rect wwidth wheight)
                      :: map (\pos -> move pos sprStar) space.stars
      help = "arrow keys to move, x to shoot"
               |> Text.fromString
               |> Text.color white
               |> Text.leftAligned
               |> toForm
               |> move (-220, 230)
      playingArea = collage wwidth wheight <|
                      environment ++ [player] ++ bullets
                      ++ [help]
  in  container wwidth wheight middle playingArea


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

