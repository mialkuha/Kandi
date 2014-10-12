module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Display

data AsteroidWorld = Play [Rock] Ship [Bullet] Ufo
                   | GameOver 
                   deriving (Eq,Show)

type Velocity     = (Float, Float)
type Size         = Float
type Age          = Float


data Ship   = Ship   PointInSpace Velocity      
    deriving (Eq,Show)
data Bullet = Bullet PointInSpace Velocity Age  
    deriving (Eq,Show)
data Rock   = Rock   PointInSpace Size Velocity 
    deriving (Eq,Show)
--Datatype for Ufo -object
data Ufo    = Ufo    PointInSpace Velocity Age Size
    deriving (Eq,Show)

initialWorld :: AsteroidWorld
initialWorld = Play
                   [Rock (150,150)  45 (2,6)    
                   ,Rock (-45,201)  45 (13,-8) 
                   ,Rock (45,22)    25 (-2,8)  
                   ,Rock (-210,-15) 30 (-2,-8) 
                   ,Rock (-45,-201) 25 (8,2)   
                   ] -- The default rocks
                   (Ship (0,0) (0,5)) -- The initial ship
                   [] -- The initial bullets (none)
		   (Ufo  (-150,-150) (0,0) 0 10) --default ufo


simulateWorld :: Float -> (AsteroidWorld -> AsteroidWorld)

simulateWorld _        GameOver          = GameOver  

--Added Ufo to arguments, also guard for collision with ufo
simulateWorld timeStep (Play rocks (Ship shipPos@(shipPosX,shipPosY) shipV) bullets (Ufo ufoPos@(ufoPosX,ufoPosY) ufoV@(ufoVX,ufoVY) ufoTime ufoSize))
  | any (collidesWith shipPos) rocks = GameOver
  | collidesWith shipPos (Rock ufoPos (1.2*ufoSize) ufoV) = GameOver
  | otherwise = Play (concatMap updateRock rocks) 
                              (Ship newShipPos shipV)
                              (concat (map updateBullet bullets))
			      updateUfo 
  where
      --If ufo is shot, it gets bigger by some strange alien tech!
      updateUfo :: Ufo
      updateUfo = if collidesWithBullet (Rock ufoPos ufoSize ufoV) 
		  then Ufo newUfoPos newUfoV (ufoTime+timeStep) (ufoSize+1)
		  else Ufo newUfoPos newUfoV (ufoTime+timeStep) ufoSize
			

      collidesWith :: PointInSpace -> Rock -> Bool
      collidesWith p (Rock rp s _) 
       = magV (rp .- p) < s 

      collidesWithBullet :: Rock -> Bool
      collidesWithBullet r 
       = any (\(Bullet bp _ _) -> collidesWith bp r) bullets 
     
      updateRock :: Rock -> [Rock]
      updateRock r@(Rock p s v) 
       | collidesWithBullet r && s < 7 
            = []
       | collidesWithBullet r && s > 7 
            = splitRock r
       | otherwise                     
            = [Rock (restoreToScreen (p .+ timeStep .* v)) s v]
 
      updateBullet :: Bullet -> [Bullet] 
      updateBullet (Bullet p v a) 
        | a > 5                      
             = []
        | any (collidesWith p) rocks 
             = [] 
        | otherwise                  
             = [Bullet (restoreToScreen (p .+ timeStep .* v)) v 
                       (a + timeStep)] 

      newShipPos :: PointInSpace
      newShipPos = restoreToScreen (shipPos .+ timeStep .* shipV)

      --function for ufo to move
      newUfoPos :: PointInSpace
      newUfoPos = restoreToScreen (ufoPos .+ timeStep .* ufoV)

      --ufo follows ship with sinusoidally accelerating boosters
      newUfoV :: Velocity
      newUfoV = (0.7*(shipPosX-ufoPosX)*(sin (1.5*ufoTime)+1),
		 0.7*(shipPosY-ufoPosY)*(sin (1.5*ufoTime)+1))

splitRock :: Rock -> [Rock]
splitRock (Rock p s v) = [Rock p (s/2) (3 .* rotateV (pi/3)  v)
                         ,Rock p (s/2) (3 .* rotateV (-pi/3) v) ]

restoreToScreen :: PointInSpace -> PointInSpace
restoreToScreen (x,y) = (cycleCoordinates x, cycleCoordinates y)

cycleCoordinates :: (Ord a, Num a) => a -> a
cycleCoordinates x 
    | x < (-400) = 800+x
    | x > 400    = x-800
    | otherwise  = x

drawWorld :: AsteroidWorld -> Picture

--Edited the ending screen
drawWorld GameOver 
   = pictures [scale 0.3 0.3 . translate (-400) 0 
     	       . color red . text $ "Game Over!",
	       scale 0.1 0.1 . translate (-1150) (-550)
	       . color white . text $ 
	       "Click right mousebutton to restart"]

--Added ufos graphics, updated rocks' graphics
drawWorld (Play rocks (Ship (x,y) (vx,vy)) bullets (Ufo (ux,uy) _ _ us))
  = pictures [ship,asteroids,shots,ufo]
   where 
    ship      = color red (pictures [translate x y (circle 10)])
    asteroids = pictures [(color orange (polygon (asteroidsPoints x y s)))
                         | Rock   (x,y) s _ <- rocks]
    shots     = pictures [translate x y (color red (circle 2)) 
                         | Bullet (x,y) _ _ <- bullets]
    ufo       = color magenta (pictures [(translate (ux) (uy-(0.5*us)) (Arc 30 150 us)),
				        (translate (ux) (uy+(0.5*us)) (Arc 210 330 us))])

--Added a new function to create pointmaps for drawing polygons
asteroidsPoints :: Float -> Float -> Float -> [Point]
asteroidsPoints x y s = [(x,y+s),(x,y-s),(x+0.5*s,y-0.3*s),(x-0.8*s,y+0.1*s),(x-0.7*s,y-0.65*s),(x+0.8*s,y+0.4*s)]

handleEvents :: Event -> AsteroidWorld -> AsteroidWorld

--Added a new eventhandler for game restarting
handleEvents (EventKey (MouseButton RightButton) Down _ _) GameOver
	      = initialWorld

handleEvents (EventKey (MouseButton LeftButton) Down _ clickPos)
             (Play rocks (Ship shipPos shipVel) bullets ufo)
             = Play rocks (Ship shipPos newVel) 
                          (newBullet : bullets)
			  ufo
 where 
     newBullet = Bullet shipPos 
                        (-150 .* norm (shipPos .- clickPos)) 
                        0
     newVel    = shipVel .+ (50 .* norm (shipPos .- clickPos))

handleEvents _ w = w

type PointInSpace = (Float, Float)

(.-) , (.+) :: PointInSpace -> PointInSpace -> PointInSpace
(x,y) .- (u,v) = (x-u,y-v)
(x,y) .+ (u,v) = (x+u,y+v)

(.*) :: Float -> PointInSpace -> PointInSpace
s .* (u,v) = (s*u,s*v)

infixl 6 .- , .+
infixl 7 .*

norm :: PointInSpace -> PointInSpace
norm (x,y) = let m = magV (x,y) in (x/m,y/m)

magV :: PointInSpace -> Float
magV (x,y) = sqrt (x**2 + y**2) 

rotateV :: Float -> PointInSpace -> PointInSpace
rotateV r (x,y) = (x * cos r - y * sin r
                  ,x * sin r + y * cos r)

main = play
         (InWindow "Asteroids!" (550,550) (20,20))
         black
         24
         initialWorld
         drawWorld
         handleEvents
         simulateWorld
