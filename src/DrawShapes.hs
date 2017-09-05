module DrawShapes where

import Graphics.Gloss
import qualified Data.Map as M
import Control.Applicative
import LanguageCore

type TurtleConfiguration = TurtleState -> TurtleState

instance Show TurtleConfiguration where
  show xs = "Turtle Function"

data TurtleState = Turtle {
  befehlStand :: [(Color, [Point])],
  angle       :: Float,
  pen         :: Pen,
  theColor    :: Color,
  stack       :: M.Map String [TurtleConfiguration],
  scope       :: M.Map String [Commands],
  argsValue   :: M.Map String (M.Map String Float),
  getArgOrder :: M.Map String (M.Map Int String),
  errors      :: M.Map String String
} deriving Show

data Pen = Up | Down deriving Show

initTurtleState :: TurtleState
initTurtleState = Turtle {  befehlStand = [(black ,[(0,0)])],
                            angle       = 0,
                            pen         = Down,
                            theColor    = black,
                            stack       = M.singleton "Main" [],
                            scope       = M.empty,
                            argsValue   = M.empty,
                            getArgOrder = M.empty,
                            errors      = M.empty
                         }

rotateP :: Float -> Point -> Point -> Point
rotateP a (x, y) (ox, oy) = ((cos deg) * (x-ox) - (sin deg) * (y-oy) + ox,
                             (sin deg) * (x-ox) + (cos deg) * (y-oy) + oy)
  where deg = a * pi / 180

turnL :: Float -> TurtleConfiguration
turnL d ts@Turtle{..} = ts {angle = angle+d}

turnR :: Float -> TurtleConfiguration
turnR d ts = turnL (-d) ts

forward :: Float -> TurtleConfiguration
forward n ts@Turtle{..} = case pen of
                  Down -> ts {
                    befehlStand = (init befehlStand)
                    ++ [(theColor, (snd $ last befehlStand)
                    ++ [rotateP angle (x, y+n) (x,y)])]
                      }
                  Up   -> ts {
                    befehlStand = befehlStand
                    ++ [(theColor, [rotateP angle (x, y+n) (x, y)])]
                      }
  where (x,y) = (last . snd . last) befehlStand

backward :: Float -> TurtleConfiguration
backward n ts = forward (-n) ts

penUp :: TurtleConfiguration
penUp ts = ts {pen = Up}

penDown :: TurtleConfiguration
penDown ts = ts {pen = Down}

setColor :: Color -> TurtleConfiguration
setColor c ts = ts {theColor = c}

repeateFunc :: Float -> [Commands] -> [Commands]
repeateFunc n xs = reverse $ helpFunc n xs xs
  where helpFunc 1 [] _         = []
        helpFunc k [] zs        = helpFunc (k-1) zs zs
        helpFunc k (Stop:_) zs  = helpFunc 1 [] zs
        helpFunc k (y:ys) zs    = y : helpFunc k ys zs

comment :: TurtleConfiguration
comment = id

toComp :: [TurtleConfiguration] -> TurtleConfiguration
toComp []     = id
toComp (y:ys) = y . toComp ys

moreShapes :: TurtleState -> [Picture]
moreShapes ts@Turtle{..}  = helpFunc befehlStand where
  helpFunc []         = []
  helpFunc ((c,x):xs) = (color c $ line x) : helpFunc xs

render :: TurtleConfiguration -> TurtleState -> Picture
render ys xs = (pictures . moreShapes . ys) xs

window :: Display
window = InWindow "Game Window" (600, 600) (100, 100)

background :: Color
background = white

runTurtle :: TurtleState -> TurtleConfiguration -> IO ()
runTurtle initState xs = display window background (render xs initState)

runTurtleWithErrorCheck :: TurtleState -> TurtleConfiguration -> IO ()
runTurtleWithErrorCheck initState xs = case M.null errs of
                                          True  -> runTurtle initState xs
                                          False -> mapM_ putStrLn $ M.elems errs
  where errs = errors $ xs initState
