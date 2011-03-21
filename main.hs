{-# LANGUAGE Arrows #-} 


--Requirements (all available via hackage):
--sdl
--yampa
--monad-loops

--The key learning points, that will hopefully stick with me, are that you should always check the competition deadlines
--twice (I thought I had another weekend to try and understand FRP, Arrows etc more) and that running out of coffee is a
--very bad thing.


--an allocative list, where the keys are automatically generated
import IdentityList

import Maybe
import System.Random
import Control.Monad.Loops

----------------------------------
--Yampa Includes
--Assigning names to the inputs are required due to a name conflict
--between SDL.Event and Yampa.Event
import FRP.Yampa                    as Yampa
import FRP.Yampa.Geometry           

----------------------------------
--SDL Includes
import Graphics.UI.SDL              as SDL
import Graphics.UI.SDL.Events       as SDL.Events

--import Graphics.UI.SDL.TTF          as SDL.TTF

----------------------------------
--Generally useful code

--Generates all possible combinations of the elements of a list.
--Note, this treats (x,y) as the same as (y,x) (i.e. order doesn't matter)
combinations :: [a] -> [(a,a)]
combinations []     = []
combinations (x:xs) = [ (x,y) | y <- xs ] ++ combinations xs 

--clamps a variable between two bounds
--TODO consider not requiring the min < max, just take the min/max of them
clamp v min max | min > max = error "Min cannot be greater than max"
                | v < min   = min
                | v > max   = max
                | otherwise = v 
----------------------------------
--Type Defs

type Position = Point2 Double
type Velocity = Vector2 Double

type Inputs = [SDL.Event]


--We must know what type of hit it is as that way we then know which component of the
--velocity vector to flip
data Hit = HorizontalLeft | HorizontalRight | VerticalTop | VerticalBottom
            deriving Eq

data Team = Left | Right
            deriving Eq

--There are two times when objects need to react
--  When there is a collision between two objects
--  When there is some external input from the user
data ObjInput = ObjInput {
   hit        :: Yampa.Event Hit, --deterministic events from object preprocessor in route
   scored     :: Yampa.Event Team, --TODO
   gameInputs :: Inputs          --non deterministic events comming from IO devices 
}

-- Output
--   The state of the object
--   Removing or adding objects from the collections of signals
data ObjOutput = ObjOutput {
    objState    :: ObjState,
    killReq     :: Yampa.Event (),
    spawnReq    :: Yampa.Event [Object]
}

data Renderable = 
            Rectangle {
                 width  :: Int,
                 height :: Int
            }
            | Circle {
                radius :: Int
            }

--This contains the observable state of in game entities
--This was split into Paddle and Put, but as each had the same contained types it made
--it far simpler to have a single type
data ObjState = ObjState {
            position   :: Position,
            velocity   :: Velocity,
            renderable :: Renderable
        } 
        | Score {
            score    :: Int,
            position :: Position
        }


--dpSwitch requires that all objects are of a uniform type
type Object = SF ObjInput ObjOutput


----------------------------------
--Constants

screenWidth  = 256
screenHeight = 128

backgroundColour =  0x003F3F3F --Ox00RRGGBB

frameDelay = 20

--Keys, picked because they are on the home row
-- (SDL.SDLK_UP, SDL.SDLK_DOWN) may be more "natural"
player1Keys = (SDL.SDLK_s, SDL.SDLK_d) 
player2Keys = (SDL.SDLK_k, SDL.SDLK_j) 

--Min and max speeds of the puts
putMinSpeed = 1.5
putMaxSpeed = 2.0

--And speed of the paddle
paddleSpeed = 2

----------------------------------
--Sets up the SDL surface
initialize :: IO Inputs
initialize = do
    SDL.init [SDL.InitVideo]

    screen <- SDL.setVideoMode screenWidth screenHeight 32 [SDL.HWSurface]
    SDL.setCaption "Y Pong" []

    SDL.fillRect screen Nothing (SDL.Pixel backgroundColour)
    SDL.flip screen

    unfoldWhileM (/= SDL.NoEvent) SDL.pollEvent
    

----------------------------------
-- The put is recreated, so it makes sense to have
-- this out of the main function. It also simplifies
-- things as a infinite list of random numbers is
-- passed to each successive put object
getPutObj :: [Double] -> Object
getPutObj vs = put (Point2 (fromIntegral screenWidth / 2 ) (fromIntegral screenHeight / 2)) 
                   (vector2 (vs !! 0) (vs !! 1)) 
                   (Circle 6) 
                   (drop 2 vs)
----------------------------------

main :: IO ()
main = do
    --SDL.TTF.init
    --font <- SDL.TTF.openFont "RedFive.otf" 72

    reactimate
         initialize
         input          --is the function that gets the input events 
         --(output font)  --
         output
         (process objs) -- signal function to animate

    --SDL.TTF.quit
    SDL.quit

    where
        --list of random values of put velocities, with a min speed
        --this has a performance issue if min and max are close or min is very large
        putV0 :: [Double]
        putV0 = filter (\x-> x < (-putMinSpeed) || x > (putMinSpeed)) $
                             randomRs (-putMaxSpeed, putMaxSpeed) (mkStdGen 42)

        paddleLO = paddle player1Keys
                          (Point2 16 (fromIntegral screenHeight / 2) ) 
                          (vector2 0 0) 
                          (Rectangle 8 32)
        
        paddleRO = paddle player2Keys
                          (Point2 (fromIntegral $ screenWidth - 16) (fromIntegral screenHeight / 2))
                          (vector2 0 0) 
                          (Rectangle 8 32)

        putO     = getPutObj putV0

        objs = listToIL [paddleLO, paddleRO, putO]

----------------------------------


paddle :: (SDL.SDLKey, SDL.SDLKey) -> Position -> Velocity -> Renderable -> Object
paddle (up, down) p0 v0 r = proc evnts -> do
    v <- (v0 ^+^) ^<< integral -< foldl (^+^) (vector2 0 0) $ mapMaybe checkInput (gameInputs evnts)
    rec
        p <- (p0 .+^) ^<< integral -< bound (hit evnts) v 
    returnA -< ObjOutput { objState = ObjState{ position = p, velocity = v, renderable = r }, killReq = Yampa.NoEvent, spawnReq = Yampa.NoEvent }
    where
        --Idea of this function is to get a new movement speed when a key is pressed, and the reverse movement vector when it is released
        checkInput (SDL.KeyDown (SDL.Keysym k  _ _ ) ) | k == up   = Just $ vector2 0 (-paddleSpeed)
                                                       | k == down = Just $ vector2 0 (paddleSpeed)
                                                       | otherwise = Nothing
        checkInput (SDL.KeyUp (SDL.Keysym k  _ _ ) )   | k == up   = Just $ vector2 0 (paddleSpeed)
                                                       | k == down = Just $ vector2 0 (-paddleSpeed)
                                                       | otherwise = Nothing
        checkInput _                                               = Nothing

        bound (Yampa.Event VerticalTop)     v = vector2 0 (paddleSpeed) 
        bound (Yampa.Event VerticalBottom)  v = vector2 0 (-paddleSpeed) 

        --The paddle should only be able to move up and down, however it will receive 
        --horizontal collision events when the ball hits it that we need to ignore
        bound _                             v = v 


put :: Position -> Velocity -> Renderable -> [Double] -> Object
put p0 v0 r v0s = proc evnts -> do
    rec
        v <- (v0 ^+^) ^<< integral -< checkHit (hit evnts) v
    p <- (p0 .+^) ^<< integral -< v

    returnA -< ObjOutput { objState = ObjState{ position = p, velocity = v0, renderable = r }, killReq = kill (hit evnts) p, spawnReq = spawn (hit evnts) p }
    where
        --TODO this surly is implemented in Yampa. Find it
        vecMult v1 v2 = vector2 (vector2X v1 * vector2X v2) (vector2Y v1 * vector2Y v2)

        --these are the vector multipliers for the velocity of the put.
        checkHit (Yampa.Event VerticalBottom)  v | vector2Y v > 0 =  v `vecMult` vector2  0  (-2)
                                                 | otherwise      = vector2 0 0 
        checkHit (Yampa.Event VerticalTop)     v | vector2Y v < 0 =  v `vecMult` vector2  0  (-2)
                                                 | otherwise      = vector2 0 0 
        checkHit (Yampa.Event HorizontalLeft)  v | vector2X v < 0 =  v `vecMult` vector2 (-2)  0 
                                                 | otherwise      = vector2 0 0 
        checkHit (Yampa.Event HorizontalRight) v | vector2X v > 0 =  v `vecMult` vector2 (-2)  0 
                                                 | otherwise      = vector2 0 0 
        checkHit _                             _                  = vector2 0    0

        kill e p  | checkBounds e p = Yampa.Event ()
                  | otherwise       = Yampa.NoEvent 

        spawn e p | checkBounds e p = Yampa.Event [getPutObj v0s] 
                  | otherwise       = Yampa.NoEvent

        --checks if the object is going off the left or right of the screen,with some error margin
        --TODO Remove the Magic number
        checkBounds ( Yampa.Event HorizontalLeft )  p | point2X p < 8 = True
                                                      | otherwise     = False
        checkBounds ( Yampa.Event HorizontalRight ) p | point2X p > fromIntegral (screenWidth - 8) = True
                                                      | otherwise = False
        checkBounds _                               _             = False



{-
scoreBoard :: Int -> Position -> Team -> Object
scoreBoard s0 p team = proc evnts -> do
    s <- accumHold s0 -< (playerScore `tag` (+1))
    returnA -<  ObjOutput { objState = Score { score = s, position = p }, killReq = Yampa.NoEvent, spawnReq = Yampa.NoEvent }
-}


----------------------------------
--IO functions
--
input :: Bool -> IO (DTime, Maybe Inputs)
input _ = do
    evnts <- unfoldWhileM (/=SDL.NoEvent) SDL.pollEvent
    return ( 1.0, Just evnts )

--output :: SDL.TTF.Font -> Bool -> IL ObjOutput -> IO Bool
output :: Bool -> IL ObjOutput -> IO Bool
output _ objs = do
    screen <- SDL.getVideoSurface

    SDL.fillRect screen Nothing (SDL.Pixel backgroundColour) 
    mapM ((render screen) . objState ) (elemsIL objs)

    SDL.flip screen
    SDL.delay frameDelay

    return . null $ keysIL objs
    where
        render :: SDL.Surface -> ObjState -> IO ()
        render screen (ObjState{ position=p, renderable=r@(Rectangle{}) }) = do
            SDL.fillRect screen ( Just $ SDL.Rect px py w h )  (SDL.Pixel 0x00DCDCCC)
            return ()
            where
                w = width r
                h = height r
                px = (fromIntegral . round $ point2X p) - ( (fromIntegral w) `div` 2 )
                py = (fromIntegral . round $ point2Y p) - ( (fromIntegral h) `div` 2 )

        render screen (ObjState{ position=p, renderable=c@(Circle{}) }) = do
            SDL.fillRect screen ( Just $ SDL.Rect px py d d )  (SDL.Pixel 0x00F0DFAF)
            return ()
            where
                r  = radius c
                d  = 2*r
                px = (fromIntegral . round $ point2X p) - (fromIntegral r)  
                py = (fromIntegral . round $ point2Y p) - (fromIntegral r)

        render _ _ = do
            return ()

----------------------------------
--receive the list of objects and the input and passes the states to core
process :: IL Object -> SF Inputs (IL ObjOutput)
process objs = proc input -> do
    rec
        s <- core objs -< (input, s)
    returnA -< s

-- The paper "The Yampa Arcade" gives a much better explanation of the function
-- dpSwitch than I can, as well as the differences between things like
-- dpSwitch (delayed parallel switch) and pSwitch (ofc parallel switch)
--
-- dpSwitch is required here due to the use of rec
core :: IL Object -> SF (Inputs, IL ObjOutput) (IL ObjOutput)
core objs = dpSwitch
                route
                 
                  --the initial collection of signal functions
                objs

                --Signal function that observes the external input and the output signals to produce
                --a switching event.
                --
                --Note, 'arr f' converts f to a signal function, and '>>>' is left to right composition
                (arr killAndSpawn >>> notYet) 

                --this is called when new switching event (from killAndSpawn) occurs
                (\x f -> (core . f) x) 


--checks the *previous object states* to projuce events  and routes (as per the name) the events to objects
route :: (Inputs, IL ObjOutput) -> IL sf -> IL (ObjInput, sf)
route (input, out) objs = mapIL routeE objs
    where
        routeE (key, obj) = (ObjInput { gameInputs = input, hit =  getHitE key, scored=Yampa.NoEvent}, obj)
        hs = ( bounds $ assocsIL $ fmap objState out ) ++ ( colisions $ assocsIL (fmap objState out) )

        --todo fix this
        getHitE k | (k, HorizontalLeft) `elem`  hs = Yampa.Event HorizontalLeft
                  | (k, HorizontalRight) `elem` hs = Yampa.Event HorizontalRight
                  | (k, VerticalTop) `elem` hs     = Yampa.Event VerticalTop
                  | (k, VerticalBottom) `elem` hs  = Yampa.Event VerticalBottom
                  | otherwise                      = Yampa.NoEvent

colisions :: [(ILKey, ObjState)] -> [(ILKey, Hit)]
colisions objs = concat $ map col $ combinations objs
    where
        col (a@(k1,o1), b@(k2, o2)) = toList $ colision a b
            where
                toList (Just (a, b)) = [a,b]
                toList _             = [] 

--checks the collision of a circle and a rectangle
--this is the only thing we need to check (atm)
colision :: (ILKey, ObjState) -> (ILKey, ObjState) -> Maybe ((ILKey, Hit), (ILKey, Hit)) 
--I am fairly sure this sort of pattern matching is a sin
colision ( k1, (ObjState{ position = pr, renderable = rect@(Rectangle{})}) ) --rectangle
         ( k2, (ObjState{ position = pc, renderable = circle@(Circle{})})  ) --circle
        | colOccured && point2X pr > point2X pc     = Just $ ((k1, HorizontalLeft), (k2, HorizontalRight))        
        | colOccured && point2X pr < point2X pc     = Just $ ((k1, HorizontalRight), (k2, HorizontalLeft))         
        | otherwise                                 = Nothing
    where
        cx  = point2X pc 
        cy  = point2Y pc 
        cr  = fromIntegral $ radius circle 

        rw  = fromIntegral (width rect) / 2
        rh  = fromIntegral (height rect) / 2

        rx  = point2X pr
        ry  = point2Y pr

        --the closest point on the rectangle to the circle
        closex = clamp cx (rx - rw) (rx + rw) 
        closey = clamp cy (ry - rh) (ry + rw)

        --distance between the above two points
        dx     = cx - closex
        dy     = cy - closey

        --simple application of pythag. Avoiding using sqrRoot
        colOccured = dx*dx + dy*dy < cr*cr


--This combination of arguments can be checked if we just flip the argument order
colision ( k1, c@(ObjState{ position = pr, renderable = (Circle{})})    ) --circle  
         ( k2, r@(ObjState{ position = pc, renderable = (Rectangle{})}) ) --rectangle
         = reorder $ colision (k2, r) (k1, c)
         where
            reorder (Just (a,b)) = Just (b,a)
            reorder _            = Nothing

--Otherwise we don't know how to handle the colision so we ignore it
colision _ _ = Nothing


--checks that objects are still within the bounds of the screen
bounds :: [(ILKey, ObjState)] -> [(ILKey, Hit)]
bounds objs = mapMaybe bound objs
    where
        --deals with differing widths of objects
        bound (k, o@(ObjState{renderable = (Rectangle{width=w, height = h})}) ) = bound' (k, o) w (h`div`2)
        bound (k, o@(ObjState{renderable = (Circle{radius=r})}) )               = bound' (k, o) r r
        bound (k, o)                                                            = bound' (k, o) 0 0

        bound' (k, ostate) x y | (point2X $ position ostate) < fromIntegral x                    = Just (k, HorizontalLeft)
                               | (point2X $ position ostate) > (fromIntegral $ screenWidth - x)  = Just (k, HorizontalRight)
                               | (point2Y $ position ostate) < fromIntegral y                    = Just (k, VerticalTop)
                               | (point2Y $ position ostate) > (fromIntegral $ screenHeight - y) = Just (k, VerticalBottom)
                               | otherwise                                                       = Nothing

-- possibly get a new IL Object collection to be fed back into core
killAndSpawn :: ((Inputs, IL ObjOutput), IL ObjOutput) -> Yampa.Event (IL Object -> IL Object )
killAndSpawn ((inputs, _), objs ) | any isEscDown inputs ||
                                    any isQuit inputs       = Yampa.Event (\_ -> emptyIL ) 
                                  | otherwise               = foldl (mergeBy (.)) Yampa.noEvent events
                                    where
                                        --check for sdl quit events
                                        --I am not to sure if this works...
                                        isQuit (SDL.Quit) = True 
                                        isQuit _          = False 

                                        --checks if the ESC key is pressed
                                        isEscDown (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _ )) = True
                                        isEscDown _                                               = False 

                                        --TODO reorder args
                                        spn o k = fmap  (foldl (.) id . map insertIL_) (spawnReq o) 
                                        del o k = tag ( killReq o ) (deleteIL k)
                                        events = [mergeBy (.) (spn obj key) (del obj key )| (key, obj) <- assocsIL objs] 
