import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp key ((k, value) : remaining)
  = if key == k then value else lookUp key remaining

states :: LTS -> [State]
states lts = (nub . states') lts
    where states' [] = []
          states' (((a, b), _) : rest) = a : b : (states' rest)

transitions :: State -> LTS -> [Transition]
transitions state lts
  = filter (\x -> fst (fst x) == state) lts

alphabet :: LTS -> Alphabet
alphabet = map snd

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions STOP           = []
actions (Ref id)       = []
actions (Prefix id p)  = id : (actions p)
actions (Choice plist) = concatMap actions plist

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts ids process
  = ids == (take (length ids) (accepts' (snd (process !! 0)) process))

accepts' :: Process -> [ProcessDef] -> [Id]
accepts' _ []                   = []
accepts' STOP process           = []
accepts' (Ref id) process       = accepts' nxt process
    where nxt = lookUp id process
accepts' (Prefix id p) process  = id : (accepts' p process)
accepts' (Choice plist) process = concatMap (\x -> accepts' x process) plist

------------------------------------------------------
-- PART III

--composeTransitions :: Transition -> Transition 
--                   -> Alphabet -> Alphabet 
--                   -> StateMap 
--                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions :: Transition -> Transition -> Alphabet -> Alphabet -> StateMap -> [Transition]
composeTransitions ((s, t), t1) ((s', t'), t2) alpha1 alpha2 m
  | t1 == t2                             = [((lookUp (s, s') m, lookUp (t, t') m), t1)]
  | (elem t1 alpha2) && (elem t2 alpha1) = []
  | elem t2 alpha1                       = [transition1]
  | elem t1 alpha2                       = [transition2]
  | otherwise                            = [transition1, transition2]
    where transition1 = ((lookUp (s, s') m, lookUp (t, s') m), t1)
          transition2 = ((lookUp (s, s') m, lookUp (s, t') m), t2)


pruneTransitions :: [Transition] -> LTS
pruneTransitions slist
  = visit 0 []
       where visit :: State -> [State] -> [Transition]
             visit state visited
               | elem state visited = []
               | otherwise = transit ++ (concatMap (\x -> visit x (visited ++ nxtstates)) nxtstates) --[((from, to), _)...]
                 where nxtstates = map (\x -> snd (fst x)) transit
                       transit   = transitions state slist

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose lts1 lts2
  = (concat [composeTransitions i j (alphabet lts1) (alphabet lts2) cartProduct | i <- lts1, j <- lts2])
      where len1  = length (states lts1)
            len2  = length (states lts2)
            cartProduct = [((i, j), i * len2 + j) | i <- [0..len1], j <- [0..len2]]

------------------------------------------------------
-- PART V

type VisitedLTS = String
buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS processdef@((name, process) : rest)
  = build process 0 [i | i <- [1..length (concatMap actions (map snd processdef))]] []
      where build :: Process -> State -> [State] -> [VisitedLTS] -> [Transition]
            build (Prefix id STOP) state states visited = [((state, head states), id)]
            build (Prefix id (Ref id')) state states visited
              = if id' == name || elem id' visited then [((state, 0), id)] else prefixRefBuild
            build (Ref id) state states visited = refBuild
            build (Prefix id p) state states visited = prefixBuild
            build (Choice plist) state states visited
              = concat (zipWith (\x y -> build x state y visited) plist (tails states))
              where prefixRefBuild = ((state, head states), id) : build (lookUp id' processdef) (head states) (tail states) (id' : visited)
                    refBuild       = build (lookUp id processdef) state states (id : visited)
                    prefixBuild    = ((state, head states), id) : (build p (head states) (tail states) visited)

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,2),"end"),((0,1),"think"),((1,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]
