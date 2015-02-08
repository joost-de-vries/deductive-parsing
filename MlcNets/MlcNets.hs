module MlcNets where

import List
import System

data ModeJ = ZeroJ | OneJ deriving (Eq,Ord)

instance Show ModeJ where 
 show ZeroJ = "0"
 show OneJ  = "1"

data ModeI = ZeroI | OneI deriving (Eq,Ord)

instance Show ModeI where 
 show ZeroI = "0"
 show OneI  = "1"

data Form = Atom String
          | Diamond  ModeJ Form 
          | Rbox     ModeJ Form
          | Bullet   ModeI Form Form 
          | SlashL   ModeI Form Form 
          | SlashR   ModeI Form Form  
          deriving (Eq,Ord)

atomic :: Form -> Bool
atomic (Atom _) = True
atomic _        = False

np    = Atom "NP" 
s     = Atom "S"
n     = Atom "N"
det   = SlashR ZeroI np n
adj   = SlashR ZeroI n n 
adj'  = SlashL ZeroI n n 
iv    = SlashL ZeroI np s 
tv    = SlashR ZeroI iv np 
cv    = SlashR ZeroI (SlashR ZeroI iv adj) np
np0   = Diamond ZeroJ (Rbox ZeroJ np)
np1   = Diamond OneJ  (Rbox OneJ  np)
rel0  = SlashR ZeroI adj' (SlashR ZeroI s np0)
rel1  = SlashR ZeroI adj' (SlashL ZeroI np s)

instance Show Form where 
  show (Atom s) = s 
  show (Diamond j f)  =  "<" ++ show j ++ ">" ++ show f 
  show (Rbox    j f)  =  "[" ++ show j ++ "~]" ++ show f
  show (Bullet i a b) = 
      "(" ++ show a  ++ "." ++ show i ++ show b ++ ")" 
  show (SlashR i a b) = 
      "(" ++ show a  ++ " /" ++ show i ++ " " ++ show b ++ ")"
  show (SlashL  i b a)  = 
      "(" ++ show b  ++ " \\" ++ show i ++ " " ++ show a ++ ")"

lexicon :: String -> [Form]
lexicon "Michael"     = [np]
lexicon "Jan"         = [np]
lexicon "the"         = [det]
lexicon "man"         = [n]
lexicon "woman"       = [n]
lexicon "nice"        = [adj]
lexicon "intelligent" = [adj]
lexicon "smiled"      = [iv]
lexicon "talked"      = [iv]
lexicon "liked"       = [tv]
lexicon "admired"     = [tv]
lexicon "considered"  = [cv]
lexicon "that"        = [rel0,rel1]
lexicon "who"         = [rel1]
lexicon "whom"        = [rel0]
lexicon _             = []

lookupLex :: [String] -> [[Form]]
lookupLex [] = [[]]
lookupLex (word:words) = [ f:fs | f <- lexicon word, fs <- lookupLex words ]

scan :: String -> String 
scan [] = []
scan (x:xs) | x == '.' || x =='?'  = ' ':scan xs
            | otherwise            =  x :scan xs

lexer :: String -> [String]
lexer = words . scan

type Edge a b = (a,a,b)

p1 :: (a,b,c) -> a
p1 (x,y,z) = x
p2 :: (a,b,c) -> b
p2 (x,y,z) = y
p3 :: (a,b,c) -> c
p3 (x,y,z) = z

type Graph a b = ([a],[Edge a b])

mapTriple :: (a -> b, c -> d, e -> f) -> [(a,c,e)] -> [(b,d,f)]
mapTriple (f,g,h) triples = [(f x, g y, h z) | (x,y,z) <- triples ]

mapGraph :: (a -> b) -> (c -> d) -> Graph a c -> Graph b d
mapGraph f g (nodes,edges) = (map f nodes, mapTriple (f,f,g) edges)

type Link a b = ([a],[a],b)

type Mgraph a b = ([a],[Link a b])

mapLink :: (a -> b) -> (c -> d) -> Link a c -> Link b d
mapLink f g (xs,ys,label) = (map f xs, map f ys, g label)

mapMgraph :: (a -> b) -> (c -> d) -> Mgraph a c -> Mgraph b d
mapMgraph f g (nodes,links) = (map f nodes, map (mapLink f g) links)

fuseMgraph :: (Eq a,Ord a) => (a,a) -> Mgraph a b -> Mgraph a b 
fuseMgraph (x,y) (nodes,edges) = (nodes \\ [two], map (mapLink f id) edges)
  where 
    one = min x y 
    two = max x y 
    f = \ z -> if z == two then one else z

data Tag  = Ldia    ModeJ
          | Rdia    ModeJ 
          | Lrbox   ModeJ
          | Rrbox   ModeJ
          | Brack   ModeJ
          | Lbullet ModeI
          | Rbullet ModeI
          | Lslashl ModeI
          | Rslashl ModeI
          | Lslashr ModeI
          | Rslashr ModeI 
          | Concat  ModeI
          deriving Eq

instance Show Tag where
  show (Ldia    j) = "L<" ++ show j ++ ">"
  show (Rdia    j) = "R<" ++ show j ++ ">"
  show (Lrbox   j) = "L[" ++ show j ++ "~]"
  show (Rrbox   j) = "R[" ++ show j ++ "~]"
  show (Brack   j) = "<<" ++ show j ++ ">>"
  show (Lbullet i) = "L." ++ show i 
  show (Rbullet i) = "R." ++ show i 
  show (Lslashl i) = "L\\" ++ show i 
  show (Rslashl i) = "R\\" ++ show i 
  show (Lslashr i) = "L/" ++ show i 
  show (Rslashr i) = "R/" ++ show i 
  show (Concat  i) = "C." ++ show i 

jTag :: Tag -> Maybe ModeJ
jTag (Ldia j)   = Just j
jTag (Rdia j)   = Just j
jTag (Lrbox j)  = Just j
jTag (Rrbox j)  = Just j
jTag (Brack j)  = Just j
jTag _          = Nothing

iTag :: Tag -> Maybe ModeI
iTag (Lbullet i) = Just i
iTag (Rbullet i) = Just i
iTag (Lslashl i) = Just i
iTag (Rslashl i) = Just i
iTag (Lslashr i) = Just i
iTag (Rslashr i) = Just i 
iTag (Concat i)  = Just i
iTag _           = Nothing 

structuralTag :: Tag -> Bool
structuralTag (Brack _)  = True 
structuralTag (Concat _) = True 
structuralTag _          = False 

type Fi = (Form,Int) 

type Annotation = Int -> (String,String) 

mark :: Int -> String -> String -> Annotation
mark i str1 str2 = \ m -> if i == m then (str1,str2) else ("","")

annotate :: (String,String) -> String
annotate = \ (x,y) -> if x == "" 
                         then y
                         else if y == ""
                                 then x 
                                 else x ++ "," ++ y

ann2pairs :: [Int] -> Annotation -> [(Int,String)]
ann2pairs dom f = [ (i,annotate (f i)) | i <- dom, (f i) /= ("","") ]

data PS = PS (Mgraph Fi Tag) Annotation

instance Show PS where 
  show (PS (nodes,links) annotation) = 
     show nodes ++ show links ++ show (ann2pairs dom annotation)
       where dom = map snd nodes

hypotheses :: PS -> [Fi]
hypotheses (PS (nodes,links) _) = 
  [ f | f <- nodes, notElem f conclusions ] 
    where conclusions = [ c | (xs,ys,tag) <- links, c <- ys ]

conclusions :: PS -> [Fi]
conclusions (PS (nodes,links) _) = 
  [ f | f <- nodes, notElem f premisses ] 
    where premisses = [ c | (xs,ys,tag) <- links, c <- xs ]

expand :: Int -> [Fi] -> [Fi] -> [Fi] -> [Link Fi Tag] -> Mgraph Fi Tag
expand n all [] [] links = (all,links) 

expand n all ((Atom s,m):xs) ys links = 
  expand n ((Atom s,m):all) xs ys links
expand n all ((Diamond j f,m):xs) ys links = 
  expand (n+1) ((Diamond j f,m):all) ((f,n):xs) ys (link:links) 
  where link = ([(Diamond j f,m)],[(f,n)],Ldia j)
expand n all ((Rbox j f,m):xs) ys links = 
  expand (n+1) ((Rbox j f,m):all) ((f,n):xs) ys (link:links) 
  where link = ([(Rbox j f,m)],[(f,n)],Lrbox j)

expand n all ((Bullet i f1 f2,m):xs) ys links = 
  expand (n+2) ((Bullet i f1 f2,m):all) 
               ((f1,n):(f2,n+1):xs) ys (link:links) 
  where link = ([(Bullet i f1 f2,m)],[(f1,n),(f2,n+1)],Lbullet i)
expand n all ((SlashL i f1 f2,m):xs) ys links = 
  expand (n+2) ((SlashL i f1 f2,m):all) 
               ((f2,n+1):xs) ((f1,n):ys) (link:links) 
  where link = ([(f1,n),(SlashL i f1 f2,m)],[(f2,n+1)],Lslashl i)
expand n all ((SlashR i f1 f2,m):xs) ys links = 
  expand (n+2) ((SlashR i f1 f2,m):all) 
               ((f1,n):xs) ((f2,n+1):ys) (link:links) 
  where link = ([(SlashR i f1 f2,m),(f2,n+1)],[(f1,n)],Lslashr i)

expand n all xs ((Atom s,m):ys) links = 
  expand n ((Atom s,m):all) xs ys links
expand n all xs ((Diamond j f,m):ys) links = 
  expand (n+1) ((Diamond j f,m):all) xs ((f,n):ys) (link:links) 
  where link = ([(f,n)],[(Diamond j f,m)],Rdia j)
expand n all xs ((Rbox j f,m):ys) links = 
  expand (n+1) ((Rbox j f,m):all) xs ((f,n):ys) (link:links) 
  where link = ([(f,n)],[(Rbox j f,m)],Rrbox j)

expand n all xs ((Bullet i f1 f2,m):ys) links = 
  expand (n+2) ((Bullet i f1 f2,m):all) xs 
               ((f1,n):(f2,n+1):ys) (link:links) 
  where link = ([(f1,n),(f2,n+1)],[(Bullet i f1 f2,m)],Rbullet i)
expand n all xs ((SlashL i f1 f2,m):ys) links = 
  expand (n+2) ((SlashL i f1 f2,m):all) 
               ((f1,n):xs) ((f2,n+1):ys) (link:links) 
  where link = ([(f2,n+1)],[(f1,n),(SlashL i f1 f2,m)],Rslashl i)
expand n all xs ((SlashR i f1 f2,m):ys) links = 
  expand (n+2) ((SlashR i f1 f2,m):all) 
               ((f2,n+1):xs) ((f1,n):ys) (link:links) 
  where link = ([(f1,n)],[(SlashR i f1 f2,m),(f2,n+1)],Rslashr i)

expandPremisse :: String -> Form -> PS 
expandPremisse str form = PS (expand 1 [] [(form,0)] [] []) annotation
   where annotation = mark 0 str ""

expandGoal :: Form -> PS
expandGoal form = PS (expand 1 [] [] [(form,0)] []) annotation 
   where annotation = mark 0 "" "GOAL"

type CS = Mgraph Int Tag

display :: Show a => [a] -> IO()
display [] = return ()  
display (x:xs) = do print x
                    display xs

displayCS :: CS -> IO()
displayCS (nodes,links) = do print nodes 
                             display links

type Labelling = Int -> ([(Form,String)],[(Form,String)])

type Labels = [(Int,String,[Form],[Form])]

lab2labels :: [Int] -> Labelling -> Labels
lab2labels dom f =  
   [ (n, 
      ann ((map snd (hyps n)) ++ (map snd (concs n))),
      map fst (hyps n), 
      map fst (concs n)) | n <- dom, (hyps n /= [] || concs n /= [])   ]
     where 
       hyps i    = fst (f i) 
       concs i   = snd (f i) 
       ann []    = ""
       ann [x]   = x
       ann [x,y] = annotate (x,y)

adjustLabelling :: (Int,Int) -> Labelling -> Labelling 
adjustLabelling (goalNode,premisseNode) lab = \ z -> 
   if z == goalNode || z == premisseNode 
      then   (fst (lab goalNode), 
              snd (lab premisseNode))
      else lab z

adjustLabC :: (Int,Int) -> Labelling -> Labelling 
adjustLabC = adjustLabelling . (\ (x,y) -> (y,x))

data HS = HS CS Labelling

instance Show HS where 
  show (HS (nodes,edges) f) =  show nodes 
                            ++ show edges
                            ++ show (lab2labels nodes f) 

displayHS :: HS -> IO()
displayHS (HS (nodes,links) f) = do displayCS (nodes,links)
                                    display (lab2labels nodes f)

isLexical :: Int -> HS -> Bool
isLexical i (HS _ lab) = 
   fst (lab i) /= [] && (snd . head . fst . lab) i /= "" 

isGoal :: Int -> HS -> Bool
isGoal i (HS _ lab) =  
   snd (lab i) /= [] && (snd . head . snd . lab) i /= "" 

lexAnn :: HS -> Int -> String
lexAnn hs@(HS _ lab) i 
   | isLexical i hs = (snd . head . fst . lab) i
   | otherwise      = ""

hypsHS :: HS -> [(Int,Form,String)]
hypsHS (HS (nodes,links) f) = 
    [ (i,form,string) | i <- nodes, (form,string) <- fst (f i) ]

linkinghypsHS :: HS -> [(Int,Form)]
linkinghypsHS hs@(HS (nodes,links) f) = 
  [ (i, form) | (i,form,str) <- hypsHS hs, atomic form, str == "" ]

concsHS :: HS -> [(Int,Form,String)]
concsHS (HS (nodes,links) f) = 
    [ (i,form,str) | i <- nodes, (form,str) <- snd (f i) ]

linkingconcsHS :: HS -> [(Int,Form)]
linkingconcsHS hs@(HS (nodes,links) f) = 
  [ (i, form) | (i,form,str) <- concsHS hs, atomic form, str == ""  ]

data Direction = Le | Ri deriving  (Eq,Show) 

direction :: Int -> HS -> [Direction]
direction i (HS (nodes,edges) lab) = 
 [ Le | elem i leftnodes ] 
   ++ 
 [ Ri | elem i rightnodes ] 
   where 
     leftnodes     = map head binaryLinks
     rightnodes    = map (head.tail) binaryLinks
     binaryLinks   = filter (\ x -> length x == 2) edgePositions
     edgePositions = 
       map (\ x -> p1 x) edges ++ map (\ x -> p2 x) edges 

fuseHS :: HS -> (Int,Int) -> HS 
fuseHS (HS ps f) (goalNode,hypothesisNode) =
  HS (fuseMgraph (goalNode,hypothesisNode) ps) 
     (adjustLabelling (goalNode,hypothesisNode) f)

unionHS :: HS -> HS -> HS
unionHS (HS ps1@(nodes1,links1) lab1) (HS ps2 lab2) = HS ps3 lab3
    where 
      k = if null nodes1 then 0 else (last nodes1) + 1
      (nodes2,links2) = mapMgraph (\ n -> n + k) id ps2
      ps3 = (nodes1 ++ nodes2, links1 ++ links2)
      lab3 = \ m -> if m < k then lab1 m else lab2 (m - k) 

trivHS :: HS
trivHS =  HS ([],[]) (\ x -> undefined)

unionHSs :: [HS] -> HS
unionHSs = foldl unionHS trivHS 

transformTensorLink :: Link a Tag -> Link a Tag
transformTensorLink ([p],[c], Rdia j)        = ([p],[c], Brack j)
transformTensorLink ([p],[c], Lrbox j)       = ([p],[c], Brack j)
transformTensorLink ([p1,p2],[c], Lslashl i) = ([p1,p2],[c], Concat i)
transformTensorLink ([p1,p2],[c], Lslashr i) = ([p1,p2],[c], Concat i)
transformTensorLink   parlink                = parlink

ps2hs :: PS -> HS
ps2hs ps@(PS (nodes,links) annotation) = HS (ns,ilinks) lab
  where 
    f      = \ (_,i) -> i 
    ns     = sort (map f nodes) 
    ilinks = map ((mapLink f id) . transformTensorLink) links
    lab    = \ i -> 
             (  
              [ (h, fst (annotation i)) | (h,j) <- hypotheses ps,  i == j ], 
              [ (g, snd (annotation i)) | (g,k) <- conclusions ps, i == k ]
             )

buildHS :: [(String,Form)] -> Form -> HS
buildHS premisses goal = 
  unionHSs (premissesHSs ++ [goalHS])
    where premissesHSs = [ ps2hs (expandPremisse w f) | (w,f) <- premisses ] 
          goalHS       =   ps2hs (expandGoal goal) 

problemHSs :: [String] -> Form -> [HS]
problemHSs words goal = 
   [ buildHS (zip words premisses) goal | premisses <- lookupLex words ]

linkable :: HS -> Bool
linkable hs@(HS (nodes,links) lab) = hyps == concs
   where 
   hyps  = sort (map snd (linkinghypsHS hs)) 
   concs = sort (map snd (linkingconcsHS hs))

type Linking = [(Int,Int)]

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs))
  where 
  insrt :: a -> [a] -> [[a]]
  insrt x [] = [[x]]
  insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

agree :: [(Int,Form)] -> [(Int,Form)] -> Bool
agree []    []    = True 
agree (_:_) []    = False 
agree []    (_:_) = False 
agree ((_,f):xs) ((_,f'):ys) = f == f' && agree xs ys 

linkings:: HS -> [Linking]
linkings hs = 
   [ zipWith (\ x y -> (fst x, fst y)) concs hyps' | 
         hyps' <- perms hyps, agree concs hyps'       ] 
   where 
   concs = linkingconcsHS hs
   hyps  = linkinghypsHS hs   

linkup :: HS -> Linking -> HS 
linkup = foldl fuseHS 

linkedHSs :: HS -> [HS] 
linkedHSs hs = if not (linkable hs) 
                  then []
                  else [ linkup hs link | link <- linkings hs ] 

makeHSs :: [String] -> Form -> [HS]
makeHSs words form = concat (map linkedHSs (problemHSs words form))

type Crule = HS -> [(Int,Int,[Int],[Link Int Tag])]

ldiaM ::  Crule 
ldiaM (HS (nodes,links) lab) = 
  [(g,p,[n], [link1,link2]) | 
       link1@([p],[n],  Ldia j)   <- links, 
       link2@([n'],[g],Brack j')  <- links, 
       n == n', 
       j == j'                              ]

lbulletM ::  Crule
lbulletM (HS (nodes,links) lab) = 
  [(g,p,[n,k], [link1,link2]) | 
       link1@([p],[n,k],    Lbullet i)  <- links, 
       link2@([n',k'],[g], Concat i')  <- links, 
       n == n', 
       k == k',
       i == i'                                 ]

rrboxM ::  Crule 
rrboxM (HS (nodes,links) lab) = 
  [(g,p,[n],[link1,link2]) | 
       link1@([p],[n],  Brack j)   <- links, 
       link2@([n'],[g],Rrbox j')  <- links, 
       n == n', 
       j == j'                                ]

rslashlM ::  Crule 
rslashlM (HS (nodes,links) lab) = 
  [(g,p,[n,k],[link1,link2]) | 
       link1@([n,p],[k],   Concat i)    <- links, 
       link2@([k'],[n',g], Rslashl i')  <- links, 
       n == n', 
       k == k',
       i == i'                                       ]

rslashrM ::  Crule 
rslashrM (HS (nodes,links) lab) = 
  [(g,p,[n,k],[link1,link2]) | 
       link1@([p,k],[n],   Concat i)    <- links, 
       link2@([n'],[g,k'], Rslashr i')  <- links, 
       n == n', 
       k == k',
       i == i'                                      ]

contract :: Crule -> HS -> [HS]
contract rule hs = contract' (rule hs) hs where 
  contract' :: [(Int,Int,[Int],[Link Int Tag])] -> HS -> [HS]
  contract' [] hs = [] 
  contract' ((g,p,ns,ls):rest) hs@(HS (nodes,links) lab) = 
    (HS (nodes',links'') lab'): contract' rest hs 
    where 
      one     = min g p 
      two     = max g p 
      nodes'  = nodes \\ (two:ns) 
      links'  = links \\ ls
      links'' = map (mapLink (\x -> if x == two then one else x) id) links'
      lab'    = adjustLabC (g,p) lab

cRules :: [Crule] 
cRules = [ldiaM, lbulletM, rrboxM, rslashlM, rslashrM]

tryCrules :: HS -> [HS] 
tryCrules hs = 
  concat [ contract rule hs | rule <- cRules ]

ass00 :: (CS,CS)
ass00 = (([0,1,2,3,4,5],
                  [
                    ([0,4],[3],Concat ZeroI), 
                    ([1,5],[4],Concat ZeroI),
                    ([2],[5],Brack ZeroJ)
                  ]), 
                ([0,1,2,3,4,5], 
                  [
                    ([0,1],[4],Concat ZeroI), 
                    ([4,5],[3],Concat ZeroI),
                    ([2],[5],Brack ZeroJ)
                  ])) 

ass01 :: (CS,CS)
ass01 = (([0,1,2,3,4,5],
                  [
                    ([0,4],[3],Concat ZeroI), 
                    ([1,5],[4],Concat ZeroI),
                    ([2],[5],Brack OneJ)
                  ]), 
                ([0,1,2,3,4,5], 
                  [
                    ([0,1],[4],Concat ZeroI), 
                    ([4,5],[3],Concat ZeroI),
                    ([2],[5],Brack OneJ)
                  ])) 

mxCom00 ::(CS,CS) 
mxCom00 = (([0,1,2,3,4,5],
                    [
                       ([0,5],[4],Concat ZeroI), 
                       ([4,2],[3],Concat ZeroI),
                       ([1],[5],Brack ZeroJ)
                    ]), 
                  ([0,1,2,3,4,5],
                    [
                       ([0,2],[4],Concat ZeroI), 
                       ([4,5],[3],Concat ZeroI),
                       ([1],[5],Brack ZeroJ)
                    ]))

mxCom01 :: (CS,CS) 
mxCom01 = (([0,1,2,3,4,5],
                    [
                       ([0,5],[4],Concat ZeroI), 
                       ([4,2],[3],Concat ZeroI),
                       ([1],[5],Brack OneJ)
                    ]), 
                  ([0,1,2,3,4,5],
                    [
                       ([0,2],[4],Concat ZeroI), 
                       ([4,5],[3],Concat ZeroI),
                       ([1],[5],Brack OneJ)
                    ]))

com00 ::(CS,CS) 
com00 = (([0,1,2,3],
                    [
                       ([3,1],[2],Concat ZeroI), 
                       ([0],[3],Brack ZeroJ)
                    ]), 
                  ([0,1,2,3],
                    [
                       ([1,3],[2],Concat ZeroI),
                       ([0],[3],Brack ZeroJ)
                    ]))

displayCP :: (CS,CS) -> IO()
displayCP (before,after) = do displayCS before 
                              putStrLn "===>"
                              displayCS after 

correctionPatterns :: [(CS,CS)]
correctionPatterns = [ass00, ass01, mxCom00, mxCom01]

matchLink :: Link Int Tag -> Link Int Tag -> [Linking]
matchLink (xs1,ys1,tag1) (xs2,ys2,tag2) 
   | tag1 == tag2 = [ zip  (xs1 ++ ys1) (xs2 ++ ys2) ]
   | otherwise    = []

match :: Link Int Tag -> [Link Int Tag] -> [(Link Int Tag, Linking)]
match link links = 
   [ (link1, linking) | link1 <- links, linking <- matchLink link link1 ]

fuseLinkings :: Linking -> Linking -> [Linking]
fuseLinkings [] ls = [ls]
fuseLinkings ls [] = [ls]
fuseLinkings ((x,y):ls) ls' = 
   if lookup x ls' == Nothing 
     then [ (x,y): rest | rest <- fuseLinkings ls ls' ]
     else 
       if lookup x ls' == Just y 
         then [ (x,y): rest | rest <- fuseLinkings ls (ls' \\ [(x,y)]) ]
         else []

matchLinks :: [Link Int Tag] -> [Link Int Tag] -> [Linking]
matchLinks [] ls = [[]]
matchLinks ls [] = [[]]
matchLinks (link:links) links1 = 
  concat 
  [ fuseLinkings linking1 linking2 | 
         (link1,linking1) <- match link links1, 
          linking2        <- matchLinks links (links1 \\ [link1])  ]

linking2fct :: [Int] -> [Int] -> Linking -> Int -> Int
linking2fct new old linking = 
    \ x -> if elem x (map fst linking) 
              then head [ z | (y,z) <- linking, x == y ] 
              else 
                if elem x new 
                   then x + k 
                   else x 
   where k = if null old then 0 else (last old) + 1                  

matchPattern :: (CS,CS) -> HS -> [(CS,CS)]
matchPattern (before@(nodes,links),after@(nodes1,links1))
             (HS (nodes2,links2) lab) = 
  [ (mapMgraph f id before, mapMgraph f id after) | 
       f <- map (linking2fct (nodes1 \\ nodes) nodes2) 
                (matchLinks links links2)                         ]

applyPattern :: (CS,CS) -> HS -> [HS]
applyPattern pattern hs@(HS (nodes,links) lab) = 
   [ HS (sort ((nodes \\ ns1) ++ ns2), union (links \\ ls1) ls2)
        (adjustLab (ns2 \\ ns1) lab) | 
         ((ns1,ls1),(ns2,ls2)) <- matchPattern pattern hs  ]
     where adjustLab dom f = \ x -> if elem x dom then ([],[]) else f x

tryCorrections  :: HS -> [HS] 
tryCorrections hs = 
  concat [ applyPattern pattern hs | pattern <- correctionPatterns ]

tracePrems :: HS -> Int -> [Int]
tracePrems hs@(HS (nodes,edges) lab) i  
  | isLexical i hs  = [i] 
  | otherwise       = concat (map (tracePrems hs) is)
      where is = [ j | (x,z,t) <- edges, j <- x, elem i z ]

traceP :: HS -> [String]
traceP hs@(HS (nodes,edges) lab) = 
  map (lexAnn hs) (concat (map (tracePrems hs) is))
     where is = [ i | i <- nodes, isGoal i hs ]

hypothesisTree :: [String] -> HS -> Bool
hypothesisTree words hs@(HS (nodes,links) lab) = 
  all (\ (x,y,z) -> structuralTag z) links
  && 
  traceP hs == words

processHS :: [String] -> HS -> [HS]
processHS words hs = 
  if hypothesisTree words hs 
    then [hs] 
    else 
      concat (map (processHS words) ((tryCrules hs) ++ (tryCorrections hs)))

process :: String -> Form -> [HS]
process string form = concat (map (processHS words) (makeHSs words form))
  where words  = lexer string  

parse :: String -> Form -> IO()
parse string form = 
  let results = process string form 
  in 
    if null results then putStrLn "no parse"
                    else mapM_ displayHS results 

parseVerbose :: String -> Form -> IO()
parseVerbose string form = 
  let results = process string form 
  in 
    if null results 
      then 
        do  (putStr . show) string 
            putStr "  "
            print form 
            putStrLn "no parse"
      else 
        do  (putStr . show) string 
            putStr "  "
            print form 
            mapM_ displayHS results 

examples = 
  [
  ("Jan liked Michael.",  s), 
  ("Jan liked the man that liked Michael.",  s), 
  ("the man that liked Michael smiled." , s), 
  ("the man that Michael liked smiled." , s), 
  ("the man whom Michael liked smiled." , s), 
  ("the man whom liked Michael smiled." , s),
  ("man who considered Michael intelligent.",  n), 
  ("man who Michael considered intelligent.",  n)
  ]

test :: IO() 
test = sequence_ [ parseVerbose string form | (string,form) <- examples ]

