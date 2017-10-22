

--Map contenant les premieres consonnes
firstConsonant = [(0,"g"),(1,"kk"),(2,"n"),(3,"d"),(4,"tt"),(5,"r"),(6,"m"),(7,"b"),(8,"pp"),(9,"s"),(10,"ss"),(11,""),(12,"j"),(13,"jj"),(14,"ch"),(15,"k"),(16,"t"),(17,"p"),(18,"h")]
--Map contenant les voyelles
voyels = [(0,"a"),(1,"ae"),(2,"ya"),(3,"yae"),(4,"eo"),(5,"e"),(6,"yeo"),(7,"ye"),(8,"o"),(9,"wa"),(10,"wae"),(11,"oe"),(12,"yo"),(13,"u"),(14,"wo"),(15,"we"),(16,"wi"),(17,"yu"),(18,"eu"),(19,"ui"),(20,"i")]
--Map contenant les dernieres consonnes
secondConsonant = [(0,""),(1,"k"),(2,"k"),(3,"kt"),(4,"n"),(5,"nt"),(6,"nh"),(7,"t"),(8,"l"),(9,"lk"),(10,"lm"),(11,"lp"),(12,"lt"),(13,"lt"),(14,"lp"),(15,"lh"),(16,"m"),(17,"p"),(18,"pt"),(19,"t"),(20,"t"),(21,"ng"),(22,"t"),(23,"t"),(24,"k"),(25,"t"),(26,"p"),(27,"h")]
--Map de lettre et de map, qui eux contiennent une autre lettre et la lettre a changer
replaceList = [(1,[(11,"g"),(2,"ngn"),(5,"ngn"),(6,"ngm"),(15,"k-k")]),(4,[(0,"n-g"),(5,"ll")]),(7,[(11,"d"),(2,"nn"),(5,"nn"),(6,"nm"),(16,"t-t")]),(8,[(11,"r"),(2,"ll"),(5,"ll")]),(16,[(5,"mn")]),(17,[(11,"b"),(2,"mn"),(5,"mn"),(6,"mm"),(17,"p-p")]),(19,[(11,"s"),(2,"nn"),(5,"nn"),(6,"nm"),(16,"t-t")]),(21,[(11,"ng-"),(5,"ngn")]),(22,[(11,"J"),(2,"nn"),(5,"nn"),(6,"nm")]),(23,[(11,"ch"),(2,"nn"),(5,"nn"),(6,"nm"),(16,"t-t")]),(25,[(2,"nn"),(5,"nn"),(6,"nm"),(16,"t-t")]),(27,[(11,"h"),(0,"k"),(2,"nn"),(3,"t"),(5,"nn"),(6,"nm"),(7,"p"),(9,"hs"),(12,"ch"),(18,"t")])]

main = do
        a <-getLine;
        putStrLn(work a)

work = cleanInput>.>createJamosList>.>romanisation>.>correctSentence>.>printListOfListWithSeperator

--Fonction permettant d'inverser le sens de l'évaluation partielle. Provient des notes de cours de Bruno Malenfant.
(>.>) :: (a->b) -> (b->c) -> (a->c)
g >.> f = f.g

--Grosse fonction qui permet d'effectuer toutes les opérations nécéssaires pour la validation de l'entrée de l'utilisateur.
cleanInput :: [Char] -> [[Int]]
cleanInput = seperateIntoSentences>.>seperateIntoWords>.>cleanString>.>validateAllNumbers

--Permet de valider tous les Strings afin de s'assurer s'ils sont des nombres.
validateAllNumbers :: [[[Char]]] -> [[Int]]
validateAllNumbers xs = (map.map) validateNumber xs

--Vérifie si le String est un nombre valide.
validateNumber :: [Char] -> Int
validateNumber x
    | isNumber x = (read x :: Int)
    | otherwise = error "Unicode numbers must only contain digits..."

isNumber :: [Char] -> Bool
isNumber [] = True
isNumber (x:xs)
    | elem x ['1','2','3','4','5','6','7','8','9','0'] = isNumber xs
    | otherwise = False

--Sépare le texte entre les caractères '.' (construit une liste de phrases)
seperateIntoSentences :: [Char] -> [[Char]]
seperateIntoSentences = divideChain ['.']

seperateIntoWords :: [[Char]] -> [[[Char]]]
seperateIntoWords xs = map (divideChain [' ']) xs

cleanString :: [[[Char]]] -> [[[Char]]]
cleanString xs = (map.map) verifyWord xs

verifyWord :: [Char] -> [Char]
verifyWord x
    | (take 2 x) == "&#" && (last x) == ';' = filter (not . (`elem` "#&;")) x
    | otherwise = error "Wrong input..."

createJamosList :: [[Int]] -> [[(Int, Int, Int)]]
createJamosList [] = []
createJamosList xs = (map.map) createJamosFromUnicode xs

createJamosFromUnicode :: Int -> (Int, Int, Int)
createJamosFromUnicode x = (ci, v, cf)
    where   ci = (x - 44032) `div` 588
            v = ((x - 44032) `mod` 588) `div` 28
            cf = ((x - 44032) `mod` 588) `mod` 28

divideChain :: [Char] -> [Char] -> [[Char]]
divideChain dividers cs =
    case dropWhile ((flip elem) dividers) cs of
    "" -> []
    cs' -> e : divideChain dividers cs''
        where (e,cs'') = break ((flip elem) dividers) cs'

romanisation :: [[(Int, Int, Int)]] -> [[((Int, String), (Int, String), (Int, String))]]
romanisation xs = (map.map) (\(x, y, z)  -> (myLookup x firstConsonant, myLookup y voyels, myLookup z secondConsonant)) xs

myLookup :: Eq a => a -> [(a, b)] -> (a,b)
myLookup key [] = error "A wrong unicode number was found..."
myLookup key ((x, y):list) =
    if key == x
       then (key,y)
       else myLookup key list

correctSentence :: [[((Int, String), (Int, String), (Int, String))]] -> [[(String, String, String)]]
correctSentence [] = []
correctSentence xs = map (\(list) -> (transformHangeul list)) xs

transformHangeul :: [((Int, String), (Int, String), (Int, String))] -> [(String, String, String)]
transformHangeul ((first1,second1,third1):(first2,second2,third2):list)
    | length list == 0 =
        let
            word1 = (first1,second1,third1)
            word2 = (first2,second2,third2)
            newWord = (replaceLetter word1 word2 replaceList)
        in
            if(newWord == (snd first1, snd second1, snd third1))
                then (snd first1, snd second1, snd third1) : (snd first2,snd second2,snd third2) : []
                else newWord : ("",snd second2,snd third2) : []
    | otherwise =
        let
            word1 = (first1,second1,third1)
            word2 = (first2,second2,third2)
            newWord = (replaceLetter word1 word2 replaceList)
        in
            if(newWord == (snd first1, snd second1,snd third1))
                then (snd first1, snd second1, snd third1) : transformHangeul (word2:list)
                else newWord : transformHangeul (((0,[]),second2,third2):list)

replaceLetter :: ((Int, String), (Int, String), (Int, String)) -> ((Int, String), (Int, String), (Int, String)) -> [(Int,[(Int, String)])] -> (String, String, String)
replaceLetter word1 word2 [] = tupleToString word1
replaceLetter (first1, second1, third1) (first2, second2, third2) ((number, letterList):list) =
    if fst third1 == number
        then (snd first1, snd second1, (findMatch third1 first2 letterList))
        else replaceLetter (first1, second1, third1) (first2, second2, third2) list

findMatch :: (Int, String) -> (Int, String) -> [(Int, String)] -> String
findMatch third _ [] = snd third
findMatch third first ((number, letter):list) =
    if fst first == number
        then letter
        else findMatch third first list

tupleToString :: ((Int, String), (Int, String), (Int, String)) -> (String, String, String)
tupleToString (first, second, third) = (snd first, snd second, snd third)

printListOfListWithSeperator :: [[(String, String, String)]] -> String
printListOfListWithSeperator [] = []
printListOfListWithSeperator (x:xs) = printListOfTuple x ++ "." ++ printListOfListWithSeperator xs

printListOfTuple :: [(String, String, String)] -> String
printListOfTuple [] = []
printListOfTuple (x:xs) = printTuple x ++ printListOfTuple xs

printTuple :: (String, String, String) -> String
printTuple (x, y, z) = x ++ y ++ z
