import Text.Read

--Map contenant les premieres consonnes
premieresConsonnes = [(0,"g"),(1,"kk"),(2,"n"),(3,"d"),(4,"tt"),(5,"r"),(6,"m"),(7,"b"),(8,"pp"),(9,"s"),(10,"ss"),(11,""),(12,"j"),(13,"jj"),(14,"ch"),(15,"k"),(16,"t"),(17,"p"),(18,"h")]
--Map contenant les voyelles
voyelles = [(0,"a"),(1,"ae"),(2,"ya"),(3,"yae"),(4,"eo"),(5,"e"),(6,"yeo"),(7,"ye"),(8,"o"),(9,"wa"),(10,"wae"),(11,"oe"),(12,"yo"),(13,"u"),(14,"wo"),(15,"we"),(16,"wi"),(17,"yu"),(18,"eu"),(19,"ui"),(20,"i")]
--Map contenant les dernieres consonnes
deuxiemeVoyelles = [(0,""),(1,"k"),(2,"k"),(3,"kt"),(4,"n"),(5,"nt"),(6,"nh"),(7,"t"),(8,"l"),(9,"lk"),(10,"lm"),(11,"lp"),(12,"lt"),(13,"lt"),(14,"lp"),(15,"lh"),(16,"m"),(17,"p"),(18,"pt"),(19,"t"),(20,"t"),(21,"ng"),(22,"t"),(23,"t"),(24,"k"),(25,"t"),(26,"p"),(27,"h")]

b :: [Char]
b = "&#44039; &#53844; &#46944;    &#49240; &#53364;  ."
--b = "&#46980; &#46980; &#46980; . &#46980; &#46980; &#46980; . &#46980; &#46980; &#46980; ."

main = do
        --a <-getLine;
        putStrLn(work b)

work = cleanInput>.>createJamosList>.>romanisation>.>printListOfListWithSeperator

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
    | (readMaybe x :: Maybe Int) == Nothing = error "Unicode numbers must only contain digits..."
    | otherwise = (read x :: Int)

--Sépare le texte entre les caractères '.' (construit une liste de phrases)
seperateIntoSentences :: [Char] -> [[Char]]
seperateIntoSentences = divideChain ['.']

seperateIntoWords :: [[Char]] -> [[[Char]]]
seperateIntoWords xs = map (divideChain [' ']) xs

--convertStringListToIntList :: [[Char]] -> [Int]
--convertStringListToIntList xs = map (read::[Char]->Int) xs

cleanString :: [[[Char]]] -> [[[Char]]]
cleanString xs = (map.map) verifyWord xs

verifyWord :: [Char] -> [Char]
verifyWord x
    | (take 2 x) == "&#" && (last x) == ';' = filter (not . (`elem` "#&;")) x
    | otherwise = error "Mauvais input..."

--turnDotsIntoNumbers :: [[Char]] -> [[Char]]
--turnDotsIntoNumbers [] = []
--turnDotsIntoNumbers xs = map (\x -> if x == "." then "9999" else x) xs

--createJamosList :: [Int] -> [(Int, Int, Int)]
--createJamosList [] = []
--createJamosList (x:xs) = createJamosFromUnicode x: createJamosList xs

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

romanisation :: [[(Int, Int, Int)]] -> [[(String, String, String)]]
romanisation xs = (map.map) (\(x, y, z)  -> (myLookup x premieresConsonnes, myLookup y voyelles, myLookup z deuxiemeVoyelles)) xs

myLookup :: Eq a => a -> [(a, b)] -> b
myLookup _ [] = error "A wrong unicode number was found..."
myLookup key ((x, y):list) =
    if key == x
       then y
       else myLookup key list

correctSentence :: [[(String, String, String)]] -> [[(String, String, String)]]
correctSentence [] = []

printListOfListWithSeperator :: [[(String, String, String)]] -> String
printListOfListWithSeperator [] = []
printListOfListWithSeperator (x:xs) = printListOfTuple x ++ "." ++ printListOfListWithSeperator xs

printListOfTuple :: [(String, String, String)] -> String
printListOfTuple [] = []
printListOfTuple (x:xs) = printTuple x ++ printListOfTuple xs

printTuple :: (String, String, String) -> String
printTuple (x, y, z) = x ++ y ++ z
