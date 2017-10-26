----------------------------------------------------------------------
-- INF2160 - Paradigmes de programmation
-- TP1
-- Fait par Jean-Lou Hallée (HALJ05129309) et Alexis Millette (MILA24099409)
--
-- Commentaires généraux:
-- Pour le input, nous avons déduit qu'il doit au moins avoir un espace entre chaque "mot".
--
----------------------------------------------------------------------

--Map contenant les premieres consonnes
firstConsonant = [(0,"g"),(1,"kk"),(2,"n"),(3,"d"),(4,"tt"),(5,"r"),(6,"m"),(7,"b"),(8,"pp"),(9,"s"),(10,"ss"),(11,""),(12,"j"),(13,"jj"),(14,"ch"),(15,"k"),(16,"t"),(17,"p"),(18,"h")]
--Map contenant les voyelles
voyels = [(0,"a"),(1,"ae"),(2,"ya"),(3,"yae"),(4,"eo"),(5,"e"),(6,"yeo"),(7,"ye"),(8,"o"),(9,"wa"),(10,"wae"),(11,"oe"),(12,"yo"),(13,"u"),(14,"wo"),(15,"we"),(16,"wi"),(17,"yu"),(18,"eu"),(19,"ui"),(20,"i")]
--Map contenant les dernieres consonnes
secondConsonant = [(0,""),(1,"k"),(2,"k"),(3,"kt"),(4,"n"),(5,"nt"),(6,"nh"),(7,"t"),(8,"l"),(9,"lk"),(10,"lm"),(11,"lp"),(12,"lt"),(13,"lt"),(14,"lp"),(15,"lh"),(16,"m"),(17,"p"),(18,"pt"),(19,"t"),(20,"t"),(21,"ng"),(22,"t"),(23,"t"),(24,"k"),(25,"t"),(26,"p"),(27,"h")]
--Map de lettre et de map, qui eux contiennent le numero d'une lettre et la lettre a changer
replaceList = [(1,[(11,"g"),(2,"ngn"),(5,"ngn"),(6,"ngm"),(15,"k-k")]),(4,[(0,"n-g"),(5,"ll")]),(7,[(11,"d"),(2,"nn"),(5,"nn"),(6,"nm"),(16,"t-t")]),(8,[(11,"r"),(2,"ll"),(5,"ll")]),(16,[(5,"mn")]),(17,[(11,"b"),(2,"mn"),(5,"mn"),(6,"mm"),(17,"p-p")]),(19,[(11,"s"),(2,"nn"),(5,"nn"),(6,"nm"),(16,"t-t")]),(21,[(11,"ng-"),(5,"ngn")]),(22,[(11,"J"),(2,"nn"),(5,"nn"),(6,"nm")]),(23,[(11,"ch"),(2,"nn"),(5,"nn"),(6,"nm"),(16,"t-t")]),(25,[(2,"nn"),(5,"nn"),(6,"nm"),(16,"t-t")]),(27,[(11,"h"),(0,"k"),(2,"nn"),(3,"t"),(5,"nn"),(6,"nm"),(7,"p"),(9,"hs"),(12,"ch"),(18,"t")])]

main = do
        a <-getLine;
        putStrLn(work a)

-- Argument 1: Prend un String en paramètre (le String à traduire)
-- Enchaîne les appels de fonction permettant de traduire
work = cleanInput>.>createJamosList>.>romanisation>.>correctSentence>.>printListOfListWithSeperator

--Fonction permettant d'inverser le sens de l'évaluation partielle. Provient des notes de cours de Bruno Malenfant.
(>.>) :: (a->b) -> (b->c) -> (a->c)
g >.> f = f.g

-- Argument 1: Prend un String (tableau de caractères) en paramètre
--
-- Fonction qui permet d'effectuer toutes les opérations nécéssaires pour la validation de l'entrée de l'utilisateur
-- et retourne une liste de nombres entier
cleanInput :: [Char] -> [[Int]]
cleanInput = seperateIntoSentences>.>seperateIntoWords>.>cleanString>.>validateAllNumbers

-- Argument 1: Prend une liste de String en paramètre
--
-- Permet de valider si tous les Strings contenu dans une liste sont des nombres
-- et retourne une liste de nombres entier
validateAllNumbers :: [[[Char]]] -> [[Int]]
validateAllNumbers xs = (map.map) validateNumber xs

-- Argument 1: Prend un String en paramètre
--
-- Permet de valider si un String nombres
-- et retourne le nombre correspondant s'il est valide
validateNumber :: [Char] -> Int
validateNumber x
    | isNumber x = (read x :: Int)
    | otherwise = error "Unicode numbers must only contain digits..."

-- Argument 1: Prend un String en paramètre
--
-- Fonction permettant de détecter si tous les caractères dans un string sont des chiffres
-- et retourne vrai si la condition est respectée
isNumber :: [Char] -> Bool
isNumber [] = True
isNumber (x:xs)
    | elem x ['1','2','3','4','5','6','7','8','9','0'] = isNumber xs
    | otherwise = False

--Argument 1: Prend un string en paramètre
--
-- Sépare le texte entre les caractères '.' (construit une liste de phrases)
-- et retourne une liste de string correspondant au texte séparé
seperateIntoSentences :: [Char] -> [[Char]]
seperateIntoSentences = divideChain ['.']

-- Argument 1: Prend une liste de String en paramètre
--
-- Permet de séparer des string dans listes dans une liste
-- et retourne une liste avec des listes de string
seperateIntoWords :: [[Char]] -> [[[Char]]]
seperateIntoWords xs = map (divideChain [' ']) xs

-- Argument 1: Prend une liste de listes de string
-- Permet d'extraire seulement les nombres d'une liste de listes
-- et retourne cette liste de listes nettoyée
cleanString :: [[[Char]]] -> [[[Char]]]
cleanString xs = (map.map) verifyWord xs

-- Argument 1: Prend un string en paramètre
--
-- Permet de valider le format spécifique "&#xxxxx;"
-- et retourne le string sans les caractères "&#;"
verifyWord :: [Char] -> [Char]
verifyWord x
    | (take 2 x) == "&#" && (last x) == ';' = filter (not . (`elem` "#&;")) x
    | otherwise = error "Wrong input..."

-- Argument 1: Prend une liste de nombre entier en paramètre
--
-- Permet de créer la liste de jamos à partir de la liste de nombres unicodes
-- et retourne une liste de listes de tuples (liste de listes de jamos)
createJamosList :: [[Int]] -> [[(Int, Int, Int)]]
createJamosList [] = []
createJamosList xs = (map.map) createJamosFromUnicode xs

-- Argument 1: Prend un nombre entier en paramètre
--
-- Permet de transformer un nombre unicode en jamos
-- et retourne un tuple de nombres entiers (jamos)
createJamosFromUnicode :: Int -> (Int, Int, Int)
createJamosFromUnicode x = (ci, v, cf)
    where   ci = (x - 44032) `div` 588
            v = ((x - 44032) `mod` 588) `div` 28
            cf = ((x - 44032) `mod` 588) `mod` 28

-- Argument 1: Un string (liste de caractères étant les séparateurs)
-- Argument 2: Un string que l'on doit diviser
--
-- Permet de séparer un string en une liste de String par un séparateur
-- et retourne retourne une liste de string correspondant au texte séparé
divideChain :: [Char] -> [Char] -> [[Char]]
divideChain dividers cs =
    case dropWhile ((flip elem) dividers) cs of
    "" -> []
    cs' -> e : divideChain dividers cs''
        where (e,cs'') = break ((flip elem) dividers) cs'

-- Argument 1: Une liste de listes de tuples contenant des nombres entiers
-- Permet de transformer les jamos en lettres (romanisation)
romanisation :: [[(Int, Int, Int)]] -> [[((Int, String), (Int, String), (Int, String))]]
romanisation xs = (map.map) (\(x, y, z)  -> (myLookup x firstConsonant, myLookup y voyels, myLookup z secondConsonant)) xs

-- Argument 1: Une value clé que l'on doit chercher
-- Argument 2: Une liste de tuples
--
-- Permet d'aller chercher un élément dans une liste (key: value), et qu'une erreur soit retournée si l'élément n'est pas trouvé
-- si l'element est trouvé, elle retourne le tuple correspondant à la key
myLookup :: Eq a => a -> [(a, b)] -> (a,b)
myLookup key [] = error "A wrong unicode number was found..."
myLookup key ((x, y):list) =
    if key == x
       then (key,y)
       else myLookup key list

-- Argument 1: Liste de liste de tuple de tuples (Int, String) représentant le texte divisé en phrases
--
-- Cette fonction prend la liste de phrase et retourne la liste de phrase transformée selon le tableau de conversion
correctSentence :: [[((Int, String), (Int, String), (Int, String))]] -> [[(String, String, String)]]
correctSentence [] = []
correctSentence xs = map (\(list) -> (transformHangeul list)) xs

-- Argument 1: Liste de tuple de tuples (Int, String) représentant une phrase
--
-- Cette fonction prend la phrase en parametre et tourne la phrase sous forme de liste de tupe de String,
-- avec les mots transformés selon le tableau de de conversion
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

-- Argument 1: Tuple de tuple de (Int, String) représentant un mot
-- Argument 2: Tuple de tuple de (Int, String) représentant le mot suivant
-- Argument 3: Liste de tuple de (Int, [(Int,String)]), représentant le tableau de conversion
--
-- Cette fonction passe au travers du tableau de conversion et compare la premiere valeur du tuple de la liste, à la 1e valeur
-- du 3e tuple du 1er argument, s'il trouve une correspondantce, il appelle findMatch pour y trouver la lettre et retourne
-- un tuple de String avec les 2 premieres valeurs du 1er argument et le string trouve par findMatch comme 3e valeur.
replaceLetter :: ((Int, String), (Int, String), (Int, String)) -> ((Int, String), (Int, String), (Int, String)) -> [(Int,[(Int, String)])] -> (String, String, String)
replaceLetter word1 word2 [] = tupleToTupleString word1
replaceLetter (first1, second1, third1) (first2, second2, third2) ((number, letterList):list) =
    if fst third1 == number
        then (snd first1, snd second1, (findMatch third1 first2 letterList))
        else replaceLetter (first1, second1, third1) (first2, second2, third2) list

-- Argument 1: Un tuple qui représente la derniere lettre d'un mot
-- Argument 2: Un tuple qui représente la 1e lettre du mot suivant
-- Argument 3: Une liste de tuple (Int, String)
--
-- La fonction passe à travers la liste de tuple et vérifie si un tuple correspond au tuple du 2e arguement, s'il le trouve,
-- il retourne le String du tuple qui correspond dans la liste.
-- S'il n'en trouve pas, il retourne le string du tuple du 1er arguement
findMatch :: (Int, String) -> (Int, String) -> [(Int, String)] -> String
findMatch third _ [] = snd third
findMatch third first ((number, letter):list) =
    if fst first == number
        then letter
        else findMatch third first list

-- Argument 1: prend un tuple de (Int, String) en argument
-- Permet de retourner un tuple contenant seulement les strings de celui passé an argument
tupleToTupleString :: ((Int, String), (Int, String), (Int, String)) -> (String, String, String)
tupleToTupleString (first, second, third) = (snd first, snd second, snd third)

-- Argument 1: Prend une liste de liste de tuple de string en argument
--
-- Permet de retourner les strings concaténé avec les points
-- séparant les phrases.
printListOfListWithSeperator :: [[(String, String, String)]] -> String
printListOfListWithSeperator [] = []
printListOfListWithSeperator (x:xs) = printListOfTuple x ++ "." ++ printListOfListWithSeperator xs

-- Argument 1: Prend une liste de tuple de string en argument
--
-- Permet de retourner les strings concaténés
printListOfTuple :: [(String, String, String)] -> String
printListOfTuple [] = []
printListOfTuple (x:xs) = printTuple x ++ printListOfTuple xs

-- Argument 1: Prend un tuple de string en argument
--
-- Permet de retourner les strings concaténé
printTuple :: (String, String, String) -> String
printTuple (x, y, z) = x ++ y ++ z
