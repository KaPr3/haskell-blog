import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Error
import Text.Parsec.Pos

import Data.Char (isSpace)
import Data.List (dropWhileEnd, singleton)

import Control.Monad (join)

import Data.Functor.Identity

data Tag =
    Heading
    | Paragraph
    | Bold
    | Italic
    | Strikethrough
    | Blockquote
    | OrderedList
    | UnorderedList
    | Code
    | Preformatted
    | HorizontalRule
    | LineBreak
    | Link
    | Image
    | Container
    | Lined
    deriving (Eq, Show)    

data Element =
    Text String
    | Line String
    | ListElem Tag [Element]
    | SingleElem Tag
    | NumberElem Tag Int [Element]
    | RepeatedApplication Tag Int [Element]
    | String1Elem Tag String [Element]
    | String2Elem Tag String String [Element]
    | String3 Tag String String String
    deriving (Eq, Show)

data Markdown = Markdown [Element]
    deriving (Eq, Show)

type ParseResult = Either ParseError Element

after :: Char -> String -> String
after c (x:xs) = if c == x then after c xs else x : xs
after _ [] = []

ltrim :: String -> String
ltrim = after ' '

rtrim :: String -> String
rtrim = dropWhileEnd isSpace

trim :: String -> String
trim = ltrim . rtrim

count_indent :: String -> Int
count_indent xs = (num_spaces + 4 * num_tabs) `div` 4 where
    num_spaces = length $ filter (== ' ') xs
    num_tabs = length $ filter (== '\t') xs

endsWith :: String -> String -> Bool
endsWith [] [] = True
endsWith [] _ = False
endsWith (x:xs) y = ((x:xs) == y) || endsWith xs y

escape :: String -> String
escape = concat . (map escapeChar)
    where
        escapeChar '<' = "&lt"
        escapeChar '>' = "&gt"
        escapeChar '&' = "&amp"
        escapeChar '"' = "&quot;"
        escapeChar '\'' = "&#39;"
        escapeChar x = [x]

errorMessage :: String -> String -> ParseResult
errorMessage tag1 tag2 = Left $ newErrorMessage (Message msg) (initialPos "Inside") where
    msg = "Can't connect different tags: " ++ tag1 ++ " : " ++ tag2

connect :: Element -> Element -> ParseResult
connect a (Text "") = Right a
connect (Text "") a = Right a
connect (Text a) (Text b) = Right $ Text (a ++ b)
connect (Text a) (Line b) = Right $ Line (a ++ b)
connect (ListElem Lined xs) (ListElem Lined ys) = Right $ ListElem Container [ListElem Lined xs, ListElem Lined ys] 
connect (ListElem tag1 xs) (ListElem tag2 ys) = 
    if tag1 == tag2
        then Right $ ListElem tag1 (xs ++ ys)
        else errorMessage ((show tag1) ++ (show xs)) ((show tag2) ++ (show ys))
connect (ListElem tag xs) (Text a) = Right $ ListElem tag (xs ++ [Text a])
connect (Text a) (ListElem tag xs) = Right $ ListElem tag ((Text a) : xs)
connect (NumberElem tag1 n xs) (NumberElem tag2 m ys) =
    if tag1 == tag2
        then Right $ ListElem tag1 [NumberElem tag1 n xs, NumberElem tag2 m ys]
        else errorMessage ((show tag1) ++ (show xs)) ((show tag2) ++ (show ys))
connect a (ListElem tag xs) = Right $ ListElem tag (a : xs)
connect (ListElem tag xs) a = Right $ ListElem tag (xs ++ [a])
connect a b = Right $ ListElem Container [a, b]
--connect a b = Left $ newErrorMessage (Message msg) (initialPos "Inside") where
--    msg = "Error when connecting parsed elements: " ++ (show a) ++ " : " ++ (show b)

connectE :: ParseResult -> ParseResult -> ParseResult
connectE (Left a) _ = Left a
connectE _ (Left a) = Left a
connectE (Right a) (Right b) = connect a b

headingParser :: Parser Element
headingParser = do
    hashes <- many1 $ char '#'
    spaces
    name <- many anyChar
    pure $ NumberElem Heading (length hashes) [Text name] 

blockquoteParser :: Parser Element
blockquoteParser = do
    _ <- char '>'
    spaces
    text <- many anyChar
    pure $ ListElem Blockquote [Text text]

oListParser :: Parser Element
oListParser = do
    indentation <- many $ choice [space, tab]
    _ <- digit
    _ <- char '.'
    spaces
    text <- many anyChar
    pure $ RepeatedApplication OrderedList (count_indent indentation) [Text text]

uListParser :: Parser Element
uListParser = do
    indentation <- many $ choice [space, tab]
    spaces
    _ <- oneOf "-*+"
    spaces
    text <- many anyChar
    pure $ RepeatedApplication UnorderedList (count_indent indentation) [Text text]

codeBlockParser :: Parser Element
codeBlockParser = do
    _ <- choice [count 4 space, count 1 tab]
    text <- many anyChar
    pure $ ListElem Preformatted [ListElem Code [Text text]]

horizontalParser :: Parser Element
horizontalParser = do
    _ <- count 3 (oneOf "*-_")
    _ <- many (oneOf "*-_")
    pure $ SingleElem HorizontalRule

paragraphParser :: Parser Element
paragraphParser = do
    text <- many anyChar
    pure $ ListElem Paragraph [Line text]

lineParser :: Parser Element
lineParser = ((try headingParser)
                <|> (try blockquoteParser)
                <|> (try horizontalParser)
                <|> (try oListParser)
                <|> (try uListParser)
                <|> (try codeBlockParser)
                <|> paragraphParser)

boldParser :: Parser Element
boldParser = do
    text1 <- many (noneOf "*_")
    _ <- count 2 (oneOf "*_")
    text2 <- many (noneOf "*_")
    _ <- count 2 (oneOf "*_")
    text3 <- many (noneOf "*_")
    pure $ ListElem Container [Text text1, ListElem Bold [Text text2], Text text3]

allParser :: Parser Element
allParser = do
    text <- many anyChar
    pure $ Text text

wordParser :: Parser Element --TODO
wordParser = (try boldParser)
                <|> allParser

addBr :: Element -> [Element]
addBr (Text text) = if endsWith text "  " then [Text (trim text), SingleElem LineBreak] else [Text text]
addBr (ListElem tag xs) = [ListElem tag (concat $ map addBr xs)]
addBr x = [x]

concatMembers :: Element -> [Element] -> [Element]
concatMembers (ListElem tag1 xs) ((ListElem tag2 ys) : zs) =
    if tag1 == tag2
        then (ListElem tag1 (xs ++ ys)) : zs
        else (ListElem tag1 xs) : (ListElem tag2 ys) : zs
concatMembers x xs = x : xs

concatListElem :: Element -> Element
concatListElem (ListElem tag xs) = ListElem tag $ map concatListElem $ foldr concatMembers [] xs
concatListElem x = x

applyTag :: Element -> Element
applyTag (RepeatedApplication tag 0 xs) = ListElem tag xs
applyTag (RepeatedApplication tag n xs) = ListElem tag [applyTag newelem]
    where newelem = RepeatedApplication tag (n - 1) xs
applyTag x = x

modifyElement :: Element -> Element
modifyElement (RepeatedApplication tag n xs) = modifyElement $ applyTag (RepeatedApplication tag n xs)
modifyElement (ListElem Paragraph xs) = ListElem Paragraph $ concat $ map addBr xs
--modifyElement (ListElem tag xs) = concatListElem $ ListElem tag xs
modifyElement x = x

blocks :: [String] -> [[String]]
blocks texts = filter (not . null) $ reverse $ map reverse $ blocksRec [] texts

blocksRec :: [[String]] -> [String] -> [[String]]
blocksRec xs [] = xs
blocksRec xs ("":texts) = blocksRec ([] : xs) texts
blocksRec (x:xs) (text:texts) = blocksRec ((text : x) : xs) texts
blocksRec [] (text:texts) = blocksRec ([[text]]) texts

parseMd :: String -> Either ParseError Markdown
parseMd text = fmap Markdown (sequence $ map parseBlock (blocks $ lines text))

parseBlock :: [String] -> ParseResult
parseBlock = (fmap modifyElement) . (foldr connectE (Right (Text ""))) . (map parseLine)

parseList :: [Element] -> Either ParseError [Element]
parseList xs = sequence $ (map parseWords xs)

parseWords :: Element -> ParseResult
parseWords (Text text) = parse wordParser "" text
parseWords (Line text) = (ListElem Lined) <$> singleton <$> (parse wordParser "" text)
parseWords (ListElem tag xs) = (ListElem tag) <$> (parseList xs)
parseWords (SingleElem tag) = Right $ SingleElem tag
parseWords (NumberElem tag n xs) = (NumberElem tag n) <$> (parseList xs)
parseWords (RepeatedApplication tag n xs) = (RepeatedApplication tag n) <$> (parseList xs)
parseWords (String1Elem tag s xs) = (String1Elem tag s) <$> (parseList xs)
parseWords (String2Elem tag s1 s2 xs) = (String2Elem tag s1 s2) <$> (parseList xs)
parseWords (String3 tag s1 s2 s3) = Right $ String3 tag s1 s2 s3

parseLine :: String -> ParseResult
parseLine line = join $ parseWords <$> modifyElement <$> parse lineParser "" line

renderAsHtml :: Markdown -> String
renderAsHtml (Markdown md) = htmlHead $ concat $ map renderElem md

htmlHead :: String -> String
htmlHead x = el "html" (el "head" "" ++ el "body" x)

el :: String -> String -> String
el e text = "<" ++ e ++ ">" ++ text ++ "</" ++ e ++ ">"

renderHeading :: (Int, String) -> String
renderHeading (n, text) = el ("h" ++ num) text where num = show n

renderLi :: Element -> String
renderLi (ListElem OrderedList xs) = renderElem $ ListElem OrderedList xs
renderLi (ListElem UnorderedList xs) = renderElem $ ListElem UnorderedList xs
renderLi x = (el "li" . renderElem) x

renderList :: [Element] -> String
renderList xs = concat $ map renderLi xs

renderTag :: Tag -> String
renderTag Heading = "h"
renderTag Paragraph = "p"
renderTag Bold = "b"
renderTag Italic = "em"
renderTag Strikethrough = "s"
renderTag Blockquote = "blockquote"
renderTag OrderedList = "ol"
renderTag UnorderedList = "ul"
renderTag Code = "code"
renderTag Preformatted = "pre"
renderTag HorizontalRule = "hr"
renderTag LineBreak = "br"
renderTag Link = "a"
renderTag Image = "img"
renderTag Container = ""

renderElem :: Element -> String
renderElem (Text text) = text
renderElem (Line text) = text ++ "\n"
renderElem (ListElem Lined xs) = (concat $ map renderElem xs) ++ "\n"
renderElem (ListElem Container xs) = concat $ map renderElem xs
renderElem (ListElem Code xs) = el "code" $ escape $ concat $ map ((++ "\n") . renderElem) xs
renderElem (ListElem tag xs) =
    if tag == OrderedList || tag == UnorderedList
        then el (renderTag tag) $ renderList xs
        else el (renderTag tag) $ concat $ map renderElem xs
--renderElem (ListElem tag xs) = el (renderTag tag) $ concat $ map renderElem xs
renderElem (SingleElem tag) = "<" ++ renderTag tag ++ ">"
renderElem (NumberElem tag n xs) = el t $ concat $ map renderElem xs where t = renderTag tag ++ show n
renderElem (RepeatedApplication tag n xs) = renderElem $ applyTag $ RepeatedApplication tag n xs
renderElem (String1Elem tag s1 xs) = undefined -- TODO
renderElem (String2Elem tag s1 s2 xs) = undefined -- TODO
renderElem (String3 tag s1 s2 s3) = undefined -- TODO

convert :: String -> Either ParseError String
convert text = renderAsHtml <$> parseMd text

writeIfRight :: Show a => Either a String -> String -> IO ()
writeIfRight (Right result) file = writeFile file result >> putStrLn "Great success"
writeIfRight (Left x) _ = putStrLn "failed" >> print x

main :: IO ()
main = do
    --filename <- getLine
    let filename = "example copy.md" -- TODO: remove later
    text <- readFile filename
    let md = parseMd text -- TODO: remove later
    print (map (map parseLine) (blocks $ lines text))
    --print md -- TODO: remove later
    let res = convert text
    writeIfRight res "first.html"

