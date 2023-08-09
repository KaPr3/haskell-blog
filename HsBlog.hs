import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Error
import Text.Parsec.Pos

import Data.Functor.Identity

{-
data MarkdownElem =
    Text String
    | Headings [(Int, String)] --
    | Paragraph [MarkdownElem] --
    | Bold MarkdownElem
    | Italic MarkdownElem
    | BoldAndItalic MarkdownElem
    | Strikethrough MarkdownElem
    | Blockquote [MarkdownElem] --
    | OrderedList [MarkdownElem] --
    | UnorderedList [MarkdownElem] --
    | OrderedListNum [MarkdownElem] Int --
    | UnorderedListNum [MarkdownElem] Int --
    | Code MarkdownElem
    | CodeBlock [MarkdownElem] --
    | HorizontalRule
    | Link MarkdownElem String
    | LinkTitle MarkdownElem String String
    | Image String String String
    deriving (Show)
-}

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
    deriving (Eq, Show)    

data Element =
    Text String
    | ListElem Tag [Element]
    | SingleElem Tag
    | NumberElem Tag [Element] Int
    | RepeatedApplication Tag [Element] Int
    | String1Elem Tag [Element] String
    | String2Elem Tag [Element] String String
    | String3 Tag String String String
    deriving (Show)

data Markdown = Markdown [Element]
    deriving (Show)

{-
data MarkdownContext =
    EmptyC
    | ParagraphC [MarkdownElem]
    | BlockquoteC
-}

type ParseResult = Either ParseError Element

after :: Char -> String -> String
after c (x:xs) = if c == x then after c xs else x : xs
after _ [] = []

ltrim :: String -> String
ltrim = after ' '

count_indent :: String -> Int
count_indent xs = (num_spaces + 4 * num_tabs) `div` 4 where
    num_spaces = length $ filter (== ' ') xs
    num_tabs = length $ filter (== '\t') xs

escape :: String -> String
escape = concat . (map escapeChar)
    where
        escapeChar '<' = "&lt"
        escapeChar '>' = "&gt"
        escapeChar '&' = "&amp"
        escapeChar '"' = "&quot;"
        escapeChar '\'' = "&#39;"
        escapeChar x = [x]

connect :: Element -> Element -> ParseResult
connect a (Text "") = Right a
connect (Text "") a = Right a
connect (Text a) (Text b) = Right $ Text (a ++ b)
connect (ListElem tag1 xs) (ListElem tag2 ys) = 
    if tag1 == tag2
        then Right $ ListElem tag1 (xs ++ ys)
        else Left $ newErrorMessage (Message msg) (initialPos "Inside") where
            msg = "Can't connect different tags: " ++ (show tag1) ++ (show xs) ++ " : " ++ (show tag2) ++ (show ys)
connect (ListElem tag xs) (Text a) = Right $ ListElem tag (xs ++ [Text a])
connect (Text a) (ListElem tag xs) = Right $ ListElem tag ((Text a) : xs)
connect (NumberElem tag1 xs n) (NumberElem tag2 ys m) =
    if tag1 == tag2
        then Right $ ListElem tag1 [NumberElem tag1 xs n, NumberElem tag2 ys m]
        else Left $ newErrorMessage (Message msg) (initialPos "Inside") where
            msg = "Can't connect different tags: " ++ (show tag1) ++ (show xs) ++ " : " ++ (show tag2) ++ (show ys)
connect a (ListElem tag xs) = Right $ ListElem tag (a : xs)
connect (ListElem tag xs) a = Right $ ListElem tag (xs ++ [a])
{-
connect (Headings xs) (Headings ys) = Right $ Headings (xs ++ ys)
connect (Blockquote a) (Blockquote b) = Right $ Blockquote (a ++ b)
connect (OrderedList a) (OrderedList b) = Right $ OrderedList (a ++ b)
connect (UnorderedList a) (UnorderedList b) = Right $ UnorderedList (a ++ b)
connect (CodeBlock a) (CodeBlock b) = Right $ CodeBlock (a ++ b)
connect (Paragraph a) (Paragraph b) = Right $ Paragraph (a ++ b)

connect (Headings a) (Text b) = Right $ Headings (a ++ [(1, b)])
connect (Text a) (Headings b)  = Right $ Headings ((1, a) : b)
connect (Blockquote a) (Text b) = Right $ Blockquote (a ++ [Text b])
connect (Text a) (Blockquote b) = Right $ Blockquote ((Text a) : b)
connect (OrderedList a) (Text b) = Right $ OrderedList (a ++ [Text b])
connect (Text a) (OrderedList b) = Right $ OrderedList ((Text a) : b)
connect (UnorderedList a) (Text b) = Right $ UnorderedList (a ++ [Text b])
connect (Text a) (UnorderedList b) = Right $ UnorderedList ((Text a) : b)
connect (CodeBlock a) (Text b) = Right $ CodeBlock (a ++ [Text b])
connect (Text a) (CodeBlock b) = Right $ CodeBlock ((Text a) : b)
connect (Paragraph a) (Text b) = Right $ Paragraph (a ++ [Text b])
connect (Text a) (Paragraph b) = Right $ Paragraph ((Text a) : b)
-}
connect a b = Left $ newErrorMessage (Message msg) (initialPos "Inside") where
    msg = "Error when connecting parsed elements: " ++ (show a) ++ " : " ++ (show b)


connectE :: ParseResult -> ParseResult -> ParseResult
connectE (Left a) _ = Left a
connectE _ (Left a) = Left a
connectE (Right a) (Right b) = connect a b

headingParser :: Parser Element
headingParser = do
    hashes <- many1 $ char '#'
    spaces
    name <- many anyChar
    pure $ NumberElem Heading [Text name] (length hashes)

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
    pure $ RepeatedApplication OrderedList [Text text] (count_indent indentation)

uListParser :: Parser Element
uListParser = do
    indentation <- many $ choice [space, tab]
    spaces
    _ <- oneOf "-*+"
    spaces
    text <- many anyChar
    pure $ RepeatedApplication UnorderedList [Text text] (count_indent indentation)

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
    pure $ ListElem Paragraph $ map Text $ lines text

lineParser :: Parser Element
lineParser = ((try headingParser)
                <|> (try blockquoteParser)
                <|> (try horizontalParser)
                <|> (try oListParser)
                <|> (try uListParser)
                <|> (try codeBlockParser)
                <|> paragraphParser)

wordParser :: Parser Element
wordParser = undefined --TODO



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
applyTag (RepeatedApplication tag xs 0) = ListElem tag xs
applyTag (RepeatedApplication tag xs n) = ListElem tag [applyTag newelem]
    where newelem = RepeatedApplication tag xs (n - 1)
applyTag x = x

modifyElement :: Element -> Element
modifyElement (RepeatedApplication tag xs n) = modifyElement $ applyTag (RepeatedApplication tag xs n)
modifyElement (ListElem tag xs) = concatListElem $ ListElem tag xs
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
parseBlock = (fmap modifyElement) . (foldr connectE (Right (Text ""))) . (map parseLine) -- 

{-
parseWords :: MarkdownElem -> MarkdownElem
parseWords (String x) = parse wordParser ""
parseWords (Paragraph xs) = Paragraph $ map parseWords xs
parseWords (Bold x) = Bold $ parseWords x
parseWords (Italic x) = Italic $ parseWords x
parseWords (BoldAndItalic x) = BoldAndItalic $ parseWords x
parseWords (Strikethrough x) = Strikethrough $ parseWords x
parseWords (Blockquote xs) = Blockquote $ map parseWords xs
parseWords (OrderedList xs) = OrderedList $ map parseWords xs
parseWords (UnorderedList xs) = UnorderedList $ map parseWords xs
parseWords (Link x y) = Link (parseWords x) y
parseWords (LinkTitle x y z) = LinkTitle (parseWords x) y z
-}

parseWords x = x

parseLine :: String -> ParseResult
parseLine line = parseWords <$> modifyElement <$> parse lineParser "" line

renderAsHtml :: Markdown -> String
renderAsHtml (Markdown md) = htmlHead $ concat $ map renderElem md

htmlHead :: String -> String
htmlHead x = el "html" (el "head" "" ++ el "body" x)

el :: String -> String -> String
el e text = "<" ++ e ++ ">" ++ text ++ "</" ++ e ++ ">" ++ "\n"

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

renderElem :: Element -> String
renderElem (Text text) = text
renderElem (ListElem Code xs) = el "code" $ escape $ concat $ map ((++ "\n") . renderElem) xs
renderElem (ListElem tag xs) =
    if tag == OrderedList || tag == UnorderedList
        then el (renderTag tag) $ renderList xs
        else el (renderTag tag) $ concat $ map renderElem xs
--renderElem (ListElem tag xs) = el (renderTag tag) $ concat $ map renderElem xs
renderElem (SingleElem tag) = "<" ++ renderTag tag ++ ">"
renderElem (NumberElem tag xs n) = el t $ concat $ map renderElem xs where t = renderTag tag ++ show n
renderElem (RepeatedApplication tag xs n) = renderElem $ applyTag $ RepeatedApplication tag xs n
renderElem (String1Elem tag xs s1) = undefined -- TODO
renderElem (String2Elem tag xs s1 s2) = undefined -- TODO
renderElem (String3 tag s1 s2 s3) = undefined -- TODO

{-
renderElem (Headings xs) = concat (map renderHeading xs)
renderElem (Paragraph xs) = el "p" $ concat $ map renderElem xs
renderElem (Bold element) = el "b" $ renderElem element
renderElem (Italic element) = el "em" $ renderElem element
renderElem (BoldAndItalic element) = el "em" $ el "b" $ renderElem element
renderElem (Strikethrough element) = el "s" $ renderElem element
renderElem (Blockquote xs) = el "blockquote" $ concat $ map renderElem xs
renderElem (OrderedList xs) = el "ol" $ renderList xs
renderElem (UnorderedList xs) = el "ul" $ renderList xs
renderElem (CodeBlock xs) = el "pre" $ el "code" $ escape $ concat $ map ((++ "\n") . renderElem) xs
renderElem HorizontalRule = "<hr>"
-}

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
    print md -- TODO: remove later
    let res = convert text
    writeIfRight res "first.html"

