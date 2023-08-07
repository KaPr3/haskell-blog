import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Error
import Text.Parsec.Pos

import Data.Functor.Identity

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


data Markdown = Markdown [MarkdownElem]
    deriving (Show)

data MarkdownContext =
    EmptyC
    | ParagraphC [MarkdownElem]
    | BlockquoteC

type ParseResult = Either ParseError MarkdownElem

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

connect :: MarkdownElem -> MarkdownElem -> ParseResult
connect a (Text "") = Right a
connect (Text "") a = Right a
connect (Text a) (Text b) = Right $ Text (a ++ b)
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
connect a b = Left $ newErrorMessage (Message msg) (initialPos "Inside") where
    msg = "Error when connecting parsed elements: " ++ (show a) ++ " : " ++ (show b)


connectE :: ParseResult -> ParseResult -> ParseResult
connectE (Left a) _ = Left a
connectE _ (Left a) = Left a
connectE (Right a) (Right b) = connect a b

headingParser :: Parser MarkdownElem
headingParser = do
    hashes <- many1 $ char '#'
    spaces
    name <- many anyChar
    pure $ Headings [((length hashes), name)]

blockquoteParser :: Parser MarkdownElem
blockquoteParser = do
    _ <- char '>'
    spaces
    text <- many anyChar
    pure $ Blockquote [Text text]

oListParser :: Parser MarkdownElem
oListParser = do
    indentation <- many $ choice [space, tab]
    _ <- digit
    _ <- char '.'
    spaces
    text <- many anyChar
    pure $ OrderedListNum [Text text] (count_indent indentation)

uListParser :: Parser MarkdownElem
uListParser = do
    indentation <- many $ choice [space, tab]
    spaces
    _ <- oneOf "-*+"
    spaces
    text <- many anyChar
    pure $ UnorderedListNum [Text text] (count_indent indentation)

codeBlockParser :: Parser MarkdownElem
codeBlockParser = do
    _ <- choice [count 4 space, count 1 tab]
    text <- many anyChar
    pure $ CodeBlock [Text text]

horizontalParser :: Parser MarkdownElem
horizontalParser = do
    _ <- count 3 (oneOf "*-_")
    _ <- many (oneOf "*-_")
    pure HorizontalRule

lineParser :: Parser MarkdownElem
lineParser = ((try headingParser)
                <|> (try blockquoteParser)
                <|> (try horizontalParser)
                <|> (try oListParser)
                <|> (try uListParser)
                <|> (try codeBlockParser)
                <|> paragraphParser)

wordParser :: Parser MarkdownElem
wordParser = undefined --TODO

allParser :: Parsec String MarkdownContext MarkdownElem
allParser = do
    char '>'
    spaces
    text <- many anyChar
    putState BlockquoteC
    pure $ Blockquote [Text text]

paragraphParser :: Parser MarkdownElem
paragraphParser = do
    text <- many anyChar
    pure $ Paragraph $ map Text $ lines text

concatListElem :: MarkdownElem -> [MarkdownElem] -> [MarkdownElem]
concatListElem (OrderedList xs) ((OrderedList ys) : zs) = (OrderedList (xs ++ ys)) : zs
concatListElem (UnorderedList xs) ((UnorderedList ys) : zs) = (UnorderedList (xs ++ ys)) : zs
concatListElem x xs = x : xs

concatHtmlList :: MarkdownElem -> MarkdownElem
concatHtmlList (OrderedList xs) = OrderedList $ map concatHtmlList $ foldr concatListElem [] xs
concatHtmlList (UnorderedList xs) = UnorderedList $ map concatHtmlList $ foldr concatListElem [] xs
concatHtmlList x = x

unpackListElem :: MarkdownElem -> MarkdownElem
unpackListElem (OrderedListNum xs 0) = OrderedList xs
unpackListElem (OrderedListNum xs n) = OrderedList [unpackListElem newelem] where newelem = OrderedListNum xs (n - 1)
unpackListElem (UnorderedListNum xs 0) = UnorderedList xs
unpackListElem (UnorderedListNum xs n) = UnorderedList [unpackListElem newelem] where newelem = UnorderedListNum xs (n - 1)
unpackListElem x = x

modifyElement :: MarkdownElem -> MarkdownElem
modifyElement (OrderedListNum xs n) = concatHtmlList $ unpackListElem (OrderedListNum xs n)
modifyElement (UnorderedListNum xs n) = concatHtmlList $ unpackListElem (UnorderedListNum xs n)
modifyElement (OrderedList xs) = concatHtmlList $ OrderedList xs
modifyElement (UnorderedList xs) = concatHtmlList $ UnorderedList xs
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

renderLi :: MarkdownElem -> String
renderLi (OrderedList xs) = renderElem $ OrderedList xs
renderLi (UnorderedList xs) = renderElem $ UnorderedList xs
renderLi x = (el "li" . renderElem) x

renderList :: [MarkdownElem] -> String
renderList xs = concat $ map renderLi xs

renderElem :: MarkdownElem -> String
renderElem (Text text) = text
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
    --print md -- TODO: remove later
    let res = convert text
    writeIfRight res "first.html"

