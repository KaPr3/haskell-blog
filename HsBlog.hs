import GHC.Utils.Misc (count)

data MarkdownElem =
    Text String
    | Heading Int String
    | Paragraph [MarkdownElem]
    | Bold MarkdownElem
    | Italic MarkdownElem
    | BoldAndItalic MarkdownElem
    | Strikethrough MarkdownElem
    | Blockquote MarkdownElem
    | OrderedList [MarkdownElem]
    | UnorderedList [MarkdownElem]
    | Code MarkdownElem
    | CodeBlock [MarkdownElem]
    | HorizontalRule
    | Link MarkdownElem String
    | LinkTitle MarkdownElem String String
    | Image String String String
    deriving (Show)


data Markdown = Markdown [MarkdownElem]
    deriving (Show)

data MarkdownContext =
    Empty
    | ParagraphC [MarkdownElem]

after :: Char -> String -> String
after c (x:xs) = if c == x then after c xs else x : xs
after _ [] = []

ltrim :: String -> String
ltrim = after ' '

parseMd :: String -> Markdown
parseMd text = Markdown $ reverse $ parseMdState Empty (lines text) []

parseMdState :: MarkdownContext -> [String] -> [MarkdownElem] -> [MarkdownElem]
parseMdState Empty (('#':line):text) parsed = parseMdState Empty text ((Heading number trimmed) : parsed) where
    trimmed = ltrim $ after '#' line
    number = count (\c -> c == '#') line + 1
parseMdState Empty ("":text) parsed = parseMdState Empty text parsed
parseMdState Empty (line:text) parsed = parseMdState (ParagraphC [parseLine line]) text parsed
parseMdState (ParagraphC p) ("":text) parsed = parseMdState Empty text ((Paragraph p) : parsed)
parseMdState (ParagraphC p) (line:text) parsed = parseMdState (ParagraphC (p ++ [parseLine line])) text parsed
parseMdState (ParagraphC p) [] parsed =  (Paragraph p) : parsed
parseMdState _ [] parsed = parsed

parseLine :: String -> MarkdownElem
parseLine = Text -- TODO

renderAsHtml :: Markdown -> String
renderAsHtml (Markdown md) = concat $ map renderElem md

el :: String -> String -> String
el e text = "<" ++ e ++ ">" ++ text ++ "</" ++ e ++ ">"

renderElem :: MarkdownElem -> String
renderElem (Text text) = text
renderElem (Heading n text) = el ("h" ++ num) text where num = show n
renderElem (Paragraph xs) = el "p" $ concat $ map renderElem xs
renderElem (Bold element) = el "b" $ renderElem element
renderElem (Italic element) = el "em" $ renderElem element
renderElem (BoldAndItalic element) = el "em" $ el "b" $ renderElem element
renderElem (Strikethrough element) = el "strike" $ renderElem element -- TODO: lookup actual html

convert :: String -> String
convert = renderAsHtml . parseMd

main :: IO ()
main = do
    --filename <- getLine
    let filename = "example.md" -- TODO: remove later
    text <- readFile filename
    let md = parseMd text -- TODO: remove later
    print md -- TODO: remove later
    --let res = convert text
    --putStrLn res


