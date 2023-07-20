data MarkdownElem =
    Text String
    | Heading Int MarkdownElem
    | Paragraph MarkdownElem
    | Bold MarkdownElem
    | Italic MarkdownElem
    | BoldAndItalic MarkdownElem
    | Blockquote MarkdownElem
    | OrderedList [MarkdownElem]
    | UnorderedList [MarkdownElem]
    | Code MarkdownElem
    | CodeBlock [MarkdownElem]
    | HorizontalRule
    | Link MarkdownElem String
    | LinkTitle MarkdownElem String String
    | Image String String String


data Markdown = Markdown [MarkdownElem]

parseMd :: String -> Markdown
parseMd = undefined

renderAsHtml :: Markdown -> String
renderAsHtml = undefined

main :: IO ()
main = do
    filename <- getLine
    text <- readFile filename
    let res = renderAsHtml $ parseMd text
    putStrLn res
    pure ()


