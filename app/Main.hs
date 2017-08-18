{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Monad.Fix
import           Data.Default
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Reflex.Dom
import           HNock

main :: IO ()
main = mainWidgetWithCss (encodeUtf8 css) content


content :: ( DomBuilder (SpiderTimeline Global) m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadHold  (SpiderTimeline Global) m
           , PostBuild (SpiderTimeline Global) m
           )
        => m ()
content = divClass "Content" $ do
    el "div" evalDiv
    el "div" $ do
        el "div" $ el "p" $
                 text "Press enter in text field to\
                     \ interpret aribitrary Nock. As an example, try `[5 4 0 1]`\
                     \ which increments the atom `5` by one or `[[0 9] 3 [0 1]]`\
                     \ which checks if `[0 9]` is a cell or an atom."

        el "table" $
            mapM_ (\(x, y) -> el "tr" $ do el "td" $ text x
                                           el "td" $ text y) nockRedexes

evalDiv :: ( DomBuilder (SpiderTimeline Global) m
           , DomBuilderSpace m ~ GhcjsDomSpace
           , MonadHold (SpiderTimeline Global) m
           , PostBuild (SpiderTimeline Global) m
           )
        => m ()

evalDiv = el "div" $ do

    el "p" $ text "Nock Interpreter"

    nockEvent <- el "div" $ do
        nockInput <- textInput def

        -- TODO: textInputGetEnter = keypress Enter
        let evalEvent = textInputGetEnter nockInput
            input = tagPromptlyDyn (_textInput_value nockInput) evalEvent
            interpResult :: Event (SpiderTimeline Global) T.Text
            interpResult = (T.pack . show . parseEvalNoun . T.unpack) <$> input

        return interpResult

    el "div" $ do
        interpText <- holdDyn "" nockEvent
        dynText interpText


nockRedexes :: [(T.Text, T.Text)]
nockRedexes = [("?[a b]", "0"),
               ("?a", "1"),
               ("+[a b]", "+[a b]"),
               ("+a", "1 + a"),
               ("=[a a]", "0"),
               ("=[a b]", "1"),
               ("=a", "=a"),
               ("", ""),
               ("/[1 a]", "a"),
               ("/[2 a b]", "a"),
               ("/[3 a b]", "b"),
               ("/[(a + a) b]", "/[2 /[a b]]"),
               ("/[(a + a + 1) b]", "/[3 /[a b]]"),
               ("/a", "/a"),
               ("", ""),
               ("*[a [b c] d]", "[*[a b c] *[a d]]"),
               ("", ""),
               ("*[a 0 b]", "/[b a]"),
               ("*[a 1 b]", "b"),
               ("*[a 2 b c]", "*[*[a b] *[a c]]"),
               ("*[a 3 b]", "?*[a b]"),
               ("*[a 4 b]", "+*[a b]"),
               ("*[a 5 b]", "=*[a b]"),
               ("", ""),
               ("*[a 6 b c d]", "*[a 2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]"),
               ("*[a 7 b c]", "*[a 2 b 1 c]"),
               ("*[a 8 b c]", "*[a 7 [[7 [0 1] b] 0 1] c]"),
               ("*[a 9 b c]", "*[a 7 c 2 [0 1] 0 b]"),
               ("*[a 10 [b c] d]", "*[a 8 c 7 [0 3] d]"),
               ("*[a 10 b c]", "*[a c]"),
               ("", ""),
               ("*a", "*a")]


css :: T.Text
css = " \
    \html,\
    \body {\
    \    margin: 0;\
    \    padding: 0;\
    \}\
    \\
    \button {\
    \    margin: 0;\
    \    padding: 0;\
    \    border: 0;\
    \    background: none;\
    \    font-size: 100%;\
    \    vertical-align: baseline;\
    \    font-family: inherit;\
    \    font-weight: inherit;\
    \    color: inherit;\
    \    -webkit-appearance: none;\
    \    appearance: none;\
    \    -webkit-font-smoothing: antialiased;\
    \    -moz-font-smoothing: antialiased;\
    \    font-smoothing: antialiased;\
    \}\
    \\
    \body {\
    \    font: 14px 'Helvetica Neue', Helvetica, Arial, sans-serif;\
    \    line-height: 1.4em;\
    \    background: #f5f5f5;\
    \    color: #4d4d4d;\
    \    min-width: 230px;\
    \    max-width: 550px;\
    \    margin: 0 auto;\
    \    -webkit-font-smoothing: antialiased;\
    \    -moz-font-smoothing: antialiased;\
    \    font-smoothing: antialiased;\
    \    font-weight: 300;\
    \}\
    \/*\
    \    Hack to remove background from Mobile Safari.\
    \    Can't use it globally since it destroys checkboxes in Firefox\
    \*/\
    \@media screen and (-webkit-min-device-pixel-ratio:0) {\
    \    .toggle-all,\
    \    .todo-list li .toggle {\
    \     background: none;\
    \    }\
    \\
    \    .todo-list li .toggle {\
    \       height: 40px;\
    \    }\
    \\
    \    .toggle-all {\
    \       -webkit-transform: rotate(90deg);\
    \       transform: rotate(90deg);\
    \       -webkit-appearance: none;\
    \       appearance: none;\
    \    }\
    \}\
    \\
    \@media (max-width: 430px) {\
    \    .footer {\
    \       height: 50px;\
    \    }\
    \\
    \    .filters {\
    \       bottom: 10px;\
    \    }\
    \}"
