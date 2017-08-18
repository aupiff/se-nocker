{-# LANGUAGE OverloadedStrings #-}

import           Data.Default
import qualified Data.Text as T
import           Reflex.Dom
import           Nock

main :: IO ()
main = mainWidget $ do

    el "div" $
        text "Nock Interpreter"

    nockEvent <- el "div" $ do
        nockInput <- textInput def
        -- textInputGetEnter = keypress Enter
        let evalEvent = textInputGetEnter nockInput
            input = tagPromptlyDyn (_textInput_value nockInput) evalEvent
            interpResult :: Event (SpiderTimeline Global) T.Text
            interpResult = (T.pack . show . parseEvalNoun . T.unpack) <$> input

        return interpResult

    el "div" $ do
        interpText <- holdDyn "..." nockEvent
        dynText interpText

    return ()
