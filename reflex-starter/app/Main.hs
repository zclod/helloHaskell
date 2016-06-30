{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Dom
import qualified Data.Map as Map
import Safe (readMay)
import Control.Applicative ((<*>), (<$>))

main :: IO ()
main = mainWidget $ el "div" $ do
  nx <- numberInput
  d <- dropdown "*" (constDyn ops) def
  ny <- numberInput
  values <- combineDyn (,) nx ny
  result <- combineDyn (\o (x,y) -> stringToOp o <$> x <*> y) (_dropdown_value d) values
  resultString <- mapDyn show result
  text " = "
  dynText resultString

numberInput :: MonadWidget t m => m (Dynamic t (Maybe Double))
numberInput = do
  let errorState = Map.singleton "style" "border-color: red"
      validState = Map.singleton "style" "border-color: green"
  rec n <- textInput $ def & textInputConfig_inputType .~ "number"
                           & textInputConfig_initialValue .~ "0"
                           & textInputConfig_attributes .~ attrs
      result <- mapDyn readMay $ _textInput_value n
      attrs <- mapDyn (\r -> case r of
                                  Just _ -> validState
                                  Nothing -> errorState) result
  return result

ops = Map.fromList [("+", "+"), ("-", "-"), ("*", "*"), ("/", "/")]

stringToOp s = case s of
                    "-" -> (-)
                    "*" -> (*)
                    "/" -> (/)
                    _ -> (+)
