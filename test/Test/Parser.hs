module Test.Parser where
import Text.Megaparsec
import Test.Tasty.HUnit
import Parser
import Lambda
import Test.Terms


unit_parser :: IO ()
unit_parser = do
  parseMaybe parseTerm "     x   " @?= Just t1
  parseMaybe parseTerm "   x   y   " @?= Just t2 
  parseMaybe parseTerm "\\x.x" @?= Just tId
  parseMaybe parseTerm "\\  x  .  x " @?= Just tId
  parseMaybe parseTerm "\\ x.\\y .x " @?= Just Lambda.true
  parseMaybe parseTerm "(\\ x.(\\y . (x) ) )" @?= Just Lambda.true
  parseMaybe parseTerm "((x))" @?= Just t1
  parseMaybe parseTerm "((x)" @?= Nothing
