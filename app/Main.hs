module Main where

import Lib

main :: IO ()
main = do
  mktexfile "template.tex" "config.yaml" "out.tex"
