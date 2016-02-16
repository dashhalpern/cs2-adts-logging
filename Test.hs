-- This is how we import only a few definitions from Test.Tasty
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Log
import LogInstances

-- import everything *except* `main` from LogAnalysis
import LogAnalysis hiding (main)
new :: LogMessage
new = (LogMessage Warning 3 "hi1")
new2 :: LogMessage
new2 = (LogMessage Warning 4 "by1e")
new3 :: LogMessage
new3 = (LogMessage Warning 5 "hi2")
new4 :: LogMessage
new4 = (LogMessage Warning 6 "bye2")
new5 :: LogMessage
new5 = (LogMessage Warning 7 "hi3")
new6 :: LogMessage
new6 = (LogMessage Warning 8 "bye3")
new7 :: LogMessage
new7 = (LogMessage Warning 9 "hi4")
new8 :: LogMessage
new8 = (LogMessage Warning 10 "bye4")
testes :: TestTree
testes = testGroup "unit tests"
  [ testCase "parseMessage Info"
    ( parseMessage "I 6 Completed armadillo processing" @?=
      LogMessage Info 6 "Completed armadillo processing" )


    ,testCase "parseMessage error"
    ( parseMessage "E 34 6 Completed armadillo processing" @?=
      LogMessage (Error 34) 6 "Completed armadillo processing" )


    , testCase "parseMessage warning"
    ( parseMessage "W 6 Completed armadillo processing" @?=
      LogMessage Warning 6 "Completed armadillo processing" )

    , testCase "parseMessage unknown"
    ( parseMessage "wierd 6 Completed armadillo processing" @?=
      Unknown "wierd 6 Completed armadillo processing" )

    ,testCase "parseType warning"
    ( parseType "W 6 Completed armadillo processing" @?=
      (Just Warning," 6 Completed armadillo processing"))

    ,testCase "parseType Info"
    ( parseType "I 6 Completed armadillo processing" @?=
      (Just Info," 6 Completed armadillo processing"))


    ,testCase "parseType error"
    ( parseType "E 34 6 Completed armadillo processing" @?=
      (Just (Error 34), "6 Completed armadillo processing"))

    ,testCase "er"
    ( er " 34 6 Completed armadillo processing" @?=
      (Just (Error 34), "6 Completed armadillo processing"))

    ,testCase "stamp1"
    ( parsestamp (Just Warning, " 1 armadillo an warning enough") @?=
      (Just Warning, 1 ,"armadillo an warning enough"))

    ,testCase "stamp2"
    ( parsestamp (Just Warning," 16 armadillo an warning enough") @?=
      (Just Warning, 16 ,"armadillo an warning enough"))

    ,testCase "stamp3"
    ( parsestamp (Just Warning," 166 armadillo an warning enough") @?=
      (Just Warning, 166 ,"armadillo an warning enough"))


    ,testCase "get stamp"
    ( getstamp (LogMessage (Error 34) 6 "Completed armadillo processing") @?=
      6)

    ,testCase "insert unknown"
    ( insert (Unknown "saller stamp does") (Node Leaf (LogMessage (Error 34) 6 "Completed armadillo processing") Leaf) @?=
      Node Leaf (LogMessage (Error 34) 6 "Completed armadillo processing") Leaf)


   ,testCase "insert empty"
    ( insert new Leaf @?=
      Node Leaf new Leaf)


    ,testCase "insert left"
    ( insert new (Node Leaf new2 Leaf) @?=
      Node (Node Leaf new Leaf) new2 Leaf)

    ,testCase "insert right"
    ( insert new2 (Node Leaf new Leaf) @?=
      Node Leaf new (Node Leaf new2 Leaf))

    ,testCase "insert rightright"
    ( insert new3 (Node Leaf new (Node Leaf new2 Leaf)) @?=
      Node Leaf new (Node Leaf new2 (Node Leaf new3 Leaf)))

    ,testCase "insert rightleft"
    ( insert new2 (Node Leaf new (Node Leaf new3 Leaf)) @?=
      Node Leaf new (Node (Node Leaf new2 Leaf) new3 Leaf))
    
    ,testCase "inorder"
    ( inOrder (Node Leaf new (Node (Node (Node Leaf new2 Leaf) new3 (Node (Node Leaf new4 Leaf) new5 Leaf)) new6 (Node Leaf new7 (Node Leaf new8 Leaf)))) @?=
      [new, new2, new3, new4, new5, new6, new7, new8])

    ,testCase "inorderempty"
    ( inOrder Leaf @?=
      [])







    , testProperty "build sorted"
    (\msgList -> isSorted (inOrder (build msgList)))

    -- show :: Int -> String
    -- gives the String representation of an Int
    -- Use show to test your code to parse Ints

    -- Write a function that takes a MessageType, and makes a String
    -- with the same format as the log file:
    -- stringMessageType :: MessageType -> String
    -- Use this to test your code that parses MessageType

    -- Make another function that makes a String from a whole LogMessage
    -- stringLogMessage :: LogMessage -> String
    -- Use it to test parseMessage
  ]


main = defaultMain testes
