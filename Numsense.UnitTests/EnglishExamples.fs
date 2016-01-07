﻿module Ploeh.Numsense.EnglishExamples

open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData("zero"                                                                                   , 0)>]
[<InlineData(" zero"                                                                                  , 0)>]
[<InlineData("zero "                                                                                  , 0)>]
[<InlineData("  zero  "                                                                               , 0)>]
[<InlineData("Zero"                                                                                   , 0)>]
[<InlineData("ZERO"                                                                                   , 0)>]
[<InlineData(" zERo\t"                                                                                , 0)>]
[<InlineData("one"                                                                                    , 1)>]
[<InlineData("ONE"                                                                                    , 1)>]
[<InlineData("two"                                                                                    , 2)>]
[<InlineData(" two"                                                                                   , 2)>]
[<InlineData("three"                                                                                  , 3)>]
[<InlineData("three  "                                                                                , 3)>]
[<InlineData("four"                                                                                   , 4)>]
[<InlineData("  four "                                                                                , 4)>]
[<InlineData("five"                                                                                   , 5)>]
[<InlineData("FivE"                                                                                   , 5)>]
[<InlineData("six"                                                                                    , 6)>]
[<InlineData("  SIX  "                                                                                , 6)>]
[<InlineData("seven"                                                                                  , 7)>]
[<InlineData("    seVen"                                                                              , 7)>]
[<InlineData("eight"                                                                                  , 8)>]
[<InlineData("eIGHT"                                                                                  , 8)>]
[<InlineData("nine"                                                                                   , 9)>]
[<InlineData("NiNe  "                                                                                 , 9)>]
[<InlineData("ten"                                                                                    , 10)>]
[<InlineData("eleven"                                                                                 , 11)>]
[<InlineData("twelve"                                                                                 , 12)>]
[<InlineData("thirteen"                                                                               , 13)>]
[<InlineData("fourteen"                                                                               , 14)>]
[<InlineData("fifteen"                                                                                , 15)>]
[<InlineData("sixteen"                                                                                , 16)>]
[<InlineData("seventeen"                                                                              , 17)>]
[<InlineData("eighteen"                                                                               , 18)>]
[<InlineData("nineteen"                                                                               , 19)>]
[<InlineData("twenty"                                                                                 , 20)>]
[<InlineData("twentyone"                                                                              , 21)>]
[<InlineData("twenty-one"                                                                             , 21)>]
[<InlineData("thirty"                                                                                 , 30)>]
[<InlineData("thirtyfour"                                                                             , 34)>]
[<InlineData("thirty-eight"                                                                           , 38)>]
[<InlineData("forty"                                                                                  , 40)>]
[<InlineData("forty-one"                                                                              , 41)>]
[<InlineData("fortyfour"                                                                              , 44)>]
[<InlineData("fifty"                                                                                  , 50)>]
[<InlineData("fiftyseven"                                                                             , 57)>]
[<InlineData("sixty"                                                                                  , 60)>]
[<InlineData("sixtyfive"                                                                              , 65)>]
[<InlineData("seventy"                                                                                , 70)>]
[<InlineData("seventy-seven"                                                                          , 77)>]
[<InlineData("seventynine"                                                                            , 79)>]
[<InlineData("eighty"                                                                                 , 80)>]
[<InlineData("eightysix"                                                                              , 86)>]
[<InlineData("ninety"                                                                                 , 90)>]
[<InlineData("ninetythree"                                                                            , 93)>]
[<InlineData("hundred"                                                                                , 100)>]
[<InlineData("onehundred"                                                                             , 100)>]
[<InlineData("hundredone"                                                                             , 101)>]
[<InlineData("hundredandone"                                                                          , 101)>]
[<InlineData("hundred-and-one"                                                                        , 101)>]
[<InlineData("onehundredone"                                                                          , 101)>]
[<InlineData("onehundredandone"                                                                       , 101)>]
[<InlineData("one-hundred-and-one"                                                                    , 101)>]
[<InlineData("twohundred"                                                                             , 200)>]
[<InlineData("twohundredthirtyseven"                                                                  , 237)>]
[<InlineData("threehundredseventyfive"                                                                , 375)>]
[<InlineData("fourhundredninety"                                                                      , 490)>]
[<InlineData("fivehundredsixtythree"                                                                  , 563)>]
[<InlineData("sixhundredeighteen"                                                                     , 618)>]
[<InlineData("sevenhundredseventyseven"                                                               , 777)>]
[<InlineData("eighthundredfive"                                                                       , 805)>]
[<InlineData("eighthundredandsix"                                                                     , 806)>]
[<InlineData("eight-hundred-and-seven"                                                                , 807)>]
[<InlineData("ninehundredfiftynine"                                                                   , 959)>]
[<InlineData("thousand"                                                                               , 1000)>]
[<InlineData("onethousand"                                                                            , 1000)>]
[<InlineData("thousandone"                                                                            , 1001)>]
[<InlineData("onethousandtwo"                                                                         , 1002)>]
[<InlineData("onethousandsixtyfour"                                                                   , 1064)>]
[<InlineData("twothousand"                                                                            , 2000)>]
[<InlineData("eightthousandsevenhundredtwentyone"                                                     , 8721)>]
[<InlineData("nine-thousand-one-hundred-twenty-three"                                                 , 9123)>]
[<InlineData("tenthousand"                                                                            , 10000)>]
[<InlineData("tenthousandonehundredone"                                                               , 10101)>]
[<InlineData("ten-thousand-one-hundred-and-two"                                                       , 10102)>]
[<InlineData("twentythreethousandfivehundredsixtyfour"                                                , 23564)>]
[<InlineData("eightythousandten"                                                                      , 80010)>]
[<InlineData("hundredthousand"                                                                        , 100000)>]
[<InlineData("onehundredthousand"                                                                     , 100000)>]
[<InlineData("onehundredthousandone"                                                                  , 100001)>]
[<InlineData("threehundrednineteenthousandthreehundredfortynine"                                      , 319349)>]
[<InlineData("onemillion"                                                                             , 1000000)>]
[<InlineData("onemillionone"                                                                          , 1000001)>]
[<InlineData("twomillionten"                                                                          , 2000010)>]
[<InlineData("sixmillionthirtythousand"                                                               , 6030000)>]
[<InlineData("ninemilliontwohundredonethousandsixhundredeighty"                                       , 9201680)>]
[<InlineData("tenmillion"                                                                             , 10000000)>]
[<InlineData("tenmillionnine"                                                                         , 10000009)>]
[<InlineData("twentyonemillion"                                                                       , 21000000)>]
[<InlineData("fortysevenmillionsixhundredsixtyonethousandsixty"                                       , 47661060)>]
[<InlineData("hundredmillion"                                                                         , 100000000)>]
[<InlineData("onehundredmillion"                                                                      , 100000000)>]
[<InlineData("twohundredfivemillion"                                                                  , 205000000)>]
[<InlineData("three-hundred-and-seven-million"                                                        , 307000000)>]
[<InlineData("fourhundredsixtyonemillionsixtythousandsixhundred"                                      , 461060600)>]
[<InlineData("onebillion"                                                                             , 1000000000)>]
[<InlineData("twobillion"                                                                             , 2000000000)>]
[<InlineData("twobillionfortyninemillionsixhundredfiftythousand"                                      , 2049650000)>]
[<InlineData("twobilliononehundredfortysevenmillionfourhundredeightythreethousandsixhundredfortyseven",
             System.Int32.MaxValue)>]
let ``tryOfEnglish returns correct result`` (english, expected) =
    let actual = Numeral.tryParseEnglish english
    Some expected =! actual

[<Theory>]
[<InlineData(0                    , "zero")>]
[<InlineData(1                    , "one")>]
[<InlineData(2                    , "two")>]
[<InlineData(3                    , "three")>]
[<InlineData(4                    , "four")>]
[<InlineData(5                    , "five")>]
[<InlineData(6                    , "six")>]
[<InlineData(7                    , "seven")>]
[<InlineData(8                    , "eight")>]
[<InlineData(9                    , "nine")>]
[<InlineData(10                   , "ten")>]
[<InlineData(11                   , "eleven")>]
[<InlineData(12                   , "twelve")>]
[<InlineData(13                   , "thirteen")>]
[<InlineData(14                   , "fourteen")>]
[<InlineData(15                   , "fifteen")>]
[<InlineData(16                   , "sixteen")>]
[<InlineData(17                   , "seventeen")>]
[<InlineData(18                   , "eighteen")>]
[<InlineData(19                   , "nineteen")>]
[<InlineData(20                   , "twenty")>]
[<InlineData(21                   , "twenty-one")>]
[<InlineData(30                   , "thirty")>]
[<InlineData(34                   , "thirty-four")>]
[<InlineData(40                   , "forty")>]
[<InlineData(42                   , "forty-two")>]
[<InlineData(50                   , "fifty")>]
[<InlineData(58                   , "fifty-eight")>]
[<InlineData(60                   , "sixty")>]
[<InlineData(65                   , "sixty-five")>]
[<InlineData(70                   , "seventy")>]
[<InlineData(79                   , "seventy-nine")>]
[<InlineData(80                   , "eighty")>]
[<InlineData(86                   , "eighty-six")>]
[<InlineData(90                   , "ninety")>]
[<InlineData(93                   , "ninety-three")>]
[<InlineData(100                  , "one-hundred")>]
[<InlineData(101                  , "one-hundred-one")>]
[<InlineData(110                  , "one-hundred-ten")>]
[<InlineData(114                  , "one-hundred-fourteen")>]
[<InlineData(135                  , "one-hundred-thirty-five")>]
[<InlineData(200                  , "two-hundred")>]
[<InlineData(282                  , "two-hundred-eighty-two")>]
[<InlineData(331                  , "three-hundred-thirty-one")>]
[<InlineData(407                  , "four-hundred-seven")>]
[<InlineData(520                  , "five-hundred-twenty")>]
[<InlineData(666                  , "six-hundred-sixty-six")>]
[<InlineData(798                  , "seven-hundred-ninety-eight")>]
[<InlineData(857                  , "eight-hundred-fifty-seven")>]
[<InlineData(999                  , "nine-hundred-ninety-nine")>]
[<InlineData(1000                 , "one-thousand")>]
[<InlineData(1001                 , "one-thousand-one")>]
[<InlineData(1010                 , "one-thousand-ten")>]
[<InlineData(1066                 , "one-thousand-sixty-six")>]
[<InlineData(1337                 , "one-thousand-three-hundred-thirty-seven")>]
[<InlineData(1984                 , "one-thousand-nine-hundred-eighty-four")>]
[<InlineData(2015                 , "two-thousand-fifteen")>]
[<InlineData(3000                 , "three-thousand")>]
[<InlineData(3297                 , "three-thousand-two-hundred-ninety-seven")>]
[<InlineData(4080                 , "four-thousand-eighty")>]
[<InlineData(5011                 , "five-thousand-eleven")>]
[<InlineData(6025                 , "six-thousand-twenty-five")>]
[<InlineData(7441                 , "seven-thousand-four-hundred-forty-one")>]
[<InlineData(8513                 , "eight-thousand-five-hundred-thirteen")>]
[<InlineData(9000                 , "nine-thousand")>]
[<InlineData(10000                , "ten-thousand")>]
[<InlineData(12001                , "twelve-thousand-one")>]
[<InlineData(23456                , "twenty-three-thousand-four-hundred-fifty-six")>]
[<InlineData(32109                , "thirty-two-thousand-one-hundred-nine")>]
[<InlineData(40404                , "forty-thousand-four-hundred-four")>]
[<InlineData(56789                , "fifty-six-thousand-seven-hundred-eighty-nine")>]
[<InlineData(60015                , "sixty-thousand-fifteen")>]
[<InlineData(71003                , "seventy-one-thousand-three")>]
[<InlineData(80522                , "eighty-thousand-five-hundred-twenty-two")>]
[<InlineData(98765                , "ninety-eight-thousand-seven-hundred-sixty-five")>]
[<InlineData(100000               , "one-hundred-thousand")>]
[<InlineData(100001               , "one-hundred-thousand-one")>]
[<InlineData(100010               , "one-hundred-thousand-ten")>]
[<InlineData(101010               , "one-hundred-one-thousand-ten")>]
[<InlineData(200000               , "two-hundred-thousand")>]
[<InlineData(321000               , "three-hundred-twenty-one-thousand")>]
[<InlineData(411416               , "four-hundred-eleven-thousand-four-hundred-sixteen")>]
[<InlineData(530121               , "five-hundred-thirty-thousand-one-hundred-twenty-one")>]
[<InlineData(600000               , "six-hundred-thousand")>]
[<InlineData(788000               , "seven-hundred-eighty-eight-thousand")>]
[<InlineData(876540               , "eight-hundred-seventy-six-thousand-five-hundred-forty")>]
[<InlineData(908077               , "nine-hundred-eight-thousand-seventy-seven")>]
[<InlineData(1000000              , "one-million")>]
[<InlineData(2000002              , "two-million-two")>]
[<InlineData(3040506              , "three-million-forty-thousand-five-hundred-six")>]
[<InlineData(4321000              , "four-million-three-hundred-twenty-one-thousand")>]
[<InlineData(5004621              , "five-million-four-thousand-six-hundred-twenty-one")>]
[<InlineData(6982001              , "six-million-nine-hundred-eighty-two-thousand-one")>]
[<InlineData(7000000              , "seven-million")>]
[<InlineData(8000220              , "eight-million-two-hundred-twenty")>]
[<InlineData(9099000              , "nine-million-ninety-nine-thousand")>]
[<InlineData(10000000             , "ten-million")>]
[<InlineData(24000000             , "twenty-four-million")>]
[<InlineData(39020011             , "thirty-nine-million-twenty-thousand-eleven")>]
[<InlineData(40606100             , "forty-million-six-hundred-six-thousand-one-hundred")>]
[<InlineData(53000000             , "fifty-three-million")>]
[<InlineData(64000098             , "sixty-four-million-ninety-eight")>]
[<InlineData(70003190             , "seventy-million-three-thousand-one-hundred-ninety")>]
[<InlineData(80000000             , "eighty-million")>]
[<InlineData(99000099             , "ninety-nine-million-ninety-nine")>]
[<InlineData(100000000            , "one-hundred-million")>]
[<InlineData(209000000            , "two-hundred-nine-million")>]
[<InlineData(398000000            , "three-hundred-ninety-eight-million")>]
[<InlineData(439011000            , "four-hundred-thirty-nine-million-eleven-thousand")>]
[<InlineData(560400000            , "five-hundred-sixty-million-four-hundred-thousand")>]
[<InlineData(600010900            , "six-hundred-million-ten-thousand-nine-hundred")>]
[<InlineData(700000000            , "seven-hundred-million")>]
[<InlineData(800116000            , "eight-hundred-million-one-hundred-sixteen-thousand")>]
[<InlineData(900800007            , "nine-hundred-million-eight-hundred-thousand-seven")>]
[<InlineData(1000000000           , "one-billion")>]
[<InlineData(2121000000           , "two-billion-one-hundred-twenty-one-million")>]
[<InlineData(System.Int32.MaxValue,
             "two-billion-one-hundred-forty-seven-million-four-hundred-eighty-three-thousand-six-hundred-forty-seven")>]
let ``toEnglish returns correct result`` (i, expected) =
    let actual = Numeral.toEnglish i
    expected =! actual