{-# LANGUAGE OverloadedStrings #-}

module PureTests (pureTests) where

import Control.Arrow
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Lifx

import Util

pureTests = testGroup "Pure Tests" $
    [ testCase "testRGB" testRGB
    , testCase "sceneIdTest" sceneIdTest
    , testCase "selectorTest" selectorTest
    , testCase "sceneIdSelectorTest" sceneIdSelectorTest
    , testCase "selectorsToTextErrorTest" selectorsToTextErrorTest
    , testCase "lifxIdTest" lifxIdTest
    , testCase "lifxIdByteStringErrorTest" lifxIdByteStringErrorTest
    , testCase "colorErrorTest" colorErrorTest
    , testCase "selectorErrorTest" selectorErrorTest
    ]

-- These expected values were obtained from the Validate Color endpoint:
-- http://api.developer.lifx.com/docs/validate-color
rgbTests :: [(T.Text, Color)]
rgbTests =
  [ ("#ffffff", HSBK 0 0 1 0)
  , ("#000000", HSBK 0 0 0 0)
  , ("rgb:0,0,0", HSBK 0 0 0.0 0)
  , ("rgb:0,0,128", HSBK 240.0 1.0 0.5019607843137255 0)
  , ("rgb:0,0,139", HSBK 240.0 1.0 0.5450980392156862 0)
  , ("rgb:0,0,205", HSBK 240.0 1.0 0.803921568627451 0)
  , ("rgb:0,0,255", HSBK 240.0 1.0 1.0 0)
  , ("rgb:0,100,0", HSBK 120.0 1.0 0.39215686274509803 0)
  , ("rgb:0,128,0", HSBK 120.0 1.0 0.5019607843137255 0)
  , ("rgb:0,128,128", HSBK 180.0 1.0 0.5019607843137255 0)
  , ("rgb:0,139,139", HSBK 180.0 1.0 0.5450980392156862 0)
  , ("rgb:0,191,255", HSBK 195.05882352941177 1.0 1.0 0)
  , ("rgb:0,206,209", HSBK 180.86124401913875 1.0 0.8196078431372549 0)
  , ("rgb:0,250,154", HSBK 156.96 1.0 0.9803921568627451 0)
  , ("rgb:0,255,0", HSBK 120.0 1.0 1.0 0)
  , ("rgb:0,255,127", HSBK 149.88235294117646 1.0 1.0 0)
  , ("rgb:0,255,255", HSBK 180.0 1.0 1.0 0)
  , ("rgb:100,149,237", HSBK 218.54014598540147 0.5780590717299579 0.9294117647058824 0)
  , ("rgb:102,205,170", HSBK 159.61165048543688 0.5024390243902439 0.803921568627451 0)
  , ("rgb:105,105,105", HSBK 0 0.0 0.4117647058823529 0)
  , ("rgb:106,90,205", HSBK 248.34782608695653 0.5609756097560975 0.803921568627451 0)
  , ("rgb:107,142,35", HSBK 79.62616822429906 0.7535211267605634 0.5568627450980392 0)
  , ("rgb:112,128,144", HSBK 210.0 0.2222222222222222 0.5647058823529412 0)
  , ("rgb:119,136,153", HSBK 210.0 0.22222222222222218 0.6 0)
  , ("rgb:123,104,238", HSBK 248.50746268656715 0.5630252100840336 0.9333333333333333 0)
  , ("rgb:124,252,0", HSBK 90.47619047619048 1.0 0.9882352941176471 0)
  , ("rgb:127,255,0", HSBK 90.11764705882354 1.0 1.0 0)
  , ("rgb:127,255,212", HSBK 159.84375 0.5019607843137255 1.0 0)
  , ("rgb:128,0,0", HSBK 0.0 1.0 0.5019607843137255 0)
  , ("rgb:128,0,128", HSBK 300.0 1.0 0.5019607843137255 0)
  , ("rgb:128,128,0", HSBK 60.0 1.0 0.5019607843137255 0)
  , ("rgb:128,128,128", HSBK 0 0.0 0.5019607843137255 0)
  , ("rgb:135,206,235", HSBK 197.4 0.42553191489361697 0.9215686274509803 0)
  , ("rgb:135,206,250", HSBK 202.95652173913044 0.45999999999999996 0.9803921568627451 0)
  , ("rgb:138,43,226", HSBK 271.1475409836066 0.8097345132743362 0.8862745098039215 0)
  , ("rgb:139,0,0", HSBK 0.0 1.0 0.5450980392156862 0)
  , ("rgb:139,0,139", HSBK 300.0 1.0 0.5450980392156862 0)
  , ("rgb:139,69,19", HSBK 24.999999999999996 0.8633093525179856 0.5450980392156862 0)
  , ("rgb:143,188,143", HSBK 120.0 0.23936170212765961 0.7372549019607844 0)
  , ("rgb:144,238,144", HSBK 120.0 0.39495798319327735 0.9333333333333333 0)
  , ("rgb:147,112,219", HSBK 259.6261682242991 0.4885844748858447 0.8588235294117647 0)
  , ("rgb:148,0,211", HSBK 282.08530805687207 1.0 0.8274509803921568 0)
  , ("rgb:152,251,152", HSBK 120.0 0.3944223107569721 0.984313725490196 0)
  , ("rgb:153,50,204", HSBK 280.12987012987014 0.7549019607843138 0.8 0)
  , ("rgb:154,205,50", HSBK 79.74193548387098 0.7560975609756098 0.803921568627451 0)
  , ("rgb:160,82,45", HSBK 19.30434782608696 0.7187499999999999 0.6274509803921569 0)
  , ("rgb:165,42,42", HSBK 0.0 0.7454545454545455 0.6470588235294118 0)
  , ("rgb:169,169,169", HSBK 0 0.0 0.6627450980392157 0)
  , ("rgb:173,216,230", HSBK 194.73684210526315 0.24782608695652172 0.9019607843137255 0)
  , ("rgb:173,255,47", HSBK 83.65384615384615 0.8156862745098039 1.0 0)
  , ("rgb:175,238,238", HSBK 180.0 0.2647058823529412 0.9333333333333333 0)
  , ("rgb:176,196,222", HSBK 213.91304347826087 0.20720720720720723 0.8705882352941177 0)
  , ("rgb:176,224,230", HSBK 186.66666666666666 0.2347826086956522 0.9019607843137255 0)
  , ("rgb:178,34,34", HSBK 0.0 0.8089887640449438 0.6980392156862745 0)
  , ("rgb:184,134,11", HSBK 42.65895953757225 0.9402173913043479 0.7215686274509804 0)
  , ("rgb:186,85,211", HSBK 288.0952380952381 0.5971563981042654 0.8274509803921568 0)
  , ("rgb:188,143,143", HSBK 0.0 0.23936170212765961 0.7372549019607844 0)
  , ("rgb:189,183,107", HSBK 55.609756097560975 0.4338624338624339 0.7411764705882353 0)
  , ("rgb:192,192,192", HSBK 0 0.0 0.7529411764705882 0)
  , ("rgb:199,21,133", HSBK 322.24719101123594 0.8944723618090452 0.7803921568627451 0)
  , ("rgb:205,133,63", HSBK 29.577464788732396 0.6926829268292682 0.803921568627451 0)
  , ("rgb:205,92,92", HSBK 0.0 0.551219512195122 0.803921568627451 0)
  , ("rgb:210,105,30", HSBK 24.999999999999996 0.8571428571428571 0.8235294117647058 0)
  , ("rgb:210,180,140", HSBK 34.2857142857143 0.33333333333333326 0.8235294117647058 0)
  , ("rgb:211,211,211", HSBK 0 0.0 0.8274509803921568 0)
  , ("rgb:216,191,216", HSBK 300.0 0.11574074074074073 0.8470588235294118 0)
  , ("rgb:218,112,214", HSBK 302.2641509433962 0.4862385321100917 0.8549019607843137 0)
  , ("rgb:218,165,32", HSBK 42.903225806451616 0.8532110091743119 0.8549019607843137 0)
  , ("rgb:219,112,147", HSBK 340.3738317757009 0.4885844748858447 0.8588235294117647 0)
  , ("rgb:220,20,60", HSBK 348.0 0.9090909090909092 0.8627450980392157 0)
  , ("rgb:220,220,220", HSBK 0 0.0 0.8627450980392157 0)
  , ("rgb:221,160,221", HSBK 300.0 0.27601809954751133 0.8666666666666667 0)
  , ("rgb:222,184,135", HSBK 33.79310344827586 0.3918918918918919 0.8705882352941177 0)
  , ("rgb:224,255,255", HSBK 180.0 0.1215686274509804 1.0 0)
  , ("rgb:230,230,250", HSBK 240.0 0.07999999999999995 0.9803921568627451 0)
  , ("rgb:233,150,122", HSBK 15.135135135135137 0.4763948497854077 0.9137254901960784 0)
  , ("rgb:238,130,238", HSBK 300.0 0.45378151260504207 0.9333333333333333 0)
  , ("rgb:238,232,170", HSBK 54.70588235294117 0.28571428571428575 0.9333333333333333 0)
  , ("rgb:240,128,128", HSBK 0.0 0.4666666666666667 0.9411764705882353 0)
  , ("rgb:240,230,140", HSBK 54.0 0.41666666666666663 0.9411764705882353 0)
  , ("rgb:240,248,255", HSBK 208.0 0.05882352941176472 1.0 0)
  , ("rgb:240,255,240", HSBK 120.0 0.05882352941176472 1.0 0)
  , ("rgb:240,255,255", HSBK 180.0 0.05882352941176472 1.0 0)
  , ("rgb:244,164,96", HSBK 27.56756756756757 0.6065573770491803 0.9568627450980393 0)
  , ("rgb:245,222,179", HSBK 39.09090909090909 0.2693877551020409 0.9607843137254902 0)
  , ("rgb:245,245,220", HSBK 60.0 0.10204081632653059 0.9607843137254902 0)
  , ("rgb:245,245,245", HSBK 0 0.0 0.9607843137254902 0)
  , ("rgb:245,255,250", HSBK 149.99999999999991 0.039215686274509776 1.0 0)
  , ("rgb:248,248,255", HSBK 240.0 0.027450980392156876 1.0 0)
  , ("rgb:25,25,112", HSBK 240.0 0.7767857142857143 0.4392156862745098 0)
  , ("rgb:250,128,114", HSBK 6.176470588235292 0.5439999999999999 0.9803921568627451 0)
  , ("rgb:250,235,215", HSBK 34.28571428571427 0.13999999999999996 0.9803921568627451 0)
  , ("rgb:250,240,230", HSBK 30.0 0.07999999999999995 0.9803921568627451 0)
  , ("rgb:250,250,210", HSBK 60.0 0.16 0.9803921568627451 0)
  , ("rgb:253,245,230", HSBK 39.130434782608695 0.09090909090909093 0.9921568627450981 0)
  , ("rgb:255,0,0", HSBK 0.0 1.0 1.0 0)
  , ("rgb:255,0,255", HSBK 300.0 1.0 1.0 0)
  , ("rgb:255,105,180", HSBK 330.0 0.5882352941176471 1.0 0)
  , ("rgb:255,127,80", HSBK 16.114285714285714 0.6862745098039216 1.0 0)
  , ("rgb:255,140,0", HSBK 32.94117647058824 1.0 1.0 0)
  , ("rgb:255,160,122", HSBK 17.142857142857142 0.5215686274509803 1.0 0)
  , ("rgb:255,165,0", HSBK 38.82352941176471 1.0 1.0 0)
  , ("rgb:255,182,193", HSBK 350.958904109589 0.28627450980392155 1.0 0)
  , ("rgb:255,192,203", HSBK 349.5238095238096 0.24705882352941178 1.0 0)
  , ("rgb:255,20,147", HSBK 327.5744680851064 0.9215686274509804 1.0 0)
  , ("rgb:255,215,0", HSBK 50.588235294117645 1.0 1.0 0)
  , ("rgb:255,218,185", HSBK 28.285714285714278 0.27450980392156865 1.0 0)
  , ("rgb:255,222,173", HSBK 35.853658536585364 0.32156862745098036 1.0 0)
  , ("rgb:255,228,181", HSBK 38.10810810810811 0.2901960784313725 1.0 0)
  , ("rgb:255,228,196", HSBK 32.54237288135594 0.2313725490196078 1.0 0)
  , ("rgb:255,228,225", HSBK 6.000000000000034 0.11764705882352944 1.0 0)
  , ("rgb:255,235,205", HSBK 35.99999999999998 0.196078431372549 1.0 0)
  , ("rgb:255,239,213", HSBK 37.14285714285714 0.16470588235294115 1.0 0)
  , ("rgb:255,240,245", HSBK 339.99999999999994 0.05882352941176472 1.0 0)
  , ("rgb:255,245,238", HSBK 24.705882352941195 0.06666666666666665 1.0 0)
  , ("rgb:255,248,220", HSBK 47.999999999999986 0.13725490196078427 1.0 0)
  , ("rgb:255,250,205", HSBK 53.999999999999986 0.196078431372549 1.0 0)
  , ("rgb:255,250,240", HSBK 39.999999999999964 0.05882352941176472 1.0 0)
  , ("rgb:255,250,250", HSBK 0.0 0.019607843137254943 1.0 0)
  , ("rgb:255,255,0", HSBK 60.0 1.0 1.0 0)
  , ("rgb:255,255,224", HSBK 60.0 0.1215686274509804 1.0 0)
  , ("rgb:255,255,240", HSBK 60.0 0.05882352941176472 1.0 0)
  , ("rgb:255,255,255", HSBK 0 0.0 1.0 0)
  , ("rgb:255,69,0", HSBK 16.235294117647058 1.0 1.0 0)
  , ("rgb:255,99,71", HSBK 9.130434782608695 0.7215686274509804 1.0 0)
  , ("rgb:30,144,255", HSBK 209.6 0.8823529411764706 1.0 0)
  , ("rgb:32,178,170", HSBK 176.71232876712327 0.8202247191011236 0.6980392156862745 0)
  , ("rgb:34,139,34", HSBK 120.0 0.7553956834532375 0.5450980392156862 0)
  , ("rgb:46,139,87", HSBK 146.45161290322582 0.6690647482014388 0.5450980392156862 0)
  , ("rgb:47,79,79", HSBK 180.0 0.4050632911392405 0.30980392156862746 0)
  , ("rgb:50,205,50", HSBK 120.0 0.7560975609756098 0.803921568627451 0)
  , ("rgb:60,179,113", HSBK 146.72268907563026 0.664804469273743 0.7019607843137254 0)
  , ("rgb:64,224,208", HSBK 174.0 0.7142857142857143 0.8784313725490196 0)
  , ("rgb:65,105,225", HSBK 225.0 0.7111111111111111 0.8823529411764706 0)
  , ("rgb:70,130,180", HSBK 207.27272727272728 0.611111111111111 0.7058823529411765 0)
  , ("rgb:72,209,204", HSBK 177.8102189781022 0.6555023923444976 0.8196078431372549 0)
  , ("rgb:72,61,139", HSBK 248.46153846153848 0.5611510791366905 0.5450980392156862 0)
  , ("rgb:75,0,130", HSBK 274.6153846153846 1.0 0.5098039215686274 0)
  , ("rgb:85,107,47", HSBK 82.0 0.5607476635514018 0.4196078431372549 0)
  , ("rgb:95,158,160", HSBK 181.84615384615384 0.40625 0.6274509803921569 0)
  ]

testRGB :: IO ()
testRGB =
  forM_ rgbTests $ \(txt, expected) -> do
    let actual = zeroColor `combineColors` (fromJust $ parseColor txt)
        zeroColor = justColor $ HSBK 0 0 0 0
    assertColorEqual (T.unpack txt) expected (definitelyColor actual)

colorErrorTest = do
  tst "rgb:0,0,ff"
  tst "brown"
  tst "#ddffgg"
  tst "#fffff"
  tst "#fffffff"
  tst "hue:120 saturation:1.0 brightness:0.5 kelbin:5000"
  tst "rgb:0x"

  where tst c = msg c $ parseColor (T.pack c)
        msg _ Nothing = return ()
        msg c (Just _ ) = assertFailure c

frt :: LifxId a => T.Text -> a
frt = fromRight . fromText

selectorTest :: IO ()
selectorTest = do
  let txt = "all,label:Banana,id:aabbccddeeff,group:Apple,location:Orange,"
            <> "group_id:00112233445566778899aabbccddeeff,"
            <> "location_id:defacedbadfacadedefacedbadfacade"
      sels = [ SelAll
             , SelLabel      $ frt "Banana"
             , SelDevId      $ frt "aabbccddeeff"
             , SelGroup      $ frt "Apple"
             , SelLocation   $ frt "Orange"
             , SelGroupId    $ frt "00112233445566778899aabbccddeeff"
             , SelLocationId $ frt "defacedbadfacadedefacedbadfacade"
             ]
  assertEqual "parseSelectors" (Just sels) (parseSelectors txt)
  assertEqual "selectorsToText" txt (fromRight' $ selectorsToText sels)

ethSidToText :: Either String SceneId -> T.Text
ethSidToText (Left x)  = T.pack x
ethSidToText (Right x) = toText x

sceneIdTest :: IO ()
sceneIdTest = do
  let txt = "23529942-e6da-11e5-b920-0050c2490048"
      txt' = ethSidToText $ fromText txt
  assertEqual "scene ids" txt txt'

maybeSelToText :: Maybe Selector -> T.Text
maybeSelToText Nothing = "Nothing"
maybeSelToText (Just x) = selectorToText x

sceneIdSelectorTest :: IO ()
sceneIdSelectorTest = do
  let txt = "scene_id:23529942-e6da-11e5-b920-0050c2490048"
      txt' = maybeSelToText $ parseSelector txt
  assertEqual "scene ids" txt txt'

selectorErrorTest = do
  tst "allow"
  tst "id:aabbccddeeff00"
  tst "id:bbccddeeffgg"
  tst "id:aabbccddeef"
  tst "group_id:0123456789"
  tst "group_id:whatever"
  tst "group_id:"
  tst "location_id:aaaaaaaaaaaaaaaa"
  tst "location_id:aaaaaaaaaaaaaaaaa"

  where tst s = msg s $ parseSelector (T.pack s)
        msg _ Nothing = return ()
        msg s (Just _ ) = assertFailure s

selectorsToTextErrorTest :: IO ()
selectorsToTextErrorTest = do
  let sels = [ SelLabel $ frt "Hello, World!" ]
      actual = selectorsToText sels
      expected = Left $ IllegalCharacter ','
  assertEqual "selectorsToTextErrorTest" actual expected

testLifxId :: LifxId a => String -> Int -> a -> IO ()
testLifxId msg len dummy = do
  let bs = B.take len "abcdefghijklmnopqrstuvwxyz123456"
      x = (fromRight $ fromByteString bs) `asTypeOf` dummy
      y = (fromRight $ fromText (toText x)) `asTypeOf` dummy
      bs' = toByteString y
  assertEqual msg bs bs'

testLifxIdByteStringError :: (LifxId a, Show a, Eq a) => String -> Int -> a -> IO ()
testLifxIdByteStringError msg len dummy = do
  let bs = "abc"
      x = fromByteString bs `asTypeOf` Right dummy
      expected = "when constructing " ++ msg ++ " from ByteString, expected "
                 ++ show len ++ " bytes, but got 3"
  assertEqual msg (Left expected) x

testLifxIdTextError :: (LifxId a, Show a, Eq a) => String -> Int -> a -> IO ()
testLifxIdTextError msg len dummy = do
  let txt = "aabbcc"
      x = fromText txt `asTypeOf` Right dummy
      expected = "when constructing " ++ msg ++ " from Text, expected "
                 ++ show len ++ " bytes, but got 3"
  assertEqual msg (Left expected) x
  let txt' = "aabbccd"
      x' = fromText txt' `asTypeOf` Right dummy
      expected' = "Got crud \"d\" after " ++ msg
  assertEqual msg (Left expected') x'
  let txt'' = "00xx"
      x'' = fromText txt'' `asTypeOf` Right dummy
      expected'' = "Got crud \"xx\" after " ++ msg
  assertEqual msg (Left expected'') x''

lifxIdTest :: IO ()
lifxIdTest = do
  testLifxId "DeviceId"     6 (undefined :: DeviceId)
  testLifxId "GroupId"     16 (undefined :: GroupId)
  testLifxId "LocationId"  16 (undefined :: LocationId)
  testLifxId "Label"       32 (undefined :: Label)
  testLifxId "AccessToken" 32 (undefined :: AccessToken)
  testLifxId "SceneId"     16 (undefined :: SceneId)

lifxIdByteStringErrorTest :: IO ()
lifxIdByteStringErrorTest = do
  testLifxId "DeviceId"     6 (undefined :: DeviceId)
  testLifxId "GroupId"     16 (undefined :: GroupId)
  testLifxId "LocationId"  16 (undefined :: LocationId)
  testLifxId "Label"       32 (undefined :: Label)
  testLifxId "AccessToken" 32 (undefined :: AccessToken)
  testLifxId "SceneId"     16 (undefined :: SceneId)
