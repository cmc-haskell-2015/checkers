{-|
Module      : Main-wx
Description : Модуль с точкой входа для gui версии приложения
License     : LGPLv3
-}
module Main( main ) where

import Control.Concurrent

import Graphics.UI.WX
import qualified Graphics.UI.WXCore as Wx
import Graphics.UI.WXCore.Layout
import Graphics.UI.WXCore.WxcTypes

import Kernel
import PlayerBase
import PlayerConsole
import DrawingBase
import DrawingConsole
import WxSuite
import Controller
import AIPlayer

-- | Точка входа для gui версии программы
main
  = start gui

-- | Точка входа для основного потока игры
runGame :: GameConfig -> Player -> Player -> [Drawing] -> IO ()
runGame cfg player1 player2 drawings = do
    winner <- run cfg player1 player2 drawings
    case winner of
      Winner color -> putStrLn $ (show color) ++ " player wins!"
      DrawBy color -> putStrLn $ "It's a trap for " ++ (show color) ++ " player!"
    return ()

-- | Список возможных игроков для gui
playerChoices = [ "Человек из консоли"
                , "Человек с мышкой"
                , "Компьютер" ]

-- | Список названий уровней сложностей ИИ для gui
aiLevels = [ "Глупый"
           , "Средний"
           , "Неплохой" ]

-- | Список возможных модификаций шашек
gameTypeChoices = [ "Русские"
                  , "Международные"
                  , "Английские"
                  , "Армянские"
                  , "Бразильские"
                  , "Канадские"
                  , "Поддавки" ]

-- | Точка входа для gui: построение интерфейса и создание всех событий
gui :: IO ()
gui
  = do
    f <- frame [ text := "Шашки крутяшки" ]
    mainWidget <- window f []
    set f [ layout := margin 10 $ fill $ stretch $ widget mainWidget ]
    welcomeWidget <- window mainWidget []

    gameType <- radioBox welcomeWidget Vertical gameTypeChoices
                                                [ text := "Тип игры" ]
    playersWidget <- window welcomeWidget []

    player1AIConfig <- window playersWidget []
    player1AILevel <- choice player1AIConfig [ items := aiLevels ]
    player1AIDepth <- spinCtrl player1AIConfig 0 5 []
    Wx.choiceSetSelection player1AILevel 1
    Wx.spinCtrlSetValue player1AIDepth 1
    Wx.windowSetLayout player1AIConfig
        (boxed "Настройки AI" $ grid 5 5 $ [ [ label "Интеллект", label "Дальновидность" ]
                                           , [ hfill $ widget player1AILevel
                                             , hfill $ widget player1AIDepth ] ])

    player1Combo <- choice playersWidget [ items := playerChoices ]
    Wx.choiceOnCommand player1Combo (onSelect (Wx.downcastWindow f)
                                              (Wx.downcastWindow player1AIConfig)
                                              (Wx.downcastChoice player1Combo))
    Wx.choiceSetSelection player1Combo 2

    player2AIConfig <- window playersWidget []
    player2AILevel <- choice player2AIConfig [ items := aiLevels ]
    player2AIDepth <- spinCtrl player2AIConfig 0 5 []
    Wx.choiceSetSelection player2AILevel 1
    Wx.spinCtrlSetValue player2AIDepth 1
    Wx.windowSetLayout player2AIConfig
        (boxed "Настройки AI" $ grid 5 5 $ [ [ label "Интеллект", label "Дальновидность" ]
                                           , [ hfill $ widget player2AILevel
                                             , hfill $ widget player2AIDepth ] ])
    player2Combo <- choice playersWidget [ items := playerChoices ]
    Wx.choiceSetSelection player2Combo 1
    Wx.choiceOnCommand player2Combo (onSelect (Wx.downcastWindow f)
                                              (Wx.downcastWindow player2AIConfig)
                                              (Wx.downcastChoice player2Combo))
    Wx.windowHide player2AIConfig

    Wx.windowSetLayout playersWidget
        (grid 5 5 $ [ [ alignCenter $ hfill $ label "Черные"
                      , fill $ stretch $ widget player1Combo ]
                    , [ alignCenter $ stretch $ label "                     "
                      , alignRight $ stretch $ widget player1AIConfig ]
                    , [ alignCenter $ hfill $ label "Белые"
                      , fill $ stretch $ widget player2Combo ]
                    , [ alignCenter $ hfill $ label ""
                      , alignRight $ stretch $ widget player2AIConfig ]])

    okBtn <- button welcomeWidget
        [ text := "OK",
          on command := (onOK (Wx.downcastWindow f)
                              (Wx.downcastWindow mainWidget)
                              (Wx.downcastWindow welcomeWidget)
                              (Wx.downcastRadioBox gameType)
                              (Wx.downcastChoice player1Combo)
                              (Wx.downcastChoice player1AILevel)
                              (Wx.downcastSpinCtrl player1AIDepth)
                              (Wx.downcastChoice player2Combo)
                              (Wx.downcastChoice player2AILevel)
                              (Wx.downcastSpinCtrl player2AIDepth)) ]
    Wx.windowSetLayout welcomeWidget (column 5 $ [ alignCenter $ fill $ stretch $ widget gameType
                                                 , boxed "Игроки" $ alignCenter $ fill $ stretch $ widget playersWidget
                                                 , alignRight $ hfill $ widget okBtn ])

    Wx.windowSetLayout mainWidget (alignCenter $ fill $ stretch $ widget welcomeWidget)
    Wx.windowReLayout f

    return ()
  where
    onClick :: Wx.Frame a -> IO ()
    onClick f
      = do
        close f
        return ()

    onSelect :: Wx.Window () -> Wx.Window () -> Wx.Choice () -> IO ()
    onSelect root w c = do
        selected <- Wx.choiceGetSelection c
        if selected == 2
          then Wx.windowShow w
          else Wx.windowHide w
        Wx.windowReLayout root
        return ()

    onOK :: Wx.Window() -> Wx.Window() -> Wx.Window() -> Wx.RadioBox() ->
            Wx.Choice() -> Wx.Choice() -> Wx.SpinCtrl() ->
            Wx.Choice() -> Wx.Choice() -> Wx.SpinCtrl() -> IO()
    onOK f mainWidget welcomeWidget gameType
         player1Combo player1AILevel player1AIDepth
         player2Combo player2AILevel player2AIDepth = do

        gameTypeVal <- Wx.radioBoxGetSelection gameType
        cfg <- case gameTypeVal of
                 0 -> return russianConfig
                 1 -> return internationalConfig
                 2 -> return englishConfig
                 3 -> return armenianConfig
                 4 -> return brazilianConfig
                 5 -> return kanadianConfig
                 6 -> return reversedConfig
                 otherwise -> return russianConfig

        player1ComboVal <- Wx.choiceGetSelection player1Combo
        player1AILevelVal <- (Wx.choiceGetSelection player1AILevel)
        player1AIDepthVal <- (Wx.spinCtrlGetValue player1AIDepth)

        player2ComboVal <- Wx.choiceGetSelection player2Combo
        player2AILevelVal <- (Wx.choiceGetSelection player2AILevel)
        player2AIDepthVal <- (Wx.spinCtrlGetValue player2AIDepth)

        Wx.windowDestroyChildren mainWidget
        (drawing, canvas) <- createWxDrawing mainWidget cfg
        player1 <- case player1ComboVal of
                     0 -> return createPlayerConsole
                     1 -> createWxPlayer canvas
                     2 -> return $ createAIPlayer (player1AILevelVal + 1)
                                                   player1AIDepthVal

        player2 <- case player2ComboVal of
                     0 -> return createPlayerConsole
                     1 -> createWxPlayer canvas
                     2 -> return $ createAIPlayer (player2AILevelVal + 1)
                                                  player2AIDepthVal
        forkIO $ runGame cfg player1 player2 [drawing, createDrawingConsole]
        Wx.windowReLayout f
        return ()
