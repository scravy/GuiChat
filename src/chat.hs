{-# LANGUAGE Haskell2010 #-}

module Main where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Monad
import Data.Word
import Graphics.Gloss.Interface.IO.Game
import Network
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import GuiChat.Types
import GuiChat.Canvas
import GuiChat.EventHandling


main = withSocketsDo $ do

    -- Command line args processing
    (host, port) <- getArgs >>= \args -> case args of
        [host, port] -> do
            let portNumber = read port :: Word16
            return (host, PortNumber $ fromIntegral portNumber)
        _ -> do
            getProgName >>= printf "Usage: %s <host> <port>\n"
            exitFailure

    -- Internal communication and outside connection
    recv   <- newEmptyMVar
    send   <- newEmptyMVar
    handle <- connectTo host port
    hSetBuffering handle LineBuffering

    -- Receive loop
    forkIO $ forever $ do
        hIsEOF handle >>= \eof -> case eof of
            True -> do
                putStrLn "Server has gone away."
                exitSuccess
            _ -> do
                str <- hGetLine handle
                putMVar recv (read str)

    -- Send loop
    forkIO $ forever $ do
        shape <- takeMVar send
        hPutStrLn handle (show shape)
        putMVar recv shape

    -- Graphics loop
    playIO
        (InWindow "GUI Chat" (600, 400) (100, 100))
        white
        100
        emptyCanvas
        renderCanvas
        (handleEvent  send)
        (updateCanvas recv)


updateCanvas :: MVar Image -> Float -> Canvas -> IO Canvas
updateCanvas recv _ canvas@(Canvas { cPictures = pics }) = do
    pics' <- maybe pics (updatePictures pics) <$> tryTakeMVar recv
    return (canvas { cPictures = pics' })
  where
    updatePictures pics image = pics ++ [mkPicture image]

