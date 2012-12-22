{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module MinPrint (
    MinPrint, MinPrintT, Doc, Item,
    text, br, nest,
    execMinPrint, execMinPrintT, printDoc
) where

import Control.Monad.Identity     (Identity, runIdentity)
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Trans.Class  (MonadTrans (..))
import Data.Monoid                (Monoid (..))
import Data.String                (IsString, fromString)

-- import Control.Applicative        (Applicative, pure, (*>))
-- import System.IO                  (hFlush, hPutStr, stdout)
-- import qualified Data.ByteString.Char8  as S

newtype MinPrintT s m a
    = MinPrintT { unMinPrintT :: Doc s -> m (a, Doc s) }

type MinPrint s a = MinPrintT s Identity a

data Doc s
    = Nil
    | Line (Item s) (Doc s)
    | Nest !Int (Doc s) (Doc s)
    deriving (Show)

data Item s
    = Cat (Item s) (Item s)
    | Empty
    | Text !s
    deriving (Show)

instance (Monad m) => Functor (MinPrintT s m) where
    fmap f m = MinPrintT $ \d -> do
        ~(a, d') <- unMinPrintT m d
        return (f a, d')

instance (Monad m) => Monad (MinPrintT s m) where
    return a = MinPrintT $ \d -> return (a, d)
    m >>= k  = MinPrintT $ \d -> do
        ~(a, d') <- unMinPrintT m d
        unMinPrintT (k a) d'
    fail str = MinPrintT $ \_ -> fail str

instance MonadTrans (MinPrintT s) where
    lift m = MinPrintT $ \d -> do
        a <- m
        return (a, d)

instance (MonadIO m) => MonadIO (MinPrintT s m) where
    liftIO = lift . liftIO

--
-- DSL
--

text :: (Monad m) => s -> MinPrintT s m (Item s) 
text t = MinPrintT $ \d -> do
             let s' = Text t
             return $ case d of
                 (Line Empty d'@(Line _ _)) -> (s', Line s' d')
                 (Line Empty _ )            -> (s', Line s' d)
                 (Line s     d')            -> (s', Line (Cat s s') d')
                 _                          -> (s', Line s' d)
                          
br :: (Monad m) => MinPrintT s m (Item s)
br = MinPrintT $ \d -> return (Empty, Line Empty d)

nest :: (Monad m) => Int -> MinPrintT s m a -> MinPrintT s m a
nest n m = MinPrintT $ \d2 -> do
               ~(a, d1) <- unMinPrintT m Nil
               return (a, Nest n d1 d2)

--
-- Evaluation
--

execMinPrint :: MinPrintT s Identity a -> Doc s
execMinPrint = runIdentity . execMinPrintT 

execMinPrintT :: (Monad m) => MinPrintT s m a -> m (Doc s)
execMinPrintT m = do
     ~(_, d) <- runMinPrintT Nil m
     return d

runMinPrintT :: (Monad m) => Doc s -> MinPrintT s m a -> m (a, Doc s)
runMinPrintT d m = unMinPrintT m d

--
-- Printing
--

printDoc :: (Monad m, Monoid (m a), IsString s) => (s -> m a) -> Doc s -> m a
printDoc f d = printTreeR f 0 d

printTreeR :: (Monad m, Monoid (m a), IsString s) => (s -> m a) -> Int -> Doc s -> m a
printTreeR _ _ Nil = mempty
printTreeR f n (Line s Nil) = do
    --printWS f n
    printTreeL f s
printTreeR f n (Line s d) = do
    printTreeR f n d
    --printBR f
    --printWS f n
    printTreeL f s
printTreeR f n1 (Nest n2 d1 Nil) = do
    printTreeR f (n1+n2) d1
printTreeR f n1 (Nest n2 d1 d2) = do
    printTreeR f n1 d2
    --printBR f
    printTreeR f (n1+n2) d1

printTreeL :: (Monad m, Monoid (m a)) => (s -> m a) -> Item s -> m a
printTreeL _ Empty = mempty
printTreeL f (Cat s1 s2) = do
    printTreeL f s1
    printTreeL f s2
printTreeL f (Text s) = do
    f s

printWS :: (Monad m, IsString s) => (s -> m a) -> Int -> m a
printWS f n = f $ fromString $ take n $ repeat ' '

printBR :: (Monad m, IsString s) => (s -> m a) -> m a
printBR f = f $ fromString "\n"

{-
-- ---------------------------------------------------------------------------
-- Test Code

instance (Applicative f, Monoid a) => Monoid (f a) where
    mempty = pure mempty
    mappend = (*>)

main :: IO ()
main = do
    S.hPutStrLn stdout "## 1 ##########################"
    printDoc (S.hPutStr stdout) createDoc1
    S.hPutStrLn stdout "\n"
    S.hPutStrLn stdout "## 2 ##########################"
    doc <- createDoc2
    printDoc (S.hPutStr stdout) doc
    S.hPutStrLn stdout "\n"

createDoc1 :: (IsString s) => Doc s
createDoc1 = Line (Text "h")
                  (Line Empty
                        (Nest 4 (Nest 4 (Line (Cat (Text "f")
                                                   (Text "g"))
                                              Nil)
                                        (Line (Text "e")
                                              (Line (Cat (Text "c")
                                                         (Text "d"))
                                                    Nil)))
                                (Line (Cat (Text "a")
                                           (Text "b"))
                                      Nil)))
                                             
createDoc2 :: (IsString s) => IO (Doc s)
createDoc2 = execMinPrintT $ do
    text "a"
    text "b"
    nest 4 $ do
        text "c"
        text "d"
        br
        liftIO $ putStrLn "TEST"
        text "e"
        nest 4 $ do
            text "f"
            text "g"
    br
    text "h"
-}
