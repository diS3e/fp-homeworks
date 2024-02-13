{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
module HW5.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where
import Data.Set (member, Set)

import Control.Exception
import HW5.Base
import Control.Monad.Trans.Reader
import Control.Monad
import Data.ByteString as BS
import Data.Text.Encoding
import Data.Text as T
import Data.Text.IO as TIO
import System.Directory (doesFileExist, createDirectory, setCurrentDirectory, getCurrentDirectory, listDirectory)
import Data.Sequence (fromList)
import Data.Time.Clock (getCurrentTime)
import System.Random
import Data.Functor

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

newtype PermissionException = PermissionRequired HiPermission
    deriving (Show)

instance Exception PermissionException


newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad) via ReaderT (Set HiPermission) IO

instance HiMonad HIO where
  runAction =
    \case
    HiActionRead path -> HIO $ \perm -> do
      checkPermission perm AllowRead
      source <- doesFileExist path
      if source then (do
        bytes <- BS.readFile path
        case decodeUtf8' bytes of
          Left _ -> return HiValueNull
          Right r -> return $ HiValueString r) 
      else do
        l <- listDirectory path
        return $ HiValueList $ fromList $ HiValueString . T.pack <$> l
    HiActionWrite path bs -> HIO $ \perm -> do
      checkPermission perm AllowWrite
      BS.writeFile path bs $> HiValueNull
    HiActionMkDir path -> HIO $ \perm -> do
      checkPermission perm AllowWrite
      createDirectory path $> HiValueNull
    HiActionChDir path -> HIO $ \perm -> do
      checkPermission perm AllowRead
      setCurrentDirectory path $> HiValueNull
    HiActionCwd -> HIO $ \perm -> do
      checkPermission perm AllowRead
      HiValueString . T.pack <$> getCurrentDirectory
    HiActionNow -> HIO $ \perm -> do
      checkPermission perm AllowTime
      HiValueTime <$> getCurrentTime
    HiActionRand a b -> HIO $ const $ HiValueNumber . toRational <$> getStdRandom (uniformR (a, b))
    HiActionEcho s -> HIO $ \perm -> do
      checkPermission perm AllowWrite
      TIO.putStrLn s $> HiValueNull 



checkPermission :: Set HiPermission -> HiPermission -> IO ()
checkPermission set p = unless (member p set) $ throwIO $ PermissionRequired p
