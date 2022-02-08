module HW3.Action
  ( HiPermission (..)
  , PermissionException (..)
  , HIO (..)
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import Control.Exception (Exception, throwIO)
import Control.Monad
import Control.Monad.Cont (MonadIO (..))

import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time.Clock.System as Time

import System.Directory (createDirectory, doesFileExist, doesPathExist, getCurrentDirectory,
                         listDirectory, setCurrentDirectory)
import System.Random (getStdRandom, uniformR)

import HW3.Base (HiAction (..), HiMonad (..), HiValue (..))

----------------------------------------------------------------------
-- Type
----------------------------------------------------------------------

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Bounded, Enum)

newtype PermissionException = PermissionRequired HiPermission deriving Show

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance Exception PermissionException

instance Functor HIO where
  fmap func (HIO run) = HIO { runHIO = fmap func . run }

instance Applicative HIO where
  pure a = HIO { runHIO = \_ -> pure a }
  (<*>) = Control.Monad.ap

instance Monad HIO where
  m >>= f = joinHIO $ fmap f m

instance MonadIO HIO where
  liftIO io = HIO { runHIO = const io }

instance HiMonad HIO where
  runAction (HiActionRead path) = HIO $ \set -> do
    unless (Set.member AllowRead set) (throwIO $ PermissionRequired AllowRead)
    isExist <- doesPathExist path
    if not isExist then do
      return HiValueNull
    else do
      isFile <- doesFileExist path
      if isFile then do
        bytes <- BS.readFile path
        case Encoding.decodeUtf8' bytes of
          Left _     -> return . HiValueBytes $ bytes
          Right text -> return . HiValueString $ text
      else do
        dirs <- listDirectory path
        return . HiValueList . Seq.fromList $ map (HiValueString . Text.pack) dirs

  runAction (HiActionWrite path bytes) = HIO $ \set -> do
    unless (Set.member AllowWrite set) (throwIO $ PermissionRequired AllowWrite)
    BS.writeFile path bytes
    return HiValueNull

  runAction (HiActionMkDir path) = HIO $ \set -> do
    unless (Set.member AllowWrite set) (throwIO $ PermissionRequired AllowWrite)
    createDirectory path
    return HiValueNull

  runAction (HiActionChDir path) = HIO $ \set -> do
    unless (Set.member AllowRead set) (throwIO $ PermissionRequired AllowRead)
    setCurrentDirectory path
    return HiValueNull

  runAction HiActionCwd = HIO $ \set -> do
    unless (Set.member AllowRead set ) (throwIO $ PermissionRequired AllowRead)
    cwd <- getCurrentDirectory
    return . HiValueString . Text.pack $ cwd

  runAction HiActionNow = HIO $ \set -> do
    unless (Set.member AllowTime set) (throwIO $ PermissionRequired AllowTime)
    systemTime <- Time.getSystemTime
    let time = Time.systemToUTCTime systemTime
    return . HiValueTime $ time
      
  runAction (HiActionRand l r) = HIO $ \_ -> do
    rand <- getStdRandom (uniformR (l, r))
    return . HiValueNumber . toRational $ rand

  runAction (HiActionEcho text) = HIO $ \set -> do
    unless (Set.member AllowWrite set) (throwIO $ PermissionRequired AllowWrite)
    putStrLn $ Text.unpack text
    return HiValueNull

----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

joinHIO :: HIO (HIO a) -> HIO a
joinHIO outer = HIO $ \s -> do
  s' <- runHIO outer s
  runHIO s' s
