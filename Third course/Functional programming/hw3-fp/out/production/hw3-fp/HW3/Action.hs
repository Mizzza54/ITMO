module HW3.Action
  ( HiPermission (..)
  , PermissionException (..)
  , HIO (..)
  ) where

----------------------------------------------------------------------
-- Imports
----------------------------------------------------------------------

import           Control.Exception      (Exception, throwIO)
import           Control.Monad
import           Control.Monad.Cont     (MonadIO (..))
import qualified Data.ByteString        as BS
import qualified Data.Sequence          as Seq
import           Data.Set               (Set)
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Encoding
import qualified Data.Time.Clock.System as Time
import           HW3.Base               (HiAction (..), HiMonad (..),
                                         HiValue (..))
import           System.Directory       (createDirectory, doesDirectoryExist,
                                         doesFileExist, getCurrentDirectory,
                                         listDirectory, setCurrentDirectory, doesPathExist)
import           System.Random          (getStdRandom, uniformR)

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
--  fmap = liftM

instance Applicative HIO where
  pure a = HIO { runHIO = \_ -> pure a }
--  pure = return
  (<*>) = Control.Monad.ap

instance Monad HIO where
  m >>= f = joinHIO $ fmap f m

instance MonadIO HIO where
  liftIO io = HIO { runHIO = const io }

instance HiMonad HIO where
  runAction (HiActionRead path) = HIO $ \set ->
    if Set.member AllowRead set then do
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
    else
      throwIO $ PermissionRequired AllowRead

  runAction (HiActionWrite path bytes) = HIO $ \set ->
    if Set.member AllowWrite set then do
      BS.writeFile path bytes
      return HiValueNull
    else
      throwIO $ PermissionRequired AllowWrite

  runAction (HiActionMkDir path) = HIO $ \set ->
    if Set.member AllowWrite set then do
      createDirectory path
      return HiValueNull
    else
      throwIO $ PermissionRequired AllowWrite

  runAction (HiActionChDir path) = HIO $ \set -> do
    if Set.member AllowRead set then do
      setCurrentDirectory path
      return HiValueNull
    else
      throwIO $ PermissionRequired AllowRead

  runAction HiActionCwd = HIO $ \set -> do
    if Set.member AllowRead set then do
      cwd <- getCurrentDirectory
      return . HiValueString . Text.pack $ cwd
    else do
      throwIO $ PermissionRequired AllowRead

  runAction HiActionNow = HIO $ \set -> do
    if Set.member AllowTime set then do
      systemTime <- Time.getSystemTime
      let time = Time.systemToUTCTime systemTime
      return . HiValueTime $ time
    else do
      throwIO $ PermissionRequired AllowTime

  runAction (HiActionRand l r) = HIO $ \_ -> do
    rand <- getStdRandom (uniformR (l, r))
    return . HiValueNumber . toRational $ rand

  runAction (HiActionEcho text) = HIO $ \set -> do
    if Set.member AllowWrite set then do
      putStrLn $ Text.unpack text
      return HiValueNull
    else do
      throwIO $ PermissionRequired AllowWrite

----------------------------------------------------------------------
-- Internal utilities
----------------------------------------------------------------------

joinHIO :: HIO (HIO a) -> HIO a
joinHIO outer = HIO $ \s -> do
  s' <- runHIO outer s
  runHIO s' s
