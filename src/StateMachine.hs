{-# language KindSignatures, TemplateHaskell #-}

module StateMachine (tests) where

import Data.IORef
import Control.Monad.IO.Class (MonadIO(..))

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: IO Bool
tests = checkSequential $$(discover)

data TurnstileState =
    Locked
  | Unlocked
  deriving (Eq, Ord, Show)

newtype Turnstile = Turnstile {
    unTurnstile :: IORef TurnstileState
  }

newTurnstile :: IO Turnstile
newTurnstile =
  Turnstile <$> newIORef Locked


insertCoin :: Turnstile -> IO ()
insertCoin (Turnstile ref) =
  atomicModifyIORef' ref $ \_ ->
    (Unlocked, ())

pushTurnstile :: Turnstile -> IO Bool
pushTurnstile (Turnstile ref) =
  atomicModifyIORef' ref $ \s ->
    case s of
      Locked ->
        (Locked, False)
      Unlocked ->
        (Locked, True)

data ModelState (v :: * -> *) =
    TLocked
  | TUnlocked
  deriving (Eq, Ord, Show)

initialState :: ModelState v
initialState =
  TLocked

data Coin (v :: * -> *) =
  Coin
  deriving (Eq, Show)

instance HTraversable Coin where
  htraverse _ Coin = pure Coin

data Push (v :: * -> *) =
  Push
  deriving (Eq, Show)

instance HTraversable Push where
  htraverse _ Push = pure Push

s_coin :: (Monad n, MonadIO m, MonadTest m) => Turnstile -> Command n m ModelState
s_coin ts =
  let
    -- We can always insert a coin, so we don't need to see the state.
    gen _state =
      Just $
        pure Coin
    -- We execute this action by calling 'insertCoin'.
    execute Coin =
      liftIO (insertCoin ts)
  in
    Command gen execute [
        -- After a coin action, the turnstile should be unlocked.
        -- First we update our model:
        Update $ \s Coin _o ->
          TUnlocked
        -- ... then we enforce a very simple predicate on it:
      , Ensure $ \_before after Coin () -> do
          after === TUnlocked
      ]

s_push_locked :: (Monad n, MonadIO m, MonadTest m) => Turnstile -> Command n m ModelState
s_push_locked ts =
  let
    -- This generator only succeeds when the gate is thought to be locked.
    gen state =
      case state of
        TLocked ->
          Just $
            pure Push
        TUnlocked ->
          Nothing
    execute Push = do
      liftIO $ pushTurnstile ts
  in
    Command gen execute [
        -- Precondition: the gate is thought to be locked.
        Require $ \s Push ->
          s == TLocked
        -- Update: pushing the locked gate has no effect.
      , Update $ \s Push _o ->
          TLocked
        -- Postcondition: we're denied admission, the turnstile gate stays locked.
      , Ensure $ \before after Push b -> do
          before === TLocked
          assert (not b)
          after === TLocked
      ]

s_push_unlocked :: (Monad n, MonadIO m, MonadTest m) => Turnstile -> Command n m ModelState
s_push_unlocked ts =
  let
    gen state =
      case state of
        TUnlocked ->
          Just $
            pure Push
        TLocked ->
          Nothing
    execute Push = do
      liftIO $ pushTurnstile ts
  in
    Command gen execute [
        -- Precondition: the gate is thought to be unlocked.
        Require $ \s Push ->
          s == TUnlocked
        -- Update: pushing the unlocked gate locks it.
      , Update $ \s Push _o ->
          TLocked
        -- Postcondition: we gain admission, the turnstile gate is locked.
      , Ensure $ \before after Push b -> do
          before === TUnlocked
          assert b
          after === TLocked
      ]

prop_turnstile :: Property
prop_turnstile =
  property $ do
    turnstile <- liftIO newTurnstile
    actions <- forAll $
      Gen.sequential (Range.linear 1 100) initialState [
          s_coin turnstile
        , s_push_locked turnstile
        , s_push_unlocked turnstile
        ]
    executeSequential initialState actions

