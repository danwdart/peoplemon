{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}

module Battle.Moves.MeToo (
    meTooEnemy,
    meTooFriend
) where

import           Activity
import           Battle.Activity
import           Battle.Output
import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.Reader
import           Control.Monad.State
import           Controls
import           ControlsMaps
import           Data.Point2
import           Data.Vector2
import           FRP.Yampa
import           LabelName
import           Lightarrow
import           Menu
import           OfflineData
import           Output
import           Ppmn.Parameters

meTooEnemy = meToo { mpCont = move enemyDateProposal }
meTooFriend = meToo { mpCont = move friendDateProposal }

meToo = MoveParameters {
    mpAccuracy = 0.9,
    mpCont = pure (),
    mpElement = Ppmn.Parameters.Authority,
    mpName = MeToo,
    mpPower = 0
}

move proposal = do
    scene <- gets sceneDefault
    summary <- gets sceneSummary
    let addStatic = first (arr ((scene >.= summary) >.=))
    local (const $ Embedding (>>> addStatic)) (callCC proposal)

enemyDateProposal exit = do
    narration1 <- stdCommentary enemyDateProse
    accept1 <- acceptOrReject narration1
    when accept1 $ acceptance >> exit ()
    stdHesitation narration1
    desist1 <- state random
    when (desist1 :: Bool) $ firstRejection >> exit ()
    narration2 <- stdCommentary dateAgainProse
    accept2 <- acceptOrReject narration2
    when accept2 $ acceptance >> exit ()
    stdHesitation narration2
    desist2 <- state (randomR (1, 8))
    when ((desist2 :: Int) > 5) $ secondRejection >> exit ()
    narration3 <- stdCommentary dateAgainProse
    accept3 <- acceptOrReject narration3
    when accept3 $ acceptance >> exit ()
    longHesitation narration3
    thirdRejection

friendDateProposal exit = do
    narration1 <- stdCommentary friendDateProse
    stdHesitation narration1
    accept1 <- state (randomR (1, 3))
    when ((accept1 :: Int) > 2) $ acceptance >> exit ()
    narration2 <- stdCommentary rejectionProse
    desist1 <- desistOrContinue narration2
     when desist1 $ firstRejection >> exit ()
    stdHesitation narration2
    accept2 <- state (randomR (1, 5))
    when ((accept2 :: Int) > 3) $ acceptance >> exit ()
    narration3 <- stdCommentary rejectionAgainProse
    desist2 <- desistOrContinue narration3
    when desist2 $ secondRejection >> exit ()
    stdHesitation narration3
    accept3 <- state random
    when (accept3 :: Bool) $ acceptance >> exit ()
    stdCommentary rejectionAgainProse >>= stdHesitation
    thirdRejection

acceptOrReject :: (MonadTrans t, MonadReader (Embedding Controls OfflineIO b c) (t (Swont b c))) => OfflineIO -> t (Swont b c) Bool
acceptOrReject = popUpMenu dateMenu
desistOrContinue :: (MonadTrans t, MonadReader (Embedding Controls OfflineIO b c) (t (Swont b c))) => OfflineIO -> t (Swont b c) Bool
desistOrContinue = popUpMenu desistMenu

popUpMenu menu narration = do
    Embedding embed <- ask
    (next, k) <- lift . swont $ embed (menuControl >>> menu >>> first (arr (narration >.=)))
    pure next

acceptance = do
    stdCommentary niceProse >>= stdHesitation
    name <- gets actionObjectName
    stdAttackDebuffEffect

firstRejection = do
    decision <- state random
    if (decision :: Bool)
        then frustration
        else cool

secondRejection = do
    decision <- state (randomR (1, 5))
    if (decision :: Int) < 4
        then anger
        else cool

thirdRejection = do
    decision <- state (randomR (1, 3))
    if (decision :: Int) < 3
        then fury
        else cool

frustration = do
    stdCommentary frustrationProse >>= stdHesitation
    name <- gets actionSubjectName
    stdAttackBuffEffect

anger = do
    stdCommentary angerProse >>= stdHesitation
    name <- gets actionSubjectName
    stdAttackGreatBuffEffect

fury = do
    stdCommentary furyProse >>= stdHesitation
    name <- gets actionObjectName
    stdDefenseGreatDebuffEffect

cool = stdCommentary coolProse >>= stdHesitation

friendDateProse params = sentence '!' [subject, "started feeling up", object]
  where
    subject = actionSubjectName params
    object = actionObjectName params

enemyDateProse params = sentence '!' [subject, "started feeling", object, "up"]
  where
    subject = actionSubjectName params
    object = actionObjectName params

dateAgainProse params = sentence '!' [subject, "kept it up"]
  where
    subject = actionSubjectName params
    object = actionObjectName params

frustrationProse params = sentence '.' [subject, "got quietly offended"]
  where
    subject = actionSubjectName params

angerProse params = sentence '!' [subject, "is visibly angry"]
  where
    subject = actionSubjectName params

furyProse params = sentence '!' [subject, "went around saying that", object, "is a frigid bitch"]
  where
    subject = actionSubjectName params
    object = actionObjectName params

coolProse params = sentence '.' [subject, "let it go"]
  where
    subject = actionSubjectName params

niceProse params = sentence '.' [object, "feigned approval"]
  where
    object = actionObjectName params

rejectionProse params = sentence '.' [object, "pulled away awkwardly"]
  where
    object = actionObjectName params

rejectionAgainProse params = sentence '!' [object, "kept resisting"]
  where
    object = actionObjectName params

dateMenu = backgroundMenu 80 48 (Point2 64 44) $ columnMenu [(Accept, True), (Reject, False)] (False, 0) 0

desistMenu = backgroundMenu 96 48 (Point2 64 44) $ columnMenu [(Desist, True), (Continue, False)] (True, 0) 0
