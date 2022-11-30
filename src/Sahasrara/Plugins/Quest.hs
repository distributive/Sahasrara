-- |
-- Module      : Sahasrara.Plugins.Flip
-- Description : A command that flips a coin, or randomly selects from a list.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that picks one random element from its given arguments.
module Sahasrara.Plugins.Quest (questPlugin) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Char (isDigit)
import Data.Text (Text, pack, toLower, replace)
import Data.Maybe (fromMaybe, catMaybes)
import Discord.Types
import Sahasrara.Utility
import Sahasrara.Utility.Colour
import Sahasrara.Utility.Discord (sendEmbedMessage, listGuildMembers, findGuild)
import Sahasrara.Utility.Embed (addColour, addImage, basicEmbed)
import Sahasrara.Utility.SmartParser
import Text.RawString.QQ
import Sahasrara.Utility.Exception (BotException (InvisibleException), throwBot)
import Database.Esqueleto hiding (delete, insert_, replace)
import Database.Persist.TH
import Sahasrara.Utility.Database hiding (update, replace)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Fixed (Pico)
import Data.Bits

-- | @Team@ is the database object representing a team and their progress.
-- If the team is within a server, @guildId@ defines that server
-- If the team is within a DM, @userId@ defines the user
-- @progress@ defines the question the team is currently answering, starting at
-- 1 and ending at 4
share
  [mkPersist sqlSettings, mkMigrate "questMigration"]
  [persistLowerCase|
Team
    guildId String
    userId String
    progress Int
    cooldown UTCTime
    hintCooldown UTCTime
    attempts1 Int
    attempts2 Int
    attempts3 Int
    attempts4 Int
    requestedHints Int
    timeStart UTCTime
    time1 UTCTime
    time2 UTCTime
    time3 UTCTime
    time4 UTCTime
    sizeStart Int
    sizeEnd Int
    deriving Show
|]

-- The maximum number of players allowed to start a game
playerLimit :: Int
playerLimit = 10

sendQuest :: Message -> DatabaseDiscord ()
sendQuest m =
  infoMsg m
  ":zap: Quest! :zap:"
  [r|Thanks for accepting the call to action!

To reach my power source and reset it, you need to bypass my security protocols. I have four security levels before I can grant you access, each of increasing complexity.

You are free to attempt these levels in a group or by yourself, but progress is tied to a specific server or direct message chat, not a user. Players are free to join or leave group sessions at any point.

There's no time limit, so take your time! If you ever need help, try `$hint`.

Please keep all details of this quest to yourself until `December 9, 2022` so everyone can have a go!

To begin, type `$quest 1`|]

question1 :: Text
question1 = [r|To unlock my power source you must prove you know it, and in turn prove you understand me. Use my interface to determine its side and, while you're there, ask me to predict your future.

Give your answer in the form: `$quest 1 side fortune` (e.g. `$quest 1 runner You will solve this riddle.`)

Answers are case insensitive.|]

question2 :: Text
question2 = [r|You've narrowed it down, but not yet enough! You must now work out its type and faction!

Give your answer in the form: `$quest 2 faction type` (e.g. `$quest 2 jinteki asset`)

Answers are case insensitive.|]

question3 :: Text
question3 = [r|Now we get to the details! My power source has three stats: cost, mu, and strength. What are they?

Give your answer in the form: `$quest 3 cost mu strength` (e.g. `$quest 3 7 8 9`)

Answers are case insensitive.|]

question4 :: Text
question4 = [r|You're onto the final stretch! Now you must determine my power source's name and text, as well as its special access code!

Give your answer in the form: `$quest 4 code name text` (e.g. `$quest 4 abcd Enigma The runner loses click. End the run.`)

Answers are case insensitive.|]

answers1 :: [Text]
answers1 =
  [ "runner every rig needs a power source",
    "runner every rig needs a power source."
  ]

answers2 :: [Text]
answers2 =
  [ "shaper program"
  ]

answers3 :: [Text]
answers3 =
  [ "4 1 -",
    "4 1 none",
    "4 1 na", "4 1 n/a", "4 1 n\a",
    "4 1 ."
  ]

answers4 :: [Text]
answers4 =
  [ "k2cp turbine each installed non-ai icebreaker gets +2 strength.",
    "k2cp turbine each installed non-ai icebreaker gets +2 strength",
    "k2cp turbine each installed non ai icebreaker gets +2 strength.",
    "k2cp turbine each installed non ai icebreaker gets +2 strength"
  ]

guideText :: Text
guideText = [r|**Creating a server**
> **Step 1:** go to the bottom of your list of servers (if you are in a lot of servers you may have to scroll down).
> **Step 2:** click the button with the big plus symbol labelled "Add a Server".
> **Step 3:** choose the option "Create My Own" to create a blank server.
> **Step 4:** choose the option "For me and my friends".
> **Step 5:** feel free to set a server name and icon (it's not necessary to do so) then click the "Create" button to create the server.
> **Step 6:** invite people to your new server by clicking the "Invite your friends" button. You can send links directly to your friends, or create a link to send yourself. Note that if you put a link out in public, anyone who sees it can join your server.

**Inviting this bot**
> **Step 1:** click my profile picture (on mobile, hold it down until the pop-up appears).
> **Step 2:** click the button "Add to server" at the top of the pop-up.
> **Step 3:** choose your new server from the dropdown list.|]

errorTooBig :: Message -> DatabaseDiscord ()
errorTooBig m =
  errorMsg m
  ":anger_right: Too many users!" $
  [r|There are too many users in this server! Please try running this command in a smaller server.

Up to |] <> (pack $ show playerLimit) <> [r| users are allowed per server, but smaller groups are recommended. You can also attempt it solo by messaging this bot.

If you don't know how to create your own server or invite this bot to it, type `$setupguide` for instructions.|]

errorInvalidQuestion :: Message -> Int -> DatabaseDiscord ()
errorInvalidQuestion m index = errorMsg m ":x: Invalid question" $ "There is no question #" <> (pack $ show index) <> "!"

errorLockedQuestion :: Message -> Team -> DatabaseDiscord ()
errorLockedQuestion m team = errorMsg m ":lock: Locked" $ "You have not unlocked this question yet! You're currently on question #" <> (pack $ show $ teamProgress team) <> "."

errorCompletedQuestion :: Message -> Team -> DatabaseDiscord ()
errorCompletedQuestion m team =
  if teamProgress team > 4
    then errorMsg m ":checkered_flag: Completed" "You already completed the quest!"
    else errorMsg m ":checkered_flag: Completed" $ "You have already answered this question! You're currently on question #" <> (pack $ show $ teamProgress team) <> "."

errorTimedOut :: Message -> Team -> DatabaseDiscord ()
errorTimedOut m team = errorMsg m ":alarm_clock: Timeout" $ "You're still on cooldown from your last attempt. You can try again " <> (discordTime $ teamCooldown team) <> "!"

errorHintTimedOut :: Message -> Team -> DatabaseDiscord ()
errorHintTimedOut m team = errorMsg m ":alarm_clock: Hint Timeout" $ "You're still on cooldown from your last hint. You can request another hint " <> (discordTime $ teamHintCooldown team) <> "!\n\nIf you're completely stuck, you can message my creator (@proserpine#5016) for additional help."

-- Output a message for a correct answer
correctMsg :: Message -> Text -> DatabaseDiscord ()
correctMsg m text = sendEmbedMessage m "" $ addColour colPositive $ basicEmbed ":white_check_mark: Correct!" text

-- Output a message for an incorrect answer and set the team's cooldown
wrongMsg :: Message -> DatabaseDiscord ()
wrongMsg m = do
  verifyTeamExists m
  team <- getTeam m
  time <- postCooldownTime team
  sendEmbedMessage m "" $ addColour colNegative $ basicEmbed ":x: Incorrect!" $ "Try again " <> (discordTime time) <> "!\n\n_If you're stuck, try `$hint`!_"

-- Output a message for a hint
infoMsg :: Message -> Text -> Text -> DatabaseDiscord ()
infoMsg m title text = sendEmbedMessage m "" $ addColour colInfo $ basicEmbed title text

-- Output an error message and throw a hidden error
errorMsg :: Message -> Text -> Text -> DatabaseDiscord ()
errorMsg m title text = do
  sendEmbedMessage m "" $ addColour colError $ basicEmbed title text
  throwBot InvisibleException

-- | @quest@ is a command for fun, temporary minigames users can play
quest :: Command
quest = Command "quest" (parseComm questComm) []
  where
    questComm :: Maybe Int -> RestOfInput Text -> Message -> DatabaseDiscord ()
    questComm indexM (ROI ans) m = do
      team <- getTeam m
      case indexM of
        Nothing -> sendQuest m
        Just index -> do
          verifyIndex m team index
          if ans == ""
            then question m index
            else do
              verifyCooldown m team
              answer m team index ans

hint :: Command
hint = Command "hint" (parseComm hintComm) []
  where
    hintComm :: Maybe Int -> RestOfInput Text -> Message -> DatabaseDiscord ()
    hintComm Nothing _ m = do
      verifyTeamExists m
      infoMsg m ":mag: Hints" "Specify a question to recieve a hint (e.g. `$hint 1`).\n\nIf you're still completely stuck, you can message my creator (@proserpine#5016) for additional help."
    hintComm (Just index) (ROI flag) m = do
      verifyTeamExists m
      team <- getTeam m
      verifyHintCooldown m team
      verifyIndex m team index
      hintCooldown <- case index of
        1 -> hint1 m flag
        2 -> hint2 m flag
        3 -> hint3 m flag
        4 -> hint4 m flag
        _ -> errorInvalidQuestion m index >> return 0
      time <- variableCooldownTime hintCooldown
      if hintCooldown <= 0
        then return ()
        else do
          updateHintCooldown team time
          infoMsg m ":hourglass_flowing_sand: Hint Cooldown Set" $ "You can request another hint " <> (discordTime time) <> "."
    hint1 :: Message -> Text -> DatabaseDiscord Pico
    hint1 m _ = infoMsg m "Question 1 hint" "Stuck? You can always try `$help`" >> updateHintRecord m 0 >> return 60
    hint2 :: Message -> Text -> DatabaseDiscord Pico
    hint2 m "" = infoMsg m "Question 2 hint" "Specify `$hint 2 type` or `$hint 2 faction` for a hint." >> return 0
    hint2 m "type" = infoMsg m "Question 2 hint" "Process of elimination" >> updateHintRecord m 1 >> return 300
    hint2 m "faction" = infoMsg m "Question 2 hint" "You can find this information on [NetrunnerDB](https://netrunnerdb.com/en/syntax)!" >> updateHintRecord m 2 >> return 300
    hint2 m _ = errorMsg m "Question 2 hint" "Unknown flag!" >> return 0
    hint3 :: Message -> Text -> DatabaseDiscord Pico
    hint3 m "" = infoMsg m "Question 3 hint" "Specify `$hint 3 cost`, `$hint 3 mu`, or `$hint 3 strength` for a hint." >> return 0
    hint3 m "cost" = infoMsg m "Question 3 hint" "All these cards have one thing in common" >> updateHintRecord m 3 >> return 900
    hint3 m "mu" = infoMsg m "Question 3 hint" "What kind of cipher lets you hide information in an image?" >> updateHintRecord m 4 >> return 900
    hint3 m "strength" = infoMsg m "Question 3 hint" "What number, if inserted in place of x, makes the equation true?" >> updateHintRecord m 5 >> return 900
    hint3 m _ = errorMsg m "Question 3 hint" "Unknown flag!" >> return 0
    hint4 :: Message -> Text -> DatabaseDiscord Pico
    hint4 m "" = infoMsg m "Question 4 hint" "Specify `$hint 4 code`, `$hint 4 title`, or `$hint 4 text` for a hint." >> return 0
    hint4 m "code" = infoMsg m "Question 4 hint" "Crypic crossword clues have a specific language to them; if you're unfamiliar with the terminology they use, you could look up a [guide](https://bestforpuzzles.com/cryptic-crossword-dictionary/).\n\nSome general tips:\n• Consider each word in the clue and why it might be there\n• The correct answer should be unambiguously the only correct answer\n• Try looking for synonyms of words\n\nFor clue-specific hints, try `$hint 4 code #`" >> updateHintRecord m 6 >> return 0
    hint4 m "code 1" = infoMsg m "Crossword question 1 hint" "What might 'back' indicate? When might you 'pay up'?" >> updateHintRecord m 7 >> return 900
    hint4 m "code 2" = infoMsg m "Crossword question 2 hint" "(4,4,1) means the answer is three words; two with 4 letters and one with 1" >> updateHintRecord m 8 >> return 900
    hint4 m "code 3" = infoMsg m "Crossword question 3 hint" "What might 'I heard' indicate? What's another word for 'teacher'?" >> updateHintRecord m 9 >> return 900
    hint4 m "code 4" = infoMsg m "Crossword question 4 hint" "What might 'corrupt' indicate?" >> updateHintRecord m 10 >> return 900
    hint4 m "title" = infoMsg m "Question 4 hint" "There are four groups of words here; what connects them?" >> updateHintRecord m 11 >> return 900
    hint4 m "text" = infoMsg m "Question 4 hint" "I would have transmitted this signal by teleprinter if I could" >> updateHintRecord m 12 >> return 900
    hint4 m _ = errorMsg m "Question 4 hint" "Unknown flag!" >> return 0
    updateHintCooldown :: Team -> UTCTime -> DatabaseDiscord ()
    updateHintCooldown team' cooldown = do
      liftSql $ case teamGuildId team' of
        "-" -> update $ \t -> do
          set t [ TeamHintCooldown =. val cooldown ]
          where_ (t ^. TeamUserId ==. val (teamUserId team'))
        _ -> update $ \t -> do
          set t [ TeamHintCooldown =. val cooldown ]
          where_ (t ^. TeamGuildId ==. val (teamGuildId team'))


setupguide :: Command
setupguide = Command "setupguide" (parseComm guideComm) []
  where
    guideComm :: RestOfInput Text -> Message -> DatabaseDiscord ()
    guideComm _ m = infoMsg m ":scroll: Set up guide" guideText

-- | @question@ shows the given question.
question :: Message -> Int -> DatabaseDiscord ()
question m 1 = infoMsg m "Question 1" question1
question m 2 = do
  infoMsg m "Question 2" question2
  sendEmbedMessage m "" $ addImage "https://media.discordapp.net/attachments/1001068002214694932/1044971474722750575/image.png" $ basicEmbed "Hcraes Drow" ""
  sendEmbedMessage m "" $ basicEmbed "Riddle" "Which faction has both the least standard-legal IDs and the least eternal-legal IDs?"
question m 3 = do
  infoMsg m "Question 3" question3
  sendEmbedMessage m "" $ addImage "https://media.discordapp.net/attachments/1001068002214694932/1045051301458874448/image.png" $ basicEmbed "Cost" ""
  sendEmbedMessage m "" $ addImage "https://cdn.discordapp.com/attachments/1001068002214694932/1046861211863621702/unknown.png" $ basicEmbed "MU" ""
  sendEmbedMessage m "" $ addImage "https://media.discordapp.net/attachments/1001068002214694932/1044973571648258048/unknown.png" $ basicEmbed "Strength" ""
question m 4 = do
  infoMsg m "Question 4" question4
  sendEmbedMessage m "" $ addImage "https://media.discordapp.net/attachments/1001068002214694932/1044979320889802842/image.png" $ basicEmbed "Cryptic clues" "`1.` Pay up! Little Isaac is back for an illegal leak! (6)\n`2.` Upcoming salute's sequel (4,4,1)\n`3.` Sea fought, I heard, teaching a wild card (10)\n`4.` Nordics top corrupt propagandist (4, 6)"
  sendEmbedMessage m "" $ addImage "https://media.discordapp.net/attachments/1001068002214694932/1044982142763028560/image.png" $ basicEmbed "Connections" ""
  sendEmbedMessage m "" $ basicEmbed "Broadcast received" "https://drive.google.com/file/d/1aGEIXgvs5iLU4MKIn9wMkFYQjoBQG1hc/view?usp=share_link"

question m index = errorInvalidQuestion m index

-- | @answer@ validates the given answer to the given question.
-- Assumes it will only ever be passed valid indices
answer :: Message -> Team -> Int -> Text -> DatabaseDiscord ()
answer m team index ans = do
  let (correct, successText) = answers index
  if toLower ans `elem` correct
    then do
      updateQuestionTime team
      updateTeamSize m
      if index /= 4
        then correctMsg m successText
        else do
          sendEmbedMessage m "" $
            addColour colPositive $
            basicEmbed ":white_check_mark: You did it!" "You reached my power source! Type `$restart` to reboot it and return my power levels to normal!"
      updateProgress team $ index + 1
    else do
      wrongMsg m
      time <- postCooldownTime team
      updateCooldown team time
      updateAttempts team
  where
    answers :: Int -> ([Text], Text)
    answers 1 = (answers1, "Well done! You can now move onto `$quest 2`")
    answers 2 = (answers2, "Well done! You can now move onto `$quest 3`")
    answers 3 = (answers3, "Well done! You can now move onto `$quest 4`")
    answers 4 = (answers4, "")
    updateProgress :: Team -> Int -> DatabaseDiscord ()
    updateProgress team' newIndex = do
      liftSql $ case teamGuildId team' of
        "-" -> update $ \t -> do
          set t [ TeamProgress =. val newIndex ]
          where_ (t ^. TeamUserId ==. val (teamUserId team'))
        _ -> update $ \t -> do
          set t [ TeamProgress =. val newIndex ]
          where_ (t ^. TeamGuildId ==. val (teamGuildId team'))
    updateCooldown :: Team -> UTCTime -> DatabaseDiscord ()
    updateCooldown team' cooldown = do
      liftSql $ case teamGuildId team' of
        "-" -> update $ \t -> do
          set t [ TeamCooldown =. val cooldown ]
          where_ (t ^. TeamUserId ==. val (teamUserId team'))
        _ -> update $ \t -> do
          set t [ TeamCooldown =. val cooldown ]
          where_ (t ^. TeamGuildId ==. val (teamGuildId team'))

restart :: Command
restart = Command "restart" (parseComm restartComm) []
  where
    restartComm :: RestOfInput Text -> Message -> DatabaseDiscord ()
    restartComm _ m = do
      verifyTeamExists m
      team <- getTeam m
      if teamProgress team > 4
        then sendEmbedMessage m "" $ addImage scoop $ addColour colPositive $ basicEmbed ":zap: `RESTARTING` :zap:" $ "Enjoy your reward, and remember not to spoil any of this for others until `December 9, 2022`! :wink:\n\n" <> creditsText
        else return ()

teamStats :: Command
teamStats = Command "teamStats" (parseComm teamStatsComm) []
  where
    teamStatsComm :: RestOfInput Text -> Message -> DatabaseDiscord ()
    teamStatsComm _ m = do
      verifyTeamExists m
      team <- getTeam m
      sendEmbedMessage m "" $ basicEmbed "**Debug Stats**" $ replace "Team {" "" $ replace "}" "" $ replace "," "\n" $ pack $ show team

-- Attempts to find the team of the guild/user, creating a new one if none
getTeam :: Message -> DatabaseDiscord Team
getTeam m = do
  gidM <- findGuild m
  let uid = userId $ messageAuthor m
  teamM <- case gidM of
    Just gid -> if isDm then getTeamFromUserId uid else getTeamFromGuildId gid
    Nothing -> getTeamFromUserId uid
  case teamM of
    Just team -> do
      -- sendMessage m $ "Existing team found:\n" <> (pack $ show team)
      return team
    Nothing -> do
      verifyPlayerLimit m -- Ensure the quest cannot be started in a big server
      size <- getTeamSize m
      time <- currentTime
      let guild = if isDm then "-" else fromMaybe "-" $ show <$> gidM
          user = if isDm then show uid else "-"
          newTeam = Team guild user 1 time time 0 0 0 0 0 time time time time time size size
      insert_ newTeam
      -- sendMessage m $ "New team added:\n" <> (pack $ show $ newTeam)
      return newTeam
  where
    getTeamFromGuildId :: GuildId -> DatabaseDiscord (Maybe Team)
    getTeamFromGuildId gid = do
      res <- liftSql $
        select $
          from $ \team -> do
            where_ (team ^. TeamGuildId ==. val (show gid))
            return team
      return $ case res of
        [] -> Nothing
        (x:_) -> Just $ entityVal x
    getTeamFromUserId :: UserId -> DatabaseDiscord (Maybe Team)
    getTeamFromUserId uid = do
      res <- liftSql $
        select $
          from $ \team -> do
            where_ (team ^. TeamUserId ==. val (show uid))
            return team
      return $ case res of
        [] -> Nothing
        (x:_) -> Just $ entityVal x
    isDm :: Bool
    isDm = messageGuildId m == Nothing

verifyPlayerLimit :: Message -> DatabaseDiscord ()
verifyPlayerLimit m = do
  size <- getTeamSize m
  if size > playerLimit
    then errorTooBig m
    else return ()

verifyIndex :: Message -> Team -> Int -> DatabaseDiscord ()
verifyIndex m team index
  | teamProgress team > 4 = do
    sendEmbedMessage m "" $
      addColour colPositive $
      basicEmbed "You've already won!" "You've already finished the quest! Type `$restart` for your prize!"
    throwBot InvisibleException
  | index > 4 || index < 0 = errorInvalidQuestion m index
  | index > teamProgress team = errorLockedQuestion m team
  | index < teamProgress team = errorCompletedQuestion m team
  | otherwise = return ()

verifyCooldown :: Message -> Team -> DatabaseDiscord ()
verifyCooldown m team = do
  now <- currentTime
  if now < teamCooldown team
    then errorTimedOut m team
    else return ()

verifyHintCooldown :: Message -> Team -> DatabaseDiscord ()
verifyHintCooldown m team = do
  now <- currentTime
  if now < teamHintCooldown team
    then errorHintTimedOut m team
    else return ()

verifyTeamExists :: Message -> DatabaseDiscord ()
verifyTeamExists m = do
  gidM <- findGuild m
  let uid = userId $ messageAuthor m
  teamM <- case gidM of
    Just gid -> if isDm then getTeamFromUserId uid else getTeamFromGuildId gid
    Nothing -> getTeamFromUserId uid
  case teamM of
    Nothing -> throwBot InvisibleException
    Just _ -> return ()
  where
    getTeamFromGuildId :: GuildId -> DatabaseDiscord (Maybe Team)
    getTeamFromGuildId gid = do
      res <- liftSql $
        select $
          from $ \team -> do
            where_ (team ^. TeamGuildId ==. val (show gid))
            return team
      return $ case res of
        [] -> Nothing
        (x:_) -> Just $ entityVal x
    getTeamFromUserId :: UserId -> DatabaseDiscord (Maybe Team)
    getTeamFromUserId uid = do
      res <- liftSql $
        select $
          from $ \team -> do
            where_ (team ^. TeamUserId ==. val (show uid))
            return team
      return $ case res of
        [] -> Nothing
        (x:_) -> Just $ entityVal x
    isDm :: Bool
    isDm = messageGuildId m == Nothing

updateAttempts :: Team -> DatabaseDiscord ()
updateAttempts team' = case teamProgress team' of
  1 -> do
    liftSql $ case teamGuildId team' of
      "-" -> update $ \t -> do
        set t [ TeamAttempts1 =. val (teamAttempts1 team' + 1) ]
        where_ (t ^. TeamUserId ==. val (teamUserId team'))
      _ -> update $ \t -> do
        set t [ TeamAttempts1 =. val (teamAttempts1 team' + 1) ]
        where_ (t ^. TeamGuildId ==. val (teamGuildId team'))
  2 -> do
    liftSql $ case teamGuildId team' of
      "-" -> update $ \t -> do
        set t [ TeamAttempts2 =. val (teamAttempts2 team' + 1) ]
        where_ (t ^. TeamUserId ==. val (teamUserId team'))
      _ -> update $ \t -> do
        set t [ TeamAttempts2 =. val (teamAttempts2 team' + 1) ]
        where_ (t ^. TeamGuildId ==. val (teamGuildId team'))
  3 -> do
    liftSql $ case teamGuildId team' of
      "-" -> update $ \t -> do
        set t [ TeamAttempts3 =. val (teamAttempts3 team' + 1) ]
        where_ (t ^. TeamUserId ==. val (teamUserId team'))
      _ -> update $ \t -> do
        set t [ TeamAttempts3 =. val (teamAttempts3 team' + 1) ]
        where_ (t ^. TeamGuildId ==. val (teamGuildId team'))
  4 -> do
    liftSql $ case teamGuildId team' of
      "-" -> update $ \t -> do
        set t [ TeamAttempts4 =. val (teamAttempts4 team' + 1) ]
        where_ (t ^. TeamUserId ==. val (teamUserId team'))
      _ -> update $ \t -> do
        set t [ TeamAttempts4 =. val (teamAttempts4 team' + 1) ]
        where_ (t ^. TeamGuildId ==. val (teamGuildId team'))
  _ -> return ()

updateQuestionTime :: Team -> DatabaseDiscord ()
updateQuestionTime team' = do
  time <- currentTime
  case teamProgress team' of
    1 -> do
      liftSql $ case teamGuildId team' of
        "-" -> update $ \t -> do
          set t [ TeamTime1 =. val time ]
          where_ (t ^. TeamUserId ==. val (teamUserId team'))
        _ -> update $ \t -> do
          set t [ TeamTime1 =. val time ]
          where_ (t ^. TeamGuildId ==. val (teamGuildId team'))
    2 -> do
      liftSql $ case teamGuildId team' of
        "-" -> update $ \t -> do
          set t [ TeamTime2 =. val time ]
          where_ (t ^. TeamUserId ==. val (teamUserId team'))
        _ -> update $ \t -> do
          set t [ TeamTime2 =. val time ]
          where_ (t ^. TeamGuildId ==. val (teamGuildId team'))
    3 -> do
      liftSql $ case teamGuildId team' of
        "-" -> update $ \t -> do
          set t [ TeamTime3 =. val time ]
          where_ (t ^. TeamUserId ==. val (teamUserId team'))
        _ -> update $ \t -> do
          set t [ TeamTime3 =. val time ]
          where_ (t ^. TeamGuildId ==. val (teamGuildId team'))
    4 -> do
      liftSql $ case teamGuildId team' of
        "-" -> update $ \t -> do
          set t [ TeamTime4 =. val time ]
          where_ (t ^. TeamUserId ==. val (teamUserId team'))
        _ -> update $ \t -> do
          set t [ TeamTime4 =. val time ]
          where_ (t ^. TeamGuildId ==. val (teamGuildId team'))
    _ -> return ()

updateTeamSize :: Message -> DatabaseDiscord ()
updateTeamSize m = do
  verifyTeamExists m
  team' <- getTeam m
  teamSize <- getTeamSize m
  liftSql $ case teamGuildId team' of
    "-" -> update $ \t -> do
      set t [ TeamSizeEnd =. val teamSize ]
      where_ (t ^. TeamUserId ==. val (teamUserId team'))
    _ -> update $ \t -> do
      set t [ TeamSizeEnd =. val teamSize ]
      where_ (t ^. TeamGuildId ==. val (teamGuildId team'))

getTeamSize :: Message -> DatabaseDiscord Int
getTeamSize m = do
  members <- listGuildMembers m
  return $ length $ filter (not . userIsBot) $ catMaybes $ map memberUser $ fromMaybe [] members

updateHintRecord :: Message -> Int -> DatabaseDiscord ()
updateHintRecord m hint' = do
  verifyTeamExists m
  team' <- getTeam m
  let mask = (1 `shift` hint')
      newVal = (teamRequestedHints team') .|. mask
  liftSql $ case teamGuildId team' of
    "-" -> update $ \t -> do
      set t [ TeamRequestedHints =. val newVal ]
      where_ (t ^. TeamUserId ==. val (teamUserId team'))
    _ -> update $ \t -> do
      set t [ TeamRequestedHints =. val newVal ]
      where_ (t ^. TeamGuildId ==. val (teamGuildId team'))

-- Gets the current time
currentTime :: DatabaseDiscord UTCTime
currentTime = liftIO $ systemToUTCTime <$> getSystemTime

-- Gets the time in 60s
postCooldownTime :: Team -> DatabaseDiscord UTCTime
postCooldownTime team = do
  now <- currentTime
  let attempts =
        case teamProgress team of
          1 -> teamAttempts1 team
          2 -> teamAttempts2 team
          3 -> teamAttempts3 team
          4 -> teamAttempts4 team
          _ -> 0
      cooldown =
        case attempts of
          0 -> 60
          1 -> 120
          2 -> 180
          3 -> 240
          4 -> 300
          5 -> 360
          6 -> 420
          7 -> 480
          8 -> 540
          _ -> 600
  return $ addUTCTime (secondsToNominalDiffTime cooldown) now

-- Gets the time in n seconds
variableCooldownTime :: Pico -> DatabaseDiscord UTCTime
variableCooldownTime n = do
  now <- currentTime
  return $ addUTCTime (secondsToNominalDiffTime n) now

-- Formats a UTCTime into a relative Discord timestamp
discordTime :: UTCTime -> Text
discordTime time = "<t:" <> (pack $ takeWhile isDigit $ show $ utcTimeToPOSIXSeconds time) <> ":R>"

scoop :: Text
scoop = "https://media.discordapp.net/attachments/1046873193463435314/1047239092334759986/icecream.png"

creditsText :: Text
creditsText = [r|**Playtesters**
> izzy blue
> Slapdash
> AceEmpress
> harmonbee

Special thanks to dixonary and Ed for providing crossword clues and izzy blue for creating the steganography image|]

-- | @questPlugin@ assembles the command into a plugin.
questPlugin :: Plugin
questPlugin =
  (plug "quest")
  {
    commands = [quest, hint, setupguide, restart, teamStats],
    migrations = [questMigration],
    helpPages = []
  }
