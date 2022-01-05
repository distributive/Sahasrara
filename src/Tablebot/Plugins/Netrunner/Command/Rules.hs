-- |
-- Module      : Tablebot.Plugins.Netrunner.Netrunner
-- Description : Handles the internal functionality of the Netrunner command.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Backend for the rules command.
-- NOTE: To enforce indentation in embeds, the spaces preceeding indented lines
-- are actually an invisible unicode character.
-- If you modify this file, please copy that unicode character for consistency.
-- (Alternatively, implement this in a not-entirely-terrible manner)
module Tablebot.Plugins.Netrunner.Command.Rules (Ruling (..), getRuling) where

import Data.Text (Text, strip)
import Tablebot.Utility.Utils (standardise)
import Text.RawString.QQ (r)

-- | @Ruling@ represents the formatted information of a ruling.
data Ruling = Ruling
  { title :: Text,
    body :: Text
  }

-- | @getRuling@ matches a query to its ruling.
getRuling :: Text -> Either Ruling Ruling
getRuling t = case strip $ standardise t of
  "access" -> Right tsAccess
  "breach" -> Right tsBreach
  "run" -> Right tsRun
  "turn" -> Right tsTurn
  "corp" -> Right tsCorp
  "runner" -> Right tsRunner
  "" -> Left empty
  _ -> Left unknown

empty :: Ruling
empty = Ruling ":warning: No Ruling Provided! :warning:" helpText

unknown :: Ruling
unknown = Ruling ":warning: Unrecognised Ruling! :warning:" helpText

helpText :: Text
helpText =
  [r|
Please try one of the following:
• `access` - _Steps of Accessing a Card_
• `breach` - _Steps of Breaching a Server_
• `run` - _Timing Structure of a Run_
• `turn` - _Timing Structure of a Turn_
• `corp` - _Timing Structure of a Corp's Turn_
• `runner` - _Timing Structure of a Runner's Turn_|]

tsRun :: Ruling
tsRun =
  Ruling
    "Timing Structure of a Run"
    [r|
1) Initiation Phase
⠀⠀a) The Runner announces the attacked server.
⠀⠀b) The Runner gains bad publicity credits.
⠀⠀c) The run begins.
⠀⠀d) Is there ice protecting the server?
⠀⠀⠀⠀i) YES: The Runner’s position moves to the outermost piece of ice. Go to (2).
⠀⠀⠀⠀ii) NO: Go to (4).
2) Approach Ice Phase
⠀⠀a) The Runner approaches the ice at their position.
⠀⠀b) Paid ability window: (P) (R) and the Corp can rez the ice at the Runner’s position.
⠀⠀c) Is the ice at the Runner’s position rezzed?
⠀⠀⠀⠀i) YES: Continue to (3).
⠀⠀⠀⠀ii) NO: Go to (4).
3) Encounter Ice Phase
⠀⠀a) The Runner encounters the ice at their position.
⠀⠀b) Paid ability window: (P) and subroutines on the encountered ice can be broken.
⠀⠀c) The Corp resolves unbroken subroutines.
⠀⠀d) Continue to (4).
4) Movement Phase
⠀⠀a) If the run got here from (2) or (3), the Runner passes the ice in their position.
⠀⠀b) Paid ability window: (P)
⠀⠀c) The Runner may jack out.
⠀⠀d) If there is ice between the Runner’s current position and the server, the Runner’s position moves to the next inward ice.
⠀⠀e) Paid ability window: (P) (R)
⠀⠀f) Did the Runner move to a new piece of ice?
⠀⠀⠀⠀i) YES: Return to (2).
⠀⠀⠀⠀ii) NO: The Runner approaches the server. Then, continue to (5).
5) Success Phase
⠀⠀a) The run is declared successful.
⠀⠀b) The Runner breaches the attacked server. (Access cards)
⠀⠀c) Continue to (6).
6) Run Ends Phase
⠀⠀a) Close/resolve any open priority windows from before “end the run.”
⠀⠀b) The Runner loses any unspent bad publicity credits.
⠀⠀c) If the run did not reach (5), it is declared unsuccessful.
⠀⠀d) The run is complete.|]

tsBreach :: Ruling
tsBreach =
  Ruling
    "Steps of Breaching a Server"
    [r|
1) Breaching the server formally begins. Conditions related to breaching this server
are met.
2) If the breached server is Archives, turn all cards in the Corp's discard pile faceup.
3) If the breached server is HQ or R&D, determine the limit to how many times the Runner can choose a candidate from the Corp's hand or deck.
4) The Runner chooses a candidate. If they cannot, skip to step (7).
5) The Runner accesses the chosen card.
6) Return to (4).
7) The breach is complete. Conditions related to the breach ending are met.
|]

tsAccess :: Ruling
tsAccess =
  Ruling
    "Steps of Accessing a Card"
    [r|
1) The card is accessed. Conditions related to accessing this card are met.
2) The Runner may use a single mid-access ability, such as the basic trash ability.
3) If the accessed card is an agenda, the Runner must steal it.
4) The access is complete. Conditions related to an access ending are met.
|]

tsTurn :: Ruling
tsTurn = Ruling "Timing Structure of a Turn" $ "**Corp's Turn**" <> corpText <> "\n\n**Runner's Turn**" <> runnerText

tsCorp :: Ruling
tsCorp = Ruling "Timing Structure of a Corp's Turn" corpText

tsRunner :: Ruling
tsRunner = Ruling "Timing Structure of a Runner's Turn" runnerText

corpText :: Text
corpText =
  [r|
1) Draw Phase
⠀⠀a) The Corp gains allotted clicks.
⠀⠀b) Paid ability window: (P) (R) (S).
⠀⠀c) The Corp’s recurring credits refill.
⠀⠀d) The Corp’s turn begins.
⠀⠀e) The Corp draws 1 card.
2) Action Phase
⠀⠀a) Paid ability window: (P) (R) (S).
⠀⠀b) Does the Corp have unspent clicks?
⠀⠀⠀⠀i) If yes, the Corp takes an action.
⠀⠀⠀⠀ii) If no, go to (3).
⠀⠀c) Return to (a).
3) Discard Phase
⠀⠀a) The Corp discards cards.
⠀⠀b) Paid ability window: (P) (R).
⠀⠀c) The Corp loses unspent [click].
⠀⠀d) The Corp’s turn ends.
⠀⠀e) The Corp’s turn is complete, and the game moves to the Runner’s turn.|]

runnerText :: Text
runnerText =
  [r|
1) Action Phase
⠀⠀a) The Runner gains allotted clicks.
⠀⠀b) Paid ability window: (P) (R).
⠀⠀c) The Runner’s recurring credits refill.
⠀⠀d) The Runner’s turn begins.
⠀⠀e) Paid ability window: (P) (R).
⠀⠀f) Does the Runner have unspent clicks?
⠀⠀⠀⠀i) If yes, the Runner takes an action.
⠀⠀⠀⠀ii) If no, go to (2).
⠀⠀g) Return to (e).
2) Discard Phase
⠀⠀a) The Runner discards cards.
⠀⠀b) Paid ability window: (P) (R).
⠀⠀c) The Runner loses unspent [click].
⠀⠀d) The Runner’s turn ends.
⠀⠀e) The Runner’s turn is complete, and the game moves to the Corp’s turn.|]
