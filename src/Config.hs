{-# LANGUAGE TemplateHaskell #-}
module Config  where

import           System.Directory                ( getCurrentDirectory, getDirectoryContents )
import           System.FilePath                 ( pathSeparator )
import           GHC.Records
import           Control.Lens hiding             (element)
import           Control.Lens.TH
import           Data.Time
import qualified Data.HashMap.Strict as HM    
import           TgUpdatesTypes
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Network.HTTP.Simple as HTTP hiding ( Response )



data PostAd = HousingType | Location | Photos | Descr | Price | End       deriving (Eq, Ord, Show, Enum)
			  
data Querys = Q1 | Q2     deriving (Show, Eq, Ord, Enum)


data Dialog_stage = Posting PostAd | Querying Querys       deriving (Eq, Show)

data PostingData = PostingData { _housingType :: Text,
                                 _location    :: Text,
                                 _photos      :: [PhotoData],
                                 _description :: Text,
                                 _price       :: Text    } 
					                  deriving (Show)
makeLenses '' PostingData

data QueryingData = QueryingData deriving Show

data Dialog_data = Dialog_data { _time :: UTCTime,
                                 _stage :: Dialog_stage,
                                 _posting_data :: PostingData,
                                 _querying_data :: QueryingData } 
					                  deriving (Show)
makeLenses '' Dialog_data
									  
data Config = Config { _token :: String,
                       _current_time :: UTCTime,
                       _recent_dialogs :: HashMap Chat_ID Dialog_data,
					   _answered_updates :: HashMap Update_ID UTCTime } 
					   deriving (Show)
makeLenses '' Config
  
  
token_filename = "bot_token.txt"

getConfig :: IO Config
getConfig = do
    time <- getCurrentTime
    curr_dir     <- getCurrentDirectory
    cur_dir_cont <- getDirectoryContents curr_dir
    token_contents <- if elem token_filename cur_dir_cont
                       then readFile $ curr_dir ++ (pathSeparator : token_filename)
                       else  return $ error $ fromString ("Unable to find bot_token.txt in " ++ 
					           curr_dir ++ (pathSeparator : token_filename) ++ 
							   ". Bot can`t work without token)" )
    return Config { _token = token_contents,
                    _current_time = time,
                    _recent_dialogs = HM.empty,
					_answered_updates = HM.empty }
					
					
