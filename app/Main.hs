module Main (main) where

import           Config
import           TgUpdatesTypes
import           Network.HTTP.Simple as HTTP hiding ( Response )
import           Control.Lens
import           Data.Aeson  
import qualified Data.HashMap.Strict as HM    
import           Control.Concurrent              ( threadDelay )
import           Data.Time
import           Dialog_maintenance

-- TODO
-- delete outdated updates from answered
-- delete entries with last dialog stage

-- start
main :: IO ()
main = do
    cfg <- getConfig
    main_loop cfg
    pure ()



main_loop :: Config -> IO Config
main_loop old_cfg = do
    time <- getCurrentTime
    let cfg = set current_time time old_cfg
    resRaw <- httpLBS . parseRequest_ $ "https://api.telegram.org/bot" ++ (_token cfg) ++ "/getUpdates"
    let res = safe_response $ (eitherDecode ( getResponseBody resRaw) :: Either String Response)
        -- ignore answered updates
        update_IDs = HM.fromList $ map (\x -> (update_id x,time)) (result res)
        unanswered_updates = HM.difference update_IDs (_answered_updates cfg)
        is_member = \x -> HM.member (update_id x) unanswered_updates
        -- get answers and new dialog stages
        (answers,new_settings) = prettify_result ( map (process_update cfg) $ filter is_member (result res) )
    responses <- mapM httpLBS answers
    threadDelay 3000000
    let -- config with written answered updates
        cfg_w_answered = over answered_updates (HM.union update_IDs) cfg
        --updated dialogs settings
        new_recent_dialogs = foldr (\(chatID,dd) hm -> HM.insert chatID dd hm ) (_recent_dialogs cfg_w_answered) new_settings 
        new_cfg = set recent_dialogs new_recent_dialogs cfg_w_answered
    print new_recent_dialogs
    main_loop new_cfg


safe_response :: Either String Response -> Response
safe_response (Left _) = Response { ok = False, result = [] }
safe_response (Right r) = r
