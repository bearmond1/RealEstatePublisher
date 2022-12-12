module Dialog_maintenance where

import           Config
import           TgUpdatesTypes
import           Network.HTTP.Simple as HTTP hiding ( Response )
import           Control.Lens
import qualified Data.HashMap.Strict as HM        
import qualified Data.Text as T 
import           Publishing_Requests
import           Posing_Ad_logic




prettify_result :: [(HTTP.Request,Maybe (Chat_ID,Dialog_data) )] -> ( [HTTP.Request] , [(Chat_ID,Dialog_data)] )
prettify_result list = 
    let reqs  = map fst list
        chats = catMaybes $ map snd list
    in (reqs,chats)


process_update :: Config -> UpdateResult -> (HTTP.Request, Maybe (Chat_ID,Dialog_data) )
process_update cfg upd =
    case get_commands upd of 
        []  -> try_continue_dialog cfg upd
        [c] -> (\(r,d) -> (r, Just d)) $ answer_command cfg upd c
        _   -> seweral_comamnds


answer_command ::  Config -> UpdateResult -> T.Text -> (HTTP.Request, (Chat_ID,Dialog_data) )
answer_command cfg upd "/publish" = 
    let req = housingTypeReq cfg upd
        chatID = get_chat_ID upd
        post_data = PostingData { _housingType="",_location="",_photos=[],_description="",_price=""}
        dd = Dialog_data { _time = ( _current_time cfg), 
                           _stage = Posting HousingType, 
                           _posting_data = post_data,
                           _querying_data = QueryingData }
    in (req,(chatID,dd))

answer_command cfg upd "/help" = 
    let req = housingTypeReq cfg upd
        chatID = get_chat_ID upd
        post_data = PostingData { _housingType="",_location="",_photos=[],_description="",_price=""}
        dd = Dialog_data { _time = ( _current_time cfg), 
                           _stage = Posting HousingType, 
                           _posting_data = post_data,
                           _querying_data = QueryingData }
    in (req,(chatID,dd))


seweral_comamnds :: (HTTP.Request, Maybe (Chat_ID,Dialog_data) )
seweral_comamnds = undefined

try_continue_dialog :: Config -> UpdateResult -> (HTTP.Request, Maybe (Chat_ID,Dialog_data) )
try_continue_dialog cfg upd = 
    let chatID = get_chat_ID upd
        dialog_data = HM.lookup chatID ( _recent_dialogs cfg )
    in case dialog_data of 
        -- just random message from user
        Nothing -> (explanation_request cfg upd,Nothing)
        -- dialog taking place
        Just dd -> continue_dialog cfg upd dd


get_commands :: UpdateResult -> [T.Text]
get_commands (UpdateResult { message = Message {..} }) = 
    let commands = case entities  of 
                    Nothing -> []
                    Just entities -> filter (\x -> entity_type x == "bot_command") entities
        get_cmd_txt cmd = case text of
							Just txt -> T.take (e_length cmd) $ T.drop (offset cmd) txt
							Nothing -> ""
    in  map get_cmd_txt commands