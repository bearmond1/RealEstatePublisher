module Posing_Ad_logic where

import           Config
import           Publishing_Requests
import           TgUpdatesTypes
import           Network.HTTP.Simple as HTTP hiding ( Response )   
import qualified Data.Text as T 
import           Control.Lens



continue_dialog :: Config -> UpdateResult -> Dialog_data -> (HTTP.Request, Maybe (Chat_ID,Dialog_data) )
continue_dialog cfg upd (Dialog_data { _stage = Posting HousingType, .. }) =     
    let chatID = get_chat_ID upd
        new_pd = \txt -> set housingType txt _posting_data
        new_dd = \txt -> Dialog_data { _time = (_current_time cfg), _stage = Posting Location, _posting_data = new_pd txt, .. }
	in case (text.message $ upd) of 
          Nothing -> (enter_text_req cfg upd, Nothing)
          Just txt -> (text_req cfg upd housing_text, Just (chatID, new_dd $ T.take 50 txt))

continue_dialog cfg upd (Dialog_data { _stage = Posting Location, .. }) =     
    let chatID = get_chat_ID upd
        new_pd = \txt -> set location txt _posting_data
        new_dd = \txt -> Dialog_data { _time = (_current_time cfg), _stage = Posting Photos, _posting_data = new_pd txt, .. }
	in case (text.message $ upd) of 
          Nothing -> (enter_text_req cfg upd, Nothing)
          Just txt -> (text_req cfg upd location_text, Just (chatID, new_dd $ T.take 50 txt))

continue_dialog cfg upd (Dialog_data { _stage = Posting Photos, .. }) =
    if is_ready 
      then if length (_photos _posting_data) < 4
             then (text_req cfg upd desription_text, Just (chatID,Dialog_data { _time = (_current_time cfg), _stage = Posting Description, .. }))
             else (text_req cfg upd demand_photos_text, Nothing)
      else case (photo.message $ upd) of
             Nothing -> (text_req cfg upd demand_photos_text, Nothing)
             Just photos -> let new_pd = over photos ((last photos):) _posting_data
                                new_dd = Dialog_data { _time = (_current_time cfg), _posting_data = new_pd, .. }
                            in (HTTP.defaultRequest,new_dd)

continue_dialog cfg upd (Dialog_data { _stage = Posting Description, .. }) =
    let chatID = get_chat_ID upd
        new_pd = \txt -> set location txt _posting_data
        new_dd = \txt -> Dialog_data { _time = (_current_time cfg), _stage = Posting Price, _posting_data = new_pd txt, .. }
	in case (text.message $ upd) of 
          Nothing -> (enter_text_req cfg upd, Nothing)
          Just txt -> (priceReq cfg upd, Just (chatID, new_dd $ T.take 500 txt))

continue_dialog cfg upd (Dialog_data { _stage = Posting Price, .. }) =
    let chatID = get_chat_ID upd
        new_pd = \txt -> set location txt _posting_data
        new_dd = \txt -> Dialog_data { _time = (_current_time cfg), _stage = Posting Price, _posting_data = new_pd txt, .. }
	in case (text.message $ upd) of 
          Nothing -> (enter_text_req cfg upd, Nothing)
          Just txt -> (locationReq cfg upd, Just (chatID, new_dd $ T.take 500 txt))


    -- let chatID = get_chat_ID upd
        -- photos = view photos _posting_data
        -- append_photos = \list -> over photos ((last list):) _posting_data 
        -- new_dd = \list -> Dialog_data { _stage = Posting Photos, _posting_data = append_photos list, .. }
    -- in case (photo.message $ upd) of 
         -- Just list -> (append_photos list,Just (chatID,new_dd list))
         -- _ -> (require_photos_req,Nothing)






-- class Process_dialog_stage a where
    -- process_stage :: Config -> UpdateResult -> a -> (HTTP.Request, Maybe (Chat_ID,Dialog_data) )
	
-- instance Process_dialog_stage Dialog_stage where
    -- process_stage cfg upd (Dialog_data { _stage = Posting HousingType, .. }) = 
        -- let chatID = get_chat_ID upd
            -- new_pd = \txt -> set housingType txt _posting_data
            -- new_dd = \txt -> Dialog_data { _time = (_current_time cfg), _stage = Posting Location, _posting_data = new_pd txt, .. }
    	-- in case (text.message $ upd) of 
              -- Nothing -> (enter_text_req cfg upd,Nothing)
              -- Just txt -> (housingTypeReq cfg upd,Just (chatID, new_dd $ T.take 50 txt))
			  