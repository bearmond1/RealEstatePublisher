module Publishing_Requests where

import           Config
import           TgUpdatesTypes
import           Data.ByteString.Char8 ( unpack )
import           Network.HTTP.Simple as HTTP hiding ( Response )


enter_text_req :: Config -> UpdateResult -> HTTP.Request
enter_text_req = undefined


explanation_text = "Через этот бот вы можете опубликовать свое объявление - команда '/publish', или найти объявление - команда '/query'.  "

housing_text = "Укажите тип жилья : комната, квартра, дом..."

location_text = "Укажите адрес или район"

unknown_command_text = "Нераспознанная команда"

demand_photos_text = "Добавьте хотя бы 4 фото"

description_text = "Расскажите подробней: есть ли интернет, постельное белье, возможно соседи - все релевантные подробности - до 500 символов"


text_req :: Config -> UpdateResult -> Text -> HTTP.Request
text_req (Config { _token }) upd text = 
   let req = "https://api.telegram.org/bot" ++ _token ++
              "/sendMessage?chat_id="       ++ show_chatID (get_chat_ID upd) ++ 
              "&text="                      ++ text
   in  parseRequest_ req


explanation_request :: Config -> UpdateResult -> HTTP.Request
explanation_request (Config { _token }) upd = 
   let req = "https://api.telegram.org/bot" ++ _token ++
              "/sendMessage?chat_id="       ++ show_chatID (get_chat_ID upd) ++ 
              "&text="                      ++ "Через этот бот вы можете опубликовать свое объявление - команда '/publish', или найти объявление - команда '/query'.  "
   in  parseRequest_ req
housingTypeReq :: Config -> UpdateResult -> HTTP.Request
housingTypeReq (Config{ _token }) upd = 
   let req = "https://api.telegram.org/bot" ++ _token ++
              "/sendMessage?chat_id="       ++ show_chatID (get_chat_ID upd) ++ 
              "&text="                      ++ "Укажите тип жилья : комната, квартра, дом..."
   in setRequestMethod "POST" $ parseRequest_ req   
locationReq :: Config -> UpdateResult -> HTTP.Request
locationReq (Config{ _token }) upd = 
   let req = "https://api.telegram.org/bot" ++ _token ++
              "/sendMessage?chat_id="       ++ show_chatID (get_chat_ID upd)   ++ 
              "&text="                      ++ "Укажите адрес или район"
   in  parseRequest_ req  
unknownRequestType :: Config -> UpdateResult -> HTTP.Request
unknownRequestType (Config{ _token }) upd = 
   let req = "https://api.telegram.org/bot" ++ _token ++
              "/sendMessage?chat_id="       ++ show_chatID (get_chat_ID upd)  ++ 
              "&text="                      ++ "Нераспознанная команда"
   in  parseRequest_    
demand_photos :: Config -> UpdateResult -> HTTP.Request
demand_photos (Config{ _token }) upd = 
   let req = "https://api.telegram.org/bot" ++ _token ++
              "/sendMessage?chat_id="       ++ show_chatID (get_chat_ID upd)  ++ 
              "&text="                      ++ "Добавьте хотя бы 4 фото"
   in  parseRequest_ req   
desription_req :: Config -> UpdateResult -> HTTP.Request
desription_req (Config{ _token }) upd = 
   let req = "https://api.telegram.org/bot" ++ _token ++
              "/sendMessage?chat_id="       ++ show_chatID (get_chat_ID upd)  ++ 
              "&text="                      ++ "Расскажите подробней: есть ли интернет, постельное белье, возможно соседи - все релевантные подробности - до 500 символов"
   in  parseRequest_ req