{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module TgUpdatesTypes where

import Data.Aeson
import Data.Hashable
import GHC.Generics ( Generic )


newtype Chat_ID = Chat_ID Int    deriving ( Eq, Show, Read, Generic, Hashable )
show_chatID :: Chat_ID -> String
show_chatID (Chat_ID id) = show id
	
type Update_ID = Int



data Response = Response { ok :: Bool,
                           result :: [UpdateResult] }   		deriving ( Eq, Show, Read, Generic )
						   
data UpdateResult = UpdateResult { update_id :: Int,
								   message :: Message }  		deriving ( Eq, Show, Read, Generic )

data Message = Message { message_id :: Int,
						 from :: From,
						 chat :: Chat,
						 date :: Int,
						 text :: Maybe Text,
						 photo :: Maybe [PhotoData],
					     entities :: Maybe [Entity] }  		deriving ( Eq, Show, Read, Generic )

data PhotoData = PhotoData { file_id :: Text,
                             file_unique_id :: Text,
							 file_size :: Int }  		deriving ( Eq, Show, Read, Generic )

data From = From { id :: Int,
				   is_bot :: Bool,
				   first_name :: Text,
				   username :: Text,
				   language_code :: Text }  		deriving ( Eq, Show, Read, Generic )

data Chat = Chat { id :: Chat_ID,
				   first_name :: Text,
				   username :: Text,
				   chat_type :: Text  }          deriving ( Eq, Show, Read, Generic )
				   
data Entity = Entity { offset :: Int,
					   e_length :: Int,
					   entity_type :: Text }  		deriving ( Eq, Show, Read, Generic )

class Chat_ID_Gettable a where
    get_chat_ID :: a -> Chat_ID
	
instance Chat_ID_Gettable Chat where
    get_chat_ID ( Chat {id} ) = id

instance Chat_ID_Gettable Message where
    get_chat_ID ( Message {chat} ) = get_chat_ID chat
	
instance Chat_ID_Gettable UpdateResult where
    get_chat_ID ( UpdateResult {message} ) = get_chat_ID message
			
			
instance FromJSON Response		
instance FromJSON UpdateResult	   
instance FromJSON Message
instance FromJSON From
instance FromJSON PhotoData


instance FromJSON Chat where 	
    parseJSON = withObject "Chat" $ \o -> do
      id    <- o .: "id"
      first_name <- o .: "first_name"
      username <- o .: "username"
      chat_type <- o .: "type"
      return Chat{id = Chat_ID id,..}
		
		
instance FromJSON Entity where 
    parseJSON = withObject "Entity" $ \v -> Entity
        <$> v .: "offset"
        <*> v .: "length"
        <*> v .: "type"