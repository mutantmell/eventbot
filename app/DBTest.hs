module DBTest where

data TestField = TestField Int String deriving (Show)

instance SQL.FromRow TestField where
    fromRow = TestField <$> SQL.field <*> SQL.field

-- https://github.com/nurpax/sqlite-simple/blob/master/test/Simple.hs

main :: IO ()
main = do
    conn <- SQL.open "test.db"
    SQL.execute_ conn "CREATE TABLE servers (id INTEGER PRIMARY KEY, server_id TEXT, channel_id TEXT)"
    SQL.execute_ conn "CREATE TABLE messages (id INTEGER PRIMARY KEY, message_id TEXT, message TEXT)"
    -- SQL.execute conn "INSERT INTO test (str) VALUES (?)"
    --   (SQL.Only ("test string 2" :: String))
    -- r <- SQL.query_ conn "SELECT * from test" :: IO [TestField]
    -- mapM_ print r
    SQL.close conn
