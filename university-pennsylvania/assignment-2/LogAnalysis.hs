module LogAnalysis where

import Log

interpret :: [String] -> LogMessage
interpret ("I":timestamp:message) = LogMessage Info (read timestamp) (unwords message)
interpret ("W":timestamp:message) = LogMessage Warning (read timestamp) (unwords message)
interpret ("E":severity:timestamp:message) = LogMessage (Error (read severity)) (read timestamp) (unwords message)
interpret line = Unknown (unwords line)

parseMessage :: String -> LogMessage
parseMessage line = interpret (words line)

parse :: String -> [LogMessage]
parse content = map parseMessage (lines content)

compareTimestamp :: LogMessage -> LogMessage -> Int
compareTimestamp (LogMessage _ timestampOne _) (LogMessage _ timestampTwo _) = timestampOne - timestampTwo

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage (Node leftTree nodeMessage rightTree)
    | (compareTimestamp logMessage nodeMessage) < 0 = Node (insert logMessage leftTree) nodeMessage rightTree
    | otherwise = Node leftTree nodeMessage (insert logMessage rightTree)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (logMessage:otherMessages) = insert logMessage (build otherMessages)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree logMessage rightTree) = (inOrder leftTree) ++ [logMessage] ++ (inOrder rightTree)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ((LogMessage (Error severity) _ message):otherMessages)
    | severity >= 50 = [message] ++ (whatWentWrong otherMessages)
    | otherwise = whatWentWrong otherMessages
whatWentWrong (nonErrorMessage:otherMessages) = whatWentWrong otherMessages
whatWentWrong _ = []
