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
