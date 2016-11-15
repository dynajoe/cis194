{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage m = case words m of
   ("I":d:lm) -> LogMessage Info (read d::Int) (unwords lm)
   ("E":l:d:lm) -> LogMessage (Error (read l::Int)) (read d::Int) (unwords lm)
   ("W":d:lm) -> LogMessage Warning (read d::Int) (unwords lm)
   _ -> Unknown m

parse :: String -> [LogMessage]
parse d = parseMessage <$> lines d

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) root = root
insert msg (Leaf) = Node Leaf msg Leaf
insert msg@(LogMessage _ nts _) (Node l lm@(LogMessage _ ts _) r) =
  case nts < ts of
    False -> Node l lm (insert msg r)
    True -> Node (insert msg l) lm r

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = [msg | (LogMessage (Error severity) _ msg) <- inOrder (build xs), severity >= 50]
