module Days.Common where

rightOrDie :: Show b => Either b a -> a
rightOrDie x = case x of
  Left l -> error $ "bad : " ++ show l
  Right r -> r