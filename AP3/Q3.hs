-- Questão 3 -  AP 3
-- [IF686EC] - 2019.1
-- autor: jrsf e tasm2

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
  deriving (Eq, Show)

data Direction = ToNorth | ToSouth | ToWest | ToEast
  deriving (Eq, Show)

faces :: Direction -> [Command] -> Direction
faces dir [] = dir
faces dir ((Forward y): xs) = faces dir xs
faces dir ((Backward y): xs) = faces dir xs
faces dir (TurnLeft:xs) = faces (tratarTurnLeft dir) xs
faces dir (TurnRight:xs) = faces (tratarTurnRight dir) xs

tratarTurnLeft :: Direction -> Direction
tratarTurnLeft z
  | z == ToNorth = ToWest
  | z == ToSouth = ToEast
  | z == ToWest = ToSouth
  | z == ToEast = ToNorth

tratarTurnRight :: Direction -> Direction
tratarTurnRight z
  | z == ToNorth = ToEast
  | z == ToSouth = ToWest
  | z == ToWest = ToNorth
  | z == ToEast = ToSouth