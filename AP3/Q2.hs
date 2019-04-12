-- Questão 2 -  AP 3
-- [IF686EC] - 2019.1
-- autor: jrsf e tasm2

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
  deriving (Eq, Show)

data Direction = ToNorth | ToSouth | ToWest | ToEast
  deriving (Eq, Show)

x = (Forward 2)

dropLastElement :: (Int, Int, Direction) -> (Int, Int)
dropLastElement (x, y, dir) = (x, y)

destinationDirection :: (Int, Int, Direction) -> [Command] -> (Int, Int, Direction)
destinationDirection (xStart, yStart, dir) [] = (xStart, yStart, ToNorth)
destinationDirection (xStart, yStart, dir) ((Forward y): xs) =  destinationDirection (tratarForward (xStart, yStart, dir) (Forward y)) xs
destinationDirection (xStart, yStart, dir) ((Backward y): xs) = destinationDirection (tratarBackward (xStart, yStart, dir) (Backward y)) xs 
destinationDirection (xStart, yStart, dir) (TurnLeft: xs) = destinationDirection (tratarTurnLeft (xStart, yStart, dir)) xs
destinationDirection (xStart, yStart, dir) (TurnRight: xs) = destinationDirection (tratarTurnRight (xStart, yStart, dir)) xs

tratarForward :: (Int, Int, Direction) -> Command -> (Int, Int, Direction)
tratarForward (x, y, dir) (Forward quant)
  | dir == ToNorth = (x, y + quant, ToNorth)
  | dir == ToSouth = (x, y - quant, ToSouth)
  | dir == ToWest = (x - quant, y, ToWest)
  | dir == ToEast = (x + quant, y, ToEast)

tratarBackward :: (Int, Int, Direction) -> Command -> (Int, Int, Direction)
tratarBackward (x, y, dir) (Backward quant) = tratarForward (x, y, dir) (Forward (-quant))

tratarTurnLeft :: (Int, Int, Direction) -> (Int, Int, Direction)
tratarTurnLeft (x, y, z)
  | z == ToNorth = (x, y, ToWest)
  | z == ToSouth = (x, y, ToEast)
  | z == ToWest = (x, y, ToSouth)
  | z == ToEast = (x, y, ToNorth)

tratarTurnRight :: (Int, Int, Direction) -> (Int, Int, Direction)
tratarTurnRight (x, y, z)
  | z == ToNorth = (x, y, ToEast)
  | z == ToSouth = (x, y, ToWest)
  | z == ToWest = (x, y, ToNorth)
  | z == ToEast = (x, y, ToSouth)

destination :: (Int, Int) -> [Command] -> (Int, Int)
destination (x, y) z = dropLastElement (destinationDirection (x, y, ToNorth) z)