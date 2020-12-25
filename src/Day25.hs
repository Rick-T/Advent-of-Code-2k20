module Day25 where

part1 :: Integer
part1 = encryptionKey (loopSize pkCard) pkDoor

part2 :: String
part2 = "⭐"

encryptionKey :: Int -> Integer -> Integer
encryptionKey loopSize publicKey = apply loopSize publicKey 1

loopSize :: Integer -> Int
loopSize publicKey = length $ takeWhile (/= publicKey) $ iterate (apply 1 7) 1

apply :: Int -> Integer -> Integer -> Integer
apply loops subjectNumber value = (subjectNumber ^ loops * value) `rem` 20201227

pkCard :: Integer
pkCard = 10604480

pkDoor :: Integer
pkDoor = 4126658