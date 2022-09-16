{-# LANGUAGE LambdaCase #-}

module Language.Haskell.HsTools.LinesDiff where

import qualified Data.Map as Map
import Control.Monad (guard)
import Data.Maybe
import Data.Algorithm.Diff
import Data.List
import Data.List.Split

import Debug.Trace

data SP = SP { spLine :: Int, spCol :: Int }
  deriving (Eq, Ord, Show, Read)

startSP = SP 1 1

data DSP = DSP { dspLine :: Int, dspCol :: Int, dspEndLine :: Int }
  deriving (Eq, Ord, Show, Read)

addDsp :: DSP -> SP -> SP
addDsp (DSP dl dc endLine) (SP line col)
  | endLine == line = SP (line + dl) (col + dc)
  | otherwise = SP (line + dl) col

subtractDsp :: DSP -> SP -> SP
subtractDsp (DSP dl dc el) (SP line col)
  | el + dl == line = SP (line - dl) (col - dc)
  | otherwise = SP (line - dl) col

-- right side must be below
addDsps :: DSP -> DSP -> DSP
addDsps (DSP dl1 dc1 el1) (DSP 0 dc2 el2) | el1 == el2 = DSP dl1 (dc1 + dc2) el2
addDsps (DSP dl1 dc1 el1) (DSP dl2 dc2 el2) = DSP (dl1 + dl2) dc2 el2

spDiff :: SP -> SP -> DSP
spDiff (SP l1 c1) (SP l2 c2) = DSP (l1 - l2) (c1 - c2) l2

type SourceDiffs = Map.Map SP SourceDiffData

type FileLines = [String]

data SourceRange = SourceRange { srStart :: SP, srEnd :: SP }
  deriving (Show, Read)

data SourceDiffData = SourceDiffData { sddEnd :: SP, sddReplacement :: DSP }
  deriving (Show, Read)

data SourceDiff = SourceDiff { sdStart :: SP, sdEnd :: SP, sdReplacement :: DSP }
  deriving (Show, Read)

data SourceRewrite = SourceRewrite { srwStart :: SP, srwEnd :: SP, srwReplacement :: String }
  deriving (Show, Read)

srcDiffList :: SourceDiffs -> [SourceDiff]
srcDiffList = map srcDiff . Map.toList

srcDiff :: (SP, SourceDiffData) -> SourceDiff
srcDiff (start, SourceDiffData end replacement) = SourceDiff start end replacement

srcDiffData :: SourceDiff -> SourceDiffData
srcDiffData (SourceDiff _ end replacement) = SourceDiffData end replacement

sdRange :: SourceDiff -> SourceRange
sdRange (SourceDiff start end _) = SourceRange start end

srRange :: SourceRewrite -> SourceRange
srRange (SourceRewrite start end _) = SourceRange start end

sdEndDiff :: SourceDiff -> DSP
sdEndDiff (SourceDiff start end replacement) 
  = DSP
    (dspLine replacement - (spLine end - spLine start))
    (dspCol replacement - (spCol end - spCol start))
    (spLine start)

concatRanges :: [SourceRange] -> SourceRange
concatRanges [] = error "concatRanges: empty"
concatRanges rngs = SourceRange (minimum $ map srStart rngs) (maximum $ map srEnd rngs)

newToOriginalRange :: SourceDiffs -> SourceRange -> SourceRange
newToOriginalRange diffs (SourceRange start end) = SourceRange newStart newEnd
  where
    newStart = either srStart id $ newToOriginalPos diffs start
    newEnd = either srEnd id $ newToOriginalPos diffs end

newToOriginalRangeStrict :: SourceDiffs -> SourceRange -> Maybe SourceRange
newToOriginalRangeStrict diffs (SourceRange start end)
  = case (newToOriginalPos diffs start, newToOriginalPos diffs end) of
      (Right newStart, Right newEnd) -> Just $ SourceRange newStart newEnd
      _ -> Nothing

newToOriginalPos :: SourceDiffs -> SP -> Either SourceRange SP
newToOriginalPos diffs pos@(SP l c) = maybe (Right newPos) Left rewriterRange
  where
    newPos = subtractDsp posDiff pos 
    posDiff = case changesBefore of
      [] -> DSP 0 0 l
      ls -> snd $ last ls
    (changesBefore, changesNotBefore) = span (\(diff, dsp) -> pos > addDsp dsp (sdEnd diff)) $ sourceDiffCarry $ map srcDiff $ Map.toList diffs
    rewriterRange = do
      (SourceDiff start end _, dsp) <- listToMaybe changesNotBefore
      guard (addDsp posDiff start <= pos && pos <= addDsp dsp end)
      return (SourceRange start end)

sourceDiffCarry :: [SourceDiff] -> [(SourceDiff, DSP)]
sourceDiffCarry = go (DSP 0 0 1)
  where
    go :: DSP -> [SourceDiff] ->  [(SourceDiff, DSP)]
    go posDiff (diff : rest) 
      = let newPosDiff = addDsps posDiff (sdEndDiff diff) in (diff, newPosDiff) : go newPosDiff rest
    go _ [] = []

originalToNewRange :: SourceDiffs -> SourceRange -> SourceRange
originalToNewRange diffs (SourceRange start end) = SourceRange newStart newEnd
  where
    newStart = either srStart id $ originalToNewPos diffs start
    newEnd = either srEnd id $ originalToNewPos diffs end

originalToNewRangeStrict :: SourceDiffs -> SourceRange -> Maybe SourceRange
originalToNewRangeStrict diffs (SourceRange start end)
  = case (originalToNewPos diffs start, originalToNewPos diffs end) of
      (Right newStart, Right newEnd) -> Just $ SourceRange newStart newEnd
      _ -> Nothing

originalToNewPos :: SourceDiffs -> SP -> Either SourceRange SP
originalToNewPos diffs pos@(SP l c) = maybe (Right newPos) Left rewriterRange
  where
    newPos = addDsp posDiff pos 
    posDiff = case changesBefore of
      [] -> DSP 0 0 l
      ls -> snd $ last ls
    (changesBefore, changesNotBefore) = span ((pos >) . sdEnd . fst) $ sourceDiffCarry $ map srcDiff $ Map.toList diffs
    rewriterRange = do
      (SourceDiff start end _, dsp) <- listToMaybe changesNotBefore
      guard (start <= pos && pos <= end)
      return (SourceRange (addDsp posDiff start) (addDsp dsp end))

touchingChanges :: SourceDiffs -> SourceRange -> (SourceDiffs, SourceDiffs)
touchingChanges diffs (SourceRange start end) = (intersecting, nonIntersecting)
  where
    intersecting = Map.union singletonSame $ Map.fromAscList (reverse changes)
    nonIntersecting = Map.union after (Map.fromAscList (reverse before))
    (changes, before) = span ((start <=) . sddEnd . snd) $ Map.toDescList notAfter
    singletonSame = maybe Map.empty (Map.singleton end) same
    (notAfter, same, after) = Map.splitLookup end diffs

-- to get the best possible results we re-diff the part of the source code that is affected by the change
-- extending to encompass all existing changes around it
addExtraChange :: FileLines -> SourceRewrite -> (FileLines, SourceDiffs) -> (FileLines, SourceDiffs)
addExtraChange compiledContent rewrite@(SourceRewrite start end replacement) (actualContent, diffs) 
  = (newContent, newDiffs)
  where
    newDiffs
      = if null affectedDiffs
          then Map.insert start (SourceDiffData end replacementDSP) diffs
          else Map.union reDiffs unaffectedDiffs
    reDiffs = sourceDiffs
                (srStart origRangeToReDiff)
                (intercalate "\n" $ takeRange origRangeToReDiff compiledContent)
                (intercalate "\n" $ takeRange newestRangeToReDiff newContent)
    newContent = applySourceDiff rewrite actualContent
    newestRangeToReDiff = originalToNewRange singletonDiffs $ originalToNewRange diffs origRangeToReDiff
    origRangeToReDiff = concatRanges $ origRange : map sdRange (srcDiffList affectedDiffs)
    (affectedDiffs, unaffectedDiffs) = touchingChanges diffs origRange
    singletonDiffs = Map.singleton start (SourceDiffData end replacementDSP)
    origRange = newToOriginalRange diffs $ srRange rewrite
    replacementDSP = spDiff (spAdvanceStr start replacement) start

concatFileLines :: [FileLines] -> FileLines
concatFileLines = foldl (\a b -> init a ++ [last a ++ head b] ++ tail b) [""] . filter (not . null)

applySourceDiff :: SourceRewrite -> FileLines -> FileLines
applySourceDiff (SourceRewrite start@(SP sl sc) end replacement) lns
  = concatFileLines [prefix, splitOn "\n" replacement, postfix]
  where
    (_, postfix) = breakAtDPos start diff rest
    (prefix, rest) = breakAtPos start lns
    diff = spDiff end start

takeRange :: SourceRange -> FileLines -> FileLines
takeRange (SourceRange start end) lns = fst $ breakAtDPos start takenSection $ snd $ breakAtPos start lns
  where takenSection = spDiff end start

breakAtPos :: SP -> FileLines -> (FileLines, FileLines)
breakAtPos (SP l c) = breakAtDPos startSP (DSP (l - 1) (c - 1) l)

breakAtDPos :: SP -> DSP -> FileLines -> (FileLines, FileLines)
breakAtDPos (SP _ c) (DSP dl dc _) lns = case rightMidLines of
    [] -> ([], [])
    ll : rest -> case splitAt dCols ll of (leftChars, rightChars) -> (leftLines ++ [leftChars], rightChars : rest)
  where
    (leftLines, rightMidLines) = splitAt dl lns
    dCols = if dl == 0 then dc else (c - 1) + dc

sourceDiffs :: SP -> String -> String -> SourceDiffs
sourceDiffs start original modified = Map.fromAscList $ go start $ getGroupedDiff original modified
  where
    go :: SP -> [Diff String] -> [(SP, SourceDiffData)]
    go pos (Both x _ : rest) = go (spAdvanceStr pos x) rest
    go pos (First a : rest) = let endPos = spAdvanceStr pos a in (pos, SourceDiffData endPos (DSP 0 0 (spLine endPos))) : go endPos rest
    go pos (Second a : rest) = (pos, SourceDiffData pos (getDiffPos pos a)) : go pos rest
    go _ [] = []

getDiffPos :: SP -> String -> DSP
getDiffPos pos str = spDiff (spAdvanceStr pos str) pos

spAdvanceStr :: SP -> String -> SP
spAdvanceStr = foldl spAdvanceChar

spAdvanceChar :: SP -> Char -> SP
spAdvanceChar (SP lineNo colNo) = \case
  '\n' -> SP (lineNo + 1) 1
  '\t' -> SP lineNo (((colNo - 1) `div` 8 + 1) * 8 + 1) -- FIXME: tabs will make a mess when further changes arrive
  _ -> SP lineNo (colNo + 1)