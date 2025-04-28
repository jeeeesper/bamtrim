import Control.Monad (unless, when)
import Control.Monad qualified
import Data.Char (isDigit)
import Data.List (intercalate)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, isEOF, stderr, stdin)

type CigarOp = (Char, Int)

-- Parse CIGAR string
parseCigar :: String -> [CigarOp]
parseCigar "" = []
parseCigar s =
  let (num, rest) = span isDigit s
      (op : rest') = rest
   in (op, read num) : parseCigar rest'

-- Parse region string "chr:start-end"
parseRegion :: String -> (String, Int, Int)
parseRegion s =
  let (chr, rest1) = span (/= ':') s
      rest2 = drop 1 rest1 -- drop ':'
      (start, rest3) = span (/= '-') rest2
      end = drop 1 rest3 -- drop '-'
   in (chr, read start, read end)

-- Main extraction logic: Walk along cigar string and include the query string once and as long as it is in the specified range
extractSubseq :: Int -> Int -> String -> Int -> [CigarOp] -> String
extractSubseq regionStart regionEnd = go
  where
    go _ _ [] = "" -- no cigars left, assume we're done
    go _ refPos cigs | refPos > regionEnd = "" -- past the end of the extracted region; done
    go seq' refPos allCigs@((op, len) : cigs) =
      case op of
        'M' -> consumeQueryString True
        '=' -> consumeQueryString True
        'X' -> consumeQueryString True
        'I' -> consumeQueryString False
        'S' -> consumeQueryString False
        'D' -> go seq' (refPos + len) cigs
        'N' -> go seq' (refPos + len) cigs
        'H' -> go seq' refPos cigs
        _ -> error $ "Unhandled CIGAR op: " ++ [op]
      where
        consumeQueryString advanceRef =
          let stringStart = max refPos regionStart
              stringEnd = min (refPos + len - 1) regionEnd
              stringOffset = stringStart - refPos
              stringLen = stringEnd - stringStart + 1
           in (take stringLen . drop stringOffset $ seq')
                ++ go (drop (stringOffset + stringLen) seq') (refPos + if advanceRef then len else 0) cigs

-- Parse the SAM line, call `extractSubseq` and output the results in FASTA-format.
processRead :: String -> (String, Int, Int) -> IO ()
processRead line (rname, start, end) = do
  let fields = words line
  let qname = head fields
  let pos = read (fields !! 3) :: Int
  let cigar = fields !! 5
  let seq' = fields !! 9

  let cigs = parseCigar cigar
  let refStart = pos -- SAM POS is 1-based
  let extracted = extractSubseq start end seq' refStart cigs

  unless (null extracted) $ do
    putStrLn $ ">" ++ qname ++ "_" ++ rname ++ ":" ++ show start ++ "-" ++ show end
    putStrLn extracted

main :: IO ()
main = do
  args <- getArgs
  case args of
    [region] -> do
      let reg = parseRegion region
      loop reg
    _ -> do
      progName <- getProgName
      hPutStrLn stderr $ "Usage: " ++ progName ++ " <REGION>"
      hPutStrLn stderr   "       <REGION> is a region definition in the format `chr1:123-456`"
      hPutStrLn stderr   "       SAM input is read from stdin, line by line."
      exitFailure

-- Loop over input lines (each line is one read) and call `processRead` for every line.
loop :: (String, Int, Int) -> IO ()
loop reg = do
  done <- isEOF
  if done
    then return ()
    else do
      line <- getLine
      when (line /= "") $ processRead line reg
      loop reg
