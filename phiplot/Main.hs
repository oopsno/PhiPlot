import Control.Lens ((^.))
import Control.Monad (when)
import Language.PhiPlot.Interpreter (runProgram, variables)
import Language.PhiPlot.Parser (parsePhiplot)
import Options.Applicative
import Text.PrettyPrint.GenericPretty (pp)

data Arguments = Arguments
  { inputFile :: FilePath,
    outputFile :: FilePath,
    verbose :: Bool
  }
  deriving (Eq, Show)

arguments :: Parser Arguments
arguments =
  Arguments
    <$> strOption
      ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "Path to input script"
      )
    <*> strOption
      ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Path to output image"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Whether to be verbose"
      )

interpreter :: Arguments -> IO ()
interpreter args = do
  code <- readFile (inputFile args)
  case parsePhiplot code of
    Left error -> putStrLn $ show error
    Right stmts -> do
      when (verbose args) $ pp stmts
      result <- runProgram (outputFile args) stmts
      case result of
        Left error -> putStrLn $ show error
        Right state -> when (verbose args) (print $ state ^. variables)

main :: IO ()
main = execParser opts >>= interpreter
  where
    opts =
      info
        (arguments <**> helper)
        (fullDesc <> progDesc "The PhiPlot Interpreter")