import Language.PhiPlot.Interpreter (runProgram)
import Language.PhiPlot.Parser
import Text.PrettyPrint.GenericPretty (pp)

main :: IO ()
main = do
  source <- getContents
  case parsePhiplot source of
    Left error -> putStrLn $ show error
    Right program -> do
      result <- runProgram program
      case result of
        Left rte -> putStrLn $ show rte
        Right env -> print env