import Control.Monad ( liftM )
import Language.PhiPlot.Parser ( parsePhiplot )
import Text.PrettyPrint.GenericPretty

main :: IO ()
main = do
  result <- liftM parsePhiplot getContents
  case result of
    Right ast -> mapM_ pp ast
    Left  exp -> print exp