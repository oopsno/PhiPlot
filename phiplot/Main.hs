import Language.PhiPlot.Desugar (desugar)
import Language.PhiPlot.Parser (parsePhiplot)
import Text.PrettyPrint.GenericPretty (pp)

main :: IO ()
main = do
  source <- getContents
  case desugar <$> parsePhiplot source of
    Right ast -> pp ast
    Left exp -> print exp