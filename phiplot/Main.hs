import Control.Monad ( liftM )
import Language.PhiPlot.Parser ( parsePhiplot )

main :: IO ()
main = liftM parsePhiplot getContents >>= print
