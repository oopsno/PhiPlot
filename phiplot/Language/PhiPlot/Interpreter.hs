{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Language.PhiPlot.Interpreter (runProgram) where

import Codec.Picture.Png (writePng)
import Codec.Picture.Types (MutableImage, Pixel8, PixelRGB8 (..), freezeImage, newMutableImage, writePixel)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Data.IORef
import qualified Data.Map as Map
import Foreign.C (isValidErrno)
import GHC.Base (Levity (Lifted), VecElem (DoubleElemRep))
import Language.PhiPlot.AST (Stmt (condition))
import qualified Language.PhiPlot.AST as A
import Language.PhiPlot.Simplify (simplify)

data Value
  = VScalar Double
  | VPair Double Double
  | VUnit
  deriving (Eq, Show)

data Error
  = Unbound String
  | TypeErr String
  | UnknownFn String
  | ArityErr String Int Int -- 名字、期望个数、实际个数
  | Internal String
  | RuntimeError String
  deriving (Eq, Show)

type Env = Map.Map A.Name Value

type ImageRef = IORef (Maybe (MutableImage RealWorld PixelRGB8))

newtype Canvas = Canvas {imageRef :: ImageRef}

type StmtM a = ReaderT Canvas (StateT Env (ExceptT Error IO)) a

data Flow = Normal | Break | Return Value

symOrigin :: String
symOrigin = "origin"

symScale :: String
symScale = "scale"

symCanvasSize :: String
symCanvasSize = "canvasSize"

symRot :: String
symRot = "rot"

createEnv :: Env
createEnv =
  Map.fromList
    [ (symOrigin, (VPair 0 0)),
      (symScale, (VPair 1 1)),
      (symRot, (VScalar 0)),
      (symCanvasSize, (VPair 1024 1024)),
      ("pi", (VScalar pi)),
      ("e", (VScalar $ Prelude.exp 1))
    ]

asDouble :: Value -> Either Error Double
asDouble (VScalar x) = Right x
asDouble otherwise = Left . TypeErr $ "Cannot cast expr as double: " ++ (show otherwise)

asDoubleM :: Value -> StmtM Double
asDoubleM (VScalar x) = pure x
asDoubleM otherwise = throwError . TypeErr $ "Cannot cast expr as double: " ++ (show otherwise)

readOriginM :: StmtM (Double, Double)
readOriginM = do
  env <- get
  case Map.lookup "origin" env of
    Just (VPair x y) -> pure (x, y)
    Nothing -> throwError $ Internal "Symbol not defined: origin"

readScaleM :: StmtM (Double, Double)
readScaleM = do
  env <- get
  case Map.lookup symScale env of
    Just (VPair x y) -> pure (x, y)
    Nothing -> throwError $ Unbound "Symbol not defined: scale"

readCanvasSizeM :: StmtM (Int, Int)
readCanvasSizeM = do
  env <- get
  case Map.lookup "canvasSize" env of
    Just (VPair x y) -> pure (floor x, floor y)
    Nothing -> throwError $ Unbound "Symbol not defined: canvasSize"

readRotM :: StmtM Double
readRotM = do
  env <- get
  case Map.lookup "rot" env of
    Just (VScalar r) -> pure r
    Nothing -> throwError $ Unbound "Variable not defined: rot"

data DrawParams = DrawParams
  { origin :: (Double, Double),
    scale :: (Double, Double),
    rot :: Double,
    canvasSize :: (Int, Int)
  }

readDrawParams :: StmtM DrawParams
readDrawParams = do
  origin <- readOriginM
  scale <- readScaleM
  rot <- readRotM
  canvasSize <- readCanvasSizeM
  pure $ DrawParams origin scale rot canvasSize

transformPoint :: DrawParams -> (Double, Double) -> Maybe (Int, Int)
transformPoint param (x, y) =
  if inbound then Just (x''', y''') else Nothing
  where
    (ox, oy) = origin param
    (sx, sy) = scale param
    (cx, cy) = canvasSize param
    theta = rot param
    -- 比例变换
    x' = x * sx
    y' = y * sy
    -- 旋转变换
    x'' =   x' * cos theta + y' * sin theta
    y'' = - x' * sin theta + y' * cos theta
    -- 平移变换
    x''' = round $ ox + x''
    y''' = round $ oy + y''
    inbound = (0 <= x''' && x''' < cx) && (0 <= y''' && y''' < cy)

data Builtin = Builtin
  { arity :: Int,
    run :: [Value] -> StmtM Value
  }

unary :: (Value -> StmtM Value) -> Builtin
unary f = Builtin 1 $ \case
  [v] -> f v
  arg -> throwError $ ArityErr "Wrong number of arguments" 1 (length arg)

binary :: (Value -> Value -> StmtM Value) -> Builtin
binary f = Builtin 2 $ \case
  [x, y] -> f x y
  arg -> throwError $ ArityErr "Wrong number of arguments" 2 (length arg)

wrapUnaryMathFunction :: (Double -> Double) -> Builtin
wrapUnaryMathFunction f = unary (either throwError pure . g)
  where
    g v = do
      x <- asDouble v
      pure . VScalar $ f x

wrapBinaryMathFunction :: (Double -> Double -> Double) -> Builtin
wrapBinaryMathFunction f = binary g
  where
    g :: Value -> Value -> StmtM Value
    g x y = do
      x' <- asDoubleM x
      y' <- asDoubleM y
      pure . VScalar $ f x' y'

builtinPrint :: Builtin
builtinPrint = unary $ \x -> do
  liftIO $ putStrLn $ show x
  pure VUnit

withImage :: (MutableImage RealWorld PixelRGB8 -> StmtM Value) -> StmtM Value
withImage k = do
  Canvas ref <- ask
  mi <- liftIO (readIORef ref)
  case mi of
    Nothing -> do
      (w, h) <- readCanvasSizeM
      img <- liftIO $ newMutableImage w h
      liftIO $ writeIORef ref $ Just img
      k img
    Just img -> k img

builtinDraw :: Builtin
builtinDraw = binary draw
  where
    draw :: Value -> Value -> StmtM Value
    draw x y = do
      p <- readDrawParams
      x' <- asDoubleM x
      y' <- asDoubleM y
      case transformPoint p (x', y') of
        Just (ix, iy) -> do
          withImage $ \img -> do
            liftIO $ writePixel img ix iy (PixelRGB8 255 255 255)
            pure VUnit
        Nothing -> pure VUnit

saveImage path = withImage $ \img -> do
  liftIO $ do
    frozen <- freezeImage img
    writePng path frozen
    pure VUnit

builtins :: Map.Map String Builtin
builtins =
  Map.fromList
    [ ("sin", wrapUnaryMathFunction sin),
      ("cos", wrapUnaryMathFunction cos),
      ("tan", wrapUnaryMathFunction tan),
      ("sqrt", wrapUnaryMathFunction sqrt),
      ("ln", wrapUnaryMathFunction log),
      ("log", wrapUnaryMathFunction log),
      ("exp", wrapUnaryMathFunction exp),
      ("pow", wrapBinaryMathFunction (**)),
      ("print", builtinPrint),
      ("draw", builtinDraw)
    ]

evalExprM :: A.Expr -> StmtM Value
evalExprM (A.Var name) = do
  env <- get
  case Map.lookup name env of
    Just v -> pure v
    Nothing -> throwError $ Unbound ("Symbol not defined: " ++ name)
evalExprM (A.Imm v) = pure $ VScalar v
evalExprM (A.Pair x y) = do
  env <- get
  x' <- evalExprM x >>= asDoubleM
  y' <- evalExprM y >>= asDoubleM
  pure $ VPair x' y'
evalExprM (A.UniOp op expr) = do
  value <- evalExprM expr >>= asDoubleM
  case op of
    A.Negative -> pure $ VScalar (-value)
    A.Positive -> pure $ VScalar (value)
evalExprM (A.BinOp op lhs rhs) = do
  lhs' <- evalExprM lhs >>= asDoubleM
  rhs' <- evalExprM rhs >>= asDoubleM
  case op of
    A.Plus -> pure . VScalar $ lhs' + rhs'
    A.Minus -> pure . VScalar $ lhs' - rhs'
    A.Mul -> pure . VScalar $ lhs' * rhs'
    A.Div -> pure . VScalar $ lhs' / rhs'
    A.Pow -> pure . VScalar $ lhs' ** rhs'
evalExprM (A.Call fname args) = do
  f <- maybe (throwError (UnknownFn fname)) pure (Map.lookup fname builtins)
  a <- mapM evalExprM args
  run f a

evalBoolExprM :: A.BoolExpr -> StmtM Bool
evalBoolExprM (A.BoolAtom x) = pure x
evalBoolExprM (A.Cmp op lhs rhs) = do
  lhs' <- evalExprM lhs >>= asDoubleM
  rhs' <- evalExprM rhs >>= asDoubleM
  pure $ case op of
    A.LT -> lhs' < rhs'
    A.LE -> lhs' <= rhs'
    A.EQ -> lhs' == rhs'
    A.NE -> lhs' /= rhs'
    A.GE -> lhs' >= rhs'
    A.GT -> lhs' > rhs'
evalBoolExprM (A.LogicOp op lhs rhs) = do
  lhs' <- evalBoolExprM lhs
  rhs' <- evalBoolExprM rhs
  pure $ case op of
    A.AND -> lhs' && rhs'
    A.OR -> lhs' || rhs'
evalBoolExprM (A.Not expr) = fmap not $ evalBoolExprM expr
evalBoolExprM (A.Nonzero expr) = do
  value <- evalExprM expr >>= asDoubleM
  pure $ value /= 0

evalStmtM :: A.Stmt -> StmtM Flow
evalStmtM (A.Assign var expr) = do
  env <- get
  value <- evalExprM expr
  modify $ Map.insert var value
  pure Normal
evalStmtM (A.For var start end step body) = do
  vstart <- evalExprM start >>= asDoubleM
  vend <- evalExprM end >>= asDoubleM
  vstep <- evalExprM step >>= asDoubleM
  loop vstart vend vstep
  where
    loop :: Double -> Double -> Double -> StmtM Flow
    loop v vend vstep
      | vstep > 0 && v >= vend = pure Normal
      | vstep < 0 && v <= vend = pure Normal
      | otherwise = do
          modify $ Map.insert var (VScalar v)
          flow <- evalStmtM body
          case flow of
            Break -> pure Break
            _ -> loop (v + vstep) vend vstep
evalStmtM (A.If c t f) = do
  c' <- evalBoolExprM c
  if c' then evalStmtM t else evalStmtM f
evalStmtM (A.Block stmts) = it stmts
  where
    it [] = pure Normal
    it (s : rest) = do
      f <- evalStmtM s
      case f of
        Normal -> it rest
        _ -> pure f
evalStmtM A.Break = pure Break
evalStmtM (A.Return expr) = do
  value <- evalExprM expr
  pure $ Return value
evalStmtM (A.AExp expr) = do
  evalExprM expr
  pure Normal
evalStmtM _ = pure Normal

runProgram :: [A.Stmt] -> IO (Either Error Env)
runProgram stmts = do
  ref <- newIORef Nothing
  let canvas = Canvas ref
  let ss = simplify stmts
  let prog = mapM_ evalStmtM ss >> (saveImage "canvas.png")
  result <- runExceptT (runStateT (runReaderT prog canvas) createEnv)
  pure (fmap snd result)