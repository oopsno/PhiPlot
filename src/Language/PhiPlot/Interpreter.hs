{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.PhiPlot.Interpreter where

import Codec.Picture.Png (writePng)
import Codec.Picture.Types (MutableImage, Pixel8, PixelRGB8 (..), freezeImage, newMutableImage, writePixel)
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Primitive (RealWorld, PrimMonad, PrimState)
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State
import Data.IORef
import qualified Data.Map as Map
import Language.PhiPlot.AST (Stmt (condition))
import qualified Language.PhiPlot.AST as A
import Language.PhiPlot.Simplify (simplify)

data Value
  = VScalar Double
  | VBool Bool
  | VPair Double Double
  | VString String
  | VClosure Closure
  | VUnit
  deriving (Eq, Show)

data Closure = Closure
  { _clParams :: [A.Name],
    _clBody :: Stmt
  }
  deriving (Eq, Show)

makeLenses ''Closure

data Error
  = Unbound String
  | TypeErr String
  | UnknownFn String
  | ArityErr String Int Int -- 名字、期望个数、实际个数
  | Internal String
  | RuntimeError String
  deriving (Eq, Show)

data EvalState = EvalState
  { _variables :: [Map.Map A.Name Value],
    _functions :: Map.Map A.Name Value
  }
  deriving (Eq, Show)

makeLenses ''EvalState

type ImageRef = IORef (Maybe (MutableImage RealWorld PixelRGB8))

newtype Canvas = Canvas
  { imageRef :: ImageRef
  }

type EvalM a = ReaderT Canvas (StateT EvalState (ExceptT Error IO)) a

data Flow = Normal | Break | Return Value
  deriving (Eq, Show)

symOrigin :: String
symOrigin = "origin"

symScale :: String
symScale = "scale"

symCanvasSize :: String
symCanvasSize = "canvassize"

symRot :: String
symRot = "rot"

emptyState :: EvalState
emptyState = EvalState [variables] functions
  where
    variables =
      Map.fromList
        [ (symOrigin, (VPair 0 0)),
          (symScale, (VPair 1 1)),
          (symRot, (VScalar 0)),
          (symCanvasSize, (VPair 1024 1024)),
          ("pi", (VScalar pi)),
          ("e", (VScalar $ Prelude.exp 1))
        ]
    functions = Map.empty

lookupVar :: A.Name -> EvalState -> Maybe Value
lookupVar var = preview (variables . traverse . ix var)

lookupVarM :: A.Name -> EvalM Value
lookupVarM var = do
  vars <- use variables
  case preview (traverse . ix var) vars of
    Just value -> pure value
    Nothing -> throwError $ Unbound var

assignVar :: String -> Value -> EvalState -> EvalState
assignVar x v = variables %~ update
  where
    update [] = [Map.singleton x v]
    update (s : ss)
      | Map.member x s = Map.insert x v s : ss
      | otherwise = s : update ss

asDouble :: Value -> Either Error Double
asDouble (VScalar x) = Right x
asDouble otherwise = Left . TypeErr $ "Cannot cast expr as double: " ++ (show otherwise)

asDoubleM :: Value -> EvalM Double
asDoubleM (VScalar x) = pure x
asDoubleM otherwise = throwError . TypeErr $ "Cannot cast expr as double: " ++ (show otherwise)

readOriginM :: EvalM (Double, Double)
readOriginM = do
  result <- lookupVarM symOrigin
  case result of
    (VPair x y) -> pure (x, y)
    _ -> throwError $ Internal "'origin' is not a pair of f64"

readScaleM :: EvalM (Double, Double)
readScaleM = do
  result <- lookupVarM symScale
  case result of
    (VPair x y) -> pure (x, y)
    _ -> throwError $ Internal "'scale' is not a pair of f64"

readCanvasSizeM :: EvalM (Int, Int)
readCanvasSizeM = do
  result <- lookupVarM symCanvasSize
  case result of
    (VPair x y) -> pure (floor x, floor y)
    _ -> throwError $ Internal "'canvasSize' is not a pair of f64"

readRotM :: EvalM Double
readRotM = do
  result <- lookupVarM symRot
  case result of
    (VScalar r) -> pure r
    _ -> throwError $ Internal "'rot' is not a f64"

data DrawParams = DrawParams
  { origin :: (Double, Double),
    scale :: (Double, Double),
    rot :: Double,
    canvasSize :: (Int, Int)
  }

readDrawParams :: EvalM DrawParams
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
    x'' = x' * cos theta + y' * sin theta
    y'' = -x' * sin theta + y' * cos theta
    -- 平移变换
    x''' = round $ ox + x''
    y''' = round $ oy + y''
    inbound = (0 <= x''' && x''' < cx) && (0 <= y''' && y''' < cy)

data Builtin = Builtin
  { arity :: Int,
    run :: [Value] -> EvalM Value
  }

unary :: (Value -> EvalM Value) -> Builtin
unary f = Builtin 1 $ \case
  [v] -> f v
  arg -> throwError $ ArityErr "Wrong number of arguments" 1 (length arg)

binary :: (Value -> Value -> EvalM Value) -> Builtin
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
    g :: Value -> Value -> EvalM Value
    g x y = do
      x' <- asDoubleM x
      y' <- asDoubleM y
      pure . VScalar $ f x' y'

builtinPrint :: Builtin
builtinPrint = unary $ \x -> do
  liftIO $ putStrLn $ show x
  pure VUnit

withImage :: (MutableImage RealWorld PixelRGB8 -> EvalM Value) -> EvalM Value
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
    draw :: Value -> Value -> EvalM Value
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

evalExprM :: A.Expr -> EvalM Value
evalExprM (A.Var name) = lookupVarM name
evalExprM (A.Imm v) = pure $ VScalar v
evalExprM (A.Pair x y) = do
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
  isUserDefined <- gets $ has (functions . ix fname)
  if isUserDefined then callUserDefined else callBuiltin
  where
    traversal = has (functions . ix fname)
    callUserDefined = do
      closure <- gets (^?! functions . ix fname)
      case closure of
        VClosure c -> do
          argv <- mapM evalExprM args
          let locals = Map.fromList $ zip (c ^. clParams) argv
          variables %= (locals :)
          flow <- evalStmtM $ c ^. clBody
          variables %= drop 1
          case flow of
            Normal -> pure VUnit
            Return value -> pure value
            _ -> throwError $ Internal "function not returned"
        otherwise -> throwError . Internal $ "not a function: " ++ fname
    callBuiltin = do
      f <- maybe (throwError (UnknownFn fname)) pure (Map.lookup fname builtins)
      a <- mapM evalExprM args
      run f a

evalBoolExprM :: A.BoolExpr -> EvalM Bool
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

evalStmtM :: A.Stmt -> EvalM Flow
evalStmtM (A.Assign var expr) = do
  value <- evalExprM expr
  modify' $ assignVar var value
  pure Normal
evalStmtM (A.Def fname args body) = do
  if Map.member fname builtins
    then
      throwError . RuntimeError $ "Cannot override builtin function: " ++ fname
    else
      functions %= Map.insert fname (VClosure $ Closure args body)
  pure Normal
evalStmtM (A.For var start end step body) = do
  vstart <- evalExprM start >>= asDoubleM
  vend <- evalExprM end >>= asDoubleM
  vstep <- evalExprM step >>= asDoubleM
  loop vstart vend vstep
  where
    loop :: Double -> Double -> Double -> EvalM Flow
    loop v vend vstep
      | vstep > 0 && v >= vend = pure Normal
      | vstep < 0 && v <= vend = pure Normal
      | otherwise = do
          modify' $ assignVar var (VScalar v)
          flow <- evalStmtM body
          case flow of
            Normal -> loop (v + vstep) vend vstep
            Break -> pure Break
            rtv@(Return _) -> pure rtv
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
        Break -> pure Break
        rtv@(Return _) -> pure rtv
evalStmtM (A.Break) = pure Break
evalStmtM (A.Return expr) = evalExprM expr >>= pure . Return
evalStmtM (A.AExp expr) = evalExprM expr >> pure Normal
evalStmtM (A.BExp expr) = evalBoolExprM expr >> pure Normal
evalStmtM (A.Void) = pure Normal

runProgram :: FilePath -> [A.Stmt] -> IO (Either Error EvalState)
runProgram path stmts = do
  ref <- newIORef Nothing
  let canvas = Canvas ref
  let ss = simplify stmts
  let prog = mapM_ evalStmtM ss >> (saveImage path)
  result <- runExceptT (runStateT (runReaderT prog canvas) emptyState)
  pure (fmap snd result)