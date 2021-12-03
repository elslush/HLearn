{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators  #-}

import SubHask
import SubHask.Algebra.Array
import SubHask.Algebra.Vector
import SubHask.Algebra.Container
import SubHask.Category.Trans.Derivative
import SubHask.Compatibility.Containers

import HLearn.Data.LoadData
import HLearn.Classifiers.Linear
import HLearn.History
import HLearn.Optimization.Univariate
import HLearn.Optimization.Multivariate

import qualified Prelude as P
import qualified Data.Vector.Generic as VG
import System.IO

--------------------------------------------------------------------------------

{-# INLINEABLE loss_linear #-}
loss_linear :: (Hilbert x, Eq y) => y -> Labeled' x y -> C2 (x -> Scalar x)
loss_linear y0 (Labeled' x y) = unsafeProveC2 f f' f''
    where
        labelscore = bool2num $ y0==y

        f   w = 0.5 * (w<>x-labelscore)**2
        f'  w = x.*(w<>x-labelscore)
        f'' w = x><x

{-# INLINEABLE trainLinearRegression #-}
trainLinearRegression ::
    ( Ord y
    , Show y
    , Hilbert x
    , BoundedField (Scalar x)
    ) => Scalar x                                           -- ^ regularization parameter
      -> IsFoldable xys (Labeled' x y) => xys               -- ^ dataset
      -> ( cxt (LineBracket (Scalar x))
         , cxt (Iterator_cgd x)
         , cxt (Iterator_cgd (Scalar x))
         , cxt (Iterator_brent (Scalar x))
         , cxt (Backtracking x)
         , cxt (Map' y x)
         , cxt Int
         ) => History cxt (GLM x y)
trainLinearRegression lambda xs = trainGLM_
    ( fminunc_cgd_
         steepestDescent
        (lineSearch_brent (stop_brent 1e-6 || maxIterations 50 || noProgress || fx1grows))
--         (backtracking (strongCurvature 1e-10))
--         (backtracking fx1grows)
        (mulTolerance 1e-9 || maxIterations 50 || noProgress {-- || fx1grows-})
    )
    loss_linear
    lambda
    (toList xs)

accuracy ::
    ( Ord y
    , Hilbert x
    ) => [Labeled' x y] -> GLM x y -> Scalar x
accuracy xs model@(GLM ws _) = (sum $ map go xs)
    where
        go xy@(Labeled' x y) = if y == y' then 1 else 0
            where
                y' = classify model x

main = do
    xs :: BArray (Labeled' (SVector "dyn" Double) (Lexical String))
       -- <- loadCSVLabeled' 0 "datasets/csv/uci/wine.csv"
       <- loadCSVLabeled' 8 "datasets/csv/uci/pima-indians-diabetes.csv"

    glm <- runHistory
        ( (displayFilter (maxReportLevel 2) dispIteration)
        + summaryTable
        )
        $ trainLinearRegression 1e-3 xs

    putStrLn $ "loss_linear       = "++show (validate loss_linear       (toList xs) glm)

    putStrLn $ "accuracy       = "++show ((P.read ( show (accuracy (toList xs) glm)) :: Float) / (fromIntegral (size xs)))

    putStrLn "done."
