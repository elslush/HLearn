{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts  #-}

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
import System.IO

--------------------------------------------------------------------------------

{-# INLINEABLE trainLogisticRegression #-}
trainLogisticRegression ::
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
trainLogisticRegression lambda xs = trainGLM_
    ( fminunc_cgd_
--         hestenesStiefel
--         polakRibiere
         steepestDescent
--        fletcherReeves
        (lineSearch_brent (stop_brent 1e-6 || maxIterations 50 || noProgress || fx1grows))
--         (backtracking (strongCurvature 1e-10))
--         (backtracking fx1grows)
        (mulTolerance 1e-9 || maxIterations 50 || noProgress {-- || fx1grows-})
    )
    loss_logistic
    lambda
    (toList xs)

main = do
    xs :: BArray (Labeled' (SVector "dyn" Double) (Lexical String))
       -- <- loadCSVLabeled' 0 "datasets/csv/uci/wine.csv"
       <- loadCSVLabeled' 8 "datasets/csv/uci/pima-indians-diabetes.csv"

    glm <- runHistory
        ( (displayFilter (maxReportLevel 2) dispIteration)
        + summaryTable
        )
        $ trainLogisticRegression 1e-3 xs

    putStrLn $ "loss_01       = "++show (validate loss_01       (toList xs) glm)
    putStrLn $ "loss_logistic = "++show (validate loss_logistic (toList xs) glm)
    putStrLn $ "loss_hinge    = "++show (validate loss_hinge    (toList xs) glm)

--     putStrLn ""
--     print $ show $ weights glm!Lexical "1"
--     print $ show $ weights glm!Lexical "2"
--     print $ show $ weights glm!Lexical "3"

    putStrLn "done."
