{- | Sampling example with continuous distributions

Continuous networks can't be handled by any of the functions defined for the discrete networks.
So, instead of using exact inference algorithms like the junction trees, sampling method have to be used.

In this example, we want to estimate a parameter which is measured by noisy sensors.

There are 'nbSensors' available. They are described with a 'normal' distribution centered on the value of
the unknown parameters and with a standard deviation of 0.1.

The unknown parameter is described with a 'uniform' distribution bounded by 1.0 and 2.0.

First, we describe the sensor:

@
sensor :: 'DN' -> 'CNMonad' 'DN' 
sensor p = do 
    'normal' () p 0.1 
@

It is just a 'normal' distribution. The mean of this distribution is the parameters p. This parameter has special type 'DN'.
All expressions used to build the continuous bayesian network are using values of type 'DN'. A value of type 'DN' can either
represent a constant, a variable or an expression.

If the sensor was biased, we could write:

@
    'normal' ()  (p + 0.2) 0.1
@

The Bayesian network describing the measurement process is given by:

@
    test = 'runCN' $ do
      a <- 'uniform' \"a\" 1.0 2.0 -- Unknown parameter
      sensors <- sequence (replicate 'nbSensors' (sensor a))
      return (a:sensors)
@

We are connecting 'nbSensors' nodes corresponding to the 'nbSensors' measurements.  In real life it can either be different sensors
or the same one used several times (assuming the value of the parameter is not dependent on time).

Now, as usual in all the examples of this package, we get the bayesian graph and a list of variables used
to compute some posterior or define some evidences

@
    debugcn = do 
        let ((a:sensors), testG) = test
@

Then, we generate some random measurements and create the evidences

@
    g <- create 
    measurements <- sequence . replicate  nbSensors $ (MWC.normal 1.5 0.1 g)
    let evidence = zipWith (=:) sensors measurements
@

Evidence has type 'CVI' and is created with the assigment operator '=:' .

Now, we generate some samples to estimate the posterior distributions.

@
    n <- 'runSampling' 10000 200 ('continuousMCMCSampler' testG evidence)
@

This function is generating a sequence of graphs ! We are not interested in the sensor values. They are known and fixed
since they have been measured. So, we extract the value of the parameter.

@
    let samples = map (\g -> 'instantiationValue' . fromJust . 'vertexValue' g $ ('vertex' a)) n
@

And with the samples for the parameters we can compute an histogram and get an approximation of the posterior.

@
    let samples = map (\g -> 'instantiationValue' . fromJust . 'vertexValue' g $ ('vertex' a)) n
        h = 'histogram' 6 samples 
    print h
@

We see in the histogram that the estimated value is around 1.5.

The example 'complexsamples' will create three files alpha.txt, beta.txt and tau.txt. It is corresponding to the following
bugs model

@
model {
    for (i in 1:n) {
          mu[i] <- alpha + beta*i/n;
          y[i]   ~ dnorm(mu[i],tau);
    }
    alpha    ~ dnorm(0.0,1.0E-4);
    beta     ~ dnorm(0.0,1.0E-4);
    tau      ~ dgamma(1.0E-3,1.0E-3);
    sigma   <- 1.0/sqrt(tau);
}
@

with alpha = 0, beta = 0 and tau = 1

The Haskell code for this model is

@
complexMeasures = 100 
--
complex = 'runCN' $ do 
  let n = complexMeasures
  alpha <- 'normal' \"alpha\" 0.0 1e-4 
  beta <- 'normal' \"beta\" 0.0 1e-4 
  tau <- 'gammaD' \"tau\" 1e-3 1e-3 
  let sigma = 1.0 / sqrt(tau)
      sample i = do 
        let mu = alpha + beta * fromIntegral i / fromIntegral n
        y <- 'normal' () mu tau 
        return y
  l <- mapM sample [0..n-1]
  return (alpha:beta:tau:l)
@

And the generation of the samples is done with

@
complexsamples = do 
  let n = complexMeasures
      ((alpha:beta:tau:obs),complexg) = complex 
      alphat = 0.0 
      betat = 0.0 
      taut = 1.0 
      aMeasure g i = do 
        let mu = alphat + betat * fromIntegral i / fromIntegral n
        MWC.normal mu taut g
  g <- create
  measurements <- mapM (aMeasure g) [0..n-1] 
  let evidence = zipWith ('=:') obs measurements
@

-}
module Bayes.Examples.ContinuousSampling(
      nbSensors 
    , sensor 
    , test 
    , debugcn
  , writesamples
  , complexsamples
    ) where
import Bayes
import Bayes.Continuous 
import qualified System.Random.MWC.Distributions as MWC(normal)
import System.Random.MWC(GenIO,create)
import Data.Maybe(fromJust)
import System.IO(withFile,IOMode(..),hPrint)

nbSensors = 10 

sensor :: DN -> CNMonad DN 
sensor p = do 
    normal () p 0.1 

test = runCN $ do
  a <- uniform "a" 1.0 2.0 -- Unknown parameter
  sensors <- sequence (replicate nbSensors (sensor a))
  return (a:sensors)

complexMeasures = 100 

complex = runCN $ do 
  let n = complexMeasures
  alpha <- normal "alpha" 0.0 1e-4 
  beta <- normal "beta" 0.0 1e-4 
  tau <- gammaD "tau" 1e-3 1e-3 
  let sigma = 1.0 / sqrt(tau)
      sample i = do 
        let mu = alpha + beta * fromIntegral i / fromIntegral n
        y <- normal () mu tau 
        return y
  l <- mapM sample [0..n-1]
  return (alpha:beta:tau:l)

complexsamples = do 
  let n = complexMeasures
      ((alpha:beta:tau:obs),complexg) = complex 
      alphat = 0.0 
      betat = 0.0 
      taut = 1.0 
      aMeasure g i = do 
        let mu = alphat + betat * fromIntegral i / fromIntegral n
        MWC.normal mu taut g
  g <- create
  measurements <- mapM (aMeasure g) [0..n-1] 
  let evidence = zipWith (=:) obs measurements
  n <- runSampling 10000 200 (continuousMCMCSampler complexg evidence)
  let samples s a = do 
       let v = map (\g -> instantiationValue . fromJust . vertexValue g $ (vertex a)) n
       writeList s v
  samples "alpha.txt" alpha 
  samples "beta.txt" beta
  samples "tau.txt" tau

debugcn = do 
    let ((a:sensors), testG) = test
    g <- create 
    measurements <- sequence . replicate  nbSensors $ (MWC.normal 1.5 0.1 g)
    let evidence = zipWith (=:) sensors measurements
    n <- runSampling 10000 200 (continuousMCMCSampler testG evidence)
    let samples = map (\g -> instantiationValue . fromJust . vertexValue g $ (vertex a)) n
        h = histogram 6 samples 
    print h

writeList :: Show a => FilePath -> [a] -> IO ()
writeList s l = do 
  withFile s WriteMode $ \h -> do 
    mapM_ (hPrint h) l

writesamples s = do 
    let ((a:sensors), testG) = test
    g <- create 
    measurements <- sequence . replicate  nbSensors $ (MWC.normal 1.5 0.1 g)
    let evidence = zipWith (=:) sensors measurements
    n <- runSampling 10000 200 (continuousMCMCSampler testG evidence)
    let samples = map (\g -> instantiationValue . fromJust . vertexValue g $ (vertex a)) n
    writeList s samples
