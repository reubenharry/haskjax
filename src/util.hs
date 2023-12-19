{-# LANGUAGE GADTs #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Util where

glossClock :: RescaledClock GlossEventClockIO Double
glossClock = RescaledClock
  { unscaledClock = GlossEventClockIO
  , rescale = into @Double
  }


toGloss :: SignalFunction (Stochastic & InputOutput & Feedback) (GlossInput, T.Text) Picture -> IO ()
toGloss sf = do
    mvar <- newMVar ""
    void $ forkIO $ forever do
        x <- T.getLine
        swapMVar mvar x

    flowGlossIO defaultSettings { display = InWindow "rhine-bayes" (1724, 1260) (10, 10) }
        $ tagS @@ glossClock >--
            collect -@- glossConcurrently -->
                morphS (hoist (lift . sampleIO)) (proc events -> do
                    inputText <- (constM (liftIO (swapMVar mvar ""))) -< ()
                    -- letters <- accumulateWith handle noInput  -< events
                        -- arr ((^. keys) . flip handle noInput)  
                    gl <- accumulateWith handle noInput -< events
                    -- arrM traceM -< show (gl ^. keys)
                    -- let glossInput = GlossInput {_keys = letters, _mouse = gl ^. mouse}
                    out <- sf -< (gl, inputText)
                    returnA -< out
                    )
                >-> arrMCl paintAllIO

                @@ Example.glossClock