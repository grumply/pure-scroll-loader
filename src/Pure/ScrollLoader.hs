{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Pure.ScrollLoader where

import Pure hiding (features,key,offset,count)

import Control.Monad
import Data.Typeable

import qualified Pure.Visibility as V

type Count = Int
type Offset = Int

data ScrollLoader key view response = ScrollLoader
  { as       :: Features -> [View] -> View
  , features :: Features
  , loading  :: View
  , accept   :: response -> view
  , combine  :: view -> view -> view
  , render   :: view -> View
  , key      :: key
  , count    :: Count
  , offset   :: Offset
  , loader   :: key -> Count -> Offset -> (response -> IO ()) -> IO ()
  }

instance (Eq key, Typeable key, Typeable view, Typeable response) => Pure (ScrollLoader key view response) where
    view =
        Component $ \self ->
            let
                upd = modify_ self . const

                load update = do
                    ScrollLoader {..} <- ask self
                    (_,off,cnt,_) <- get self
                    when update $ upd $ \(_,off,cnt,v) -> (True,off + cnt,cnt,v)
                    loader key off cnt $ \rsp -> upd $ \(ldd,off,cnt,v) -> 
                      (ldd,off,cnt,Just $ maybe (accept rsp) (flip combine (accept rsp)) v)

            in
                def
                    { construct = do
                        ScrollLoader {..} <- ask self
                        return (False,offset,count,Nothing)
                    , mounted = do
                        l <- ask self
                        load True
                    , receive = \newprops oldstate -> do 
                        oldprops <- ask self
                        if offset newprops /= offset oldprops || key newprops /= key oldprops
                          then do
                            load False
                            return (True,offset newprops,count newprops,Nothing) 
                          else 
                            return oldstate
                    , Pure.render = \l (loaded,off,cnt,s) -> 
                        (as l) (features l)
                          [ maybe Null (Pure.ScrollLoader.render l) s
                          , V.Visibility def <| V.OnOnScreen (Just (const (load True))) |> 
                              [ if loaded
                                  then Div <| Display "inline" . Height (pxs 1) . Width (pxs 1)
                                  else loading l
                              ]
                          ]
                    }