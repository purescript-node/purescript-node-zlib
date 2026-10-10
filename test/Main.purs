module Test.Main where

import Prelude

import Effect (Effect)
import Node.Stream (Duplex)
import Node.Zlib as Zlib

foreign import roundTrip :: String -> Duplex -> Duplex -> Effect Unit

main :: Effect Unit
main = do
  gzip <- Zlib.createGzip
  gunzip <- Zlib.createGunzip
  roundTrip "gzip" (Zlib.toDuplex gzip) (Zlib.toDuplex gunzip)
  deflate <- Zlib.createDeflate
  inflate <- Zlib.createInflate
  roundTrip "deflate" (Zlib.toDuplex deflate) (Zlib.toDuplex inflate)
  deflateRaw <- Zlib.createDeflateRaw
  inflateRaw <- Zlib.createInflateRaw
  roundTrip "deflateRaw" (Zlib.toDuplex deflateRaw) (Zlib.toDuplex inflateRaw)
  brotli <- Zlib.createBrotliCompress
  unbrotli <- Zlib.createBrotliDecompress
  roundTrip "brotli" (Zlib.toDuplex brotli) (Zlib.toDuplex unbrotli)
