-- | Module contains the following:
-- | - Functions that work on the Zlib streams, regardless of which compression is used
-- |    - `toDuplex`
-- |    - `bytesWritten`
-- | - `create*` and `create*'` functions that create a Transformer stream which
-- |    compresses/decompresses `Stream`-based data. Intended to be used with `Stream.pipeline`.
-- | - `<algorithm>*` functions, which mutates a buffer by compressing/decompressing the data within.
-- |    Comes in 3 variants:
-- |    - `*Sync`/`*Sync'` - synchronous version with/without options
-- |    - `*`/`*'` - assynchronous version with/without options
-- |    - `*Aff`/`*Aff'` - Aff-based assynchronous version with/without options
module Node.Zlib
  ( toDuplex
  , bytesWritten
  , createBrotliCompress
  , createBrotliCompress'
  , createBrotliDecompress
  , createBrotliDecompress'
  , createDeflateRaw
  , createDeflateRaw'
  , createDeflate
  , createDeflate'
  , createGzip
  , createGzip'
  , createInflateRaw
  , createInflateRaw'
  , createInflate
  , createInflate'
  , createGunzip
  , createGunzip'
  , createUnzip
  , createUnzip'
  , brotliCompressSync
  , brotliCompressSync'
  , brotliCompress
  , brotliCompress'
  , brotliCompressAff
  , brotliCompressAff'
  , brotliDecompressSync
  , brotliDecompressSync'
  , brotliDecompress
  , brotliDecompress'
  , brotliDecompressAff
  , brotliDecompressAff'
  , deflateRawSync
  , deflateRawSync'
  , deflateRaw
  , deflateRaw'
  , deflateRawAff
  , deflateRawAff'
  , deflateSync
  , deflateSync'
  , deflate
  , deflate'
  , deflateAff
  , deflateAff'
  , gzipSync
  , gzipSync'
  , gzip
  , gzip'
  , gzipAff
  , gzipAff'
  , inflateRawSync
  , inflateRawSync'
  , inflateRaw
  , inflateRaw'
  , inflateRawAff
  , inflateRawAff'
  , inflateSync
  , inflateSync'
  , inflate
  , inflate'
  , inflateAff
  , inflateAff'
  , gunzipSync
  , gunzipSync'
  , gunzip
  , gunzip'
  , gunzipAff
  , gunzipAff'
  , unzipSync
  , unzipSync'
  , unzip
  , unzip'
  , unzipAff
  , unzipAff'
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Node.Buffer (Buffer)
import Node.Stream (Duplex)
import Node.Zlib.Brotli (BrotliOptions)
import Node.Zlib.Types (Brotli, BrotliCompress, BrotliDecompress, Deflate, DeflateRaw, Gunzip, Gzip, Inflate, InflateRaw, Unzip, Zlib, ZlibStream)
import Node.Zlib.Z (DeflateOptions, GzipOptions, InflateOptions, GunzipOptions)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

toDuplex :: forall compression. ZlibStream compression -> Duplex
toDuplex = unsafeCoerce

bytesWritten :: forall compression. ZlibStream compression -> Effect Number
bytesWritten s = runEffectFn1 bytesWrittenImpl s

foreign import bytesWrittenImpl :: forall compression. EffectFn1 (ZlibStream compression) (Number)

foreign import createBrotliCompress :: Effect (ZlibStream (Brotli BrotliCompress))

createBrotliCompress'
  :: forall r trash
   . Row.Union r trash (BrotliOptions BrotliCompress)
  => { | r }
  -> Effect (ZlibStream (Brotli BrotliCompress))
createBrotliCompress' opts = runEffectFn1 createBrotliCompressCbImpl opts

foreign import createBrotliCompressCbImpl :: forall r. EffectFn1 ({ | r }) ((ZlibStream (Brotli BrotliCompress)))

foreign import createBrotliDecompress :: Effect (ZlibStream (Brotli BrotliDecompress))

createBrotliDecompress'
  :: forall r trash
   . Row.Union r trash (BrotliOptions BrotliDecompress)
  => { | r }
  -> Effect (ZlibStream (Brotli BrotliDecompress))
createBrotliDecompress' opts = runEffectFn1 createBrotliDecompressCbImpl opts

foreign import createBrotliDecompressCbImpl :: forall r. EffectFn1 ({ | r }) ((ZlibStream (Brotli BrotliDecompress)))

foreign import createDeflate :: Effect (ZlibStream (Zlib Deflate))

createDeflate'
  :: forall r trash
   . Row.Union r trash DeflateOptions
  => { | r }
  -> Effect (ZlibStream (Zlib Deflate))
createDeflate' opts = runEffectFn1 createDeflateCbImpl opts

foreign import createDeflateCbImpl :: forall r. EffectFn1 ({ | r }) ((ZlibStream (Zlib Deflate)))

foreign import createDeflateRaw :: Effect (ZlibStream (Zlib DeflateRaw))

createDeflateRaw'
  :: forall r trash
   . Row.Union r trash DeflateOptions
  => { | r }
  -> Effect (ZlibStream (Zlib DeflateRaw))
createDeflateRaw' opts = runEffectFn1 createDeflateRawCbImpl opts

foreign import createDeflateRawCbImpl :: forall r. EffectFn1 ({ | r }) ((ZlibStream (Zlib DeflateRaw)))

foreign import createGzip :: Effect (ZlibStream (Zlib Gzip))

createGzip'
  :: forall r trash
   . Row.Union r trash GzipOptions
  => { | r }
  -> Effect (ZlibStream (Zlib Gzip))
createGzip' opts = runEffectFn1 createGzipCbImpl opts

foreign import createGzipCbImpl :: forall r. EffectFn1 ({ | r }) ((ZlibStream (Zlib Gzip)))

foreign import createInflateRaw :: Effect (ZlibStream (Zlib InflateRaw))

createInflateRaw'
  :: forall r trash
   . Row.Union r trash InflateOptions
  => { | r }
  -> Effect (ZlibStream (Zlib InflateRaw))
createInflateRaw' opts = runEffectFn1 createInflateRawCbImpl opts

foreign import createInflateRawCbImpl :: forall r. EffectFn1 ({ | r }) ((ZlibStream (Zlib InflateRaw)))

foreign import createInflate :: Effect (ZlibStream (Zlib Inflate))

createInflate'
  :: forall r trash
   . Row.Union r trash InflateOptions
  => { | r }
  -> Effect (ZlibStream (Zlib Inflate))
createInflate' opts = runEffectFn1 createInflateCbImpl opts

foreign import createInflateCbImpl :: forall r. EffectFn1 ({ | r }) ((ZlibStream (Zlib Inflate)))

foreign import createGunzip :: Effect (ZlibStream (Zlib Gunzip))

createGunzip'
  :: forall r trash
   . Row.Union r trash GunzipOptions
  => { | r }
  -> Effect (ZlibStream (Zlib Gunzip))
createGunzip' opts = runEffectFn1 createGunzipCbImpl opts

foreign import createGunzipCbImpl :: forall r. EffectFn1 ({ | r }) ((ZlibStream (Zlib Gunzip)))

foreign import createUnzip :: Effect (ZlibStream (Zlib Unzip))

createUnzip'
  :: forall r trash
   . Row.Union r trash InflateOptions
  => { | r }
  -> Effect (ZlibStream (Zlib Unzip))
createUnzip' opts = runEffectFn1 createUnzipCbImpl opts

foreign import createUnzipCbImpl :: forall r. EffectFn1 ({ | r }) ((ZlibStream (Zlib Unzip)))

brotliCompressSync :: Buffer -> Effect Unit
brotliCompressSync buf = runEffectFn1 brotliCompressSyncImpl buf

foreign import brotliCompressSyncImpl :: EffectFn1 (Buffer) (Unit)

brotliCompressSync'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | BrotliOptions BrotliCompress)
  => Buffer
  -> { | r }
  -> Effect Unit
brotliCompressSync' buf opts = runEffectFn2 brotliCompressSyncOptsImpl buf opts

foreign import brotliCompressSyncOptsImpl :: forall r. EffectFn2 (Buffer) ({ | r }) (Unit)

brotliCompress :: Buffer -> Effect Unit -> Effect Unit
brotliCompress buf cb = runEffectFn2 brotliCompressImpl buf cb

foreign import brotliCompressImpl :: EffectFn2 (Buffer) (Effect Unit) (Unit)

brotliCompress'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | BrotliOptions BrotliCompress)
  => Buffer
  -> { | r }
  -> Effect Unit
  -> Effect Unit
brotliCompress' buf opts cb = runEffectFn3 brotliCompressCbImpl buf opts cb

foreign import brotliCompressCbImpl :: forall r. EffectFn3 (Buffer) ({ | r }) (Effect Unit) (Unit)

brotliCompressAff :: Buffer -> Aff Unit
brotliCompressAff buf = makeAff \cb -> do
  brotliCompress buf (cb $ Right unit)
  pure nonCanceler

brotliCompressAff'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | BrotliOptions BrotliCompress)
  => Buffer
  -> { | r }
  -> Aff Unit
brotliCompressAff' buf opts = makeAff \cb -> do
  brotliCompress' buf opts (cb $ Right unit)
  pure nonCanceler

brotliDecompressSync :: Buffer -> Effect Unit
brotliDecompressSync buf = runEffectFn1 brotliDecompressSyncImpl buf

foreign import brotliDecompressSyncImpl :: EffectFn1 (Buffer) (Unit)

brotliDecompressSync'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | BrotliOptions BrotliDecompress)
  => Buffer
  -> { | r }
  -> Effect Unit
brotliDecompressSync' buf opts = runEffectFn2 brotliDecompressSyncOptsImpl buf opts

foreign import brotliDecompressSyncOptsImpl :: forall r. EffectFn2 (Buffer) ({ | r }) (Unit)

brotliDecompress :: Buffer -> Effect Unit -> Effect Unit
brotliDecompress buf cb = runEffectFn2 brotliDecompressImpl buf cb

foreign import brotliDecompressImpl :: EffectFn2 (Buffer) (Effect Unit) (Unit)

brotliDecompress'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | BrotliOptions BrotliDecompress)
  => Buffer
  -> { | r }
  -> Effect Unit
  -> Effect Unit
brotliDecompress' buf opts cb = runEffectFn3 brotliDecompressCbImpl buf opts cb

foreign import brotliDecompressCbImpl :: forall r. EffectFn3 (Buffer) ({ | r }) (Effect Unit) (Unit)

brotliDecompressAff :: Buffer -> Aff Unit
brotliDecompressAff buf = makeAff \cb -> do
  brotliDecompress buf (cb $ Right unit)
  pure nonCanceler

brotliDecompressAff'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | BrotliOptions BrotliDecompress)
  => Buffer
  -> { | r }
  -> Aff Unit
brotliDecompressAff' buf opts = makeAff \cb -> do
  brotliDecompress' buf opts (cb $ Right unit)
  pure nonCanceler

deflateRawSync :: Buffer -> Effect Unit
deflateRawSync buf = runEffectFn1 deflateRawSyncImpl buf

foreign import deflateRawSyncImpl :: EffectFn1 (Buffer) (Unit)

deflateRawSync'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | DeflateOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
deflateRawSync' buf opts = runEffectFn2 deflateRawSyncOptsImpl buf opts

foreign import deflateRawSyncOptsImpl :: forall r. EffectFn2 (Buffer) ({ | r }) (Unit)

deflateRaw :: Buffer -> Effect Unit -> Effect Unit
deflateRaw buf cb = runEffectFn2 deflateRawImpl buf cb

foreign import deflateRawImpl :: EffectFn2 (Buffer) (Effect Unit) (Unit)

deflateRaw'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | DeflateOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
  -> Effect Unit
deflateRaw' buf opts cb = runEffectFn3 deflateRawCbImpl buf opts cb

foreign import deflateRawCbImpl :: forall r. EffectFn3 (Buffer) ({ | r }) (Effect Unit) (Unit)

deflateRawAff :: Buffer -> Aff Unit
deflateRawAff buf = makeAff \cb -> do
  deflateRaw buf (cb $ Right unit)
  pure nonCanceler

deflateRawAff'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | DeflateOptions)
  => Buffer
  -> { | r }
  -> Aff Unit
deflateRawAff' buf opts = makeAff \cb -> do
  deflateRaw' buf opts (cb $ Right unit)
  pure nonCanceler

deflateSync :: Buffer -> Effect Unit
deflateSync buf = runEffectFn1 deflateSyncImpl buf

foreign import deflateSyncImpl :: EffectFn1 (Buffer) (Unit)

deflateSync'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | DeflateOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
deflateSync' buf opts = runEffectFn2 deflateSyncOptsImpl buf opts

foreign import deflateSyncOptsImpl :: forall r. EffectFn2 (Buffer) ({ | r }) (Unit)

deflate :: Buffer -> Effect Unit -> Effect Unit
deflate buf cb = runEffectFn2 deflateImpl buf cb

foreign import deflateImpl :: EffectFn2 (Buffer) (Effect Unit) (Unit)

deflate'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | DeflateOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
  -> Effect Unit
deflate' buf opts cb = runEffectFn3 deflateCbImpl buf opts cb

foreign import deflateCbImpl :: forall r. EffectFn3 (Buffer) ({ | r }) (Effect Unit) (Unit)

deflateAff :: Buffer -> Aff Unit
deflateAff buf = makeAff \cb -> do
  deflate buf (cb $ Right unit)
  pure nonCanceler

deflateAff'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | DeflateOptions)
  => Buffer
  -> { | r }
  -> Aff Unit
deflateAff' buf opts = makeAff \cb -> do
  deflate' buf opts (cb $ Right unit)
  pure nonCanceler

gzipSync :: Buffer -> Effect Unit
gzipSync buf = runEffectFn1 gzipSyncImpl buf

foreign import gzipSyncImpl :: EffectFn1 (Buffer) (Unit)

gzipSync'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | GzipOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
gzipSync' buf opts = runEffectFn2 gzipSyncOptsImpl buf opts

foreign import gzipSyncOptsImpl :: forall r. EffectFn2 (Buffer) ({ | r }) (Unit)

gzip :: Buffer -> Effect Unit -> Effect Unit
gzip buf cb = runEffectFn2 gzipImpl buf cb

foreign import gzipImpl :: EffectFn2 (Buffer) (Effect Unit) (Unit)

gzip'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | GzipOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
  -> Effect Unit
gzip' buf opts cb = runEffectFn3 gzipCbImpl buf opts cb

foreign import gzipCbImpl :: forall r. EffectFn3 (Buffer) ({ | r }) (Effect Unit) (Unit)

gzipAff :: Buffer -> Aff Unit
gzipAff buf = makeAff \cb -> do
  gzip buf (cb $ Right unit)
  pure nonCanceler

gzipAff'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | GzipOptions)
  => Buffer
  -> { | r }
  -> Aff Unit
gzipAff' buf opts = makeAff \cb -> do
  gzip' buf opts (cb $ Right unit)
  pure nonCanceler

inflateRawSync :: Buffer -> Effect Unit
inflateRawSync buf = runEffectFn1 inflateRawSyncImpl buf

foreign import inflateRawSyncImpl :: EffectFn1 (Buffer) (Unit)

inflateRawSync'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | InflateOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
inflateRawSync' buf opts = runEffectFn2 inflateRawSyncOptsImpl buf opts

foreign import inflateRawSyncOptsImpl :: forall r. EffectFn2 (Buffer) ({ | r }) (Unit)

inflateRaw :: Buffer -> Effect Unit -> Effect Unit
inflateRaw buf cb = runEffectFn2 inflateRawImpl buf cb

foreign import inflateRawImpl :: EffectFn2 (Buffer) (Effect Unit) (Unit)

inflateRaw'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | InflateOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
  -> Effect Unit
inflateRaw' buf opts cb = runEffectFn3 inflateRawCbImpl buf opts cb

foreign import inflateRawCbImpl :: forall r. EffectFn3 (Buffer) ({ | r }) (Effect Unit) (Unit)

inflateRawAff :: Buffer -> Aff Unit
inflateRawAff buf = makeAff \cb -> do
  inflateRaw buf (cb $ Right unit)
  pure nonCanceler

inflateRawAff'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | InflateOptions)
  => Buffer
  -> { | r }
  -> Aff Unit
inflateRawAff' buf opts = makeAff \cb -> do
  inflateRaw' buf opts (cb $ Right unit)
  pure nonCanceler

inflateSync :: Buffer -> Effect Unit
inflateSync buf = runEffectFn1 inflateSyncImpl buf

foreign import inflateSyncImpl :: EffectFn1 (Buffer) (Unit)

inflateSync'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | InflateOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
inflateSync' buf opts = runEffectFn2 inflateSyncOptsImpl buf opts

foreign import inflateSyncOptsImpl :: forall r. EffectFn2 (Buffer) ({ | r }) (Unit)

inflate :: Buffer -> Effect Unit -> Effect Unit
inflate buf cb = runEffectFn2 inflateImpl buf cb

foreign import inflateImpl :: EffectFn2 (Buffer) (Effect Unit) (Unit)

inflate'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | InflateOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
  -> Effect Unit
inflate' buf opts cb = runEffectFn3 inflateCbImpl buf opts cb

foreign import inflateCbImpl :: forall r. EffectFn3 (Buffer) ({ | r }) (Effect Unit) (Unit)

inflateAff :: Buffer -> Aff Unit
inflateAff buf = makeAff \cb -> do
  inflate buf (cb $ Right unit)
  pure nonCanceler

inflateAff'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | InflateOptions)
  => Buffer
  -> { | r }
  -> Aff Unit
inflateAff' buf opts = makeAff \cb -> do
  inflate' buf opts (cb $ Right unit)
  pure nonCanceler

gunzipSync :: Buffer -> Effect Unit
gunzipSync buf = runEffectFn1 gunzipSyncImpl buf

foreign import gunzipSyncImpl :: EffectFn1 (Buffer) (Unit)

gunzipSync'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | GunzipOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
gunzipSync' buf opts = runEffectFn2 gunzipSyncOptsImpl buf opts

foreign import gunzipSyncOptsImpl :: forall r. EffectFn2 (Buffer) ({ | r }) (Unit)

gunzip :: Buffer -> Effect Unit -> Effect Unit
gunzip buf cb = runEffectFn2 gunzipImpl buf cb

foreign import gunzipImpl :: EffectFn2 (Buffer) (Effect Unit) (Unit)

gunzip'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | GunzipOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
  -> Effect Unit
gunzip' buf opts cb = runEffectFn3 gunzipCbImpl buf opts cb

foreign import gunzipCbImpl :: forall r. EffectFn3 (Buffer) ({ | r }) (Effect Unit) (Unit)

gunzipAff :: Buffer -> Aff Unit
gunzipAff buf = makeAff \cb -> do
  gunzip buf (cb $ Right unit)
  pure nonCanceler

gunzipAff'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | GunzipOptions)
  => Buffer
  -> { | r }
  -> Aff Unit
gunzipAff' buf opts = makeAff \cb -> do
  gunzip' buf opts (cb $ Right unit)
  pure nonCanceler

unzipSync :: Buffer -> Effect Unit
unzipSync buf = runEffectFn1 unzipSyncImpl buf

foreign import unzipSyncImpl :: EffectFn1 (Buffer) (Unit)

unzipSync'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | DeflateOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
unzipSync' buf opts = runEffectFn2 unzipSyncOptsImpl buf opts

foreign import unzipSyncOptsImpl :: forall r. EffectFn2 (Buffer) ({ | r }) (Unit)

unzip :: Buffer -> Effect Unit -> Effect Unit
unzip buf cb = runEffectFn2 unzipImpl buf cb

foreign import unzipImpl :: EffectFn2 (Buffer) (Effect Unit) (Unit)

unzip'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | DeflateOptions)
  => Buffer
  -> { | r }
  -> Effect Unit
  -> Effect Unit
unzip' buf opts cb = runEffectFn3 unzipCbImpl buf opts cb

foreign import unzipCbImpl :: forall r. EffectFn3 (Buffer) ({ | r }) (Effect Unit) (Unit)

unzipAff :: Buffer -> Aff Unit
unzipAff buf = makeAff \cb -> do
  unzip buf (cb $ Right unit)
  pure nonCanceler

unzipAff'
  :: forall r trash
   . Row.Union r trash (maxOutputLength :: Int | DeflateOptions)
  => Buffer
  -> { | r }
  -> Aff Unit
unzipAff' buf opts = makeAff \cb -> do
  unzip' buf opts (cb $ Right unit)
  pure nonCanceler
