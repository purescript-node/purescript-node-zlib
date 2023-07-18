-- | For a better understanding of Zlib, see these links:
-- | - https://www.euccas.me/zlib/
-- | - https://zlib.net/manual.html#Advanced
module Node.Zlib.Z
  ( flush
  , flush'
  , params
  , reset
  , DeflateOptions
  , GzipOptions
  , InflateOptions
  , GunzipOptions
  , FlushStrategy
  , noFlush
  , partialFlush
  , syncFlush
  , fullFlush
  , finish
  , block
  , trees
  , WindowBitsSize
  , mkWindowBitsSize
  , CompressionLevel
  , defaultCompression
  , noCompression
  , bestSpeed
  , bestCompression
  , mkCompressionLevel
  , CompressionStrategy
  , defaultStrategy
  , filtered
  , huffmanOnly
  , rle
  , fixed
  ) where

import Prelude

import Data.Reflectable (class Reflectable, reflectType)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Node.Buffer (Buffer)
import Node.Zlib.Types (class UsesDeflate, Zlib, ZlibStream)
import Prim.Int as Int
import Prim.Ordering (LT, GT)
import Type.Proxy (Proxy)

flush :: forall zlibType. ZlibStream (Zlib zlibType) -> Effect Unit -> Effect Unit
flush s cb = runEffectFn2 flushZlibImpl s cb

foreign import flushZlibImpl :: forall zlibType. EffectFn2 (ZlibStream (Zlib zlibType)) (Effect Unit) (Unit)

flush' :: forall zlibType. ZlibStream (Zlib zlibType) -> FlushStrategy -> Effect Unit -> Effect Unit
flush' s kind cb = runEffectFn3 flushZlibKindCbImpl s kind cb

foreign import flushZlibKindCbImpl :: forall zlibType. EffectFn3 (ZlibStream (Zlib zlibType)) (FlushStrategy) (Effect Unit) (Unit)

params :: forall zlibType. UsesDeflate zlibType => ZlibStream (Zlib zlibType) -> CompressionLevel -> CompressionStrategy -> Effect Unit -> Effect Unit
params s lvl strategy cb = runEffectFn4 paramsImpl s lvl strategy cb

foreign import paramsImpl :: forall zlibType. EffectFn4 (ZlibStream (Zlib zlibType)) (CompressionLevel) (CompressionStrategy) (Effect Unit) (Unit)

reset :: forall zlibType. ZlibStream (Zlib zlibType) -> Effect Unit
reset s = runEffectFn1 resetImpl s

foreign import resetImpl :: forall zlibType. EffectFn1 (ZlibStream (Zlib zlibType)) (Unit)

-- | - `flush` <integer> Default: zlib.constants.Z_NO_FLUSH
-- | - `finishFlush` <integer> Default: zlib.constants.Z_FINISH
-- | - `chunkSize` <integer> Default: 16 * 1024
-- | - `windowBits` <integer>
-- | - `level` <integer> (compression only)
-- | - `memLevel` <integer> (compression only)
-- | - `strategy` <integer> (compression only)
-- | - `dictionary` <Buffer> | <TypedArray> | <DataView> | <ArrayBuffer> (deflate/inflate only, empty dictionary by default)
type DeflateOptions =
  ( flush :: FlushStrategy
  , finishFlush :: FlushStrategy
  , chunkSize :: Int
  , windowBits :: WindowBitsSize
  , level :: CompressionLevel
  , memLevel :: Int
  , strategy :: CompressionStrategy
  , dictionary :: Buffer
  )

-- | - `flush` <integer> Default: zlib.constants.Z_NO_FLUSH
-- | - `finishFlush` <integer> Default: zlib.constants.Z_FINISH
-- | - `chunkSize` <integer> Default: 16 * 1024
-- | - `windowBits` <integer>
-- | - `level` <integer> (compression only)
-- | - `memLevel` <integer> (compression only)
-- | - `strategy` <integer> (compression only)
type GzipOptions =
  ( flush :: FlushStrategy
  , finishFlush :: FlushStrategy
  , chunkSize :: Int
  , windowBits :: WindowBitsSize
  , level :: CompressionLevel
  , memLevel :: Int
  , strategy :: CompressionStrategy
  )

-- | - `flush` <integer> Default: zlib.constants.Z_NO_FLUSH
-- | - `finishFlush` <integer> Default: zlib.constants.Z_FINISH
-- | - `chunkSize` <integer> Default: 16 * 1024
-- | - `windowBits` <integer>
-- | - `dictionary` <Buffer> | <TypedArray> | <DataView> | <ArrayBuffer> (deflate/inflate only, empty dictionary by default)
type InflateOptions =
  ( flush :: FlushStrategy
  , finishFlush :: FlushStrategy
  , chunkSize :: Int
  , windowBits :: WindowBitsSize
  , dictionary :: Buffer
  )

-- | - `flush` <integer> Default: zlib.constants.Z_NO_FLUSH
-- | - `finishFlush` <integer> Default: zlib.constants.Z_FINISH
-- | - `chunkSize` <integer> Default: 16 * 1024
-- | - `windowBits` <integer>
type GunzipOptions =
  ( flush :: FlushStrategy
  , finishFlush :: FlushStrategy
  , chunkSize :: Int
  , windowBits :: WindowBitsSize
  )

newtype FlushStrategy = FlushStrategy Int

foreign import noFlush :: FlushStrategy
foreign import partialFlush :: FlushStrategy
foreign import syncFlush :: FlushStrategy
foreign import fullFlush :: FlushStrategy
foreign import finish :: FlushStrategy
foreign import block :: FlushStrategy
foreign import trees :: FlushStrategy

-- | the base two logarithm of the window size (the size of the history buffer). 
-- | It should be in the range 8..15 for this version of the library. 
-- | Larger values of this parameter result in better compression at the expense of memory usage. 
newtype WindowBitsSize = WindowBitsSize Int

derive instance Eq WindowBitsSize
derive instance Ord WindowBitsSize
derive newtype instance Show WindowBitsSize

mkWindowBitsSize
  :: forall i
   . Reflectable i Int
  => Int.Compare i 8 GT
  => Int.Compare i 16 LT
  => Proxy i
  -> WindowBitsSize
mkWindowBitsSize = WindowBitsSize <<< reflectType

newtype ReturnCode = ReturnCode Int

derive instance Eq ReturnCode
derive instance Ord ReturnCode
derive newtype instance Show ReturnCode

foreign import ok :: ReturnCode
foreign import streamEnd :: ReturnCode
foreign import needDict :: ReturnCode
foreign import errno :: ReturnCode
foreign import streamError :: ReturnCode
foreign import dataError :: ReturnCode
foreign import memError :: ReturnCode
foreign import bufError :: ReturnCode
foreign import versionError :: ReturnCode

newtype CompressionLevel = CompressionLevel Int

derive instance Eq CompressionLevel
derive instance Ord CompressionLevel
derive newtype instance Show CompressionLevel

-- | Same as `0`
foreign import noCompression :: CompressionLevel

-- | Same as `1`
foreign import bestSpeed :: CompressionLevel

-- | Same as `9`
foreign import bestCompression :: CompressionLevel

-- | Same as `-1`
foreign import defaultCompression :: CompressionLevel

-- | A value of `0` indicates no compression.
-- | A value of `1` indicates fastest speed with least beneficial compression.
-- | A value between `1` and `9` makes a tradeoff between speed and compression quality.
-- | A value of `9` indicates slowest speed with most beneficial compression.
mkCompressionLevel
  :: forall i
   . Reflectable i Int
  => Int.Compare i (-1) GT
  => Int.Compare i (10) LT
  => Proxy i
  -> CompressionLevel
mkCompressionLevel = CompressionLevel <<< reflectType

newtype CompressionStrategy = CompressionStrategy Int

derive instance Eq CompressionStrategy
derive instance Ord CompressionStrategy
derive newtype instance Show CompressionStrategy

foreign import filtered :: CompressionStrategy
foreign import huffmanOnly :: CompressionStrategy
foreign import rle :: CompressionStrategy
foreign import fixed :: CompressionStrategy
foreign import defaultStrategy :: CompressionStrategy
