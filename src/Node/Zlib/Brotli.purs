module Node.Zlib.Brotli
  ( BrotliOptions
  , FlushStrategy -- constructor intentionally not exported
  , operationProcess
  , operationFlush
  , operationFinish
  , operationEmitMetadata
  , Params
  , MutableParams
  , mkParams
  , ParamMode
  , compressMode
  , modeGeneric
  , modeText
  , modeFont
  , compressQuality
  , ParamQuality
  , minQuality
  , maxQuality
  , defaultQuality
  , mkQuality
  , compressSizeHint
  , compressLgWin
  , WindowBitsSize
  , minWindowBits
  , maxWindowBits
  , defaultWindow
  , mkWindowBitsSize
  , compressLargeWindow
  , largeMaxWindowBits
  , compressLgBlock
  , BlockBitsSize
  , minInputBlockBits
  , maxInputBlockBits
  , mkBlockBitsSize
  , compressDisableLiteralContextModeling
  , compressNPostfixSize
  , compressNPostfixSizeWithNDirect
  , decompressDisableRingBufferReallocation
  , decompressLargeWindow
  , class IsValidNDirectFor
  ) where

import Prelude

import Data.Function.Uncurried (Fn3, runFn3)
import Data.Reflectable (class Reflectable, reflectType)
import Node.Zlib.Types (BrotliCompress, BrotliDecompress, BrotliType)
import Prim.Int as Int
import Prim.Ordering (GT, LT)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

-- | - `flush` <integer> Default: BROTLI_OPERATION_PROCESS
-- | - `finishFlush` <integer> Default: BROTLI_OPERATION_FINISH
-- | - `chunkSize` <integer> Default: 16 * 1024
-- | - `params` <Object> Key-value object containing indexed Brotli parameters.
-- |
-- | The `brotliType` type variable indicates whether the params are built
-- | for the compression or decompression stream. To build a `params` value,
-- | see `mkParams`.
type BrotliOptions brotliType =
  ( flush :: FlushStrategy
  , finishFlush :: FlushStrategy
  , chunkSize :: Int
  , params :: Params brotliType
  )

newtype FlushStrategy = FlushStrategy Int

derive instance Eq FlushStrategy
derive instance Ord FlushStrategy
derive newtype instance Show FlushStrategy

foreign import operationProcess :: FlushStrategy
foreign import operationFlush :: FlushStrategy
foreign import operationFinish :: FlushStrategy
foreign import operationEmitMetadata :: FlushStrategy

foreign import data Params :: BrotliType -> Type

foreign import data MutableParams :: BrotliType -> Type

foreign import insertParam :: forall brotliType x. Fn3 (Param x) x (MutableParams brotliType) (MutableParams brotliType)

-- | To build a `Params` value for a compression stream, use functions starting with `compress`.
-- | To build a `Params` value for a decompression stream, use functions starting with `decompress`.
-- |
-- | ```
-- | -- compression
-- | mkParams
-- |  (compressMode modeGeneric
-- |    >>> compressQuality (mkQuality (Proxy :: Proxy 10))
-- |    >>> ...
-- |    >>> compressSizeHint 4)
-- |
-- | -- decompression
-- | mkParams
-- |  (decompressLargeWindow true
-- |    >>> decompressDisableRingBufferReallocation true)
-- | ```
mkParams :: forall brotliType. (MutableParams brotliType -> MutableParams brotliType) -> Params brotliType
mkParams cb = unsafeCoerce $ cb $ unsafeCoerce {}

foreign import data Param :: Type -> Type

compressMode :: ParamMode -> MutableParams BrotliCompress -> MutableParams BrotliCompress
compressMode mode params = runFn3 insertParam paramMode mode params

foreign import data ParamMode :: Type
foreign import paramMode :: Param ParamMode
foreign import modeGeneric :: ParamMode
foreign import modeText :: ParamMode
foreign import modeFont :: ParamMode

compressQuality :: ParamQuality -> MutableParams BrotliCompress -> MutableParams BrotliCompress
compressQuality qual params = runFn3 insertParam paramQuality qual params

newtype ParamQuality = ParamQuality Int

derive instance Eq ParamQuality
derive instance Ord ParamQuality
derive newtype instance Show ParamQuality

foreign import paramQuality :: Param ParamQuality
foreign import minQuality :: ParamQuality
foreign import defaultQuality :: ParamQuality
foreign import maxQuality :: ParamQuality

mkQuality
  :: forall i
   . Reflectable i Int
  => Int.Compare i (-1) GT
  => Int.Compare i (12) LT
  => Proxy i
  -> ParamQuality
mkQuality = ParamQuality <<< reflectType

foreign import paramSizeHint :: Param Int

compressSizeHint :: Int -> MutableParams BrotliCompress -> MutableParams BrotliCompress
compressSizeHint hint params = runFn3 insertParam paramSizeHint hint params

compressLgWin
  :: forall i
   . Reflectable i Int
  => Int.Compare i 9 GT
  => Int.Compare i 25 LT
  => WindowBitsSize i
  -> MutableParams BrotliCompress
  -> MutableParams BrotliCompress
compressLgWin winBits params = runFn3 insertParam paramLgwin winBits params

newtype WindowBitsSize :: Int -> Type
newtype WindowBitsSize i = WindowBitsSize Int

derive instance Eq (WindowBitsSize i)
derive instance Ord (WindowBitsSize i)
derive newtype instance Show (WindowBitsSize i)

foreign import paramLgwin :: forall i. Param (WindowBitsSize i)
foreign import minWindowBits :: WindowBitsSize 10
foreign import maxWindowBits :: WindowBitsSize 24
foreign import defaultWindow :: WindowBitsSize 22
foreign import largeMaxWindowBits :: WindowBitsSize 30

mkWindowBitsSize
  :: forall i
   . Reflectable i Int
  => Int.Compare i 9 GT
  => Int.Compare i 31 LT
  => Proxy i
  -> WindowBitsSize i
mkWindowBitsSize = WindowBitsSize <<< reflectType

compressLargeWindow
  :: forall i
   . Reflectable i Int
  => Int.Compare i 9 GT
  => Int.Compare i 31 LT
  => WindowBitsSize i
  -> MutableParams BrotliCompress
  -> MutableParams BrotliCompress
compressLargeWindow winBits params = do
  let withFlagEnabled = runFn3 insertParam paramLargeWindow true params
  runFn3 insertParam paramLgwin winBits withFlagEnabled

foreign import paramLargeWindow :: Param Boolean

compressLgBlock
  :: forall i
   . Reflectable i Int
  => Int.Compare i 15 GT
  => Int.Compare i 25 LT
  => BlockBitsSize i
  -> MutableParams BrotliCompress
  -> MutableParams BrotliCompress
compressLgBlock winBits params = runFn3 insertParam paramLgblock winBits params

newtype BlockBitsSize :: Int -> Type
newtype BlockBitsSize i = BlockBitsSize Int

derive instance Eq (BlockBitsSize i)
derive instance Ord (BlockBitsSize i)
derive newtype instance Show (BlockBitsSize i)

foreign import paramLgblock :: forall i. Param (BlockBitsSize i)
foreign import minInputBlockBits :: BlockBitsSize 16
foreign import maxInputBlockBits :: BlockBitsSize 24

mkBlockBitsSize
  :: forall i
   . Reflectable i Int
  => Int.Compare i 15 GT
  => Int.Compare i 25 LT
  => Proxy i
  -> BlockBitsSize i
mkBlockBitsSize = BlockBitsSize <<< reflectType

compressDisableLiteralContextModeling :: MutableParams BrotliCompress -> MutableParams BrotliCompress
compressDisableLiteralContextModeling params = runFn3 insertParam paramDisableLiteralContextModeling true params

foreign import paramDisableLiteralContextModeling :: Param Boolean

compressNPostfixSize
  :: forall i
   . Reflectable i Int
  => Int.Compare i (-1) GT
  => Int.Compare i 4 LT
  => Proxy i
  -> MutableParams BrotliCompress
  -> MutableParams BrotliCompress
compressNPostfixSize p params = runFn3 insertParam paramNpostfix (reflectType p) params

foreign import paramNpostfix :: Param Int

-- | Adds the `NpostfixSize` and `Ndirect` parameters since the latter depends
-- | on the former.
compressNPostfixSizeWithNDirect
  :: forall nPostfix nDirect
   . Reflectable nPostfix Int
  => Reflectable nDirect Int
  => Int.Compare nPostfix (-1) GT
  => Int.Compare nPostfix 4 LT
  => IsValidNDirectFor nPostfix nDirect
  => Proxy nPostfix
  -> Proxy nDirect
  -> MutableParams BrotliCompress
  -> MutableParams BrotliCompress
compressNPostfixSizeWithNDirect postfix direct params = do
  let withPostfix = runFn3 insertParam paramNpostfix (reflectType postfix) params
  runFn3 insertParam paramNdirect (reflectType direct) withPostfix

foreign import paramNdirect :: Param Int

foreign import decoderParamDisableRingBufferReallocation :: Param Boolean

decompressDisableRingBufferReallocation
  :: Boolean
  -> MutableParams BrotliDecompress
  -> MutableParams BrotliDecompress
decompressDisableRingBufferReallocation bool params = do
  runFn3 insertParam decoderParamDisableRingBufferReallocation bool params

foreign import decoderParamLargeWindow :: Param Boolean

decompressLargeWindow
  :: Boolean
  -> MutableParams BrotliDecompress
  -> MutableParams BrotliDecompress
decompressLargeWindow bool params = do
  runFn3 insertParam decoderParamDisableRingBufferReallocation bool params

-- | - When 'Npostfix' is 0, then possible values are: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
-- | - When 'Npostfix' is 1, then possible values are: 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30
-- | - When 'Npostfix' is 2, then possible values are: 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60
-- | - When 'Npostfix' is 3, then possible values are: 0, 8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120
class IsValidNDirectFor :: Int -> Int -> Constraint
class IsValidNDirectFor nPostfix nDirect | nPostfix -> nDirect

instance IsValidNDirectFor 0 0
else instance IsValidNDirectFor 0 1
else instance IsValidNDirectFor 0 2
else instance IsValidNDirectFor 0 3
else instance IsValidNDirectFor 0 4
else instance IsValidNDirectFor 0 5
else instance IsValidNDirectFor 0 6
else instance IsValidNDirectFor 0 7
else instance IsValidNDirectFor 0 8
else instance IsValidNDirectFor 0 9
else instance IsValidNDirectFor 0 10
else instance IsValidNDirectFor 0 11
else instance IsValidNDirectFor 0 12
else instance IsValidNDirectFor 0 13
else instance IsValidNDirectFor 0 14
else instance IsValidNDirectFor 0 15
else instance Fail (Text "number must be one of 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15") => IsValidNDirectFor 0 i

instance IsValidNDirectFor 1 0
else instance IsValidNDirectFor 1 2
else instance IsValidNDirectFor 1 4
else instance IsValidNDirectFor 1 6
else instance IsValidNDirectFor 1 8
else instance IsValidNDirectFor 1 10
else instance IsValidNDirectFor 1 12
else instance IsValidNDirectFor 1 14
else instance IsValidNDirectFor 1 16
else instance IsValidNDirectFor 1 18
else instance IsValidNDirectFor 1 20
else instance IsValidNDirectFor 1 22
else instance IsValidNDirectFor 1 24
else instance IsValidNDirectFor 1 26
else instance IsValidNDirectFor 1 28
else instance IsValidNDirectFor 1 30
else instance Fail (Text "number must be one of 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30") => IsValidNDirectFor 1 i

instance IsValidNDirectFor 2 0
else instance IsValidNDirectFor 2 4
else instance IsValidNDirectFor 2 8
else instance IsValidNDirectFor 2 12
else instance IsValidNDirectFor 2 16
else instance IsValidNDirectFor 2 20
else instance IsValidNDirectFor 2 24
else instance IsValidNDirectFor 2 28
else instance IsValidNDirectFor 2 32
else instance IsValidNDirectFor 2 36
else instance IsValidNDirectFor 2 40
else instance IsValidNDirectFor 2 44
else instance IsValidNDirectFor 2 48
else instance IsValidNDirectFor 2 52
else instance IsValidNDirectFor 2 56
else instance IsValidNDirectFor 2 60
else instance Fail (Text "number must be one of 0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60") => IsValidNDirectFor 2 i

instance IsValidNDirectFor 3 0
else instance IsValidNDirectFor 3 8
else instance IsValidNDirectFor 3 16
else instance IsValidNDirectFor 3 24
else instance IsValidNDirectFor 3 32
else instance IsValidNDirectFor 3 40
else instance IsValidNDirectFor 3 48
else instance IsValidNDirectFor 3 56
else instance IsValidNDirectFor 3 64
else instance IsValidNDirectFor 3 72
else instance IsValidNDirectFor 3 80
else instance IsValidNDirectFor 3 88
else instance IsValidNDirectFor 3 96
else instance IsValidNDirectFor 3 104
else instance IsValidNDirectFor 3 112
else instance IsValidNDirectFor 3 120
else instance Fail (Text "number must be one of 0, 8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120") => IsValidNDirectFor 3 i

