-- | For a better understanding of the types below, see these links:
-- | - Deflate, DeflateRaw, and Gzip: https://stackoverflow.com/a/68538037
-- | - Gzip vs Brotli: https://www.siteground.com/blog/brotli-vs-gzip-compression/
module Node.Zlib.Types where

-- | Transformer stream with a type-level value
-- | indicating which kind of compression is being used,
-- | either a Zlib type or a Brotli type.
-- | For example:
-- | ```
-- | Zlibstream (Brotli BrotliCompress)
-- | Zlibstream (Zlib Deflate)
-- | ```
-- |
-- | `Zlibstream` extends `Duplex` and `EventEmitter`.
foreign import data ZlibStream :: Compression -> Type

-- | Indicates whether a Zlib algorithm (i.e. "deflate" or "inflate")
-- | or Brotli algorithm is being used for compression/decompression.
data Compression

foreign import data Zlib :: ZlibType -> Compression
foreign import data Brotli :: BrotliType -> Compression

-- | Indicates which Zlib type is being used.
data ZlibType

-- | Uses "deflate" algorithm to **compress** data with the following metadata:
-- | - header: 0 bytes
-- | - footer: 0 bytes
-- | - checksum: <none>
-- |
-- | In other words, the raw deflate bit stream with no metadata.
foreign import data DeflateRaw :: ZlibType

-- | Uses "deflate" algorithm to **compress** data with the following metadata:
-- | - header: 2 bytes
-- | - footer: 0 bytes
-- | - checksum: Adler-32
-- |
-- | In other words, the raw deflate bit stream wrapped with some metadata.
foreign import data Deflate :: ZlibType

-- | Uses "deflate" algorithm to **compress** data with the following metadata:
-- | - header: 10 bytes
-- | - footer: 4 bytes
-- | - checksum: CRC32
-- |
-- | In other words, the raw deflate bit stream wrapped with some metadata.
foreign import data Gzip :: ZlibType

-- | Uses "inflate" algorithm to **decompress** data that was compressed via `DeflateRaw` stream.
foreign import data InflateRaw :: ZlibType

-- | Uses "inflate" algorithm to **decompress** data that was compressed via `Deflate` stream.
foreign import data Inflate :: ZlibType

-- | Uses "inflate" algorithm to **decompress** data that was compressed via `Gzip` stream.
foreign import data Gunzip :: ZlibType

-- | **Decompresses** data from either a `Gunzip` or `Inflate` stream by auto-detecting
-- | the header.
foreign import data Unzip :: ZlibType

-- | Indicates whether the Brotli stream is compressing/decompressing the data
data BrotliType

foreign import data BrotliCompress :: BrotliType
foreign import data BrotliDecompress :: BrotliType

-- | Indicates whether the `ZlibType` uses the "deflate" algorithm.
class UsesDeflate :: ZlibType -> Constraint
class UsesDeflate a

instance UsesDeflate Deflate
instance UsesDeflate DeflateRaw
instance UsesDeflate Gzip
