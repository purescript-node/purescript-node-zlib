import zlib from "node:zlib";

export const bytesWrittenImpl = (s) => s.bytesWritten;

export const createBrotliCompress = () => zlib.createBrotliCompress();
export const createBrotliCompressCbImpl = (opts) => zlib.createBrotliCompress(opts);
export const createBrotliDecompress = () => zlib.createBrotliDecompress();
export const createBrotliDecompressCbImpl = (opts) => zlib.createBrotliDecompress(opts);

export const createDeflateRaw = () => zlib.createDeflateRaw();
export const createDeflateRawCbImpl = (opts) => zlib.createDeflateRaw(opts);
export const createDeflate = () => zlib.createDeflate();
export const createDeflateCbImpl = (opts) => zlib.createDeflate(opts);
export const createGzip = () => zlib.createGzip();
export const createGzipCbImpl = (opts) => zlib.createGzip(opts);

export const createInflateRaw = () => zlib.createInflateRaw();
export const createInflateRawCbImpl = (opts) => zlib.createInflateRaw(opts);
export const createInflate = () => zlib.createInflate();
export const createInflateCbImpl = (opts) => zlib.createInflate(opts);
export const createGunzip = () => zlib.createGunzip();
export const createGunzipCbImpl = (opts) => zlib.createGunzip(opts);

export const createUnzip = () => zlib.createUnzip();
export const createUnzipCbImpl = (opts) => zlib.createUnzip(opts);

export const brotliCompressSyncImpl = (buf) => zlib.brotliCompressSync(buf);
export const brotliCompressSyncOptsImpl = (buf, opts) => zlib.brotliCompressSync(buf, opts);
export const brotliCompressImpl = (buf, cb) => zlib.brotliCompress(buf, cb);
export const brotliCompressCbImpl = (buf, opts, cb) => zlib.brotliCompress(buf, opts, cb);

export const brotliDecompressSyncImpl = (buf) => zlib.brotliDecompressSync(buf);
export const brotliDecompressSyncOptsImpl = (buf, opts) => zlib.brotliDecompressSync(buf, opts);
export const brotliDecompressImpl = (buf, cb) => zlib.brotliDecompress(buf, cb);
export const brotliDecompressCbImpl = (buf, opts, cb) => zlib.brotliDecompress(buf, opts, cb);

export const deflateRawSyncImpl = (buf) => zlib.deflateRawSync(buf);
export const deflateRawSyncOptsImpl = (buf, opts) => zlib.deflateRawSync(buf, opts);
export const deflateRawImpl = (buf, cb) => zlib.deflateRaw(buf, cb);
export const deflateRawCbImpl = (buf, opts, cb) => zlib.deflateRaw(buf, opts, cb);

export const deflateSyncImpl = (buf) => zlib.deflateSync(buf);
export const deflateSyncOptsImpl = (buf, opts) => zlib.deflateSync(buf, opts);
export const deflateImpl = (buf, cb) => zlib.deflate(buf, cb);
export const deflateCbImpl = (buf, opts, cb) => zlib.deflate(buf, opts, cb);

export const gzipSyncImpl = (buf) => zlib.gzipSync(buf);
export const gzipSyncOptsImpl = (buf, opts) => zlib.gzipSync(buf, opts);
export const gzipImpl = (buf, cb) => zlib.gzip(buf, cb);
export const gzipCbImpl = (buf, opts, cb) => zlib.gzip(buf, opts, cb);

export const inflateRawSyncImpl = (buf) => zlib.inflateRawSync(buf);
export const inflateRawSyncOptsImpl = (buf, opts) => zlib.inflateRawSync(buf, opts);
export const inflateRawImpl = (buf, cb) => zlib.inflateRaw(buf, cb);
export const inflateRawCbImpl = (buf, opts, cb) => zlib.inflateRaw(buf, opts, cb);

export const inflateSyncImpl = (buf) => zlib.inflateSync(buf);
export const inflateSyncOptsImpl = (buf, opts) => zlib.inflateSync(buf, opts);
export const inflateImpl = (buf, cb) => zlib.inflate(buf, cb);
export const inflateCbImpl = (buf, opts, cb) => zlib.inflate(buf, opts, cb);

export const gunzipSyncImpl = (buf) => zlib.gunzipSync(buf);
export const gunzipSyncOptsImpl = (buf, opts) => zlib.gunzipSync(buf, opts);
export const gunzipImpl = (buf, cb) => zlib.gunzip(buf, cb);
export const gunzipCbImpl = (buf, opts, cb) => zlib.gunzip(buf, opts, cb);

export const unzipSyncImpl = (buf) => zlib.unzipSync(buf);
export const unzipSyncOptsImpl = (buf, opts) => zlib.unzipSync(buf, opts);
export const unzipImpl = (buf, cb) => zlib.unzip(buf, cb);
export const unzipCbImpl = (buf, opts, cb) => zlib.unzip(buf, opts, cb);
