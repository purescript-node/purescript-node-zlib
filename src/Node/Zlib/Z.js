import { constants } from "node:zlib";

export const flushZlibImpl = (s, cb) => s.flush(cb);
export const flushZlibKindCbImpl = (s, k, cb) => s.flush(k, cb);
export const paramsImpl = (s, lvl, strat, cb) => s.params(lvl, strat, cb);
export const resetImpl = (s) => s.reset();

// flush strategies
export const noFlush = constants.Z_NO_FLUSH;
export const partialFlush = constants.Z_PARTIAL_FLUSH;
export const syncFlush = constants.Z_SYNC_FLUSH;
export const fullFlush = constants.Z_FULL_FLUSH;
export const finish = constants.Z_FINISH;
export const block = constants.Z_BLOCK;
export const trees = constants.Z_TREES;

// compression/decompression return codes
export const ok = constants.Z_OK;
export const streamEnd = constants.Z_STREAM_END;
export const needDict = constants.Z_NEED_DICT;
export const errno = constants.Z_ERRNO;
export const streamError = constants.Z_STREAM_ERROR;
export const dataError = constants.Z_DATA_ERROR;
export const memError = constants.Z_MEM_ERROR;
export const bufError = constants.Z_BUF_ERROR;
export const versionError = constants.Z_VERSION_ERROR;

// compression levels
export const noCompression = constants.Z_NO_COMPRESSION;
export const bestSpeed = constants.Z_BEST_SPEED;
export const bestCompression = constants.Z_BEST_COMPRESSION;
export const defaultCompression = constants.Z_DEFAULT_COMPRESSION;

// compression startegy
export const filtered = constants.Z_FILTERED;
export const huffmanOnly = constants.Z_HUFFMAN_ONLY;
export const rle = constants.Z_RLE;
export const fixed = constants.Z_FIXED;
export const defaultStrategy = constants.Z_DEFAULT_STRATEGY;
