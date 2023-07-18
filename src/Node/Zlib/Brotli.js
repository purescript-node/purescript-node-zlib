import { constants } from "node:zlib";

export const insertParam = (key, value, params) => {
  params[key] = value;
  return params;
}

// brotli flush strategies
export const operationProcess = constants.BROTLI_OPERATION_PROCESS;
export const operationFlush = constants.BROTLI_OPERATION_FLUSH;
export const operationFinish = constants.BROTLI_OPERATION_FINISH;
export const operationEmitMetadata = constants.BROTLI_OPERATION_EMIT_METADATA;

// brotli compression options
export const paramMode = constants.BROTLI_PARAM_MODE;
export const modeGeneric = constants.BROTLI_MODE_GENERIC;
export const modeText = constants.BROTLI_MODE_TEXT;
export const modeFont = constants.BROTLI_MODE_FONT;

export const paramQuality = constants.BROTLI_PARAM_QUALITY;
export const minQuality = constants.BROTLI_MIN_QUALITY;
export const defaultQuality = constants.BROTLI_DEFAULT_QUALITY;
export const maxQuality = constants.BROTLI_MAX_QUALITY;

export const paramSizeHint = constants.BROTLI_PARAM_SIZE_HINT;

export const paramLgwin = constants.BROTLI_PARAM_LGWIN;
export const minWindowBits = constants.BROTLI_MIN_WINDOW_BITS;
export const maxWindowBits = constants.BROTLI_MAX_WINDOW_BITS;
export const defaultWindow = constants.BROTLI_DEFAULT_WINDOW;
export const largeMaxWindowBits = constants.BROTLI_LARGE_MAX_WINDOW_BITS;
export const paramLgblock = constants.BROTLI_PARAM_LGBLOCK;
export const minInputBlockBits = constants.BROTLI_MIN_INPUT_BLOCK_BITS;
export const maxInputBlockBits = constants.BROTLI_MAX_INPUT_BLOCK_BITS;
export const paramDisableLiteralContextModeling = constants.BROTLI_PARAM_DISABLE_LITERAL_CONTEXT_MODELING;
export const paramLargeWindow = constants.BROTLI_PARAM_LARGE_WINDOW;
export const paramNpostfix = constants.BROTLI_PARAM_NPOSTFIX;
export const paramNdirect = constants.BROTLI_PARAM_NDIRECT;

export const decoderParamDisableRingBufferReallocation = constants.BROTLI_DECODER_PARAM_DISABLE_RING_BUFFER_REALLOCATION;
export const decoderParamLargeWindow = constants.BROTLI_DECODER_PARAM_LARGE_WINDOW;
