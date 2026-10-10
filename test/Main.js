import assert from "node:assert/strict";
import { Readable, Writable, pipeline } from "node:stream";

export const roundTrip = (name) => (compress) => (decompress) => () => {
  const input = Buffer.from("PureScript zlib: asymmetric data \u0000 café! ".repeat(137));
  const chunks = [];
  const output = new Writable({
    write(chunk, encoding, callback) {
      chunks.push(chunk);
      callback();
    },
  });
  pipeline(
    Readable.from([input.subarray(0, 19), input.subarray(19)]),
    compress,
    decompress,
    output,
    (error) => {
      assert.ifError(error);
      assert.deepEqual(Buffer.concat(chunks), input, name);
      console.log(name + " round trip passed");
    }
  );
};
