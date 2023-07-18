#!/usr/bin/env node

// import zlib from "node:zlib";

// console.log(zlib.constants);

const go = (arr, current, step, max) => {
  arr.push(current);
  if (current === max) { return arr; }
  return go(arr, current + step, step, max);
}

for (var i = 0; i <= 3; i++) {
  const foo = go([], 0, 1 << i, 15 << i);
  console.log(`When 'Npostfix' is ${i}, then possible values are: ${foo.join(", ")}`);

  // Generate type class instances
  // for (var j = 0; j < foo.length; j++) {
  //   console.log(`${j == 0 ? "" : "else "}instance IsValidNDirectFor ${i} ${foo[j]}`);
  //   if (j + 1 === foo.length) { 
  //     console.log(`else instance Fail "number must be one of ${foo.join(", ")}" => IsValidNDirectFor ${i} i`); 
  //     console.log("");
  //   };
  // }
}
