/*
 * A category where Obj(JS) = JavaScript objects; Hom_JS(X,Y) is
 * typeof(X) === Y.
 */

function coproduct(a, b) {
  if (a === b) return a;
  if (typeof a === b) return b;
  if (typeof b === a) return a;
  if (typeof a === typeof b) return typeof a;
  return 'string';
}

/*
 * No product.  e.g. 42 and {} are not reachable from some object.
 */

function reachable(a) {
  var result = [];
  while (true) {
    result.push(a);
    if (a === 'string') break;
    a = typeof a;
  }
  return result;
}

VALUES = [42, {}, [3], 'string', 'x', 'number', 'object'];

function checkCoproductReachableFromAAndB() {
  VALUES.forEach(function (a) {
    VALUES.forEach(function (b) {
      var cop = coproduct(a, b);
      var as = reachable(a);
      var bs = reachable(b);
      console.assert(as.includes(cop));
      console.assert(bs.includes(cop));
    });
  })
}

checkCoproductReachableFromAAndB();
