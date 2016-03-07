var numeric = numeric || null;
if(numeric === null) throw('numeric.js not found')

Elm.Native.Numeric = {};
Elm.Native.Numeric.make = function(elm) {
  elm.Native = elm.Native || {};
  elm.Native.Numeric = elm.Native.Numeric || {};
  if (elm.Native.Numeric.values) return elm.Native.Numeric.values;

  var L = Elm.Native.List.make(elm);
  var Maybe = Elm.Maybe.make(elm);
  var Utils = Elm.Native.Utils.make(elm);
  var JS = {};

  function map(ary, f) {
    var len = ary.length, out = new Array(len);
    for(var i = 0, len = ary.length; i < len; i++)
      out[i] = f(ary[i], i);

    return out;
  }

  function matrixToJSArray(m) {
    return map(L.toArray(m), L.toArray);
  }

  function jsArrayToMatrix(m) {
    return L.fromArray(map(m, L.fromArray));
  }

  function mmDot(m1, m2) {
    m1 = matrixToJSArray(m1);
    m2 = matrixToJSArray(m2);

    var result = numeric.dot(m1, m2);
    return jsArrayToMatrix(result);
  }

  function transpose(m) {
    m = matrixToJSArray(m);
    var t = numeric.transpose(m);
    return jsArrayToMatrix(t);
  }

  function inverse(m) {
    m = matrixToJSArray(m);

    try {
      var result = numeric.inv(m);

      if(result && result[0] && result[0][0] &&
         result[0][0] !== Infinity && result[0][0] !== -Infinity)
        return Maybe.Just(jsArrayToMatrix(result))
      else
        return Maybe.Nothing
    } catch (e) {
      return Maybe.Nothing;
    }
  }

  function mmPlus(m1,m2) {
    m1 = matrixToJSArray(m1);
    m2 = matrixToJSArray(m2);

    return jsArrayToMatrix(numeric.add(m1,m2))
  }

  function makeImVect(len, im) {
    if(im === undefined) {
      im = new Array(len);
      for(var i = 0; i < len; i++) im[i] = 0;
    }

    return L.fromArray(im);
  }

  function makeImMatrix(len, im) {
    if(im === undefined) {
      im = new Array(len);
      for(var i = 0; i < len; i++) im[i] = 0;
    }

    return L.fromArray(im);
  }

  return Elm.Native.Numeric.values = {
    transpose: transpose,
    inverse: inverse,
    mmDot: F2(mmDot),
    mmPlus: F2(mmPlus)
  };
};
