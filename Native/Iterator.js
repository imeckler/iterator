Elm.Native.Iterator = {};
Elm.Native.Iterator.make = function(localRunTime) {
  localRunTime.Native = localRunTime.Native || {};
  localRunTime.Native.Iterator = localRunTime.Native.Iterator || {};

  var Utils = Elm.Native.Utils.make(localRunTime);

  function foldFun(f, init, g, len) {
    for (var i = 0; i < len; ++i) {
        init = A2(f, g(i), init);
    }
    return init;
  }

  function fold(f, init, t) {
    switch (t.ctor) {
      case "Fun":
        return foldFun(f, init, t._1, t._0);
      default:
        return foldFun(F2(function(tt, acc) { return fold(f, acc, tt); })
                , init, t._1, t._0);
    }
  }

  function foldWhileFunH(f, init, g, len) {
    for (var i = 0; i < len; ++i) {
      switch (init.ctor) {
        case "Finished":
          return init;
        default:
          init = A2(f, g(i), init._0);
      }
    }
    return init;
  }

  function foldWhileH(f, init, t) {
    switch (t.ctor) {
      case "Fun":
        return foldWhileFunH(f, init, t._1, t._0);
      default:
        return foldWhileFunH(F2(function(tt, acc) { return foldWhileH(f, {ctor:'KeepGoing', _0:acc}, tt); }) // argument order
                , init, t._1, t._0);
    }
  }

  function foldWhile(f, init, t) {
    return foldWhileH(f, init, t)._0;
  }

  if (localRunTime.Native.Iterator.values) {
    return localRunTime.Native.Iterator.values;
  }

  return localRunTime.Native.Iterator.values = {
    foldWhile : F3(foldWhile),
    fold : F3(fold)
  }
};
