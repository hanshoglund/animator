/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof Thunk) {
        if(t.f) {
            t.x = t.f();
            t.f = 0;
        }
        return t.x;
    }
    return t;
}

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.

   When a thunk is evaluated, by reading the member 'x' of the "pointer," the
   closure is evaluated and the getter removed, to be replaced with the value
   returned by the thunk, and the getter finally returns the return value of
   the closure.
*/

function T(f) {
    return new Thunk(f);
}

function Thunk(f) {
    this.f = f;
}

/* Integer literal
   Generates an Integer literal from a Number.
   This might be dependent on using integer-simple for Integers.
*/
function I(n) {
    if(n > 0) {
        return [1,[1, n, 2]];
    } else if(n < 0) {
        return [2,[1,n,2]];
    } else {
        return [3]
    }
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    f = f instanceof Thunk ? E(f) : f;
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!f.apply) {
        return f;
    }

    var arity = f.arity ? f.arity : f.length;
    if(args.length === arity) {
        return f.apply(null, args);
    }
    if(args.length > arity) {
        var first = args.splice(0, arity);
        return A(f.apply(null, first), args);
    } else {
        var g = function() {
            var as = args.concat(Array.prototype.slice.call(arguments));
            return A(f, as);
        };
        g.arity = arity - args.length;
        return g;
    }
}

/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [1, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [1, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

function log2(x) {
    var high = 1024;
    var low = -1024;
    var i = 0;
    var x2;
    for(;;) {
        x2 = Math.pow(2, i);
        if(x2 <= (x >> 1)) {
            low = i;
            i += (high - i) >> 1;
        } else if(x2 > x) {
            high = i;
            i += (low - i) >> 1;
        } else {
            return i;
        }
    }
    return i;
}

function decodeFloat(x) {
    if(isNaN(x)) {
        return [1, -6755399441055744, 972];
    }
    var sig = x > 0 ? 1 : -1;
    if(!isFinite(x)) {
        return [1, sig * 4503599627370496, 972];
    }
    x = Math.abs(x);
    var exp = log2(x)-52;
    var man = x/Math.pow(2, exp);
    return [1, sig*man, exp];
}

function decodeDouble(x) {
    var decoded = decodeFloat(x);
    var sign = decoded[1] < 0 ? -1 : 1;
    var mantissa = decoded[1]*sign;
    var manLow = mantissa % 0x100000000;
    var manHigh = Math.floor(mantissa / 0x100000000);
    return [1, sign, manHigh, manLow, decoded[2]];
}

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    // Use 0 for the never-examined state argument.
    return [1, 0, arr];
}

function err(str) {
    die(toJSStr(str)[1]);
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {
    return unAppCStr(str, [1]);
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [2,[1,str.charAt(i)],T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function fromJSStr(str) {
    return unCStr(E(str)[1]);
}

function toJSStr(str) {
    str = E(str);
    var s = '';
    while(str[0] == 2) {
        var cs = readHSUnicodeChar(str);
        s += cs[0];
        str = cs[1];
    }
    return [1,s];
}

function readHSUnicodeChar(str) {
    var c = E(str[1])[1];
    // If we get slashes, read all numbers we encounter.
    if(c == '\\') {
        var num = '';
        str = E(str[2]);
        if(str == 1) {
            return ['\\', str];
        }
        c = E(str[1])[1];
        while(c >= '0' && c <= '9') {
            num += c;
            str = E(str[2]);
            c = E(str[1])[1];
        }
        if(num.length == 0) {
            return ['\\', str];
        }
        c = String.fromCharCode(Number(num));
        return [c, str];
    } else {
        return [c, E(str[2])];
    }
}

// newMutVar
function nMV(val, st) {
    return [1,st,{x: val}];
}

// readMutVar
function rMV(mv, st) {
    return [1,st,mv.x];
}

// writeMutVar
function wMV(mv, val, st) {
    mv.x = val;
    return [1,st];
}

function localeEncoding(theWorld) {
    return [1,theWorld,'UTF-8'];
}

// every newSomethingSomethingByteArray
function newBA(size, theWorld) {
    var s = '';
    while(size >= 0) {
        s += '';
        --size;
    }
    return [1,theWorld,s];
}

function wOffAddr(addr, off, val, theWorld) {
    addr[off] = val;
    return theWorld;
}

function isDoubleNaN(d,_) {
    return [1,0,isNaN(d)];
}
var isFloatNaN = isDoubleNaN;

function isDoubleInfinite(d,_) {
    return [1,0,d === Infinity];
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x,_) {
    return [1,0,x===0 && (1/x)===-Infinity];
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b, _) {
    return [1, 0, a == b];
}

function strOrd(a, b, _) {
    var ord;
    if(a < b) {
        ord = [1];
    } else if(a == b) {
        ord = [2];
    } else {
        ord = [3];
    }
    return [1, 0, [1, ord]];
}

function jsCatch(act, handler, _) {
    try {
        return [1,0,A(act,[0])[2]];
    } catch(e) {
        return [1,0,A(handler,[e,0])[2]];
    }
}

function hs_eqWord64(a, b, _) {
    return [1,0,a==b];
}

var realWorld = 0;
var coercionToken = undefined;

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0]-1;
    } else {
        return x-1;
    }
}

function jsAlert(val,_) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
    return [1,0];
}

function jsLog(val,_) {
    console.log(val);
    return [1,0];
}

function jsPrompt(str,_) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return [1,0,val == undefined ? '' : val.toString()];
}

function jsEval(str,_) {
    var x = eval(str);
    return [1,0,x == undefined ? '' : x.toString()];
}

function isNull(obj,_) {
    return [1,0,[obj === null]];
}

function jsRead(str,_) {
    return [1,0,Number(str)];
}

function jsShowI(val, _) {return [1,0,val.toString()];}
function jsShow(val, _) {
    var ret = val.toString();
    return [1,0,val == Math.round(val) ? ret + '.0' : ret];
}

function jsSetCB(elem, evt, cb, _) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n') {
                A(cb,[[1,k.keyCode], 0]);
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {A(cb,[[1,x.button], 0]);};
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[1,x.keyCode], 0]);};
        break;        
    default:
        fun = function() {A(cb,[0]);};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return [1,0,true];
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return [1,0,true];
    }
    return [1,0,false];
}

function jsSetTimeout(msecs, cb, _) {
    window.setTimeout(function() {A(cb,[0]);}, msecs);
    return [1,0];
}

// Round a Float/Double.
function rintDouble(d, _) {
    return [1,0,Math.round(d)];
}
var rintFloat = rintDouble;

// Degenerate versions of u_iswspace, u_iswalnum and u_iswalpha.
function u_iswspace(c, _) {
    return [1,0, c==9 || c==10 || c==13 || c==32];
}

function u_iswalnum(c, _) {
    return [1,0, (c >= 48 && c <= 57) || u_iswalpha(c)[0]];
}

// [a-zA-ZåäöÅÄÖ]
function u_iswalpha(c, _) {
    return [1,0, (c >= 65 && c <= 90) || (c >= 97 && c <= 122) ||
                  c == 229 || c == 228 || c == 246 ||
                  c == 197 || c == 196 || c == 214];
}

function jsGet(elem, prop, _) {
    return [1,0,elem[prop].toString()];
}

function jsSet(elem, prop, val, _) {
    elem[prop] = val;
    return [1,0];
}

function jsGetStyle(elem, prop, _) {
    return [1,0,elem.style[prop].toString()];
}

function jsSetStyle(elem, prop, val, _) {
    elem.style[prop] = val;
    return [1,0];
}

function jsKillChild(child, parent, _) {
    parent.removeChild(child);
    return [1,0];
}

function jsClearChildren(elem, _) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
    return [1,0];
}

function jsFind(elem, _) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,0,[2,[1,e]]];
    }
    return [1,0,[1]];
}

function jsCreateElem(tag, _) {
    return [1,0,document.createElement(tag)];
}

function jsGetChildBefore(elem, _) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,0,[2,[1,elem]]];
        }
        elem = elem.previousSibling;
    }
    return [1,0,[1]];
}

function jsGetLastChild(elem, _) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,0,[2,[1,elem.childNodes[i]]]];
        }
    }
    return [1,0,[1]];
}

function jsGetChildren(elem, _) {
    var children = [1];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [2, [1,elem.childNodes[i]], children];
        }
    }
    return [1,0,children];
}

function jsSetChildren(elem, children, _) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 2) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
    return [1,0];
}

function jsAppendChild(child, container, _) {
    container.appendChild(child);
    return [1,0];
}

function jsAddChildBefore(child, container, after, _) {
    container.insertBefore(child, after);
    return [1,0];
}

function jsRand(_) {
    return [1,0,Math.random()];
}

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep, _) {
    var arr = [];
    strs = E(strs);
    while(strs[0] != 1) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return [1,0,arr.join(sep)];
}

// Escape all double quotes in a string
function jsUnquote(str, _) {
    return [1,0,str.replace(/"/, '\\"')];
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str, _) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [1,0,[1]];
    }
    return [1,0,[2,hs]];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [1, [1, jsRead(obj)[2]]];
    case 'string':
        return [2, [1, obj]];
        break;
    case 'boolean':
        return [3, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [4, arr2lst(obj, 0)];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [1];
            for(var i in ks) {
                xs = [2, [1, [1,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [5, xs];
        }
    }
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [1];
    }
    return [2, toHS(arr[elem]), T(function() {return arr2lst(arr,elem+1);})]
}

function ajaxReq(method, url, async, postdata, cb, _) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);
    xhr.setRequestHeader('Cache-control', 'no-cache');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                A(cb,[[1,xhr.responseText],0]);
            } else {
                A(cb,[[1,""],0]); // Nothing
            }
        }
    }
    xhr.send(postdata);
    return [1,0];
}

function u_towlower(charCode, _) {
    return [1, 0, String.fromCharCode(charCode).toLowerCase().charCodeAt(0)];
}

function u_towupper(charCode, _) {
    return [1, 0, String.fromCharCode(charCode).toUpperCase().charCodeAt(0)];
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar(st) {
    return [1, st, {empty: true}];
}

function tryTakeMVar(mv, st) {
    if(mv.empty) {
        return [1, st, 0, undefined];
    } else {
        mv.empty = true;
        mv.x = null;
        return [1, st, 1, mv.x];
    }
}

function takeMVar(mv, st) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    mv.empty = true;
    mv.x = null;
    return [1,st,mv.x];
}

function putMVar(mv, val, st) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
    return [1,st];
}

function tryPutMVar(mv, val, st) {
    if(!mv.empty) {
        return [1, st, 0];
    } else {
        mv.empty = false;
        mv.x = val;
        return [1, st, 1];
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv, st) {
    return [1, st, mv.empty ? 1 : 0];
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x, _world) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return [1,_world,x.stableName];
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var _0 = function(_1,_2,_3){var _4 = A(_1,[_3]);var _5 = _4[1];var _6 = A(_2,[_5]);return _6;};var _7 = function(_8,_9,_a){return _0(_8,_9,_a);};var _b = function(_c,_d,_e){var _f = A(_c,[_e]);var _g = _f[1];var _h = _f[2];var _i = A(_d,[_h,_g]);return _i;};var _j = function(_k,_l){return [1,_l,_k];};var _m = T(function(){return unCStr("Maybe.fromJust: Nothing");});var _n = T(function(){return err(_m);});var _o = function(_p,_q,_r){var _s = T(function(){var _t = A(_p,[_r]);var _u = _t[1];var _v = _t[2];var _w = T(function(){var _x = E(_s);if(_x[0]==1){var _y = E(_n);}else{var _z = _x[1];var _y = E(_z);}return _y;});var _A = A(_q,[_w]);var _B = _A[1];var _C = _A[2];var _D = hs_eqWord64(_u,_B,realWorld);var _E = _D[2];var _F = E(_E);if(_F){var _G = hs_eqWord64(_v,_C,realWorld);var _H = _G[2];var _I = E(_H);var _J = _I?[2,_r]:[1];var _K = _J;}else{var _K = [1];}return _K;});return E(_s);};var _L = function(_M){var _N = E(_M);var _O = _N[1];var _P = E(_O);return _P;};var _Q = T(function(){return unCStr("base");});var _R = T(function(){return unCStr("GHC.IO.Exception");});var _S = T(function(){return unCStr("IOException");});var _T = [1,7238999624334008320,1.0769272474234763e19,_Q,_R,_S];var _U = [1];var _V = [1,7238999624334008320,1.0769272474234763e19,_T,_U];var _W = function(_X){return E(_V);};var _Y = function(_Z){var _10 = E(_Z);var _11 = _10[1];var _12 = _10[2];var _13 = _L(_11);var _14 = _o(_13,_W,_12);return _14;};var _15 = function(_16,_17){var _18 = E(_16);if(_18[0]==1){var _19 = E(_17);}else{var _1a = _18[1];var _1b = _18[2];var _1c = T(function(){return _15(_1b,_17);});var _19 = [2,_1a,_1c];}return _19;};var _1d = T(function(){return unCStr(": ");});var _1e = T(function(){return unCStr("already exists");});var _1f = T(function(){return unCStr("does not exist");});var _1g = T(function(){return unCStr("protocol error");});var _1h = T(function(){return unCStr("failed");});var _1i = T(function(){return unCStr("invalid argument");});var _1j = T(function(){return unCStr("inappropriate type");});var _1k = T(function(){return unCStr("hardware fault");});var _1l = T(function(){return unCStr("unsupported operation");});var _1m = T(function(){return unCStr("timeout");});var _1n = T(function(){return unCStr("resource vanished");});var _1o = T(function(){return unCStr("interrupted");});var _1p = T(function(){return unCStr("resource busy");});var _1q = T(function(){return unCStr("resource exhausted");});var _1r = T(function(){return unCStr("end of file");});var _1s = T(function(){return unCStr("illegal operation");});var _1t = T(function(){return unCStr("permission denied");});var _1u = T(function(){return unCStr("user error");});var _1v = T(function(){return unCStr("unsatisified constraints");});var _1w = T(function(){return unCStr("system error");});var _1x = function(_1y,_1z){var _1A = E(_1y);switch(_1A[0]){case 1:var _1B = _15(_1e,_1z);break;case 2:var _1B = _15(_1f,_1z);break;case 3:var _1B = _15(_1p,_1z);break;case 4:var _1B = _15(_1q,_1z);break;case 5:var _1B = _15(_1r,_1z);break;case 6:var _1B = _15(_1s,_1z);break;case 7:var _1B = _15(_1t,_1z);break;case 8:var _1B = _15(_1u,_1z);break;case 9:var _1B = _15(_1v,_1z);break;case 10:var _1B = _15(_1w,_1z);break;case 11:var _1B = _15(_1g,_1z);break;case 12:var _1B = _15(_1h,_1z);break;case 13:var _1B = _15(_1i,_1z);break;case 14:var _1B = _15(_1j,_1z);break;case 15:var _1B = _15(_1k,_1z);break;case 16:var _1B = _15(_1l,_1z);break;case 17:var _1B = _15(_1m,_1z);break;case 18:var _1B = _15(_1n,_1z);break;case 19:var _1B = _15(_1o,_1z);break;}return _1B;};var _1C = T(function(){return unCStr(" (");});var _1D = [1,')'];var _1E = [1,'}'];var _1F = T(function(){return unCStr("{handle: ");});var _1G = function(_1H,_1I,_1J,_1K,_1L,_1M){var _1N = T(function(){var _1O = T(function(){var _1P = T(function(){var _1Q = E(_1K);if(_1Q[0]==1){var _1R = E(_1M);}else{var _1S = T(function(){var _1T = [2,_1D,_1M];return _15(_1Q,_1T);});var _1R = _15(_1C,_1S);}return _1R;});return _1x(_1I,_1P);});var _1U = E(_1J);if(_1U[0]==1){var _1V = E(_1O);}else{var _1W = T(function(){return _15(_1d,_1O);});var _1V = _15(_1U,_1W);}return _1V;});var _1X = E(_1L);if(_1X[0]==1){var _1Y = E(_1H);if(_1Y[0]==1){var _1Z = E(_1N);}else{var _20 = _1Y[1];var _21 = E(_20);if(_21[0]==1){var _22 = _21[1];var _23 = T(function(){var _24 = T(function(){return _15(_1d,_1N);});var _25 = [2,_1E,_24];return _15(_22,_25);});var _26 = _15(_1F,_23);}else{var _27 = _21[1];var _28 = T(function(){var _29 = T(function(){return _15(_1d,_1N);});var _2a = [2,_1E,_29];return _15(_27,_2a);});var _26 = _15(_1F,_28);}var _1Z = _26;}var _2b = _1Z;}else{var _2c = _1X[1];var _2d = T(function(){return _15(_1d,_1N);});var _2b = _15(_2c,_2d);}return _2b;};var _2e = function(_2f){var _2g = E(_2f);var _2h = _2g[1];var _2i = _2g[2];var _2j = _2g[3];var _2k = _2g[4];var _2l = _2g[6];var _2m = _1G(_2h,_2i,_2j,_2k,_2l,_U);return _2m;};var _2n = [1,','];var _2o = [1,']'];var _2p = [1,'['];var _2q = function(_2r,_2s){var _2t = E(_2r);if(_2t[0]==1){var _2u = unAppCStr("[]",_2s);}else{var _2v = _2t[1];var _2w = _2t[2];var _2x = T(function(){var _2y = E(_2v);var _2z = _2y[1];var _2A = _2y[2];var _2B = _2y[3];var _2C = _2y[4];var _2D = _2y[6];var _2E = T(function(){var _2F = [2,_2o,_2s];var _2G = function(_2H){var _2I = E(_2H);if(_2I[0]==1){var _2J = E(_2F);}else{var _2K = _2I[1];var _2L = _2I[2];var _2M = T(function(){var _2N = E(_2K);var _2O = _2N[1];var _2P = _2N[2];var _2Q = _2N[3];var _2R = _2N[4];var _2S = _2N[6];var _2T = T(function(){return _2G(_2L);});var _2U = _1G(_2O,_2P,_2Q,_2R,_2S,_2T);return _2U;});var _2J = [2,_2n,_2M];}return _2J;};return _2G(_2w);});var _2V = _1G(_2z,_2A,_2B,_2C,_2D,_2E);return _2V;});var _2u = [2,_2p,_2x];}return _2u;};var _2W = function(_2X,_2Y,_2Z){var _30 = E(_2Y);var _31 = _30[1];var _32 = _30[2];var _33 = _30[3];var _34 = _30[4];var _35 = _30[6];var _36 = _1G(_31,_32,_33,_34,_35,_2Z);return _36;};var _37 = [1,_2W,_2e,_2q];var _38 = T(function(){return [1,_W,_37,_39,_Y];});var _39 = function(_3a){return [1,_38,_3a];};var _3b = [1];var _3c = [8];var _3d = function(_3e){return [1,_3b,_3c,_U,_3e,_3b,_3b];};var _3f = function(_3g,_3h){var _3i = T(function(){var _3j = T(function(){return _3d(_3g);});return _39(_3j);});return die(_3i,_3h);};var _3k = function(_3l,_3m){return _3f(_3l,_3m);};var _3n = [1,_b,_7,_j,_3k];var _3o = function(_3p){var _3q = E(_3p);var _3r = _3q[1];var _3s = E(_3r);return _3s;};var _3t = function(_3u,_3v){return A(_3u,[_3v]);};var _3w = function(_3x){var _3y = E(_3x);var _3z = _3y[2];var _3A = E(_3z);return _3A;};var _3B = I(1);var _3C = function(_3D,_3E){var _3F = E(_3D);if(_3F[0]==1){var _3G = _3F[1];var _3H = _3F[2];var _3I = E(_3E);if(_3I[0]==1){var _3J = _3I[1];var _3K = _3I[2];var _3L = _3C(_3H,_3K);if(_3L[0]==2){var _3M = _3G<_3J;if(_3M){var _3N = [1];}else{var _3O = _3G>_3J;var _3N = _3O?[3]:[2];}var _3P = _3N;}else{var _3P = E(_3L);}var _3Q = _3P;}else{var _3Q = [3];}var _3R = _3Q;}else{var _3S = E(_3E);var _3R = _3S[0]==1?[1]:[2];}return _3R;};var _3T = [2];var _3U = [1,E(47),E(_3T)];var _3V = function(_3W,_3X,_3Y){var _3Z = E(_3W);if(_3Z[0]==1){var _40 = _3Z[1];var _41 = _3Z[2];var _42 = _40==_3X;if(_42){var _43 = _44(_41,_3Y);var _45 = _43[0]==1?[1,E(0),E(_43)]:[2];}else{var _46 = _40>_3X;if(_46){var _47 = _44(_41,_3Y);var _48 = _40-_3X>>>0;var _49 = [1,E(_48),E(_47)];var _4a = _49;}else{var _4b = _44(_41,_3Y);var _4c = _3V(_4b,1,_3T);var _4d = 4294967295-_3X>>>0;var _4e = _4d+1>>>0;var _4f = _4e+_40>>>0;var _4g = [1,E(_4f),E(_4c)];var _4a = _4g;}var _45 = _4a;}var _4h = _45;}else{var _4h = E(_3U);}return _4h;};var _44 = function(_4i,_4j){var _4k = E(_4i);if(_4k[0]==1){var _4l = _4k[1];var _4m = _4k[2];var _4n = E(_4j);if(_4n[0]==1){var _4o = _4n[1];var _4p = _4n[2];var _4q = _4l==_4o;if(_4q){var _4r = _44(_4m,_4p);var _4s = _4r[0]==1?[1,E(0),E(_4r)]:[2];}else{var _4t = _4l>_4o;if(_4t){var _4u = _44(_4m,_4p);var _4v = _4l-_4o>>>0;var _4w = [1,E(_4v),E(_4u)];var _4x = _4w;}else{var _4y = _44(_4m,_4p);var _4z = _3V(_4y,1,_3T);var _4A = 4294967295-_4o>>>0;var _4B = _4A+1>>>0;var _4C = _4B+_4l>>>0;var _4D = [1,E(_4C),E(_4z)];var _4x = _4D;}var _4s = _4x;}var _4E = _4s;}else{var _4E = E(_4k);}var _4F = _4E;}else{var _4G = E(_4j);var _4F = _4G[0]==1?E(_3U):[2];}return _4F;};var _4H = [1,E(1),E(_3T)];var _4I = function(_4J){var _4K = E(_4J);if(_4K[0]==1){var _4L = _4K[1];var _4M = _4K[2];var _4N = _4L==4294967295;if(_4N){var _4O = _4I(_4M);var _4P = [1,E(0),E(_4O)];var _4Q = _4P;}else{var _4R = _4L+1>>>0;var _4S = [1,E(_4R),E(_4M)];var _4Q = _4S;}var _4T = _4Q;}else{var _4T = E(_4H);}return _4T;};var _4U = T(function(){return _4I(_3T);});var _4V = function(_4W,_4X,_4Y,_4Z,_50){var _51 = _4X<_4Z;if(_51){var _52 = _4V(_4W,_4Z,_50,_4X,_4Y);}else{var _53 = _4Z>=2147483648;if(_53){var _54 = _55(1,_4Y,_50);var _56 = _4Z-2147483648>>>0;var _57 = _4X-2147483648>>>0;var _58 = _57+_56>>>0;var _59 = _58+_4W>>>0;var _5a = [1,E(_59),E(_54)];var _5b = _5a;}else{var _5c = _4X>=2147483648;if(_5c){var _5d = _4X-2147483648>>>0;var _5e = _5d+_4Z>>>0;var _5f = _5e+_4W>>>0;var _5g = _5f<2147483648;if(_5g){var _5h = _55(0,_4Y,_50);var _5i = _5f+2147483648>>>0;var _5j = [1,E(_5i),E(_5h)];var _5k = _5j;}else{var _5l = _55(1,_4Y,_50);var _5m = _5f-2147483648>>>0;var _5n = [1,E(_5m),E(_5l)];var _5k = _5n;}var _5o = _5k;}else{var _5p = _55(0,_4Y,_50);var _5q = _4X+_4Z>>>0;var _5r = _5q+_4W>>>0;var _5s = [1,E(_5r),E(_5p)];var _5o = _5s;}var _5b = _5o;}var _52 = _5b;}return _52;};var _55 = function(_5t,_5u,_5v){var _5w = E(_5u);if(_5w[0]==1){var _5x = _5w[1];var _5y = _5w[2];var _5z = E(_5v);if(_5z[0]==1){var _5A = _5z[1];var _5B = _5z[2];var _5C = _5x<_5A;if(_5C){var _5D = _4V(_5t,_5A,_5B,_5x,_5y);}else{var _5E = _5A>=2147483648;if(_5E){var _5F = _55(1,_5y,_5B);var _5G = _5A-2147483648>>>0;var _5H = _5x-2147483648>>>0;var _5I = _5H+_5G>>>0;var _5J = _5I+_5t>>>0;var _5K = [1,E(_5J),E(_5F)];var _5L = _5K;}else{var _5M = _5x>=2147483648;if(_5M){var _5N = _5x-2147483648>>>0;var _5O = _5N+_5A>>>0;var _5P = _5O+_5t>>>0;var _5Q = _5P<2147483648;if(_5Q){var _5R = _55(0,_5y,_5B);var _5S = _5P+2147483648>>>0;var _5T = [1,E(_5S),E(_5R)];var _5U = _5T;}else{var _5V = _55(1,_5y,_5B);var _5W = _5P-2147483648>>>0;var _5X = [1,E(_5W),E(_5V)];var _5U = _5X;}var _5Y = _5U;}else{var _5Z = _55(0,_5y,_5B);var _60 = _5x+_5A>>>0;var _61 = _60+_5t>>>0;var _62 = [1,E(_61),E(_5Z)];var _5Y = _62;}var _5L = _5Y;}var _5D = _5L;}var _63 = _5D;}else{var _64 = _5t==0;var _63 = _64?E(_5w):_4I(_5w);}var _65 = _63;}else{var _66 = E(_5v);if(_66[0]==1){var _67 = _5t==0;var _68 = _67?E(_66):_4I(_66);}else{var _69 = _5t==0;var _68 = _69?[2]:E(_4U);}var _65 = _68;}return _65;};var _6a = function(_6b,_6c){while(1){var _6d = E(_6b);switch(_6d[0]){case 1:var _6e = _6d[1];var _6f = E(_6c);switch(_6f[0]){case 1:var _6g = _6f[1];var _6h = _55(0,_6e,_6g);var _6i = [1,E(_6h)];var _6j = _6i;break;case 2:var _6k = _6f[1];var _6l = _3C(_6e,_6k);switch(_6l[0]){case 1:var _6m = _44(_6k,_6e);var _6n = [2,E(_6m)];var _6o = _6n;break;case 2:var _6o = [3];break;case 3:var _6p = _44(_6e,_6k);var _6q = [1,E(_6p)];var _6o = _6q;break;}var _6j = _6o;break;case 3:var _6j = E(_6d);break;}var _6r = _6j;break;case 2:var _6s = _6d[1];var _6t = E(_6c);switch(_6t[0]){case 1:var _6u = _6t[1];var _6v = [2,E(_6s)];var _6w = [1,E(_6u)];_6b=_6w;_6c=_6v;continue;var _6x = die("Unreachable!");break;case 2:var _6y = _6t[1];var _6z = _55(0,_6s,_6y);var _6A = [2,E(_6z)];var _6x = _6A;break;case 3:var _6x = E(_6d);break;}var _6r = _6x;break;case 3:var _6r = E(_6c);break;}return _6r;}};var _6B = function(_6C,_6D){var _6E = E(_6C);var _6F = T(function(){var _6G = _6a(_6E,_6D);var _6H = _6B(_6G,_6D);return _6H;});var _6I = [2,_6E,_6F];return _6I;};var _6J = function(_6K){return _6B(_6K,_3B);};var _6L = function(_6M){var _6N = E(_6M);switch(_6N[0]){case 1:var _6O = _6N[1];var _6P = [2,E(_6O)];break;case 2:var _6Q = _6N[1];var _6P = [1,E(_6Q)];break;case 3:var _6P = [3];break;}return _6P;};var _6R = function(_6S,_6T){var _6U = _6L(_6T);var _6V = _6a(_6S,_6U);return _6V;};var _6W = function(_6X,_6Y){var _6Z = T(function(){return _6R(_6Y,_6X);});return _6B(_6X,_6Z);};var _70 = I(0);var _71 = function(_72,_73){var _74 = E(_72);switch(_74[0]){case 1:var _75 = _74[1];var _76 = E(_73);if(_76[0]==1){var _77 = _76[1];var _78 = _3C(_75,_77);}else{var _78 = [3];}var _79 = _78;break;case 2:var _7a = _74[1];var _7b = E(_73);if(_7b[0]==2){var _7c = _7b[1];var _7d = _3C(_7c,_7a);}else{var _7d = [1];}var _79 = _7d;break;case 3:var _7e = E(_73);switch(_7e[0]){case 1:var _7f = [1];break;case 2:var _7f = [3];break;case 3:var _7f = [2];break;}var _79 = _7f;break;}return _79;};var _7g = function(_7h,_7i){var _7j = _71(_7h,_7i);return _7j[0]==1?false:true;};var _7k = function(_7l,_7m){var _7n = _71(_7l,_7m);return _7n[0]==3?true:false;};var _7o = function(_7p,_7q){var _7r = _71(_7p,_7q);return _7r[0]==1?true:false;};var _7s = function(_7t,_7u,_7v){var _7w = _7g(_7u,_70);if(_7w){var _7x = function(_7y){var _7z = _7k(_7y,_7v);if(_7z){var _7A = [1];}else{var _7B = T(function(){var _7C = _6a(_7y,_7u);var _7D = _7x(_7C);return _7D;});var _7A = [2,_7y,_7B];}return _7A;};var _7E = _7x(_7t);}else{var _7F = function(_7G){var _7H = _7o(_7G,_7v);if(_7H){var _7I = [1];}else{var _7J = T(function(){var _7K = _6a(_7G,_7u);var _7L = _7F(_7K);return _7L;});var _7I = [2,_7G,_7J];}return _7I;};var _7E = _7F(_7t);}return _7E;};var _7M = function(_7N,_7O,_7P){var _7Q = _6R(_7O,_7N);var _7R = _7s(_7N,_7Q,_7P);return _7R;};var _7S = function(_7T,_7U){return _7s(_7T,_3B,_7U);};var _7V = function(_7W){var _7X = E(_7W);switch(_7X[0]){case 1:var _7Y = _7X[1];var _7Z = E(_7Y);if(_7Z[0]==1){var _80 = _7Z[1];var _81 = E(_80);}else{var _81 = 0;}var _82 = _81;break;case 2:var _83 = _7X[1];var _84 = E(_83);if(_84[0]==1){var _85 = _84[1];var _86 = 0-_85>>>0;}else{var _86 = 0;}var _82 = _86;break;case 3:var _82 = 0;break;}return _82;};var _87 = function(_88){var _89 = _7V(_88);var _8a = _89&4294967295;return _8a;};var _8b = function(_8c){var _8d = _87(_8c);var _8e = [1,_8d];return _8e;};var _8f = function(_8g){return _6R(_8g,_3B);};var _8h = function(_8i){return _6a(_8i,_3B);};var _8j = function(_8k){var _8l = _8k==0;if(_8l){var _8m = [3];}else{var _8n = [1,E(_8k),E(_3T)];var _8m = [1,E(_8n)];}return _8m;};var _8o = function(_8p){var _8q = _8p>=0;if(_8q){var _8r = _8p>>>0;var _8s = _8j(_8r);var _8t = _8s;}else{var _8u = -_8p;var _8v = _8u>>>0;var _8w = _8j(_8v);var _8x = _6L(_8w);var _8t = _8x;}return _8t;};var _8y = function(_8z){var _8A = E(_8z);var _8B = _8A[1];var _8C = _8o(_8B);return _8C;};var _8D = [1,_8h,_8f,_8y,_8b,_6J,_6W,_7S,_7M];var _8E = function(_8F){var _8G = E(_8F);if(_8G[0]==2){var _8H = _8G[1];var _8I = [1,E(_8H)];}else{var _8I = E(_8G);}return _8I;};var _8J = [2,E(_4H)];var _8K = [1,E(_4H)];var _8L = function(_8M){var _8N = E(_8M);switch(_8N[0]){case 1:var _8O = E(_8K);break;case 2:var _8O = E(_8J);break;case 3:var _8O = [3];break;}return _8O;};var _8P = function(_8Q,_8R){var _8S = _8R>>>16;var _8T = (_8Q&65535)>>>0;var _8U = imul(_8T,_8S)>>>0;var _8V = (_8R&65535)>>>0;var _8W = _8Q>>>16;var _8X = imul(_8W,_8V)>>>0;var _8Y = _8U>>>16;var _8Z = _8X>>>16;var _90 = imul(_8W,_8S)>>>0;var _91 = _90+_8Z>>>0;var _92 = _91+_8Y>>>0;var _93 = imul(_8T,_8V)>>>0;var _94 = [1,E(_93),E(_3T)];var _95 = (_8U&65535)>>>0;var _96 = _95<<16>>>0;var _97 = (_8X&65535)>>>0;var _98 = _97<<16>>>0;var _99 = _4V(0,_98,_3T,_96,_3T);var _9a = _55(0,_99,_94);var _9b = _92==0;if(_9b){var _9c = E(_9a);}else{var _9d = [1,E(_92),E(_3T)];var _9e = [1,E(0),E(_9d)];var _9c = _55(0,_9e,_9a);}return _9c;};var _9f = function(_9g,_9h){while(1){var _9i = E(_9g);if(_9i[0]==1){var _9j = _9i[1];var _9k = _9i[2];var _9l = E(_9h);if(_9l[0]==1){var _9m = _9l[1];var _9n = _9l[2];var _9o = E(_9k);if(_9o[0]==1){var _9p = E(_9n);if(_9p[0]==1){var _9q = _9f(_9o,_9l);var _9r = [1,E(0),E(_9q)];var _9s = [1,E(_9j),E(_3T)];var _9t = _9f(_9s,_9l);var _9u = _55(0,_9t,_9r);var _9v = _9u;}else{var _9w = _9j==0;if(_9w){var _9x = _9f(_9o,_9l);var _9y = [1,E(0),E(_9x)];var _9z = _9y;}else{var _9A = _9f(_9o,_9l);var _9B = [1,E(0),E(_9A)];var _9C = _8P(_9j,_9m);var _9D = _55(0,_9C,_9B);var _9z = _9D;}var _9v = _9z;}var _9E = _9v;}else{var _9F = E(_9n);if(_9F[0]==1){_9g=_9l;_9h=_9i;continue;var _9G = die("Unreachable!");}else{var _9G = _8P(_9j,_9m);}var _9E = _9G;}var _9H = _9E;}else{var _9H = E(_3U);}var _9I = _9H;}else{var _9J = E(_9h);var _9I = _9J[0]==1?E(_3U):E(_3U);}return _9I;}};var _9K = function(_9L,_9M){var _9N = E(_9L);switch(_9N[0]){case 1:var _9O = _9N[1];var _9P = E(_9M);switch(_9P[0]){case 1:var _9Q = _9P[1];var _9R = _9f(_9O,_9Q);var _9S = [1,E(_9R)];var _9T = _9S;break;case 2:var _9U = _9P[1];var _9V = _9f(_9O,_9U);var _9W = [2,E(_9V)];var _9T = _9W;break;case 3:var _9T = [3];break;}var _9X = _9T;break;case 2:var _9Y = _9N[1];var _9Z = E(_9M);switch(_9Z[0]){case 1:var _a0 = _9Z[1];var _a1 = _9f(_9Y,_a0);var _a2 = [2,E(_a1)];var _a3 = _a2;break;case 2:var _a4 = _9Z[1];var _a5 = _9f(_9Y,_a4);var _a6 = [1,E(_a5)];var _a3 = _a6;break;case 3:var _a3 = [3];break;}var _9X = _a3;break;case 3:var _a7 = E(_9M);var _a8 = [3];var _9X = _a8;break;}return _9X;};var _a9 = function(_aa){return E(_aa);};var _ab = [1,_6a,_9K,_6R,_6L,_8E,_8L,_a9];var _ac = I(0);var _ad = [1,'-'];var _ae = [3];var _af = [1,E(_3U)];var _ag = [1];var _ah = function(_ai){var _aj = E(_ai);return _aj[0]==1?[1,E(_aj)]:[3];};var _ak = function(_al,_am,_an){while(1){var _ao = E(_am);if(_ao[0]==1){var _ap = E(_an);var _aq = [1,_al,_ap];var _ar = _aq;}else{var _as = _ao[1];var _at = _ao[2];var _au = _3C(_an,_as);if(_au[0]==1){var _av = _al<<1>>>0;_al=_av;_am=_at;_an=_an;continue;var _aw = die("Unreachable!");var _ax = _aw;}else{var _ay = _44(_an,_as);var _az = _al<<1>>>0;var _aA = _az+1>>>0;_al=_aA;_am=_at;_an=_ay;continue;var _aB = die("Unreachable!");var _ax = _aB;}var _ar = _ax;}return _ar;}};var _aC = function(_aD,_aE){var _aF = E(_aE);if(_aF){var _aG = 32-_aF|0;var _aH = function(_aI,_aJ){var _aK = E(_aJ);if(_aK[0]==1){var _aL = _aK[1];var _aM = _aK[2];var _aN = _aL>>>_aG;var _aO = _aH(_aN,_aM);var _aP = _aL<<_aF>>>0;var _aQ = (_aP|_aI)>>>0;var _aR = [1,E(_aQ),E(_aO)];var _aS = _aR;}else{var _aT = _aI==0;var _aS = _aT?[2]:[1,E(_aI),E(_3T)];}return _aS;};var _aU = _aH(0,_aD);var _aV = _aU;}else{var _aV = E(_aD);}return _aV;};var _aW = function(_aX,_aY){var _aZ = E(_aY);if(_aZ[0]==1){var _b0 = [1,E(_aX),E(_aZ)];}else{var _b1 = _aX==0;var _b0 = _b1?[2]:[1,E(_aX),E(_3T)];}return _b0;};var _b2 = function(_b3,_b4){var _b5 = E(_b4);var _b6 = T(function(){var _b7 = [2,_b5,_ag];var _b8 = function(_b9){var _ba = E(_b9);if(_ba){var _bb = T(function(){var _bc = _ba-1|0;var _bd = _b8(_bc);return _bd;});var _be = T(function(){return _aC(_b5,_ba);});var _bf = [2,_be,_bb];}else{var _bf = E(_b7);}return _bf;};return _b8(31);});var _bg = function(_bh){var _bi = E(_bh);if(_bi[0]==1){var _bj = _bi[1];var _bk = _bi[2];var _bl = _bg(_bk);var _bm = _bl[1];var _bn = _bl[2];var _bo = E(_bn);if(_bo[0]==1){var _bp = [1,E(_bj),E(_bo)];var _bq = _ak(0,_b6,_bp);var _br = _bq[1];var _bs = _bq[2];var _bt = T(function(){return _aW(_br,_bm);});var _bu = [1,_bt,_bs];var _bv = _bu;}else{var _bw = _bj==0;if(_bw){var _bx = _ak(0,_b6,_3T);var _by = _bx[1];var _bz = _bx[2];var _bA = T(function(){return _aW(_by,_bm);});var _bB = [1,_bA,_bz];var _bC = _bB;}else{var _bD = [1,E(_bj),E(_3T)];var _bE = _ak(0,_b6,_bD);var _bF = _bE[1];var _bG = _bE[2];var _bH = T(function(){return _aW(_bF,_bm);});var _bI = [1,_bH,_bG];var _bC = _bI;}var _bv = _bC;}var _bJ = _bv;}else{var _bJ = [1,_3T,_3T];}return _bJ;};var _bK = _bg(_b3);var _bL = _bK[1];var _bM = _bK[2];var _bN = T(function(){return _ah(_bM);});var _bO = T(function(){return _ah(_bL);});var _bP = [1,_bO,_bN];return _bP;};var _bQ = function(_bR,_bS){var _bT = E(_bR);if(_bT[0]==3){var _bU = E(_bS);var _bV = [1,_ae,_ae];var _bW = _bV;}else{var _bX = E(_bS);if(_bX[0]==3){var _bY = [1,_af,_af];}else{var _bZ = E(_bT);if(_bZ[0]==1){var _c0 = _bZ[1];var _c1 = E(_bX);if(_c1[0]==1){var _c2 = _c1[1];var _c3 = _b2(_c0,_c2);}else{var _c4 = _c1[1];var _c5 = _b2(_c0,_c4);var _c6 = _c5[1];var _c7 = _c5[2];var _c8 = T(function(){return _6L(_c6);});var _c9 = [1,_c8,_c7];var _c3 = _c9;}var _ca = _c3;}else{var _cb = _bZ[1];var _cc = E(_bX);if(_cc[0]==1){var _cd = _cc[1];var _ce = _b2(_cb,_cd);var _cf = _ce[1];var _cg = _ce[2];var _ch = T(function(){return _6L(_cg);});var _ci = T(function(){return _6L(_cf);});var _cj = [1,_ci,_ch];var _ck = _cj;}else{var _cl = _cc[1];var _cm = _b2(_cb,_cl);var _cn = _cm[1];var _co = _cm[2];var _cp = T(function(){return _6L(_co);});var _cq = [1,_cn,_cp];var _ck = _cq;}var _ca = _ck;}var _bY = _ca;}var _bW = _bY;}return _bW;};var _cr = function(_cs,_ct,_cu){while(1){var _cv = E(_cs);if(_cv==1){var _cw = 48+_ct|0;var _cx = String.fromCharCode(_cw);var _cy = [1,_cx];var _cz = [2,_cy,_cu];var _cA = _cz;}else{var _cB = _ct%10;var _cC = 48+_cB|0;var _cD = String.fromCharCode(_cC);var _cE = [1,_cD];var _cF = [2,_cE,_cu];var _cG = quot(_ct,10);var _cH = _cv-1|0;_cs=_cH;_ct=_cG;_cu=_cF;continue;var _cI = die("Unreachable!");var _cA = _cI;}return _cA;}};var _cJ = function(_cK,_cL){while(1){var _cM = _cK<10;if(_cM){var _cN = 48+_cK|0;var _cO = String.fromCharCode(_cN);var _cP = [1,_cO];var _cQ = [2,_cP,_cL];var _cR = _cQ;}else{var _cS = _cK%10;var _cT = 48+_cS|0;var _cU = String.fromCharCode(_cT);var _cV = [1,_cU];var _cW = [2,_cV,_cL];var _cX = quot(_cK,10);_cK=_cX;_cL=_cW;continue;var _cY = die("Unreachable!");var _cR = _cY;}return _cR;}};var _cZ = I(1000000000);var _d0 = function(_d1,_d2){var _d3 = E(_d1);if(_d3[0]==1){var _d4 = E(_d2);}else{var _d5 = _d3[1];var _d6 = _d3[2];var _d7 = _bQ(_d5,_cZ);var _d8 = _d7[1];var _d9 = _d7[2];var _da = _87(_d8);var _db = T(function(){var _dc = _87(_d9);var _dd = T(function(){return _d0(_d6,_d2);});var _de = _cr(9,_dc,_dd);return _de;});var _df = _cr(9,_da,_db);var _d4 = _df;}return _d4;};var _dg = function(_dh,_di){var _dj = E(_di);if(_dj[0]==1){var _dk = [1];}else{var _dl = _dj[1];var _dm = _dj[2];var _dn = _bQ(_dl,_dh);var _do = _dn[1];var _dp = _dn[2];var _dq = T(function(){return _dg(_dh,_dm);});var _dr = [2,_dp,_dq];var _ds = [2,_do,_dr];var _dk = _ds;}return _dk;};var _dt = T(function(){return unCStr("jsplith: []");});var _du = T(function(){return err(_dt);});var _dv = function(_dw,_dx){var _dy = _7k(_dw,_dx);if(_dy){var _dz = [2,_dx,_U];}else{var _dA = _9K(_dw,_dw);var _dB = _dv(_dA,_dx);if(_dB[0]==1){var _dC = E(_du);}else{var _dD = _dB[1];var _dE = _dB[2];var _dF = _bQ(_dD,_dw);var _dG = _dF[1];var _dH = _dF[2];var _dI = _7k(_dG,_ac);if(_dI){var _dJ = T(function(){return _dg(_dw,_dE);});var _dK = [2,_dH,_dJ];var _dL = [2,_dG,_dK];}else{var _dM = T(function(){return _dg(_dw,_dE);});var _dL = [2,_dH,_dM];}var _dC = _dL;}var _dz = _dC;}return _dz;};var _dN = function(_dO,_dP){var _dQ = E(_dO);if(_dQ[0]==1){var _dR = _dQ[1];var _dS = _dQ[2];var _dT = E(_dP);if(_dT[0]==1){var _dU = _dT[1];var _dV = _dT[2];var _dW = _dN(_dS,_dV);var _dX = (_dR&_dU)>>>0;var _dY = [1,E(_dX),E(_dW)];var _dZ = _dY;}else{var _dZ = [2];}var _e0 = _dZ;}else{var _e1 = E(_dP);var _e0 = _e1[0]==1?[2]:[2];}return _e0;};var _e2 = function(_e3,_e4){var _e5 = E(_e3);if(_e5[0]==1){var _e6 = _e5[1];var _e7 = _e5[2];var _e8 = E(_e4);if(_e8[0]==1){var _e9 = _e8[1];var _ea = _e8[2];var _eb = _e2(_e7,_ea);var _ec = (_e6&_e9)>>>0;var _ed = [1,E(_ec),E(_eb)];var _ee = _ed;}else{var _ee = [2];}var _ef = _ee;}else{var _ef = E(_e4);}return _ef;};var _eg = function(_eh){var _ei = E(_eh);if(_ei[0]==1){var _ej = _ei[1];var _ek = _ei[2];var _el = _eg(_ek);var _em = ~_ej;var _en = [1,E(_em),E(_el)];var _eo = _en;}else{var _eo = [2];}return _eo;};var _ep = function(_eq,_er){var _es = E(_eq);if(_es[0]==1){var _et = _es[1];var _eu = _es[2];var _ev = E(_er);if(_ev[0]==1){var _ew = _ev[1];var _ex = _ev[2];var _ey = _ep(_eu,_ex);var _ez = (_et|_ew)>>>0;var _eA = [1,E(_ez),E(_ey)];var _eB = _eA;}else{var _eB = E(_es);}var _eC = _eB;}else{var _eC = E(_er);}return _eC;};var _eD = function(_eE){var _eF = E(_eE);if(_eF[0]==1){var _eG = _eF[1];var _eH = _eF[2];var _eI = _eG==0;if(_eI){var _eJ = _eD(_eH);var _eK = _eJ[0]==1?[1,E(_eG),E(_eJ)]:[2];}else{var _eL = _eD(_eH);var _eM = [1,E(_eG),E(_eL)];var _eK = _eM;}var _eN = _eK;}else{var _eN = [2];}return _eN;};var _eO = function(_eP,_eQ){while(1){var _eR = E(_eP);if(_eR[0]==3){var _eS = E(_eQ);}else{var _eT = E(_eQ);if(_eT[0]==3){var _eU = E(_eR);}else{var _eV = E(_eR);if(_eV[0]==1){var _eW = _eV[1];var _eX = E(_eT);if(_eX[0]==1){var _eY = _eX[1];var _eZ = _ep(_eW,_eY);var _f0 = [1,E(_eZ)];var _f1 = _f0;}else{var _f2 = _eX[1];var _f3 = _3V(_f2,1,_3T);var _f4 = _eg(_eW);var _f5 = _e2(_f4,_f3);var _f6 = _4I(_f5);var _f7 = _eD(_f6);var _f8 = _f7[0]==1?[2,E(_f7)]:[3];var _f1 = _f8;}var _f9 = _f1;}else{var _fa = _eV[1];var _fb = E(_eT);if(_fb[0]==1){var _fc = _fb[1];var _fd = [2,E(_fa)];var _fe = [1,E(_fc)];_eP=_fe;_eQ=_fd;continue;var _ff = die("Unreachable!");}else{var _fg = _fb[1];var _fh = _3V(_fg,1,_3T);var _fi = _3V(_fa,1,_3T);var _fj = _dN(_fi,_fh);var _fk = _4I(_fj);var _fl = _eD(_fk);var _fm = _fl[0]==1?[2,E(_fl)]:[3];var _ff = _fm;}var _f9 = _ff;}var _eU = _f9;}var _eS = _eU;}return _eS;}};var _fn = function(_fo,_fp){while(1){var _fq = _fp>=32;if(_fq){var _fr = E(_fo);var _fs = _fp-32|0;var _ft = [1,E(0),E(_fr)];_fo=_ft;_fp=_fs;continue;var _fu = die("Unreachable!");var _fv = _fu;}else{var _fv = _aC(_fo,_fp);}return _fv;}};var _fw = function(_fx,_fy){var _fz = E(_fx);switch(_fz[0]){case 1:var _fA = _fz[1];var _fB = _fn(_fA,_fy);var _fC = [1,E(_fB)];var _fD = _fC;break;case 2:var _fE = _fz[1];var _fF = _fn(_fE,_fy);var _fG = [2,E(_fF)];var _fD = _fG;break;case 3:var _fD = [3];break;}return _fD;};var _fH = function(_fI){var _fJ = E(_fI);if(_fJ[0]==1){var _fK = [3];}else{var _fL = _fJ[1];var _fM = _fJ[2];var _fN = E(_fL);var _fO = _fN[1];var _fP = _fH(_fM);var _fQ = _fw(_fP,31);var _fR = _8o(_fO);var _fS = _eO(_fR,_fQ);var _fK = _fS;}return _fK;};var _fT = function(_fU,_fV){var _fW = E(_fU);if(_fW){var _fX = _fH(_fV);}else{var _fY = _fH(_fV);var _fZ = _6L(_fY);var _fX = _fZ;}return _fX;};var _g0 = [1,465661287];var _g1 = [2,_g0,_U];var _g2 = [1,660865024];var _g3 = [2,_g2,_g1];var _g4 = true;var _g5 = T(function(){return _fT(_g4,_g3);});var _g6 = T(function(){return unCStr("jprinth []");});var _g7 = T(function(){return err(_g6);});var _g8 = function(_g9,_ga){var _gb = _7o(_g9,_cZ);if(_gb){var _gc = _87(_g9);var _gd = _cJ(_gc,_ga);var _ge = _gd;}else{var _gf = _dv(_g5,_g9);if(_gf[0]==1){var _gg = E(_g7);}else{var _gh = _gf[1];var _gi = _gf[2];var _gj = _bQ(_gh,_cZ);var _gk = _gj[1];var _gl = _gj[2];var _gm = _87(_gk);var _gn = _gm>0;if(_gn){var _go = T(function(){var _gp = _87(_gl);var _gq = T(function(){return _d0(_gi,_ga);});var _gr = _cr(9,_gp,_gq);return _gr;});var _gs = _cJ(_gm,_go);}else{var _gt = _87(_gl);var _gu = T(function(){return _d0(_gi,_ga);});var _gv = _cJ(_gt,_gu);var _gs = _gv;}var _gg = _gs;}var _ge = _gg;}return _ge;};var _gw = function(_gx,_gy){var _gz = _7o(_gx,_ac);if(_gz){var _gA = T(function(){var _gB = _6L(_gx);var _gC = _g8(_gB,_gy);return _gC;});var _gD = [2,_ad,_gA];}else{var _gD = _g8(_gx,_gy);}return _gD;};var _gE = [1,')'];var _gF = [1,'('];var _gG = function(_gH,_gI,_gJ){var _gK = _gH>6;if(_gK){var _gL = _7o(_gI,_ac);if(_gL){var _gM = T(function(){var _gN = [2,_gE,_gJ];return _gw(_gI,_gN);});var _gO = [2,_gF,_gM];}else{var _gO = _gw(_gI,_gJ);}var _gP = _gO;}else{var _gP = _gw(_gI,_gJ);}return _gP;};var _gQ = function(_gR){return _gG(0,_gR,_U);};var _gS = function(_gT,_gU){var _gV = E(_gT);if(_gV[0]==1){var _gW = unAppCStr("[]",_gU);}else{var _gX = _gV[1];var _gY = _gV[2];var _gZ = T(function(){var _h0 = T(function(){var _h1 = [2,_2o,_gU];var _h2 = function(_h3){var _h4 = E(_h3);if(_h4[0]==1){var _h5 = E(_h1);}else{var _h6 = _h4[1];var _h7 = _h4[2];var _h8 = T(function(){var _h9 = T(function(){return _h2(_h7);});return _gG(0,_h6,_h9);});var _h5 = [2,_2n,_h8];}return _h5;};return _h2(_gY);});return _gG(0,_gX,_h0);});var _gW = [2,_2p,_gZ];}return _gW;};var _ha = function(_hb,_hc,_hd){var _he = E(_hb);var _hf = _he[1];var _hg = _gG(_hf,_hc,_hd);return _hg;};var _hh = [1,_ha,_gQ,_gS];var _hi = function(_hj){var _hk = E(_hj);var _hl = _hk[2];var _hm = E(_hl);return _hm;};var _hn = [1,0];var _ho = function(_hp,_hq,_hr){var _hs = E(_hr);if(_hs[0]==1){var _ht = E(_hq);}else{var _hu = _hs[1];var _hv = _hs[2];var _hw = T(function(){return _ho(_hp,_hu,_hv);});var _ht = A(_hp,[_hq,_hw]);}return _ht;};var _hx = T(function(){return unCStr(": empty list");});var _hy = T(function(){return unCStr("Prelude.");});var _hz = function(_hA){var _hB = T(function(){return _15(_hA,_hx);});var _hC = _15(_hy,_hB);var _hD = err(_hC);return _hD;};var _hE = T(function(){return unCStr("foldr1");});var _hF = T(function(){return _hz(_hE);});var _hG = function(_hH,_hI,_hJ){var _hK = T(function(){return A(_hI,[_hJ]);});var _hL = [2,_2n,_hK];return A(_hH,[_hL]);};var _hM = function(_hN){var _hO = T(function(){var _hP = E(_hN);if(_hP[0]==1){var _hQ = E(_hF);}else{var _hR = _hP[1];var _hS = _hP[2];var _hT = E(_hS);if(_hT[0]==1){var _hU = E(_hR);}else{var _hV = _hT[1];var _hW = _hT[2];var _hX = T(function(){return _ho(_hG,_hV,_hW);});var _hY = function(_hZ){var _i0 = T(function(){return A(_hX,[_hZ]);});var _i1 = [2,_2n,_i0];return A(_hR,[_i1]);};var _hU = E(_hY);}var _hQ = _hU;}return _hQ;});var _i2 = function(_i3){var _i4 = T(function(){var _i5 = [2,_gE,_i3];return A(_hO,[_i5]);});return [2,_gF,_i4];};return E(_i2);};var _i6 = function(_i7){var _i8 = E(_i7);var _i9 = _i8[1];var _ia = E(_i9);return _ia;};var _ib = function(_ic,_id,_ie,_if,_ig,_ih,_ii){var _ij = T(function(){return A(_i6,[_ie,_hn,_ih]);});var _ik = [2,_ij,_U];var _il = T(function(){return A(_i6,[_id,_hn,_ig]);});var _im = [2,_il,_ik];var _in = T(function(){return A(_i6,[_ic,_hn,_if]);});var _io = [2,_in,_im];return A(_hM,[_io,_ii]);};var _ip = function(_iq,_ir,_is,_it){var _iu = E(_it);var _iv = _iu[1];var _iw = _iu[2];var _ix = _iu[3];var _iy = _ib(_iq,_ir,_is,_iv,_iw,_ix,_U);return _iy;};var _iz = function(_iA,_iB,_iC,_iD,_iE){var _iF = E(_iD);if(_iF[0]==1){var _iG = unAppCStr("[]",_iE);}else{var _iH = _iF[1];var _iI = _iF[2];var _iJ = T(function(){var _iK = E(_iH);var _iL = _iK[1];var _iM = _iK[2];var _iN = _iK[3];var _iO = T(function(){var _iP = [2,_2o,_iE];var _iQ = function(_iR){var _iS = E(_iR);if(_iS[0]==1){var _iT = E(_iP);}else{var _iU = _iS[1];var _iV = _iS[2];var _iW = T(function(){var _iX = E(_iU);var _iY = _iX[1];var _iZ = _iX[2];var _j0 = _iX[3];var _j1 = T(function(){return _iQ(_iV);});var _j2 = _ib(_iA,_iB,_iC,_iY,_iZ,_j0,_j1);return _j2;});var _iT = [2,_2n,_iW];}return _iT;};return _iQ(_iI);});var _j3 = _ib(_iA,_iB,_iC,_iL,_iM,_iN,_iO);return _j3;});var _iG = [2,_2p,_iJ];}return _iG;};var _j4 = function(_j5,_j6,_j7,_j8,_j9,_ja){var _jb = E(_j9);var _jc = _jb[1];var _jd = _jb[2];var _je = _jb[3];var _jf = _ib(_j5,_j6,_j7,_jc,_jd,_je,_ja);return _jf;};var _jg = function(_jh,_ji,_jj){var _jk = function(_jl,_jm){return _iz(_jh,_ji,_jj,_jl,_jm);};var _jn = function(_jm){return _ip(_jh,_ji,_jj,_jm);};var _jo = function(_jp,_jl,_jm){return _j4(_jh,_ji,_jj,_jp,_jl,_jm);};return [1,_jo,_jn,_jk];};var _jq = T(function(){return unCStr("Prelude.(!!): negative index\n");});var _jr = T(function(){return err(_jq);});var _js = T(function(){return unCStr("Prelude.(!!): index too large\n");});var _jt = T(function(){return err(_js);});var _ju = function(_jv,_jw){while(1){var _jx = E(_jv);if(_jx[0]==1){var _jy = E(_jt);}else{var _jz = _jx[1];var _jA = _jx[2];var _jB = E(_jw);if(_jB){var _jC = _jB-1|0;_jv=_jA;_jw=_jC;continue;var _jD = die("Unreachable!");var _jE = _jD;}else{var _jE = E(_jz);}var _jy = _jE;}return _jy;}};var _jF = T(function(){return unCStr("ACK");});var _jG = T(function(){return unCStr("BEL");});var _jH = T(function(){return unCStr("BS");});var _jI = T(function(){return unCStr("SP");});var _jJ = [2,_jI,_U];var _jK = T(function(){return unCStr("US");});var _jL = [2,_jK,_jJ];var _jM = T(function(){return unCStr("RS");});var _jN = [2,_jM,_jL];var _jO = T(function(){return unCStr("GS");});var _jP = [2,_jO,_jN];var _jQ = T(function(){return unCStr("FS");});var _jR = [2,_jQ,_jP];var _jS = T(function(){return unCStr("ESC");});var _jT = [2,_jS,_jR];var _jU = T(function(){return unCStr("SUB");});var _jV = [2,_jU,_jT];var _jW = T(function(){return unCStr("EM");});var _jX = [2,_jW,_jV];var _jY = T(function(){return unCStr("CAN");});var _jZ = [2,_jY,_jX];var _k0 = T(function(){return unCStr("ETB");});var _k1 = [2,_k0,_jZ];var _k2 = T(function(){return unCStr("SYN");});var _k3 = [2,_k2,_k1];var _k4 = T(function(){return unCStr("NAK");});var _k5 = [2,_k4,_k3];var _k6 = T(function(){return unCStr("DC4");});var _k7 = [2,_k6,_k5];var _k8 = T(function(){return unCStr("DC3");});var _k9 = [2,_k8,_k7];var _ka = T(function(){return unCStr("DC2");});var _kb = [2,_ka,_k9];var _kc = T(function(){return unCStr("DC1");});var _kd = [2,_kc,_kb];var _ke = T(function(){return unCStr("DLE");});var _kf = [2,_ke,_kd];var _kg = T(function(){return unCStr("SI");});var _kh = [2,_kg,_kf];var _ki = T(function(){return unCStr("SO");});var _kj = [2,_ki,_kh];var _kk = T(function(){return unCStr("CR");});var _kl = [2,_kk,_kj];var _km = T(function(){return unCStr("FF");});var _kn = [2,_km,_kl];var _ko = T(function(){return unCStr("VT");});var _kp = [2,_ko,_kn];var _kq = T(function(){return unCStr("LF");});var _kr = [2,_kq,_kp];var _ks = T(function(){return unCStr("HT");});var _kt = [2,_ks,_kr];var _ku = [2,_jH,_kt];var _kv = [2,_jG,_ku];var _kw = [2,_jF,_kv];var _kx = T(function(){return unCStr("ENQ");});var _ky = [2,_kx,_kw];var _kz = T(function(){return unCStr("EOT");});var _kA = [2,_kz,_ky];var _kB = T(function(){return unCStr("ETX");});var _kC = [2,_kB,_kA];var _kD = T(function(){return unCStr("STX");});var _kE = [2,_kD,_kC];var _kF = T(function(){return unCStr("SOH");});var _kG = [2,_kF,_kE];var _kH = T(function(){return unCStr("NUL");});var _kI = [2,_kH,_kG];var _kJ = function(_kK,_kL){while(1){var _kM = _kK<10;if(_kM){var _kN = 48+_kK|0;var _kO = String.fromCharCode(_kN);var _kP = [1,_kO];var _kQ = [2,_kP,_kL];var _kR = _kQ;}else{var _kS = _kK%10;var _kT = 48+_kS|0;var _kU = String.fromCharCode(_kT);var _kV = [1,_kU];var _kW = [2,_kV,_kL];var _kX = quot(_kK,10);_kK=_kX;_kL=_kW;continue;var _kY = die("Unreachable!");var _kR = _kY;}return _kR;}};var _kZ = function(_l0,_l1){var _l2 = _l0<0;if(_l2){var _l3 = E(_l0);if(_l3==(-2147483648)){var _l4 = T(function(){var _l5 = T(function(){return _kJ(8,_l1);});return _kJ(214748364,_l5);});var _l6 = [2,_ad,_l4];}else{var _l7 = T(function(){var _l8 = -_l3;var _l9 = _kJ(_l8,_l1);return _l9;});var _l6 = [2,_ad,_l7];}var _la = _l6;}else{var _la = _kJ(_l0,_l1);}return _la;};var _lb = [1,'\\'];var _lc = T(function(){return unCStr("\\DEL");});var _ld = T(function(){return unCStr("\\a");});var _le = T(function(){return unCStr("\\\\");});var _lf = T(function(){return unCStr("\\SO");});var _lg = T(function(){return unCStr("\\r");});var _lh = T(function(){return unCStr("\\f");});var _li = T(function(){return unCStr("\\v");});var _lj = T(function(){return unCStr("\\n");});var _lk = T(function(){return unCStr("\\t");});var _ll = T(function(){return unCStr("\\b");});var _lm = function(_ln,_lo){var _lp = _ln>'\DEL';if(_lp){var _lq = T(function(){var _lr = T(function(){var _ls = E(_lo);if(_ls[0]==1){var _lt = [1];}else{var _lu = _ls[1];var _lv = E(_lu);var _lw = _lv[1];var _lx = _lw>='0';if(_lx){var _ly = _lw<='9';var _lz = _ly?unAppCStr("\\&",_ls):E(_ls);}else{var _lz = E(_ls);}var _lt = _lz;}return _lt;});var _lA = _ln.charCodeAt(0);var _lB = _kZ(_lA,_lr);return _lB;});var _lC = [2,_lb,_lq];}else{var _lD = E(_ln);switch(_lD){case '\\':var _lE = _15(_le,_lo);break;case '\DEL':var _lE = _15(_lc,_lo);break;default:var _lF = _lD>=' ';if(_lF){var _lG = [1,_lD];var _lH = [2,_lG,_lo];}else{var _lI = E(_lD);switch(_lI){case '\a':var _lJ = _15(_ld,_lo);break;case '\b':var _lJ = _15(_ll,_lo);break;case '\t':var _lJ = _15(_lk,_lo);break;case '\n':var _lJ = _15(_lj,_lo);break;case '\v':var _lJ = _15(_li,_lo);break;case '\f':var _lJ = _15(_lh,_lo);break;case '\r':var _lJ = _15(_lg,_lo);break;case '\SO':var _lK = T(function(){var _lL = E(_lo);if(_lL[0]==1){var _lM = [1];}else{var _lN = _lL[1];var _lO = E(_lN);var _lP = _lO[1];var _lQ = E(_lP);var _lR = _lQ=='H'?unAppCStr("\\&",_lL):E(_lL);var _lM = _lR;}return _lM;});var _lJ = _15(_lf,_lK);break;default:var _lS = T(function(){var _lT = _lI.charCodeAt(0);var _lU = _lT<0;var _lV = _lU?E(_jr):_ju(_kI,_lT);return _lV;});var _lW = [2,_lb,_lS];var _lJ = _15(_lW,_lo);}var _lH = _lJ;}var _lE = _lH;}var _lC = _lE;}return _lC;};var _lX = [1,'\''];var _lY = [2,_lX,_U];var _lZ = T(function(){return unCStr("'\\''");});var _m0 = function(_m1){var _m2 = E(_m1);var _m3 = _m2[1];var _m4 = E(_m3);if(_m4=='\''){var _m5 = E(_lZ);}else{var _m6 = T(function(){return _lm(_m4,_lY);});var _m5 = [2,_lX,_m6];}return _m5;};var _m7 = [1,'"'];var _m8 = T(function(){return unCStr("\\\"");});var _m9 = function(_ma,_mb){var _mc = E(_ma);if(_mc[0]==1){var _md = E(_mb);}else{var _me = _mc[1];var _mf = _mc[2];var _mg = E(_me);var _mh = _mg[1];var _mi = E(_mh);if(_mi=='"'){var _mj = T(function(){return _m9(_mf,_mb);});var _mk = _15(_m8,_mj);}else{var _ml = T(function(){return _m9(_mf,_mb);});var _mk = _lm(_mi,_ml);}var _md = _mk;}return _md;};var _mm = function(_mn,_mo){var _mp = T(function(){var _mq = [2,_m7,_mo];return _m9(_mn,_mq);});return [2,_m7,_mp];};var _mr = function(_jm){return _15(_lZ,_jm);};var _ms = function(_mt,_mu){var _mv = E(_mu);var _mw = _mv[1];var _mx = E(_mw);if(_mx=='\''){var _my = E(_mr);}else{var _mz = function(_mA){var _mB = T(function(){var _mC = [2,_lX,_mA];return _lm(_mx,_mC);});return [2,_lX,_mB];};var _my = E(_mz);}return _my;};var _mD = [1,_ms,_m0,_mm];var _mE = function(_mF){var _mG = E(_mF);var _mH = _mG[3];var _mI = E(_mH);return _mI;};var _mJ = function(_mK,_mL){return A(_mE,[_mK,_mL,_U]);};var _mM = function(_mN,_mO,_mP){var _mQ = E(_mO);if(_mQ[0]==1){var _mR = unAppCStr("[]",_mP);}else{var _mS = _mQ[1];var _mT = _mQ[2];var _mU = T(function(){var _mV = T(function(){var _mW = [2,_2o,_mP];var _mX = function(_mY){var _mZ = E(_mY);if(_mZ[0]==1){var _n0 = E(_mW);}else{var _n1 = _mZ[1];var _n2 = _mZ[2];var _n3 = T(function(){var _n4 = T(function(){return _mX(_n2);});return A(_mN,[_n1,_n4]);});var _n0 = [2,_2n,_n3];}return _n0;};return _mX(_mT);});return A(_mN,[_mS,_mV]);});var _mR = [2,_2p,_mU];}return _mR;};var _n5 = function(_n6,_n7,_n8){var _n9 = T(function(){return _mE(_n6);});return _mM(_n9,_n7,_n8);};var _na = function(_nb){var _nc = T(function(){return _mE(_nb);});var _nd = function(_jl,_jm){return _n5(_nb,_jl,_jm);};var _ne = function(_jm){return _mJ(_nb,_jm);};var _nf = function(_ng){return E(_nc);};return [1,_nf,_ne,_nd];};var _nh = T(function(){return A(_na,[_mD]);});var _ni = T(function(){return A(_jg,[_nh,_nh,_nh]);});var _nj = function(_nk,_nl,_nm){var _nn = _nl<0;if(_nn){var _no = _nk>6;if(_no){var _np = T(function(){var _nq = [2,_gE,_nm];return _kZ(_nl,_nq);});var _nr = [2,_gF,_np];}else{var _nr = _kZ(_nl,_nm);}var _ns = _nr;}else{var _ns = _kZ(_nl,_nm);}return _ns;};var _nt = function(_nu){var _nv = E(_nu);var _nw = _nv[1];var _nx = _nj(0,_nw,_U);return _nx;};var _ny = function(_nz,_nA){var _nB = E(_nz);if(_nB[0]==1){var _nC = unAppCStr("[]",_nA);}else{var _nD = _nB[1];var _nE = _nB[2];var _nF = T(function(){var _nG = E(_nD);var _nH = _nG[1];var _nI = T(function(){var _nJ = [2,_2o,_nA];var _nK = function(_nL){var _nM = E(_nL);if(_nM[0]==1){var _nN = E(_nJ);}else{var _nO = _nM[1];var _nP = _nM[2];var _nQ = T(function(){var _nR = E(_nO);var _nS = _nR[1];var _nT = T(function(){return _nK(_nP);});var _nU = _nj(0,_nS,_nT);return _nU;});var _nN = [2,_2n,_nQ];}return _nN;};return _nK(_nE);});var _nV = _nj(0,_nH,_nI);return _nV;});var _nC = [2,_2p,_nF];}return _nC;};var _nW = function(_nX,_nY,_nZ){var _o0 = E(_nX);var _o1 = _o0[1];var _o2 = E(_nY);var _o3 = _o2[1];var _o4 = _nj(_o1,_o3,_nZ);return _o4;};var _o5 = [1,_nW,_nt,_ny];var _o6 = T(function(){return A(_jg,[_o5,_o5,_o5]);});var _o7 = [1];var _o8 = function(_o9,_oa,_ob){var _oc = T(function(){return A(_oa,[_ob]);});return A(_o9,[_oc]);};var _od = function(_oe,_of){var _og = E(_oe);var _oh = _og[1];var _oi = E(_of);var _oj = _oi[1];var _ok = _oh==_oj;var _ol = _ok?false:true;return _ol;};var _om = function(_on,_oo){var _op = E(_on);var _oq = _op[1];var _or = E(_oo);var _os = _or[1];var _ot = _oq==_os;return _ot;};var _ou = [1,_om,_od];var _ov = function(_ow){var _ox = E(_ow);var _oy = _ox[1];var _oz = _oy>=0;if(_oz){var _oA = E(_ox);}else{var _oB = -_oy;var _oC = [1,_oB];var _oA = _oC;}return _oA;};var _oD = function(_oE){var _oF = E(_oE);if(_oF[0]==1){var _oG = _oF[1];var _oH = _oF[2];var _oI = _oD(_oH);var _oJ = (_oG&65535)>>>0;var _oK = _oJ&4294967295;var _oL = _oK;var _oM = Math.pow(2,16);var _oN = _oG>>>16;var _oO = _oN&4294967295;var _oP = _oO;var _oQ = _oP*_oM;var _oR = Math.pow(2,32);var _oS = _oI*_oR;var _oT = _oS+_oQ;var _oU = _oT+_oL;var _oV = _oU;}else{var _oV = 0;}return _oV;};var _oW = function(_oX){var _oY = E(_oX);switch(_oY[0]){case 1:var _oZ = _oY[1];var _p0 = _oD(_oZ);break;case 2:var _p1 = _oY[1];var _p2 = _oD(_p1);var _p3 = -_p2;var _p0 = _p3;break;case 3:var _p0 = 0;break;}return _p0;};var _p4 = function(_p5){var _p6 = _oW(_p5);var _p7 = [1,_p6];return _p7;};var _p8 = [1,0];var _p9 = [1,1];var _pa = [1,(-1)];var _pb = function(_pc){var _pd = E(_pc);var _pe = _pd[1];var _pf = _pe==0;if(_pf){var _pg = E(_p8);}else{var _ph = _pe>0;var _pg = _ph?E(_p9):E(_pa);}return _pg;};var _pi = function(_pj,_pk){var _pl = E(_pj);var _pm = _pl[1];var _pn = E(_pk);var _po = _pn[1];var _pp = _pm-_po;var _pq = [1,_pp];return _pq;};var _pr = function(_ps){var _pt = E(_ps);var _pu = _pt[1];var _pv = -_pu;var _pw = [1,_pv];return _pw;};var _px = function(_py,_pz){var _pA = E(_py);var _pB = _pA[1];var _pC = E(_pz);var _pD = _pC[1];var _pE = _pB+_pD;var _pF = [1,_pE];return _pF;};var _pG = function(_pH,_pI){var _pJ = E(_pH);var _pK = _pJ[1];var _pL = E(_pI);var _pM = _pL[1];var _pN = _pK*_pM;var _pO = [1,_pN];return _pO;};var _pP = [1,_px,_pG,_pi,_pr,_ov,_pb,_p4];var _pQ = [1,'-'];var _pR = function(_pS,_pT){var _pU = E(_pT);if(_pU[0]==1){var _pV = [1];}else{var _pW = _pU[1];var _pX = _pU[2];var _pY = T(function(){return _pR(_pS,_pX);});var _pZ = T(function(){return A(_pS,[_pW]);});var _pV = [2,_pZ,_pY];}return _pV;};var _q0 = T(function(){return unCStr("base");});var _q1 = T(function(){return unCStr("Control.Exception.Base");});var _q2 = T(function(){return unCStr("PatternMatchFail");});var _q3 = [1,1.605959309876327e19,1.3945565038419476e19,_q0,_q1,_q2];var _q4 = [1,1.605959309876327e19,1.3945565038419476e19,_q3,_U];var _q5 = function(_q6){return E(_q4);};var _q7 = function(_q8){var _q9 = E(_q8);var _qa = _q9[1];var _qb = _q9[2];var _qc = _L(_qa);var _qd = _o(_qc,_q5,_qb);return _qd;};var _qe = function(_qf){var _qg = E(_qf);var _qh = _qg[1];var _qi = E(_qh);return _qi;};var _qj = function(_qk,_ql){var _qm = E(_qk);if(_qm[0]==1){var _qn = unAppCStr("[]",_ql);}else{var _qo = _qm[1];var _qp = _qm[2];var _qq = T(function(){var _qr = E(_qo);var _qs = _qr[1];var _qt = T(function(){var _qu = [2,_2o,_ql];var _qv = function(_qw){var _qx = E(_qw);if(_qx[0]==1){var _qy = E(_qu);}else{var _qz = _qx[1];var _qA = _qx[2];var _qB = T(function(){var _qC = E(_qz);var _qD = _qC[1];var _qE = T(function(){return _qv(_qA);});var _qF = _15(_qD,_qE);return _qF;});var _qy = [2,_2n,_qB];}return _qy;};return _qv(_qp);});var _qG = _15(_qs,_qt);return _qG;});var _qn = [2,_2p,_qq];}return _qn;};var _qH = function(_qI,_qJ,_qK){var _qL = E(_qJ);var _qM = _qL[1];var _qN = _15(_qM,_qK);return _qN;};var _qO = [1,_qH,_qe,_qj];var _qP = T(function(){return [1,_q5,_qO,_qQ,_q7];});var _qQ = function(_qR){return [1,_qP,_qR];};var _qS = T(function(){return unCStr("Irrefutable pattern failed for pattern");});var _qT = function(_qU,_qV){var _qW = T(function(){return A(_qV,[_qU]);});return die(_qW);};var _qX = [1,' '];var _qY = [1,'\n'];var _qZ = [2,_qY,_U];var _r0 = function(_r1){var _r2 = E(_r1);var _r3 = _r2[1];var _r4 = E(_r3);var _r5 = _r4=='|'?false:true;return _r5;};var _r6 = function(_r7,_r8){var _r9 = E(_r8);if(_r9[0]==1){var _ra = [1,_U,_U];}else{var _rb = _r9[1];var _rc = _r9[2];var _rd = A(_r7,[_rb]);if(_rd){var _re = T(function(){var _rf = _r6(_r7,_rc);var _rg = _rf[1];var _rh = _rf[2];var _ri = [1,_rg,_rh];return _ri;});var _rj = T(function(){var _rk = E(_re);var _rl = _rk[2];var _rm = E(_rl);return _rm;});var _rn = T(function(){var _ro = E(_re);var _rp = _ro[1];var _rq = E(_rp);return _rq;});var _rr = [2,_rb,_rn];var _rs = [1,_rr,_rj];}else{var _rs = [1,_U,_r9];}var _ra = _rs;}return _ra;};var _rt = function(_ru,_rv){var _rw = unCStr(_ru);var _rx = _r6(_r0,_rw);var _ry = _rx[1];var _rz = _rx[2];var _rA = function(_rB,_rC){var _rD = T(function(){var _rE = T(function(){var _rF = T(function(){return _15(_rC,_qZ);});return _15(_rv,_rF);});return unAppCStr(": ",_rE);});return _15(_rB,_rD);};var _rG = E(_rz);if(_rG[0]==1){var _rH = _rA(_ry,_U);}else{var _rI = _rG[1];var _rJ = _rG[2];var _rK = E(_rI);var _rL = _rK[1];var _rM = E(_rL);if(_rM=='|'){var _rN = [2,_qX,_rJ];var _rO = _rA(_ry,_rN);}else{var _rO = _rA(_ry,_U);}var _rH = _rO;}return _rH;};var _rP = function(_rQ){var _rR = T(function(){return _rt(_rQ,_qS);});var _rS = [1,_rR];return _qT(_rS,_qQ);};var _rT = T(function(){return _rP("GHC/Float.lhs:631:11-64|d : ds'");});var _rU = [1,0];var _rV = function(_rW){var _rX = T(function(){return _kZ(_rW,_U);});var _rY = unAppCStr("Char.intToDigit: not a digit ",_rX);var _rZ = err(_rY);return _rZ;};var _s0 = function(_s1){var _s2 = T(function(){var _s3 = _s1>=10;if(_s3){var _s4 = _s1<=15;if(_s4){var _s5 = 87+_s1|0;var _s6 = String.fromCharCode(_s5);var _s7 = [1,_s6];var _s8 = _s7;}else{var _s8 = _rV(_s1);}var _s9 = _s8;}else{var _s9 = _rV(_s1);}return _s9;});var _sa = _s1>=0;if(_sa){var _sb = _s1<=9;if(_sb){var _sc = 48+_s1|0;var _sd = String.fromCharCode(_sc);var _se = _sd;}else{var _sf = E(_s2);var _sg = _sf[1];var _sh = E(_sg);var _se = _sh;}var _si = _se;}else{var _sj = E(_s2);var _sk = _sj[1];var _sl = E(_sk);var _si = _sl;}return _si;};var _sm = function(_sn){var _so = E(_sn);var _sp = _so[1];var _sq = _s0(_sp);var _sr = [1,_sq];return _sr;};var _ss = function(_st,_su){var _sv = E(_st);var _sw = _sv[1];var _sx = _sw>0;if(_sx){var _sy = _pR(_sm,_su);if(_sy[0]==1){var _sz = E(_rT);}else{var _sA = _sy[1];var _sB = _sy[2];var _sz = [1,_sA,_sB];}var _sC = _sz;}else{var _sD = [2,_rU,_su];var _sE = _pR(_sm,_sD);if(_sE[0]==1){var _sF = E(_rT);}else{var _sG = _sE[1];var _sH = _sE[2];var _sF = [1,_sG,_sH];}var _sC = _sF;}return _sC;};var _sI = T(function(){return unCStr("base");});var _sJ = T(function(){return unCStr("GHC.Exception");});var _sK = T(function(){return unCStr("ArithException");});var _sL = [1,3089387606753565184,7918018744409604096,_sI,_sJ,_sK];var _sM = [1,3089387606753565184,7918018744409604096,_sL,_U];var _sN = function(_sO){return E(_sM);};var _sP = function(_sQ){var _sR = E(_sQ);var _sS = _sR[1];var _sT = _sR[2];var _sU = _L(_sS);var _sV = _o(_sU,_sN,_sT);return _sV;};var _sW = T(function(){return unCStr("denormal");});var _sX = T(function(){return unCStr("divide by zero");});var _sY = T(function(){return unCStr("loss of precision");});var _sZ = T(function(){return unCStr("arithmetic underflow");});var _t0 = T(function(){return unCStr("arithmetic overflow");});var _t1 = function(_t2){var _t3 = E(_t2);switch(_t3[0]){case 1:var _t4 = E(_t0);break;case 2:var _t4 = E(_sZ);break;case 3:var _t4 = E(_sY);break;case 4:var _t4 = E(_sX);break;case 5:var _t4 = E(_sW);break;}return _t4;};var _t5 = function(_t6,_t7){var _t8 = E(_t6);if(_t8[0]==1){var _t9 = unAppCStr("[]",_t7);}else{var _ta = _t8[1];var _tb = _t8[2];var _tc = T(function(){var _td = T(function(){var _te = [2,_2o,_t7];var _tf = function(_tg){var _th = E(_tg);if(_th[0]==1){var _ti = E(_te);}else{var _tj = _th[1];var _tk = _th[2];var _tl = T(function(){var _tm = E(_tj);switch(_tm[0]){case 1:var _tn = T(function(){return _tf(_tk);});var _to = _15(_t0,_tn);break;case 2:var _tp = T(function(){return _tf(_tk);});var _to = _15(_sZ,_tp);break;case 3:var _tq = T(function(){return _tf(_tk);});var _to = _15(_sY,_tq);break;case 4:var _tr = T(function(){return _tf(_tk);});var _to = _15(_sX,_tr);break;case 5:var _ts = T(function(){return _tf(_tk);});var _to = _15(_sW,_ts);break;}return _to;});var _ti = [2,_2n,_tl];}return _ti;};return _tf(_tb);});var _tt = E(_ta);switch(_tt[0]){case 1:var _tu = _15(_t0,_td);break;case 2:var _tu = _15(_sZ,_td);break;case 3:var _tu = _15(_sY,_td);break;case 4:var _tu = _15(_sX,_td);break;case 5:var _tu = _15(_sW,_td);break;}return _tu;});var _t9 = [2,_2p,_tc];}return _t9;};var _tv = function(_tw){return _15(_t0,_tw);};var _tx = function(_tw){return _15(_sW,_tw);};var _ty = function(_tw){return _15(_sX,_tw);};var _tz = function(_tw){return _15(_sY,_tw);};var _tA = function(_tw){return _15(_sZ,_tw);};var _tB = function(_tC,_tD){var _tE = E(_tD);switch(_tE[0]){case 1:var _tF = E(_tv);break;case 2:var _tF = E(_tA);break;case 3:var _tF = E(_tz);break;case 4:var _tF = E(_ty);break;case 5:var _tF = E(_tx);break;}return _tF;};var _tG = [1,_tB,_t1,_t5];var _tH = T(function(){return [1,_sN,_tG,_tI,_sP];});var _tI = function(_tw){return [1,_tH,_tw];};var _tJ = [4];var _tK = T(function(){return _qT(_tJ,_tI);});var _tL = I(1);var _tM = I(2);var _tN = T(function(){return unCStr("(Array.!): undefined array element");});var _tO = T(function(){return err(_tN);});var _tP = [1,0];var _tQ = T(function(){return unCStr(" out of range ");});var _tR = T(function(){return unCStr("}.index: Index ");});var _tS = T(function(){return unCStr("Ix{");});var _tT = [2,_gE,_U];var _tU = function(_tV,_tW,_tX,_tY,_tZ){var _u0 = T(function(){return A(_i6,[_tW,_hn,_tY]);});var _u1 = [2,_u0,_U];var _u2 = T(function(){return A(_i6,[_tV,_hn,_tX]);});var _u3 = [2,_u2,_u1];return A(_hM,[_u3,_tZ]);};var _u4 = function(_u5,_u6,_u7,_u8,_u9){var _ua = T(function(){var _ub = T(function(){var _uc = T(function(){var _ud = T(function(){var _ue = T(function(){return _tU(_u9,_u9,_u7,_u8,_tT);});var _uf = [2,_gF,_ue];return _15(_tQ,_uf);});var _ug = [2,_gE,_ud];return A(_i6,[_u9,_tP,_u6,_ug]);});var _uh = [2,_gF,_uc];return _15(_tR,_uh);});return _15(_u5,_ub);});var _ui = _15(_tS,_ua);var _uj = err(_ui);return _uj;};var _uk = function(_ul,_um,_un,_uo){var _up = E(_un);var _uq = _up[1];var _ur = _up[2];var _us = _u4(_ul,_um,_uq,_ur,_uo);return _us;};var _ut = function(_uu,_uv,_uw,_ux){return _uk(_ux,_uw,_uv,_uu);};var _uy = [1,1100];var _uz = [1,_rU,_uy];var _uA = T(function(){return unCStr("Int");});var _uB = function(_uC){var _uD = [1,_uC];return _ut(_o5,_uz,_uD,_uA);};var _uE = I(1);var _uF = function(_uG,_uH,_uI){while(1){var _uJ = _uH%2;if(_uJ){var _uK = E(_uH);if(_uK==1){var _uL = _9K(_uG,_uI);}else{var _uM = _9K(_uG,_uI);var _uN = _uK-1|0;var _uO = quot(_uN,2);var _uP = _9K(_uG,_uG);_uG=_uP;_uH=_uO;_uI=_uM;continue;var _uQ = die("Unreachable!");var _uL = _uQ;}var _uR = _uL;}else{var _uS = quot(_uH,2);var _uT = _9K(_uG,_uG);_uG=_uT;_uH=_uS;_uI=_uI;continue;var _uU = die("Unreachable!");var _uR = _uU;}return _uR;}};var _uV = function(_uW,_uX){while(1){var _uY = _uX%2;if(_uY){var _uZ = E(_uX);if(_uZ==1){var _v0 = E(_uW);}else{var _v1 = _uZ-1|0;var _v2 = quot(_v1,2);var _v3 = _9K(_uW,_uW);var _v4 = _uF(_v3,_v2,_uW);var _v0 = _v4;}var _v5 = _v0;}else{var _v6 = quot(_uX,2);var _v7 = _9K(_uW,_uW);_uW=_v7;_uX=_v6;continue;var _v8 = die("Unreachable!");var _v5 = _v8;}return _v5;}};var _v9 = T(function(){return unCStr("Negative exponent");});var _va = T(function(){return err(_v9);});var _vb = function(_vc){var _vd = newArr(1101,_tO,_vc);var _ve = _vd[1];var _vf = _vd[2];var _vg = function(_vh,_vi){while(1){var _vj = 0<=_vh;if(_vj){var _vk = _vh<=1100;if(_vk){var _vl = (function(_vh){return T(function(){var _vm = _vh<0;if(_vm){var _vn = E(_va);}else{var _vo = E(_vh);var _vn = _vo?_uV(_tM,_vo):E(_uE);}return _vn;})})(_vh);var _vp = (_vf[_vh]=_vl);var _vq = E(_vh);if(_vq==1100){var _vr = [0,0,_vf];var _vs = _vr[1];var _vt = _vr[2];var _vu = [1,E(_rU),E(_uy),1101,_vt];var _vv = [1,_vs,_vu];var _vw = _vv;}else{var _vx = _vq+1|0;_vh=_vx;_vi=_vp;continue;var _vy = die("Unreachable!");var _vw = _vy;}var _vz = _vw;}else{var _vz = _uB(_vh);}var _vA = _vz;}else{var _vA = _uB(_vh);}return _vA;}};var _vB = _vg(0,_ve);return _vB;};var _vC = function(_vD){var _vE = A(_vD,[realWorld]);var _vF = _vE[2];var _vG = E(_vF);return _vG;};var _vH = T(function(){return _vC(_vb);});var _vI = I(10);var _vJ = [1,324];var _vK = [1,_rU,_vJ];var _vL = function(_vM){var _vN = [1,_vM];return _ut(_o5,_vK,_vN,_uA);};var _vO = function(_vP){var _vQ = newArr(325,_tO,_vP);var _vR = _vQ[1];var _vS = _vQ[2];var _vT = function(_vU,_vV){while(1){var _vW = 0<=_vU;if(_vW){var _vX = _vU<=324;if(_vX){var _vY = (function(_vU){return T(function(){var _vZ = _vU<0;if(_vZ){var _w0 = E(_va);}else{var _w1 = E(_vU);var _w0 = _w1?_uV(_vI,_w1):E(_uE);}return _w0;})})(_vU);var _w2 = (_vS[_vU]=_vY);var _w3 = E(_vU);if(_w3==324){var _w4 = [0,0,_vS];var _w5 = _w4[1];var _w6 = _w4[2];var _w7 = [1,E(_rU),E(_vJ),325,_w6];var _w8 = [1,_w5,_w7];var _w9 = _w8;}else{var _wa = _w3+1|0;_vU=_wa;_vV=_w2;continue;var _wb = die("Unreachable!");var _w9 = _wb;}var _wc = _w9;}else{var _wc = _vL(_vU);}var _wd = _wc;}else{var _wd = _vL(_vU);}return _wd;}};var _we = _vT(0,_vR);return _we;};var _wf = T(function(){return _vC(_vO);});var _wg = function(_wh,_wi,_wj){var _wk = [1,_wi,_wj];return _ut(_o5,_wk,_wh,_uA);};var _wl = function(_wm,_wn,_wo){var _wp = [1,_wn,_wo];return _ut(_o5,_wp,_wm,_uA);};var _wq = function(_wr,_ws){var _wt = _71(_wr,_ws);return _wt[0]==2?true:false;};var _wu = function(_wv,_ww){var _wx = [1,_ww];var _wy = T(function(){var _wz = _wq(_wv,_vI);if(_wz){var _wA = _ww<=324;if(_wA){var _wB = E(_wf);var _wC = _wB[1];var _wD = _wB[2];var _wE = _wB[4];var _wF = E(_wC);var _wG = _wF[1];var _wH = E(_wD);var _wI = _wH[1];var _wJ = _wG<=_ww;if(_wJ){var _wK = _ww<=_wI;if(_wK){var _wL = _ww-_wG|0;var _wM = [0,_wE[_wL]];var _wN = _wM[1];var _wO = E(_wN);var _wP = _wO;}else{var _wP = _wg(_wx,_wF,_wH);}var _wQ = _wP;}else{var _wQ = _wg(_wx,_wF,_wH);}var _wR = _wQ;}else{var _wS = _ww<0;if(_wS){var _wT = E(_va);}else{var _wU = E(_ww);var _wT = _wU?_uV(_wv,_wU):E(_uE);}var _wR = _wT;}var _wV = _wR;}else{var _wW = _ww<0;if(_wW){var _wX = E(_va);}else{var _wY = E(_ww);var _wX = _wY?_uV(_wv,_wY):E(_uE);}var _wV = _wX;}return _wV;});var _wZ = _wq(_wv,_tM);if(_wZ){var _x0 = _ww>=0;if(_x0){var _x1 = _ww<=1100;if(_x1){var _x2 = E(_vH);var _x3 = _x2[1];var _x4 = _x2[2];var _x5 = _x2[4];var _x6 = E(_x3);var _x7 = _x6[1];var _x8 = E(_x4);var _x9 = _x8[1];var _xa = _x7<=_ww;if(_xa){var _xb = _ww<=_x9;if(_xb){var _xc = _ww-_x7|0;var _xd = [0,_x5[_xc]];var _xe = _xd[1];var _xf = E(_xe);var _xg = _xf;}else{var _xg = _wl(_wx,_x6,_x8);}var _xh = _xg;}else{var _xh = _wl(_wx,_x6,_x8);}var _xi = _xh;}else{var _xi = E(_wy);}var _xj = _xi;}else{var _xj = E(_wy);}var _xk = _xj;}else{var _xk = E(_wy);}return _xk;};var _xl = T(function(){return _wu(_tM,52);});var _xm = I(4);var _xn = [2,_rU,_U];var _xo = [1,E(0),E(_4H)];var _xp = [1,E(_xo)];var _xq = function(_xr){var _xs = decodeDouble(_xr);var _xt = _xs[1];var _xu = _xs[2];var _xv = _xs[3];var _xw = _xs[4];var _xx = T(function(){var _xy = _8j(_xv);var _xz = _8j(_xu);var _xA = _9K(_xz,_xp);var _xB = _6a(_xA,_xy);var _xC = _8o(_xt);var _xD = _9K(_xC,_xB);return _xD;});var _xE = [1,_xx,_xw];return _xE;};var _xF = function(_xG){var _xH = E(_xG);if(_xH[0]==1){var _xI = _xH[1];var _xJ = _xH[2];var _xK = _xF(_xJ);var _xL = (_xI&65535)>>>0;var _xM = _xL&4294967295;var _xN = _xM;var _xO = Math.pow(2,16);var _xP = _xI>>>16;var _xQ = _xP&4294967295;var _xR = _xQ;var _xS = _xR*_xO;var _xT = Math.pow(2,32);var _xU = _xK*_xT;var _xV = _xU+_xS;var _xW = _xV+_xN;var _xX = _xW;}else{var _xX = 0;}return _xX;};var _xY = function(_xZ){var _y0 = E(_xZ);switch(_y0[0]){case 1:var _y1 = _y0[1];var _y2 = _xF(_y1);break;case 2:var _y3 = _y0[1];var _y4 = _xF(_y3);var _y5 = -_y4;var _y2 = _y5;break;case 3:var _y2 = 0;break;}return _y2;};var _y6 = function(_y7,_y8){var _y9 = _71(_y7,_y8);return _y9[0]==3?false:true;};var _ya = function(_yb,_yc){var _yd = _bQ(_yb,_yc);var _ye = _yd[1];var _yf = E(_ye);return _yf;};var _yg = function(_yh,_yi){while(1){var _yj = E(_yh);if(_yj[0]==1){var _yk = E(_yi);}else{var _yl = _yj[1];var _ym = _yj[2];var _yn = [2,_yl,_yi];_yh=_ym;_yi=_yn;continue;var _yk = die("Unreachable!");}return _yk;}};var _yo = function(_yp){var _yq = _87(_yp);var _yr = [1,_yq];return _yr;};var _ys = I(0);var _yt = function(_yu,_yv){var _yw = _yv==0;if(_yw){var _yx = [1,_xn,_rU];}else{var _yy = T(function(){var _yz = _xq(_yv);var _yA = _yz[1];var _yB = _yz[2];var _yC = [1,_yB];var _yD = [1,_yA,_yC];return _yD;});var _yE = T(function(){var _yF = E(_yy);var _yG = _yF[2];var _yH = E(_yG);return _yH;});var _yI = T(function(){var _yJ = E(_yE);var _yK = _yJ[1];var _yL = (-1074)-_yK|0;var _yM = _yL>0;if(_yM){var _yN = _yK+_yL|0;var _yO = [1,_yN];var _yP = T(function(){var _yQ = _wu(_tM,_yL);var _yR = _wq(_yQ,_ys);if(_yR){var _yS = E(_tK);}else{var _yT = E(_yy);var _yU = _yT[1];var _yV = _ya(_yU,_yQ);var _yS = _yV;}return _yS;});var _yW = [1,_yP,_yO];var _yX = _yW;}else{var _yY = T(function(){var _yZ = E(_yy);var _z0 = _yZ[1];var _z1 = E(_z0);return _z1;});var _yX = [1,_yY,_yJ];}return _yX;});var _z2 = T(function(){var _z3 = E(_yI);var _z4 = _z3[2];var _z5 = E(_z4);return _z5;});var _z6 = T(function(){var _z7 = E(_yI);var _z8 = _z7[1];var _z9 = E(_z8);return _z9;});var _za = T(function(){var _zb = E(_z2);var _zc = _zb[1];var _zd = _zc>=0;if(_zd){var _ze = T(function(){return _wu(_tM,_zc);});var _zf = _wq(_z6,_xl);if(_zf){var _zg = T(function(){return _9K(_ze,_tM);});var _zh = T(function(){var _zi = _9K(_z6,_ze);var _zj = _9K(_zi,_tM);var _zk = _9K(_zj,_tM);return _zk;});var _zl = [1,_zh,_xm,_zg,_ze];}else{var _zm = T(function(){var _zn = _9K(_z6,_ze);var _zo = _9K(_zn,_tM);return _zo;});var _zl = [1,_zm,_tM,_ze,_ze];}var _zp = _zl;}else{var _zq = _zc>(-1074);if(_zq){var _zr = _wq(_z6,_xl);if(_zr){var _zs = T(function(){var _zt = -_zc;var _zu = _zt+1|0;var _zv = _wu(_tM,_zu);var _zw = _9K(_zv,_tM);return _zw;});var _zx = T(function(){var _zy = _9K(_z6,_tM);var _zz = _9K(_zy,_tM);return _zz;});var _zA = [1,_zx,_zs,_tM,_tL];}else{var _zB = T(function(){var _zC = -_zc;var _zD = _wu(_tM,_zC);var _zE = _9K(_zD,_tM);return _zE;});var _zF = T(function(){return _9K(_z6,_tM);});var _zA = [1,_zF,_zB,_tL,_tL];}var _zG = _zA;}else{var _zH = T(function(){var _zI = -_zc;var _zJ = _wu(_tM,_zI);var _zK = _9K(_zJ,_tM);return _zK;});var _zL = T(function(){return _9K(_z6,_tM);});var _zG = [1,_zL,_zH,_tL,_tL];}var _zp = _zG;}return _zp;});var _zM = T(function(){var _zN = E(_za);var _zO = _zN[2];var _zP = E(_zO);return _zP;});var _zQ = T(function(){var _zR = E(_za);var _zS = _zR[3];var _zT = E(_zS);return _zT;});var _zU = T(function(){var _zV = E(_za);var _zW = _zV[1];var _zX = E(_zW);return _zX;});var _zY = T(function(){var _zZ = T(function(){return _6a(_zU,_zQ);});var _A0 = function(_A1){while(1){var _A2 = _A1>=0;if(_A2){var _A3 = _wu(_yu,_A1);var _A4 = _9K(_A3,_zM);var _A5 = _y6(_zZ,_A4);if(_A5){var _A6 = E(_A1);}else{var _A7 = _A1+1|0;_A1=_A7;continue;var _A8 = die("Unreachable!");var _A6 = _A8;}var _A9 = _A6;}else{var _Aa = -_A1;var _Ab = _wu(_yu,_Aa);var _Ac = _9K(_Ab,_zZ);var _Ad = _y6(_Ac,_zM);if(_Ad){var _Ae = E(_A1);}else{var _Af = _A1+1|0;_A1=_Af;continue;var _Ag = die("Unreachable!");var _Ae = _Ag;}var _A9 = _Ae;}return _A9;}};var _Ah = _wq(_yu,_vI);if(_Ah){var _Ai = E(_yE);var _Aj = _Ai[1];var _Ak = 52+_Aj|0;var _Al = _Ak>=0;if(_Al){var _Am = imul(_Ak,8651)|0;var _An = quot(_Am,28738);var _Ao = _An+1|0;var _Ap = _A0(_Ao);var _Aq = [1,_Ap];var _Ar = _Aq;}else{var _As = imul(_Ak,8651)|0;var _At = quot(_As,28738);var _Au = _A0(_At);var _Av = [1,_Au];var _Ar = _Av;}var _Aw = _Ar;}else{var _Ax = _6a(_z6,_tL);var _Ay = _xY(_Ax);var _Az = E(_z2);var _AA = _Az[1];var _AB = _xY(_yu);var _AC = Math.log(_AB);var _AD = Math.log(_Ay);var _AE = Math.log(2);var _AF = _AA;var _AG = _AF*_AE;var _AH = _AD+_AG;var _AI = _AH/_AC;var _AJ = _AI;var _AK = _AJ;var _AL = _AK<_AI;if(_AL){var _AM = _AJ+1|0;var _AN = _A0(_AM);var _AO = [1,_AN];var _AP = _AO;}else{var _AQ = _A0(_AJ);var _AR = [1,_AQ];var _AP = _AR;}var _Aw = _AP;}return _Aw;});var _AS = T(function(){var _AT = E(_zY);var _AU = _AT[1];var _AV = function(_AW,_AX,_AY,_AZ,_B0){while(1){var _B1 = _wq(_AY,_ys);if(_B1){var _B2 = E(_tK);}else{var _B3 = _9K(_AX,_yu);var _B4 = _bQ(_B3,_AY);var _B5 = _B4[1];var _B6 = _B4[2];var _B7 = _9K(_B0,_yu);var _B8 = _9K(_AZ,_yu);var _B9 = _7o(_B6,_B7);if(_B9){var _Ba = _6a(_B6,_B8);var _Bb = _7k(_Ba,_AY);if(_Bb){var _Bc = _9K(_B6,_tM);var _Bd = _7o(_Bc,_AY);if(_Bd){var _Be = [2,_B5,_AW];}else{var _Bf = (function(_B5){return T(function(){return _6a(_B5,_tL);})})(_B5);var _Be = [2,_Bf,_AW];}var _Bg = _Be;}else{var _Bg = [2,_B5,_AW];}var _Bh = _Bg;}else{var _Bi = _6a(_B6,_B8);var _Bj = _7k(_Bi,_AY);if(_Bj){var _Bk = (function(_B5){return T(function(){return _6a(_B5,_tL);})})(_B5);var _Bl = [2,_Bk,_AW];}else{var _Bm = [2,_B5,_AW];_AW=_Bm;_AX=_B6;_AY=_AY;_AZ=_B8;_B0=_B7;continue;var _Bl = die("Unreachable!");}var _Bh = _Bl;}var _B2 = _Bh;}return _B2;}};var _Bn = _AU>=0;if(_Bn){var _Bo = E(_za);var _Bp = _Bo[4];var _Bq = _wu(_yu,_AU);var _Br = _9K(_zM,_Bq);var _Bs = _AV(_U,_zU,_Br,_zQ,_Bp);var _Bt = _yg(_Bs,_U);var _Bu = _pR(_yo,_Bt);var _Bv = _Bu;}else{var _Bw = E(_za);var _Bx = _Bw[4];var _By = -_AU;var _Bz = _wu(_yu,_By);var _BA = _9K(_Bx,_Bz);var _BB = _9K(_zQ,_Bz);var _BC = _9K(_zU,_Bz);var _BD = _AV(_U,_BC,_zM,_BB,_BA);var _BE = _yg(_BD,_U);var _BF = _pR(_yo,_BE);var _Bv = _BF;}return _Bv;});var _yx = [1,_AS,_zY];}return _yx;};var _BG = [1,'.'];var _BH = [1,'0'];var _BI = [2,_BH,_U];var _BJ = function(_BK,_BL){while(1){var _BM = E(_BK);if(_BM){var _BN = [2,_BH,_BL];var _BO = _BM-1|0;_BK=_BO;_BL=_BN;continue;var _BP = die("Unreachable!");var _BQ = _BP;}else{var _BR = _yg(_BL,_U);if(_BR[0]==1){var _BS = [2,_BG,_BI];var _BT = [2,_BH,_BS];}else{var _BU = [2,_BG,_BI];var _BT = _15(_BR,_BU);}var _BQ = _BT;}return _BQ;}};var _BV = function(_BW,_BX,_BY){while(1){var _BZ = E(_BW);if(_BZ){var _C0 = E(_BY);if(_C0[0]==1){var _C1 = [2,_BH,_BX];var _C2 = _BZ-1|0;var _C3 = _BJ(_C2,_C1);var _C4 = _C3;}else{var _C5 = _C0[1];var _C6 = _C0[2];var _C7 = [2,_C5,_BX];var _C8 = _BZ-1|0;_BW=_C8;_BX=_C7;_BY=_C6;continue;var _C9 = die("Unreachable!");var _C4 = _C9;}var _Ca = _C4;}else{var _Cb = _yg(_BX,_U);if(_Cb[0]==1){var _Cc = (function(_BY){return T(function(){var _Cd = E(_BY);return _Cd[0]==1?E(_BI):E(_Cd);})})(_BY);var _Ce = [2,_BG,_Cc];var _Cf = [2,_BH,_Ce];}else{var _Cg = (function(_BY){return T(function(){var _Ch = E(_BY);return _Ch[0]==1?E(_BI):E(_Ch);})})(_BY);var _Ci = [2,_BG,_Cg];var _Cf = _15(_Cb,_Ci);}var _Ca = _Cf;}return _Ca;}};var _Cj = function(_Ck,_Cl){var _Cm = _Ck>0;if(_Cm){var _Cn = _Cl<0;if(_Cn){var _Co = _Ck-1|0;var _Cp = quot(_Co,_Cl);var _Cq = _Cp-1|0;var _Cr = _Cq;}else{var _Cs = _Ck<0;if(_Cs){var _Ct = _Cl>0;if(_Ct){var _Cu = _Ck+1|0;var _Cv = quot(_Cu,_Cl);var _Cw = _Cv-1|0;var _Cx = _Cw;}else{var _Cx = quot(_Ck,_Cl);}var _Cy = _Cx;}else{var _Cy = quot(_Ck,_Cl);}var _Cr = _Cy;}var _Cz = _Cr;}else{var _CA = _Ck<0;if(_CA){var _CB = _Cl>0;if(_CB){var _CC = _Ck+1|0;var _CD = quot(_CC,_Cl);var _CE = _CD-1|0;var _CF = _CE;}else{var _CF = quot(_Ck,_Cl);}var _CG = _CF;}else{var _CG = quot(_Ck,_Cl);}var _Cz = _CG;}return _Cz;};var _CH = [1,1];var _CI = [2,_rU,_U];var _CJ = function(_CK){var _CL = _CK<=1;if(_CL){var _CM = E(_CI);}else{var _CN = T(function(){var _CO = _CK-1|0;var _CP = _CJ(_CO);return _CP;});var _CM = [2,_rU,_CN];}return _CM;};var _CQ = function(_CR,_CS,_CT){var _CU = T(function(){var _CV = E(_CR);var _CW = _CV[1];var _CX = _Cj(_CW,2);var _CY = [1,_CX];return _CY;});var _CZ = function(_D0,_D1){var _D2 = E(_D1);if(_D2[0]==1){var _D3 = T(function(){var _D4 = _D0<=0;return _D4?[1]:_CJ(_D0);});var _D5 = [1,_rU,_D3];}else{var _D6 = _D2[1];var _D7 = _D2[2];var _D8 = E(_D0);if(_D8){var _D9 = _D8-1|0;var _Da = _CZ(_D9,_D7);var _Db = _Da[1];var _Dc = _Da[2];var _Dd = E(_Db);var _De = _Dd[1];var _Df = E(_D6);var _Dg = _Df[1];var _Dh = E(_CR);var _Di = _Dh[1];var _Dj = _De+_Dg|0;var _Dk = _Dj==_Di;if(_Dk){var _Dl = [2,_rU,_Dc];var _Dm = [1,_CH,_Dl];}else{var _Dn = [1,_Dj];var _Do = [2,_Dn,_Dc];var _Dm = [1,_rU,_Do];}var _Dp = _Dm;}else{var _Dq = T(function(){var _Dr = E(_D6);var _Ds = _Dr[1];var _Dt = E(_CU);var _Du = _Dt[1];var _Dv = _Ds>=_Du;var _Dw = _Dv?E(_CH):E(_rU);return _Dw;});var _Dp = [1,_Dq,_U];}var _D5 = _Dp;}return _D5;};var _Dx = E(_CT);if(_Dx[0]==1){var _Dy = T(function(){var _Dz = E(_CS);var _DA = _Dz[1];var _DB = _DA<=0;var _DC = _DB?[1]:_CJ(_DA);return _DC;});var _DD = [1,_rU,_Dy];}else{var _DE = _Dx[1];var _DF = _Dx[2];var _DG = E(_CS);var _DH = _DG[1];var _DI = E(_DH);if(_DI){var _DJ = _DI-1|0;var _DK = _CZ(_DJ,_DF);var _DL = _DK[1];var _DM = _DK[2];var _DN = E(_DL);var _DO = _DN[1];var _DP = E(_DE);var _DQ = _DP[1];var _DR = E(_CR);var _DS = _DR[1];var _DT = _DO+_DQ|0;var _DU = _DT==_DS;if(_DU){var _DV = [2,_rU,_DM];var _DW = [2,_CH,_DV];var _DX = [1,_CH,_DW];}else{var _DY = [1,_DT];var _DZ = [2,_DY,_DM];var _DX = [1,_rU,_DZ];}var _E0 = _DX;}else{var _E1 = E(_DE);var _E2 = _E1[1];var _E3 = E(_CU);var _E4 = _E3[1];var _E5 = _E2>=_E4;if(_E5){var _E6 = [2,_CH,_U];var _E7 = [1,_CH,_E6];}else{var _E7 = [1,_rU,_U];}var _E0 = _E7;}var _DD = _E0;}return _DD;};var _E8 = [1,10];var _E9 = T(function(){return unCStr("e0");});var _Ea = function(_Eb,_Ec){var _Ed = E(_Eb);if(_Ed[0]==1){var _Ee = E(_E9);}else{var _Ef = _Ed[1];var _Eg = _Ed[2];var _Eh = _Ec<=1;if(_Eh){var _Ei = [2,_Ef,_E9];}else{var _Ej = T(function(){var _Ek = _Ec-1|0;var _El = _Ea(_Eg,_Ek);return _El;});var _Ei = [2,_Ef,_Ej];}var _Ee = _Ei;}return _Ee;};var _Em = T(function(){return unCStr("formatRealFloat/doFmt/FFExponent: []");});var _En = T(function(){return err(_Em);});var _Eo = T(function(){return unCStr("0.0e0");});var _Ep = T(function(){return _rP("GHC/Float.lhs:603:12-70|(d : ds')");});var _Eq = [1,'e'];var _Er = T(function(){return _8o(10);});var _Es = T(function(){return unCStr("Infinity");});var _Et = T(function(){return unCStr("-Infinity");});var _Eu = T(function(){return unCStr("NaN");});var _Ev = T(function(){return [2,_BH,_Ev];});var _Ew = function(_Ex,_Ey){var _Ez = E(_Ex);if(_Ez){var _EA = E(_Ey);if(_EA[0]==1){var _EB = [1,_U,_U];}else{var _EC = _EA[1];var _ED = _EA[2];var _EE = T(function(){var _EF = _Ez-1|0;var _EG = _Ew(_EF,_ED);var _EH = _EG[1];var _EI = _EG[2];var _EJ = [1,_EH,_EI];return _EJ;});var _EK = T(function(){var _EL = E(_EE);var _EM = _EL[2];var _EN = E(_EM);return _EN;});var _EO = T(function(){var _EP = E(_EE);var _EQ = _EP[1];var _ER = E(_EQ);return _ER;});var _ES = [2,_EC,_EO];var _EB = [1,_ES,_EK];}var _ET = _EB;}else{var _ET = [1,_U,_Ey];}return _ET;};var _EU = function(_EV,_EW){var _EX = E(_EW);if(_EX[0]==1){var _EY = [1];}else{var _EZ = _EX[1];var _F0 = _EX[2];var _F1 = T(function(){return _EU(_EZ,_F0);});var _EY = [2,_EV,_F1];}return _EY;};var _F2 = T(function(){return unCStr("init");});var _F3 = T(function(){return _hz(_F2);});var _F4 = function(_F5,_F6,_F7){var _F8 = isDoubleNaN(_F7,realWorld);var _F9 = _F8[2];var _Fa = E(_F9);if(_Fa){var _Fb = E(_Eu);}else{var _Fc = isDoubleInfinite(_F7,realWorld);var _Fd = _Fc[2];var _Fe = E(_Fd);if(_Fe){var _Ff = _F7<0;var _Fg = _Ff?E(_Et):E(_Es);}else{var _Fh = function(_Fi,_Fj){var _Fk = E(_F6);if(_Fk[0]==1){var _Fl = _pR(_sm,_Fi);if(_Fl[0]==1){var _Fm = E(_En);}else{var _Fn = _Fl[1];var _Fo = _Fl[2];var _Fp = E(_Fn);var _Fq = _Fp[1];var _Fr = T(function(){var _Fs = E(_Fo);if(_Fs[0]==1){var _Ft = T(function(){var _Fu = T(function(){var _Fv = _Fj-1|0;var _Fw = _nj(0,_Fv,_U);return _Fw;});return unAppCStr(".0e",_Fu);});var _Fx = [2,_Fp,_Ft];}else{var _Fy = T(function(){var _Fz = T(function(){var _FA = _Fj-1|0;var _FB = _kZ(_FA,_U);return _FB;});var _FC = [2,_Eq,_Fz];return _15(_Fs,_FC);});var _FD = [2,_BG,_Fy];var _Fx = [2,_Fp,_FD];}return _Fx;});var _FE = E(_Fq);if(_FE=='0'){var _FF = E(_Fo);var _FG = _FF[0]==1?E(_Eo):E(_Fr);}else{var _FG = E(_Fr);}var _Fm = _FG;}var _FH = _Fm;}else{var _FI = _Fk[1];var _FJ = T(function(){var _FK = E(_FI);var _FL = _FK[1];var _FM = _FL<=1;var _FN = _FM?E(_CH):E(_FK);return _FN;});var _FO = T(function(){var _FP = T(function(){var _FQ = E(_FJ);var _FR = _FQ[1];var _FS = _FR+1|0;var _FT = [1,_FS];return _FT;});var _FU = _CQ(_E8,_FP,_Fi);var _FV = _FU[1];var _FW = _FU[2];var _FX = [1,_FV,_FW];return _FX;});var _FY = T(function(){var _FZ = E(_FO);var _G0 = _FZ[1];var _G1 = E(_G0);return _G1;});var _G2 = T(function(){var _G3 = E(_FY);var _G4 = _G3[1];var _G5 = _G4>0;if(_G5){var _G6 = E(_FO);var _G7 = _G6[2];var _G8 = E(_G7);if(_G8[0]==1){var _G9 = E(_F3);}else{var _Ga = _G8[1];var _Gb = _G8[2];var _Gc = _EU(_Ga,_Gb);var _Gd = _pR(_sm,_Gc);if(_Gd[0]==1){var _Ge = E(_Ep);}else{var _Gf = _Gd[1];var _Gg = _Gd[2];var _Ge = [1,_Gf,_Gg];}var _G9 = _Ge;}var _Gh = _G9;}else{var _Gi = E(_FO);var _Gj = _Gi[2];var _Gk = _pR(_sm,_Gj);if(_Gk[0]==1){var _Gl = E(_Ep);}else{var _Gm = _Gk[1];var _Gn = _Gk[2];var _Gl = [1,_Gm,_Gn];}var _Gh = _Gl;}return _Gh;});var _Go = T(function(){var _Gp = E(_G2);var _Gq = _Gp[2];var _Gr = T(function(){var _Gs = E(_FY);var _Gt = _Gs[1];var _Gu = _Fj-1|0;var _Gv = _Gu+_Gt|0;var _Gw = _kZ(_Gv,_U);return _Gw;});var _Gx = [2,_Eq,_Gr];var _Gy = _15(_Gq,_Gx);return _Gy;});var _Gz = E(_Fi);if(_Gz[0]==1){var _GA = [2,_BG,_Go];var _GB = T(function(){var _GC = E(_G2);var _GD = _GC[1];var _GE = E(_GD);return _GE;});var _GF = [2,_GB,_GA];}else{var _GG = _Gz[1];var _GH = _Gz[2];var _GI = E(_GG);var _GJ = _GI[1];var _GK = E(_GJ);if(_GK){var _GL = [2,_BG,_Go];var _GM = T(function(){var _GN = E(_G2);var _GO = _GN[1];var _GP = E(_GO);return _GP;});var _GQ = [2,_GM,_GL];}else{var _GR = E(_GH);if(_GR[0]==1){var _GS = T(function(){var _GT = E(_FJ);var _GU = _GT[1];var _GV = _GU<=0;var _GW = _GV?E(_E9):_Ea(_Ev,_GU);return _GW;});var _GX = [2,_BG,_GS];var _GY = [2,_BH,_GX];}else{var _GZ = [2,_BG,_Go];var _H0 = T(function(){var _H1 = E(_G2);var _H2 = _H1[1];var _H3 = E(_H2);return _H3;});var _GY = [2,_H0,_GZ];}var _GQ = _GY;}var _GF = _GQ;}var _FH = _GF;}return _FH;};var _H4 = function(_H5,_H6){var _H7 = E(_F6);if(_H7[0]==1){var _H8 = _H6<=0;if(_H8){var _H9 = T(function(){var _Ha = -_H6;var _Hb = _Ha<=0;if(_Hb){var _Hc = _pR(_sm,_H5);}else{var _Hd = T(function(){return _pR(_sm,_H5);});var _He = [2,_BH,_Hd];var _Hf = function(_Hg){var _Hh = _Hg<=1;if(_Hh){var _Hi = E(_He);}else{var _Hj = T(function(){var _Hk = _Hg-1|0;var _Hl = _Hf(_Hk);return _Hl;});var _Hi = [2,_BH,_Hj];}return _Hi;};var _Hc = _Hf(_Ha);}return _Hc;});var _Hm = unAppCStr("0.",_H9);}else{var _Hn = T(function(){return _pR(_sm,_H5);});var _Hm = _BV(_H6,_U,_Hn);}var _Ho = _Hm;}else{var _Hp = _H7[1];var _Hq = _H6>=0;if(_Hq){var _Hr = T(function(){var _Hs = E(_Hp);var _Ht = _Hs[1];var _Hu = _Ht<=0;if(_Hu){var _Hv = [1,_H6];}else{var _Hw = _Ht+_H6|0;var _Hx = [1,_Hw];var _Hv = _Hx;}return _Hv;});var _Hy = _CQ(_E8,_Hr,_H5);var _Hz = _Hy[1];var _HA = _Hy[2];var _HB = E(_Hz);var _HC = _HB[1];var _HD = _H6+_HC|0;var _HE = _HD<0;if(_HE){var _HF = T(function(){var _HG = _pR(_sm,_HA);return _HG[0]==1?[1]:[2,_BG,_HG];});var _HH = [2,_BH,_HF];}else{var _HI = T(function(){return _pR(_sm,_HA);});var _HJ = _Ew(_HD,_HI);var _HK = _HJ[1];var _HL = _HJ[2];var _HM = E(_HK);if(_HM[0]==1){var _HN = T(function(){var _HO = E(_HL);return _HO[0]==1?[1]:[2,_BG,_HO];});var _HP = [2,_BH,_HN];}else{var _HQ = T(function(){var _HR = E(_HL);return _HR[0]==1?[1]:[2,_BG,_HR];});var _HP = _15(_HM,_HQ);}var _HH = _HP;}var _HS = _HH;}else{var _HT = T(function(){var _HU = -_H6;var _HV = _HU<=0;if(_HV){var _HW = T(function(){var _HX = E(_Hp);var _HY = _HX[1];var _HZ = _HY<=0;var _I0 = _HZ?E(_rU):E(_HX);return _I0;});var _I1 = _CQ(_E8,_HW,_H5);var _I2 = _I1[1];var _I3 = _I1[2];var _I4 = _ss(_I2,_I3);var _I5 = _I4;}else{var _I6 = [2,_rU,_H5];var _I7 = function(_I8){var _I9 = _I8<=1;if(_I9){var _Ia = E(_I6);}else{var _Ib = T(function(){var _Ic = _I8-1|0;var _Id = _I7(_Ic);return _Id;});var _Ia = [2,_rU,_Ib];}return _Ia;};var _Ie = _I7(_HU);var _If = T(function(){var _Ig = E(_Hp);var _Ih = _Ig[1];var _Ii = _Ih<=0;var _Ij = _Ii?E(_rU):E(_Ig);return _Ij;});var _Ik = _CQ(_E8,_If,_Ie);var _Il = _Ik[1];var _Im = _Ik[2];var _In = _ss(_Il,_Im);var _I5 = _In;}return _I5;});var _Io = T(function(){var _Ip = E(_HT);var _Iq = _Ip[2];var _Ir = E(_Iq);var _Is = _Ir[0]==1?[1]:[2,_BG,_Ir];return _Is;});var _It = T(function(){var _Iu = E(_HT);var _Iv = _Iu[1];var _Iw = E(_Iv);return _Iw;});var _HS = [2,_It,_Io];}var _Ho = _HS;}return _Ho;};var _Ix = function(_Iy,_Iz,_IA){var _IB = E(_Iy);switch(_IB[0]){case 1:var _IC = E(_F6);if(_IC[0]==1){var _ID = _pR(_sm,_Iz);if(_ID[0]==1){var _IE = E(_En);}else{var _IF = _ID[1];var _IG = _ID[2];var _IH = E(_IF);var _II = _IH[1];var _IJ = T(function(){var _IK = E(_IG);if(_IK[0]==1){var _IL = T(function(){var _IM = T(function(){var _IN = E(_IA);var _IO = _IN[1];var _IP = _IO-1|0;var _IQ = _nj(0,_IP,_U);return _IQ;});return unAppCStr(".0e",_IM);});var _IR = [2,_IH,_IL];}else{var _IS = T(function(){var _IT = T(function(){var _IU = E(_IA);var _IV = _IU[1];var _IW = _IV-1|0;var _IX = _kZ(_IW,_U);return _IX;});var _IY = [2,_Eq,_IT];return _15(_IK,_IY);});var _IZ = [2,_BG,_IS];var _IR = [2,_IH,_IZ];}return _IR;});var _J0 = E(_II);if(_J0=='0'){var _J1 = E(_IG);var _J2 = _J1[0]==1?E(_Eo):E(_IJ);}else{var _J2 = E(_IJ);}var _IE = _J2;}var _J3 = _IE;}else{var _J4 = _IC[1];var _J5 = T(function(){var _J6 = E(_J4);var _J7 = _J6[1];var _J8 = _J7<=1;var _J9 = _J8?E(_CH):E(_J6);return _J9;});var _Ja = T(function(){var _Jb = T(function(){var _Jc = E(_J5);var _Jd = _Jc[1];var _Je = _Jd+1|0;var _Jf = [1,_Je];return _Jf;});var _Jg = _CQ(_E8,_Jb,_Iz);var _Jh = _Jg[1];var _Ji = _Jg[2];var _Jj = [1,_Jh,_Ji];return _Jj;});var _Jk = T(function(){var _Jl = E(_Ja);var _Jm = _Jl[1];var _Jn = E(_Jm);return _Jn;});var _Jo = T(function(){var _Jp = E(_Jk);var _Jq = _Jp[1];var _Jr = _Jq>0;if(_Jr){var _Js = E(_Ja);var _Jt = _Js[2];var _Ju = E(_Jt);if(_Ju[0]==1){var _Jv = E(_F3);}else{var _Jw = _Ju[1];var _Jx = _Ju[2];var _Jy = _EU(_Jw,_Jx);var _Jz = _pR(_sm,_Jy);if(_Jz[0]==1){var _JA = E(_Ep);}else{var _JB = _Jz[1];var _JC = _Jz[2];var _JA = [1,_JB,_JC];}var _Jv = _JA;}var _JD = _Jv;}else{var _JE = E(_Ja);var _JF = _JE[2];var _JG = _pR(_sm,_JF);if(_JG[0]==1){var _JH = E(_Ep);}else{var _JI = _JG[1];var _JJ = _JG[2];var _JH = [1,_JI,_JJ];}var _JD = _JH;}return _JD;});var _JK = T(function(){var _JL = E(_Jo);var _JM = _JL[2];var _JN = T(function(){var _JO = E(_IA);var _JP = _JO[1];var _JQ = E(_Jk);var _JR = _JQ[1];var _JS = _JP-1|0;var _JT = _JS+_JR|0;var _JU = _kZ(_JT,_U);return _JU;});var _JV = [2,_Eq,_JN];var _JW = _15(_JM,_JV);return _JW;});var _JX = E(_Iz);if(_JX[0]==1){var _JY = [2,_BG,_JK];var _JZ = T(function(){var _K0 = E(_Jo);var _K1 = _K0[1];var _K2 = E(_K1);return _K2;});var _K3 = [2,_JZ,_JY];}else{var _K4 = _JX[1];var _K5 = _JX[2];var _K6 = E(_K4);var _K7 = _K6[1];var _K8 = E(_K7);if(_K8){var _K9 = [2,_BG,_JK];var _Ka = T(function(){var _Kb = E(_Jo);var _Kc = _Kb[1];var _Kd = E(_Kc);return _Kd;});var _Ke = [2,_Ka,_K9];}else{var _Kf = E(_K5);if(_Kf[0]==1){var _Kg = T(function(){var _Kh = E(_J5);var _Ki = _Kh[1];var _Kj = _Ki<=0;var _Kk = _Kj?E(_E9):_Ea(_Ev,_Ki);return _Kk;});var _Kl = [2,_BG,_Kg];var _Km = [2,_BH,_Kl];}else{var _Kn = [2,_BG,_JK];var _Ko = T(function(){var _Kp = E(_Jo);var _Kq = _Kp[1];var _Kr = E(_Kq);return _Kr;});var _Km = [2,_Ko,_Kn];}var _Ke = _Km;}var _K3 = _Ke;}var _J3 = _K3;}var _Ks = _J3;break;case 2:var _Kt = E(_F6);if(_Kt[0]==1){var _Ku = E(_IA);var _Kv = _Ku[1];var _Kw = _Kv<=0;if(_Kw){var _Kx = T(function(){var _Ky = -_Kv;var _Kz = _Ky<=0;if(_Kz){var _KA = _pR(_sm,_Iz);}else{var _KB = T(function(){return _pR(_sm,_Iz);});var _KC = [2,_BH,_KB];var _KD = function(_KE){var _KF = _KE<=1;if(_KF){var _KG = E(_KC);}else{var _KH = T(function(){var _KI = _KE-1|0;var _KJ = _KD(_KI);return _KJ;});var _KG = [2,_BH,_KH];}return _KG;};var _KA = _KD(_Ky);}return _KA;});var _KK = unAppCStr("0.",_Kx);}else{var _KL = T(function(){return _pR(_sm,_Iz);});var _KK = _BV(_Kv,_U,_KL);}var _KM = _KK;}else{var _KN = _Kt[1];var _KO = E(_IA);var _KP = _KO[1];var _KQ = _KP>=0;if(_KQ){var _KR = T(function(){var _KS = E(_KN);var _KT = _KS[1];var _KU = _KT<=0;if(_KU){var _KV = E(_KO);}else{var _KW = _KT+_KP|0;var _KX = [1,_KW];var _KV = _KX;}return _KV;});var _KY = _CQ(_E8,_KR,_Iz);var _KZ = _KY[1];var _L0 = _KY[2];var _L1 = E(_KZ);var _L2 = _L1[1];var _L3 = _KP+_L2|0;var _L4 = _L3<0;if(_L4){var _L5 = T(function(){var _L6 = _pR(_sm,_L0);return _L6[0]==1?[1]:[2,_BG,_L6];});var _L7 = [2,_BH,_L5];}else{var _L8 = T(function(){return _pR(_sm,_L0);});var _L9 = _Ew(_L3,_L8);var _La = _L9[1];var _Lb = _L9[2];var _Lc = E(_La);if(_Lc[0]==1){var _Ld = T(function(){var _Le = E(_Lb);return _Le[0]==1?[1]:[2,_BG,_Le];});var _Lf = [2,_BH,_Ld];}else{var _Lg = T(function(){var _Lh = E(_Lb);return _Lh[0]==1?[1]:[2,_BG,_Lh];});var _Lf = _15(_Lc,_Lg);}var _L7 = _Lf;}var _Li = _L7;}else{var _Lj = T(function(){var _Lk = -_KP;var _Ll = _Lk<=0;if(_Ll){var _Lm = T(function(){var _Ln = E(_KN);var _Lo = _Ln[1];var _Lp = _Lo<=0;var _Lq = _Lp?E(_rU):E(_Ln);return _Lq;});var _Lr = _CQ(_E8,_Lm,_Iz);var _Ls = _Lr[1];var _Lt = _Lr[2];var _Lu = _ss(_Ls,_Lt);var _Lv = _Lu;}else{var _Lw = [2,_rU,_Iz];var _Lx = function(_Ly){var _Lz = _Ly<=1;if(_Lz){var _LA = E(_Lw);}else{var _LB = T(function(){var _LC = _Ly-1|0;var _LD = _Lx(_LC);return _LD;});var _LA = [2,_rU,_LB];}return _LA;};var _LE = _Lx(_Lk);var _LF = T(function(){var _LG = E(_KN);var _LH = _LG[1];var _LI = _LH<=0;var _LJ = _LI?E(_rU):E(_LG);return _LJ;});var _LK = _CQ(_E8,_LF,_LE);var _LL = _LK[1];var _LM = _LK[2];var _LN = _ss(_LL,_LM);var _Lv = _LN;}return _Lv;});var _LO = T(function(){var _LP = E(_Lj);var _LQ = _LP[2];var _LR = E(_LQ);var _LS = _LR[0]==1?[1]:[2,_BG,_LR];return _LS;});var _LT = T(function(){var _LU = E(_Lj);var _LV = _LU[1];var _LW = E(_LV);return _LW;});var _Li = [2,_LT,_LO];}var _KM = _Li;}var _Ks = _KM;break;case 3:var _LX = E(_IA);var _LY = _LX[1];var _LZ = _LY<0;if(_LZ){var _M0 = _Fh(_Iz,_LY);}else{var _M1 = _LY>7;var _M0 = _M1?_Fh(_Iz,_LY):_H4(_Iz,_LY);}var _Ks = _M0;break;}return _Ks;};var _M2 = T(function(){var _M3 = -_F7;var _M4 = _yt(_Er,_M3);var _M5 = _M4[1];var _M6 = _M4[2];var _M7 = _Ix(_F5,_M5,_M6);return _M7;});var _M8 = _F7<0;if(_M8){var _M9 = [2,_pQ,_M2];}else{var _Ma = isDoubleNegativeZero(_F7,realWorld);var _Mb = _Ma[2];var _Mc = E(_Mb);if(_Mc){var _Md = [2,_pQ,_M2];}else{var _Me = _yt(_Er,_F7);var _Mf = _Me[1];var _Mg = _Me[2];var _Mh = _Ix(_F5,_Mf,_Mg);var _Md = _Mh;}var _M9 = _Md;}var _Fg = _M9;}var _Fb = _Fg;}return _Fb;};var _Mi = [3];var _Mj = function(_Mk){var _Ml = E(_Mk);var _Mm = _Ml[1];var _Mn = _Mm<0;if(_Mn){var _Mo = T(function(){var _Mp = -_Mm;var _Mq = _F4(_Mi,_3b,_Mp);var _Mr = _15(_Mq,_U);return _Mr;});var _Ms = [2,_pQ,_Mo];}else{var _Mt = isDoubleNegativeZero(_Mm,realWorld);var _Mu = _Mt[2];var _Mv = E(_Mu);if(_Mv){var _Mw = T(function(){var _Mx = -_Mm;var _My = _F4(_Mi,_3b,_Mx);var _Mz = _15(_My,_U);return _Mz;});var _MA = [2,_pQ,_Mw];}else{var _MB = _F4(_Mi,_3b,_Mm);var _MC = _15(_MB,_U);var _MA = _MC;}var _Ms = _MA;}return _Ms;};var _MD = function(_ME,_MF){var _MG = E(_ME);if(_MG[0]==1){var _MH = unAppCStr("[]",_MF);}else{var _MI = _MG[1];var _MJ = _MG[2];var _MK = T(function(){var _ML = E(_MI);var _MM = _ML[1];var _MN = T(function(){var _MO = [2,_2o,_MF];var _MP = function(_MQ){var _MR = E(_MQ);if(_MR[0]==1){var _MS = E(_MO);}else{var _MT = _MR[1];var _MU = _MR[2];var _MV = T(function(){var _MW = E(_MT);var _MX = _MW[1];var _MY = _MX<0;if(_MY){var _MZ = T(function(){var _N0 = T(function(){return _MP(_MU);});var _N1 = -_MX;var _N2 = _F4(_Mi,_3b,_N1);var _N3 = _15(_N2,_N0);return _N3;});var _N4 = [2,_pQ,_MZ];}else{var _N5 = isDoubleNegativeZero(_MX,realWorld);var _N6 = _N5[2];var _N7 = E(_N6);if(_N7){var _N8 = T(function(){var _N9 = T(function(){return _MP(_MU);});var _Na = -_MX;var _Nb = _F4(_Mi,_3b,_Na);var _Nc = _15(_Nb,_N9);return _Nc;});var _Nd = [2,_pQ,_N8];}else{var _Ne = T(function(){return _MP(_MU);});var _Nf = _F4(_Mi,_3b,_MX);var _Ng = _15(_Nf,_Ne);var _Nd = _Ng;}var _N4 = _Nd;}return _N4;});var _MS = [2,_2n,_MV];}return _MS;};return _MP(_MJ);});var _Nh = _MM<0;if(_Nh){var _Ni = T(function(){var _Nj = -_MM;var _Nk = _F4(_Mi,_3b,_Nj);var _Nl = _15(_Nk,_MN);return _Nl;});var _Nm = [2,_pQ,_Ni];}else{var _Nn = isDoubleNegativeZero(_MM,realWorld);var _No = _Nn[2];var _Np = E(_No);if(_Np){var _Nq = T(function(){var _Nr = -_MM;var _Ns = _F4(_Mi,_3b,_Nr);var _Nt = _15(_Ns,_MN);return _Nt;});var _Nu = [2,_pQ,_Nq];}else{var _Nv = _F4(_Mi,_3b,_MM);var _Nw = _15(_Nv,_MN);var _Nu = _Nw;}var _Nm = _Nu;}return _Nm;});var _MH = [2,_2p,_MK];}return _MH;};var _Nx = function(_Ny){var _Nz = T(function(){var _NA = E(_Ny);var _NB = _NA[1];var _NC = _F4(_Mi,_3b,_NB);return _NC;});return A(_15,[_Nz]);};var _ND = function(_NE,_NF,_NG){var _NH = T(function(){var _NI = -_NG;var _NJ = [1,_NI];var _NK = A(_NE,[_NJ]);return _NK;});var _NL = T(function(){var _NM = E(_NF);var _NN = _NM[1];var _NO = _NN>6;if(_NO){var _NP = function(_NQ){var _NR = T(function(){var _NS = [2,_gE,_NQ];return A(_NH,[_NS]);});var _NT = [2,_pQ,_NR];return [2,_gF,_NT];};var _NU = E(_NP);}else{var _NV = function(_NW){var _NX = T(function(){return A(_NH,[_NW]);});return [2,_pQ,_NX];};var _NU = E(_NV);}return _NU;});var _NY = _NG<0;if(_NY){var _NZ = E(_NL);}else{var _O0 = isDoubleNegativeZero(_NG,realWorld);var _O1 = _O0[2];var _O2 = E(_O1);if(_O2){var _O3 = E(_NL);}else{var _O4 = [1,_NG];var _O3 = A(_NE,[_O4]);}var _NZ = _O3;}return _NZ;};var _O5 = function(_O6,_O7){var _O8 = E(_O7);var _O9 = _O8[1];var _Oa = _ND(_Nx,_O6,_O9);return _Oa;};var _Ob = [1,_O5,_Mj,_MD];var _Oc = function(_Od){var _Oe = E(_Od);var _Of = _Oe[1];var _Og = E(_Of);return _Og;};var _Oh = function(_Oi){var _Oj = E(_Oi);var _Ok = _Oj[1];var _Ol = E(_Ok);return _Ol;};var _Om = function(_On){var _Oo = E(_On);var _Op = _Oo[3];var _Oq = E(_Op);return _Oq;};var _Or = function(_Os){var _Ot = E(_Os);var _Ou = _Ot[7];var _Ov = E(_Ou);return _Ov;};var _Ow = function(_Ox,_Oy,_Oz,_OA){var _OB = function(_OC){var _OD = T(function(){var _OE = I(0);return A(_Or,[_Oz,_OE]);});var _OF = A(_Oc,[_Ox,_OC,_OD]);if(_OF){var _OG = I(0);var _OH = A(_Or,[_Oy,_OG]);}else{var _OI = T(function(){var _OJ = I(1);return A(_Or,[_Oz,_OJ]);});var _OK = A(_Oc,[_Ox,_OC,_OI]);if(_OK){var _OL = I(1);var _OM = A(_Or,[_Oy,_OL]);}else{var _ON = T(function(){var _OO = T(function(){var _OP = T(function(){var _OQ = I(2);return A(_Or,[_Oz,_OQ]);});return A(_Om,[_Oz,_OC,_OP]);});return _OB(_OO);});var _OR = T(function(){var _OS = T(function(){var _OT = T(function(){var _OU = I(1);return A(_Or,[_Oz,_OU]);});return A(_Om,[_Oz,_OC,_OT]);});return _OB(_OS);});var _OM = A(_Oh,[_Oy,_OR,_ON]);}var _OH = _OM;}return _OH;};return _OB(_OA);};var _OV = function(_OW,_OX){var _OY = E(_OW);switch(_OY[0]){case 1:var _OZ = T(function(){var _P0 = T(function(){return _Ow(_ou,_ab,_pP,_OX);});var _P1 = T(function(){var _P2 = T(function(){return _hi(_hh);});return A(_o8,[_P3,_P2]);});return A(_3t,[_P1,_P0]);});var _P4 = T(function(){var _P5 = T(function(){var _P6 = T(function(){var _P7 = T(function(){return A(unCStr,[" in Haskell"]);});var _P8 = T(function(){return A(_hi,[_Ob,_OX]);});return A(_15,[_P8,_P7]);});var _P9 = T(function(){return A(unCStr,["Running fib "]);});return A(_15,[_P9,_P6]);});return A(_3t,[_P3,_P5]);});var _Pa = A(_3w,[_3n,_P4,_OZ]);break;case 2:var _Pb = T(function(){var _Pc = T(function(){var _Pd = E(_OX);var _Pe = _Pd[1];var _Pf = animator_fib2(_Pe,realWorld);var _Pg = _Pf[2];var _Ph = [1,_Pg];return _Ph;});var _Pi = T(function(){var _Pj = T(function(){return _hi(_Ob);});return A(_o8,[_P3,_Pj]);});return A(_3t,[_Pi,_Pc]);});var _Pk = T(function(){var _Pl = T(function(){var _Pm = T(function(){var _Pn = T(function(){return A(unCStr,[" in JavaScript"]);});var _Po = T(function(){return A(_hi,[_Ob,_OX]);});return A(_15,[_Po,_Pn]);});var _Pp = T(function(){return A(unCStr,["Running fib "]);});return A(_15,[_Pp,_Pm]);});return A(_3t,[_P3,_Pl]);});var _Pa = A(_3w,[_3n,_Pk,_Pb]);break;case 3:var _Pq = T(function(){var _Pr = T(function(){var _Ps = E(_OX);var _Pt = _Ps[1];var _Pu = animator_fib3(_Pt,realWorld);var _Pv = _Pu[2];var _Pw = [1,_Pv];return _Pw;});var _Px = T(function(){var _Py = T(function(){return _hi(_Ob);});return A(_o8,[_P3,_Py]);});return A(_3t,[_Px,_Pr]);});var _Pz = T(function(){var _PA = T(function(){var _PB = T(function(){var _PC = T(function(){return A(unCStr,[" in JavaScript"]);});var _PD = T(function(){return A(_hi,[_Ob,_OX]);});return A(_15,[_PD,_PC]);});var _PE = T(function(){return A(unCStr,["Running fib "]);});return A(_15,[_PE,_PB]);});return A(_3t,[_P3,_PA]);});var _Pa = A(_3w,[_3n,_Pz,_Pq]);break;}return _Pa;};var _PF = I(0);var _PG = function(_PH,_PI){var _PJ = function(_PK,_PL){while(1){var _PM = E(_PK);if(_PM[0]==1){var _PN = E(_PL);}else{var _PO = _PM[1];var _PP = _PM[2];var _PQ = (function(_PL,_PH,_PO){return T(function(){return A(_Oh,[_PH,_PL,_PO]);})})(_PL,_PH,_PO);_PK=_PP;_PL=_PQ;continue;var _PN = die("Unreachable!");}return _PN;}};var _PR = T(function(){return A(_Or,[_PH,_PF]);});return _PJ(_PI,_PR);};var _PS = function(_PT){var _PU = E(_PT);var _PV = _PU[7];var _PW = E(_PV);return _PW;};var _PX = function(_PY){var _PZ = E(_PY);var _Q0 = _PZ[2];var _Q1 = E(_Q0);return _Q1;};var _Q2 = function(_Q3,_Q4){var _Q5 = _bQ(_Q3,_Q4);var _Q6 = _Q5[1];var _Q7 = _Q5[2];var _Q8 = _8L(_Q4);var _Q9 = _6L(_Q8);var _Qa = _8L(_Q7);var _Qb = _wq(_Qa,_Q9);if(_Qb){var _Qc = T(function(){return _6a(_Q7,_Q4);});var _Qd = T(function(){return _6R(_Q6,_8K);});var _Qe = [1,_Qd,_Qc];}else{var _Qe = [1,_Q6,_Q7];}return _Qe;};var _Qf = function(_Qg,_Qh){var _Qi = _wq(_Qh,_ys);if(_Qi){var _Qj = E(_tK);}else{var _Qk = _Q2(_Qg,_Qh);var _Ql = _Qk[1];var _Qm = E(_Ql);var _Qj = _Qm;}return _Qj;};var _Qn = function(_Qo,_Qp){var _Qq = _wq(_Qp,_ys);if(_Qq){var _Qr = E(_tK);}else{var _Qs = _Q2(_Qo,_Qp);var _Qt = _Qs[1];var _Qu = _Qs[2];var _Qv = [1,_Qt,_Qu];var _Qr = _Qv;}return _Qr;};var _Qw = function(_Qx,_Qy){var _Qz = _wq(_Qy,_ys);if(_Qz){var _QA = E(_tK);}else{var _QB = _Q2(_Qx,_Qy);var _QC = _QB[2];var _QD = E(_QC);var _QA = _QD;}return _QA;};var _QE = function(_QF,_QG){var _QH = _wq(_QG,_ys);return _QH?E(_tK):_ya(_QF,_QG);};var _QI = function(_QJ,_QK){var _QL = _wq(_QK,_ys);if(_QL){var _QM = E(_tK);}else{var _QN = _bQ(_QJ,_QK);var _QO = _QN[1];var _QP = _QN[2];var _QQ = [1,_QO,_QP];var _QM = _QQ;}return _QM;};var _QR = function(_QS,_QT){var _QU = _bQ(_QS,_QT);var _QV = _QU[2];var _QW = E(_QV);return _QW;};var _QX = function(_QY,_QZ){var _R0 = _wq(_QZ,_ys);return _R0?E(_tK):_QR(_QY,_QZ);};var _R1 = function(_R2){return E(_R2);};var _R3 = function(_R4,_R5){var _R6 = _71(_R4,_R5);return _R6[0]==2?false:true;};var _R7 = [1,_wq,_R3];var _R8 = function(_R9,_Ra){var _Rb = _y6(_R9,_Ra);return _Rb?E(_Ra):E(_R9);};var _Rc = function(_Rd,_Re){var _Rf = _y6(_Rd,_Re);return _Rf?E(_Rd):E(_Re);};var _Rg = [1,_R7,_71,_7o,_7g,_7k,_y6,_R8,_Rc];var _Rh = function(_Ri){var _Rj = E(_Ri);var _Rk = [1,E(_Rj),E(_uE)];return _Rk;};var _Rl = [1,_ab,_Rg,_Rh];var _Rm = [1,_Rl,_8D,_QE,_QX,_Qf,_Qw,_QI,_Qn,_R1];var _Rn = function(_Ro){var _Rp = E(_Ro);var _Rq = _Rp[1];var _Rr = E(_Rq);return _Rr;};var _Rs = function(_Rt){var _Ru = E(_Rt);var _Rv = _Ru[1];var _Rw = E(_Rv);return _Rw;};var _Rx = I(2);var _Ry = function(_Rz,_RA,_RB){var _RC = E(_Rz);var _RD = _RC[1];var _RE = _RC[2];var _RF = T(function(){return A(_Or,[_RD,_ys]);});var _RG = T(function(){var _RH = T(function(){return A(_Or,[_RD,_Rx]);});return A(_RA,[_RB,_RH]);});var _RI = _Rs(_RE);var _RJ = A(_Oc,[_RI,_RG,_RF]);return _RJ;};var _RK = T(function(){return err(_v9);});var _RL = function(_RM,_RN,_RO,_RP){var _RQ = _Rn(_RN);var _RR = _RQ[1];var _RS = _RQ[2];var _RT = E(_RS);var _RU = _RT[1];var _RV = _RT[3];var _RW = T(function(){return A(_Or,[_RR,_ys]);});var _RX = A(_RV,[_RP,_RW]);if(_RX){var _RY = E(_RK);}else{var _RZ = T(function(){return A(_Or,[_RR,_ys]);});var _S0 = A(_Oc,[_RU,_RP,_RZ]);if(_S0){var _S1 = A(_Or,[_RM,_uE]);}else{var _S2 = E(_RU);var _S3 = _S2[1];var _S4 = T(function(){return A(_Or,[_RR,_Rx]);});var _S5 = T(function(){return A(_Or,[_RR,_uE]);});var _S6 = T(function(){return A(_Or,[_RR,_Rx]);});var _S7 = T(function(){return A(_Or,[_RR,_uE]);});var _S8 = function(_S9,_Sa,_Sb){var _Sc = E(_RN);var _Sd = _Sc[1];var _Se = _Sc[3];var _Sf = _Sc[4];var _Sg = _Ry(_Sd,_Sf,_Sa);if(_Sg){var _Sh = function(_Si,_Sj,_Sk){while(1){var _Sl = _Ry(_Sd,_Sf,_Sj);if(_Sl){var _Sm = (function(_S4,_Se,_Sj){return T(function(){return A(_Se,[_Sj,_S4]);})})(_S4,_Se,_Sj);var _Sn = (function(_RM,_Si){return T(function(){return A(_PX,[_RM,_Si,_Si]);})})(_RM,_Si);_Si=_Sn;_Sj=_Sm;_Sk=_Sk;continue;var _So = die("Unreachable!");}else{var _Sp = A(_S3,[_Sj,_S5]);if(_Sp){var _Sq = A(_PX,[_RM,_Si,_Sk]);}else{var _Sr = (function(_RM,_Si,_Sk){return T(function(){return A(_PX,[_RM,_Si,_Sk]);})})(_RM,_Si,_Sk);var _Ss = (function(_RR,_S6,_S7,_Se,_Sj){return T(function(){var _St = T(function(){return A(_Om,[_RR,_Sj,_S7]);});return A(_Se,[_St,_S6]);})})(_RR,_S6,_S7,_Se,_Sj);var _Su = (function(_RM,_Si){return T(function(){return A(_PX,[_RM,_Si,_Si]);})})(_RM,_Si);_Si=_Su;_Sj=_Ss;_Sk=_Sr;continue;var _Sq = die("Unreachable!");}var _So = _Sq;}return _So;}};var _Sv = T(function(){return A(_Se,[_Sa,_S4]);});var _Sw = T(function(){return A(_PX,[_RM,_S9,_S9]);});var _Sx = _Sh(_Sw,_Sv,_Sb);}else{var _Sy = A(_S3,[_Sa,_S5]);if(_Sy){var _Sz = A(_PX,[_RM,_S9,_Sb]);}else{var _SA = function(_SB,_SC,_SD){while(1){var _SE = _Ry(_Sd,_Sf,_SC);if(_SE){var _SF = (function(_S4,_SC,_Se){return T(function(){return A(_Se,[_SC,_S4]);})})(_S4,_SC,_Se);var _SG = (function(_RM,_SB){return T(function(){return A(_PX,[_RM,_SB,_SB]);})})(_RM,_SB);_SB=_SG;_SC=_SF;_SD=_SD;continue;var _SH = die("Unreachable!");}else{var _SI = A(_S3,[_SC,_S5]);if(_SI){var _SJ = A(_PX,[_RM,_SB,_SD]);}else{var _SK = (function(_RM,_SB,_SD){return T(function(){return A(_PX,[_RM,_SB,_SD]);})})(_RM,_SB,_SD);var _SL = (function(_RR,_S6,_S7,_SC,_Se){return T(function(){var _SM = T(function(){return A(_Om,[_RR,_SC,_S7]);});return A(_Se,[_SM,_S6]);})})(_RR,_S6,_S7,_SC,_Se);var _SN = (function(_RM,_SB){return T(function(){return A(_PX,[_RM,_SB,_SB]);})})(_RM,_SB);_SB=_SN;_SC=_SL;_SD=_SK;continue;var _SJ = die("Unreachable!");}var _SH = _SJ;}return _SH;}};var _SO = T(function(){return A(_PX,[_RM,_S9,_Sb]);});var _SP = T(function(){var _SQ = T(function(){return A(_Om,[_RR,_Sa,_S7]);});return A(_Se,[_SQ,_S6]);});var _SR = T(function(){return A(_PX,[_RM,_S9,_S9]);});var _Sz = _SA(_SR,_SP,_SO);}var _Sx = _Sz;}return _Sx;};var _SS = T(function(){return A(_Or,[_RR,_uE]);});var _ST = T(function(){return A(_Or,[_RR,_Rx]);});var _SU = T(function(){return A(_Or,[_RR,_uE]);});var _SV = T(function(){return A(_Or,[_RR,_Rx]);});var _SW = E(_RN);var _SX = _SW[1];var _SY = _SW[3];var _SZ = _SW[4];var _T0 = _Ry(_SX,_SZ,_RP);if(_T0){var _T1 = function(_T2,_T3){while(1){var _T4 = _Ry(_SX,_SZ,_T3);if(_T4){var _T5 = (function(_SV,_SY,_T3){return T(function(){return A(_SY,[_T3,_SV]);})})(_SV,_SY,_T3);var _T6 = (function(_RM,_T2){return T(function(){return A(_PX,[_RM,_T2,_T2]);})})(_RM,_T2);_T2=_T6;_T3=_T5;continue;var _T7 = die("Unreachable!");}else{var _T8 = A(_S3,[_T3,_SU]);if(_T8){var _T9 = E(_T2);}else{var _Ta = (function(_RR,_SS,_ST,_SY,_T3){return T(function(){var _Tb = T(function(){return A(_Om,[_RR,_T3,_SS]);});return A(_SY,[_Tb,_ST]);})})(_RR,_SS,_ST,_SY,_T3);var _Tc = (function(_RM,_T2){return T(function(){return A(_PX,[_RM,_T2,_T2]);})})(_RM,_T2);var _T9 = _S8(_Tc,_Ta,_T2);}var _T7 = _T9;}return _T7;}};var _Td = T(function(){return A(_SY,[_RP,_SV]);});var _Te = T(function(){return A(_PX,[_RM,_RO,_RO]);});var _Tf = _T1(_Te,_Td);}else{var _Tg = A(_S3,[_RP,_SU]);if(_Tg){var _Th = E(_RO);}else{var _Ti = T(function(){var _Tj = T(function(){return A(_Om,[_RR,_RP,_SS]);});return A(_SY,[_Tj,_ST]);});var _Tk = T(function(){return A(_PX,[_RM,_RO,_RO]);});var _Th = _S8(_Tk,_Ti,_RO);}var _Tf = _Th;}var _S1 = _Tf;}var _RY = _S1;}return _RY;};var _Tl = function(_Tm,_Tn,_To,_Tp){var _Tq = T(function(){var _Tr = T(function(){var _Ts = T(function(){return A(unCStr,[" in JavaScript"]);});var _Tt = T(function(){var _Tu = T(function(){var _Tv = T(function(){var _Tw = T(function(){var _Tx = I(1);return A(_Or,[_Tn,_Tx]);});return A(_PS,[_Tm,_Tw,_Tp]);});var _Ty = T(function(){var _Tz = T(function(){var _TA = T(function(){var _TB = I(2);var _TC = function(_TD){return A(_RL,[_Tn,_Rm,_TD,_TB]);};return A(_pR,[_TC]);});var _TE = T(function(){var _TF = T(function(){var _TG = I(3);return A(_Or,[_Tn,_TG]);});var _TH = function(_TI){return A(_PX,[_Tn,_TI,_TF]);};return A(_pR,[_TH]);});return A(_o8,[_TE,_TA]);});var _TJ = T(function(){return A(_PG,[_Tn]);});return A(_o8,[_TJ,_Tz]);});return A(_3t,[_Ty,_Tv]);});return A(_hi,[_To,_Tu]);});return A(_15,[_Tt,_Ts]);});var _TK = T(function(){return A(unCStr,["The sum is "]);});return A(_15,[_TK,_Tr]);});return A(_3t,[_P3,_Tq]);};var _TL = function(_TM){var _TN = T(function(){var _TO = T(function(){var _TP = T(function(){var _TQ = function(_TR){var _TS = function(_TT){var _TU = function(_TV){var _TW = T(function(){var _TX = function(_TY){var _TZ = T(function(){var _U0 = T(function(){var _U1 = T(function(){var _U2 = function(_U3){var _U4 = function(_U5){var _U6 = function(_U7){var _U8 = T(function(){var _U9 = T(function(){var _Ua = T(function(){var _Ub = T(function(){var _Uc = T(function(){var _Ud = T(function(){var _Ue = T(function(){return A(unCStr,["This goes in the doc"]);});return A(_Uf,[_Ue]);});var _Ug = T(function(){var _Uh = T(function(){return A(unCStr,["This goes in the log"]);});return A(_P3,[_Uh]);});return A(_3w,[_3n,_Ug,_Ud]);});var _Ui = T(function(){var _Uj = [1,22];return _OV(_o7,_Uj);});return A(_3w,[_3n,_Ui,_Uc]);});var _Uk = T(function(){var _Ul = [1,20];return _OV(_o7,_Ul);});return A(_3w,[_3n,_Uk,_Ub]);});var _Um = T(function(){var _Un = [1,4];return _OV(_o7,_Un);});return A(_3w,[_3n,_Um,_Ua]);});var _Uo = T(function(){var _Up = I(200);return _Tl(_8D,_ab,_hh,_Up);});return A(_3w,[_3n,_Uo,_U9]);});var _Uq = T(function(){var _Ur = T(function(){var _Us = [1,_U3,_U5,_U7];return A(_hi,[_ni,_Us]);});return A(_3t,[_Uf,_Ur]);});return A(_3w,[_3n,_Uq,_U8]);};var _Ut = T(function(){var _Uu = T(function(){return A(unCStr,["baz"]);});return A(_Uv,[_Uu,_TY]);});return A(_3o,[_3n,_Ut,_U6]);};var _Uw = T(function(){var _Ux = T(function(){return A(unCStr,["bar"]);});return A(_Uv,[_Ux,_TY]);});return A(_3o,[_3n,_Uw,_U4]);};var _Uy = T(function(){var _Uz = T(function(){return A(unCStr,["foo"]);});return A(_Uv,[_Uz,_TY]);});return A(_3o,[_3n,_Uy,_U2]);});var _UA = T(function(){var _UB = T(function(){return A(unCStr,["baz"]);});var _UC = T(function(){return A(unCStr,["baz"]);});return A(_UD,[_UC,_TY,_UB]);});return A(_3w,[_3n,_UA,_U1]);});var _UE = T(function(){var _UF = T(function(){return A(unCStr,["bar"]);});var _UG = T(function(){return A(unCStr,["bar"]);});return A(_UD,[_UG,_TY,_UF]);});return A(_3w,[_3n,_UE,_U0]);});var _UH = T(function(){var _UI = T(function(){return A(unCStr,["foo"]);});var _UJ = T(function(){return A(unCStr,["foo"]);});return A(_UD,[_UJ,_TY,_UI]);});return A(_3w,[_3n,_UH,_TZ]);};return A(_3o,[_3n,_UK,_TX]);});var _UL = T(function(){var _UM = T(function(){var _UN = [1,_TR,_TT,_TV];return A(_hi,[_o6,_UN]);});return A(_3t,[_Uf,_UM]);});return A(_3w,[_3n,_UL,_TW]);};var _UO = T(function(){var _UP = T(function(){return A(unCStr,["baz"]);});return A(_UQ,[_UR,_UP,_TM]);});return A(_3o,[_3n,_UO,_TU]);};var _US = T(function(){var _UT = T(function(){return A(unCStr,["bar"]);});return A(_UQ,[_UR,_UT,_TM]);});return A(_3o,[_3n,_US,_TS]);};var _UU = T(function(){var _UV = T(function(){return A(unCStr,["foo"]);});return A(_UQ,[_UR,_UV,_TM]);});return A(_3o,[_3n,_UU,_TQ]);});var _UW = T(function(){var _UX = [1,3];var _UY = T(function(){return A(unCStr,["baz"]);});return A(_UZ,[_UY,_TM,_UX]);});return A(_3w,[_3n,_UW,_TP]);});var _V0 = T(function(){var _V1 = [1,2];var _V2 = T(function(){return A(unCStr,["bar"]);});return A(_UZ,[_V2,_TM,_V1]);});return A(_3w,[_3n,_V0,_TO]);});var _V3 = T(function(){var _V4 = [1,1];var _V5 = T(function(){return A(unCStr,["foo"]);});return A(_UZ,[_V5,_TM,_V4]);});return A(_3w,[_3n,_V3,_TN]);};var _V6 = T(function(){return A(_3o,[_3n,_UK,_TL]);});
window.onload = function() {E(E(_V6)(0));};
