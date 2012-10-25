
/* 
  Global scope:
    * Unfortunately, the Haskell compiler currently places everything in the global scope
      * The RTS exports several functions
      * The genrated functions all start with an underscore, called things like ''
    * Processing exports the function 'Processing'
    * Animator exportrs the function 'Animator'
    
  FFI peculiarities:
    * No varargs
    * Each function takes a dummy argument after all other arguments
      * The value can be ignored, I call it  _world
    * Each function returns a boxed pair (list) where the first value is a dummy value
      * To return (), return [0, w] where w is any value
      * To return x, return [0, w, x] where w is any value
      * I return _world, but this is not required          
    * Acceptable FFI types:
      * Bool, Int, Word, Float, Double, Word32, JSString
      * Strings can be converted to String or JSON on Haskell side

 */
function aPrimGlobal(_) {
    return [1, _, window];
}           
function aPrimObj(_) {
    return [1, _, {}];
}          
function aPrimGet(name, obj, _) {
    return [1, _, obj[name]];
}          
function aPrimSet(name, obj, value, _) {
    obj[name] = value;
    return [1, _];
}          






// TODO remove these

function animator_log_prim(obj, _) {
    // debugger;
    document.write(obj);
    return [1, _];
}
function animator_log(text, _) {
    window.console.log(text);
    return [1, _];
}
function animator_write(text, _) {
    document.write(text);
    return [1, _];
}
function animator_alert(text, _) {
    window.alert(text);
    return [1, _];
}     
function animator_processing(_) {
  return [1, _, window.Processing];
}

function fib2(n) {
  if (n < 2) return n;
  return fib2(n - 1) + fib2(n - 2);
}        

function fib3(n){
  var i;
  var fibs = [0, 1];
  for (i = 2; i <= n; ++i) { 
    fibs[i] = fibs[i - 1] + fibs[i - 2];
  }
  return fibs[n];
}

function animator_fib2(n, _) {
  return [1, _, fib2(n)];
}
function animator_fib3(n, _) {
  return [1, _, fib3(n)];
}



// -------------------- Generated code start --------------------
