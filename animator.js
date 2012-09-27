
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

function animator_object_create(_world) {
    return [1, _world, {}];
}          
function animator_object_get(name, obj, _world) {
    return [1, _world, obj[name]];
}          
function animator_object_set(name, obj, value, _world) {
    obj[name] = value;
    return [1, _world];
}          



function animator_log_prim(obj, _world) {
    // debugger;
    document.write(obj);
    return [1, _world];
}
function animator_log(text, _world) {
    window.console.log(text);
    return [1, _world];
}
function animator_write(text, _world) {
    document.write(text);
    return [1, _world];
}
function animator_alert(text, _world) {
    window.alert(text);
    return [1, _world];
}     
function animator_processing(_world) {
  return [1, _world, window.Processing];
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

function animator_fib2(n, _world) {
  return [1, _world, fib2(n)];
}
function animator_fib3(n, _world) {
  return [1, _world, fib3(n)];
}



// -------------------- Generated code start --------------------
