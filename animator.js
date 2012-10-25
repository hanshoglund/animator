
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

function aCheck(type, value, error) {
    switch(type)                                   
    {                                              
        case 0:                                    
            if (typeof value !== "number") throw error;    
            break;                                 
        case 1:                                    
            if (typeof value !== "string") throw error;    
            break;                                 
        case 2:                                    
            if (typeof type !== "object") throw error;    
            break;                                 
    }                                
}              

function aPrimGlobal(_) {
    return [1, _, window];
}
function aPrimObj(_) {
    return [1, _, {}];
}
function aPrimGet(type, name, obj, _) {
    aCheck(type, obj[name], "Animator: Type error");
    return [1, _, obj[name]];
}
function aPrimSet(type, name, obj, value, _) {
    aCheck(type, value, "Animator: Type error");
    obj[name] = value;
    return [1, _];
}
function aPrimAdd(a, b, _) {
    return [1, _, (a + b)]
}
function aPrimTypeOf(a, _) {
    return [1, _, (typeof a)]
}

aTestObj = {
    n : null,
    u : undefined,
    i : 1,
    f : 2.5,
    a : [1,2,3],
    o : { foo: 1, bar: 2 },
    s : "hello!",
    f : function () {}
};





// TODO remove these

function aPrimLog(text, _) {
    window.console.log(text);
    return [1, _];
}
function aPrimWrite(text, _) {
    document.write(text);
    return [1, _];
}
function aPrimAlert(text, _) {
    window.alert(text);
    return [1, _];
}




// -------------------- Generated code start --------------------
