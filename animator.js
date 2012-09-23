
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
    * The Haskell side have to distinguish pure and impure functions
      * Pure functions have the type (a1 -> ... aN -> r)
      * Impure functions have the type (a1 -> ... aN -> IO r)
    * Acceptable FFI types:
      * Bool, Float, Double, Int, Word, Word32, JSString
      * Strings can be converted to String or JSON on Haskell side

 */           
// function animator_ready(f) {                
//   // document.addEventListener('DOMContentLoaded', f, true, true);
//   animator_ready.func = f;
// }
// function animator_init()
// {
//   animator_ready.func();
// }

function animator_log_prim(obj, _world) {
    // debugger;
    document.write(obj);
    // window.console.log(obj);
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

// function animator_test_processing(_world) {
//   function sketchProc(processing) {
//     // Override draw function, by default it will be called 60 times per second
//     processing.draw = function() {
//       // determine center and max clock arm length
//       var centerX = processing.width / 2, centerY = processing.height / 2;
//       var maxArmLength = Math.min(centerX, centerY);
// 
//       function drawArm(position, lengthScale, weight) {
//         processing.strokeWeight(weight);
//         processing.line(centerX, centerY,
//           centerX + Math.sin(position * 2 * Math.PI) * lengthScale * maxArmLength,
//           centerY - Math.cos(position * 2 * Math.PI) * lengthScale * maxArmLength);
//       }
// 
//       // erase background
//       processing.background(200);
// 
//       var now = new Date();
// 
//       // Moving hours arm by small increments
//       var hoursPosition = (now.getHours() % 12 + now.getMinutes() / 60) / 12;
//       drawArm(hoursPosition, 0.5, 5);
// 
//       // Moving minutes arm by small increments
//       var minutesPosition = (now.getMinutes() + now.getSeconds() / 60) / 60;
//       drawArm(minutesPosition, 0.80, 3);
// 
//       // Moving hour arm by second increments
//       var secondsPosition = now.getSeconds() / 60;
//       drawArm(secondsPosition, 0.90, 1);
//     };
//     processing.size(500, 500);
//   }
//   var canvas = window.document.getElementById("animator-canvas");
//   var processingInstance = new Processing(canvas, sketchProc);  
// 
//   return [1,0]; // Return ()
// }                        

// function Animator() {
//   
//   Animator.functions = {
//     log : function (text, _world) {
//         window.console.log(text);
//         return [1,0]; // Return ()
//     }
//     write : function (text, _world) {
//         document.write(text);
//         return [1,0]; // Return ()
//     }
//     alert : function (text, _world) {
//         window.alert(text);
//         return [1,0]; // Return ()
//     }     
//   };
//   return [1, Animator.functions];
// }

