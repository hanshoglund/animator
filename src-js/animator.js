
/*jslint 
    browser: true, 
    debug: true, 
    evil: true, 
    vars: true, 
    nomen: true */
/*globals 
    A, E */

/*
    Namespace issues

        The Haste compiler relies heavily on the global scope ...
            - All RTS functions are global
            - All generated functions (_N) are global
            - Primitive functions must be global
 */


#ifdef ENABLE_TYPE_CHECKS

/** 
    Throw an error if `value` is not of type `type`.
 */
var aPrimTypeCheck = (function () {

    // Keep in sync with prim module
    var booleanType   = 0;
    var numberType    = 1;
    var stringType    = 2;
    var objectType    = 3;
    var functionType  = 4;
    var undefinedType = 5;

    return function (type, value, error) {
        var typeOfValue = typeof value;

        var getName = function () {
            switch(type) {
            case booleanType:   return "boolean";
            case numberType:    return "number";
            case stringType:    return "string";
            case objectType:    return "object";
            case functionType:  return "function";
            case undefinedType: return "undefined";
            default:
                throw "Animator: Missing case";
            }
        };

        var getMessage = function() {
            var base     = error || "Animator: Type error: ";
            var expected = getName();
            var actual   = typeOfValue;
            return String() + base + "Expected '" + expected + "' not '" + actual + "'";
        };

        switch (type) {
        case booleanType:
            if (typeOfValue !== "boolean") {
                throw getMessage();                
            }
            break;

        case numberType:
            if (typeOfValue !== "number") {                
                throw getMessage();
            }
            break;

        case stringType:
            if (typeOfValue !== "string") {
                throw getMessage();
            }
            break;

        case objectType:
            if (typeOfValue !== "object") {
                throw getMessage();
            }
            break;

        case functionType:
            if (typeOfValue !== "function") {
                throw getMessage();
            }
            break;

        case undefinedType:
            if (typeOfValue !== "undefined") {
                throw getMessage();                
            }
            break;
        default:
            throw "Animator: Missing case";
        }
    };
}()); 

#else // ENABLE_TYPE_CHECKS

function aPrimTypeCheck() {}

#endif // ENABLE_TYPE_CHECKS

function aPrimObj(_) {
    return [1, _,
        {}
    ];
}
function aPrimArr(_) {
    return [1, _,
        []
    ];
}
function aPrimNull(_) {
    return [1, _,
        null
    ];
}
function aPrimGlobal(_) {
    return [1, _,
        window
    ];
}
function aPrimAdd(a, b, _) {
    return [1, _,
        (a + b)
    ];
}
function aPrimTypeOf(a, _) {
    return [1, _,
        (typeof a)
    ];
}
function aPrimInstanceOf(a, b, _) {
    return [1, _,
        (a instanceof b)
    ];
}
function aPrimEval(s, _) {
    return [1, _,
        eval(s)
    ];
}
function aPrimDebug(s, _) {
    debugger;
    return [1, _];
}


function aPrimGet(type, obj, name, _) {
    aPrimTypeCheck(type, obj[name]);
    return [1, _,
        obj[name]
    ];
}
function aPrimHas(type, obj, name, _) {
    aPrimTypeCheck(type, obj[name]);
    return [1, _,
        (obj[name] !== undefined)
    ];
}
function aPrimSet(type, obj, name, value, _) {
    aPrimTypeCheck(type, obj[name]);
    obj[name] = value;
    return [1, _];
}
function aPrimDelete(type, obj, name, _) {
    delete obj[name];
    return [1, _];
}

function aPrimCall0(f, t, _) {
    return [1, _,
        // f()
        f.call(t)
    ];
}
function aPrimCall1(f, t, a, _) {
    return [1, _,
        // f(a)
        f.call(t, a)
    ];
}
function aPrimCall2(f, t, a, b, _) {
    return [1, _,
        // f(a, y)
        f.call(t, a, b)
    ];
}

function aPrimBind0(f, t, _) {
    return [1, _,
        f.bind(t)
    ];
}
function aPrimBind1(f, t, a, _) {
    return [1, _,
        f.bind(t, a)
    ];
}

var aPrimApp  = A;
var aPrimEval = E;

function aPrimLift0(f, _) {
    return [1, _,
        function () {
            var r = aPrimApp(f, [_]);
            return aPrimEval(r[2])[1];
        }
    ];
}
function aPrimLift1(f, _) {
    return [1, _,
        function (a) {
            var r = aPrimApp(f, [[1,a], _]);
            return aPrimEval(r[2])[1];
        }
    ];
}
function aPrimLift2(f, _) {
    return [1, _,
        function (a, b) {
            var r = aPrimApp(f, [[1,a], [1,b], _]);
            return aPrimEval(r[2])[1];
        }
    ];
}

function aPrimLiftPure0(f, _) {
    return [1, _,
        function () {
            var r = aPrimApp(f, [_]);
            return r[1];
        }
    ];
}
function aPrimLiftPure1(f, _) {
    return [1, _,
        function (a) {
            var r = aPrimApp(f, [[1,a], _]);
            return r[1];
        }
    ];
}
function aPrimLiftPure2(f, _) {
    return [1, _,
        function (a, b) {
            var r = aPrimApp(f, [[1,a], [1,b], _]);
            return r[1];
        }
    ];
}


function aPrimLog(text, _) {
    window.console.log(typeof text);
    window.console.log(text);
    window.console.log(" ");
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

