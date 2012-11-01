
/*
    Namespace issues
        The Haste compiler relies heavily on the global scope ...
            - All RTS functions are global
            - All generated functions (_N) are global
            - Primitive functions must be global
 */

function aPrimTypeCheck(type, value, error) {

    // Keep in sync with prim module
    var booleanType   = 0;
    var numberType    = 1;
    var stringType    = 2;
    var objectType    = 3;
    var functionType  = 4;
    var undefinedType = 5;
    
    switch (type) {
        case booleanType:
            if (typeof type !== "boolean") throw error;
            break;
        case numberType:
            if (typeof value !== "number") throw error;
            break;
        case stringType:
            if (typeof value !== "string") throw error;
            break;
        case objectType:
            if (typeof type !== "object") throw error;
            break;
        case functionType:
            if (typeof type !== "function") throw error;
            break;
        case undefinedType:
            if (typeof type !== "undefined") throw error;
            break;
    }
}

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
    ]
}
function aPrimTypeOf(a, _) {
    return [1, _,
        (typeof a)
    ]
}
function aPrimInstanceOf(a, b, _) {
    return [1, _,
        (a instanceof b)
    ]
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
    // aPrimTypeCheck(type, obj[name], "Animator: Type error");
    return [1, _,
        obj[name]
    ];
}
function aPrimHas(type, obj, name, _) {
    // aPrimTypeCheck(type, obj[name], "Animator: Type error");
    return [1, _,
        (obj[name] !== undefined)
    ];
}
function aPrimSet(type, obj, name, value, _) {
    // aPrimTypeCheck(type, value, "Animator: Type error");
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


function aPrimLift0(f, _) {
    return [1, _,
        function () {
            var r = A(f, [_]);
            return E(r[2])[1];
        }
    ];
}
function aPrimLift1(f, _) {
    return [1, _,
        function (a) {
            var r = A(f, [[1,a], _]);
            return E(r[2])[1];
        }
    ];
}
function aPrimLift2(f, _) {
    return [1, _,
        function (a, b) {
            var r = A(f, [[1,a], [1,b], _]);
            return E(r[2])[1];
        }
    ];
}

function aPrimLiftPure0(f, _) {
    return [1, _,
        function () {
            var r = A(f, [_]);
            return r[1];
        }
    ];
}
function aPrimLiftPure1(f, _) {
    return [1, _,
        function (a) {
            var r = A(f, [[1,a], _]);
            return r[1];
        }
    ];
}
function aPrimLiftPure2(f, _) {
    return [1, _,
        function (a, b) {
            var r = A(f, [[1,a], [1,b], _]);
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

