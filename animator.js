
/*
    Namespace issues
        The Haste compiler relies heavily on the global scope ...
            - All RTS functions are global
            - All generated functions (_N) are global
            - Primitive functions must be global
 */

// TODO hide
function aInternalCheck(type, value, error) {
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


function aPrimGet(type, obj, name, _) {
    // aInternalCheck(type, obj[name], "Animator: Type error");
    return [1, _,
        obj[name]
    ];
}
function aPrimSet(type, obj, name, value, _) {
    // aInternalCheck(type, value, "Animator: Type error");
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
function aPrimCall3(f, t, a, b, c, _) {
    return [1, _,
        // f(a, b, c)
        f.call(t, a, b, c)
    ];
}
function aPrimCall4(f, t, a, b, c, d, _) {
    return [1, _,
        // f(a, b, c, d)
        f.call(t, a, b, c, d)
    ];
}
function aPrimCall5(f, t, a, b, c, d, e, _) {
    return [1, _,
        // f(a, b, c, d, e)
        f.call(t, a, b, c, d, e)
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
function aPrimBind2(f, t, a, b, _) {
    return [1, _,
        f.bind(t, a, b)
    ];
}
function aPrimBind3(f, t, a, b, c, _) {
    return [1, _,
        f.bind(t, a, b, c)
    ];
}
function aPrimBind4(f, t, a, b, c, d, _) {
    return [1, _,
        f.bind(t, a, b, c, d)
    ];
}
function aPrimBind5(f, t, a, b, c, d, e, _) {
    return [1, _,
        f.bind(t, a, b, c, d, e)
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

function foo(x) {
    window.console.log("----------")
    aPrimLog(this);
    aPrimLog(x);
    window.console.log("----------")
    return x;
}


// TODO Must these be primitives (?)

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

