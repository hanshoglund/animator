
/*jslint
    browser: true,
    sloppy: true,
    vars: true,
    debug: true,
    evil: true,
    nomen: true */
/*globals
    A, E */

// Keep in sync with prim module
var aPrimTypes = {
    b : 0,
    n : 1,
    s : 2,
    o : 3,
    f : 4,
    u : 5
};


#ifdef ENABLE_TYPE_CHECKS

/**
    Throw an error if `value` is not of type `type`. Returns `value` otherwise.
 */
var aPrimTypeCheck = function (type, value, error) {
    var typeOfValue = typeof value;

    var getName = function () {
        switch(type) {
        case aPrimTypes.b:
            return "boolean";
        case aPrimTypes.n:
            return "number";
        case aPrimTypes.s:
            return "string";
        case aPrimTypes.o:
            return "object";
        case aPrimTypes.f:
            return "function";
        case aPrimTypes.u:
            return "undefined";
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
    case aPrimTypes.b:
        if (typeOfValue !== "boolean") {
            throw getMessage();
        }
        break;

    case aPrimTypes.n:
        if (typeOfValue !== "number") {
            throw getMessage();
        }
        break;

    case aPrimTypes.s:
        if (typeOfValue !== "string") {
            throw getMessage();
        }
        break;

    case aPrimTypes.o:
        if (typeOfValue !== "object") {
            throw getMessage();
        }
        break;

    case aPrimTypes.f:
        if (typeOfValue !== "function") {
            throw getMessage();
        }
        break;

    case aPrimTypes.u:
        if (typeOfValue !== "undefined") {
            throw getMessage();
        }
        break;
    default:
        throw "Animator: Missing case";
    }
    return value;
};

#else // ENABLE_TYPE_CHECKS

function aPrimTypeCheck(type, value, error) {}

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


function aPrimHas(type, obj, name, _) {
    aPrimTypeCheck(type, obj[name]);
    return [1, _,
        (obj[name] !== undefined)
    ];
}
function aPrimGet(type, obj, name, _) {
    aPrimTypeCheck(type, obj[name]);
    return [1, _,
        obj[name]
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
    // aPrimTypeCheck(t,aPrimTypes.o);
    return [1, _,
        // f()
        f.call(t)
    ];
}
function aPrimCall1(f, t, a, _) {
    // aPrimTypeCheck(t,aPrimTypes.o);
    // aPrimTypeCheck(a,?);
    return [1, _,
        // f(a)
        f.call(t, a)
    ];
}
function aPrimCall2(f, t, a, b, _) {
    // aPrimTypeCheck(t,aPrimTypes.o);
    // aPrimTypeCheck(a,?);
    // aPrimTypeCheck(b,?);
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
    var ap = A;
    var ev = E;
    return [1, _,
        function () {
            var r = ap(f, [_]);
            return ev(r[2])[1];
        }
    ];
}
function aPrimLift1(f, _) {
    var ap = A;
    var ev = E;
    return [1, _,
        function (a) {
            var r = ap(f, [[1,a], _]);
            return ev(r[2])[1];
        }
    ];
}
function aPrimLift2(f, _) {
    var ap = A;
    var ev = E;
    return [1, _,
        function (a, b) {
            var r = ap(f, [[1,a], [1,b], _]);
            return ev(r[2])[1];
        }
    ];
}

function aPrimLiftPure0(f, _) {
    var ap = A;
    return [1, _,
        function () {
            var r = ap(f, [_]);
            return r[1];
        }
    ];
}
function aPrimLiftPure1(f, _) {
    var ap = A;
    return [1, _,
        function (a) {
            var r = ap(f, [[1,a], _]);
            return r[1];
        }
    ];
}
function aPrimLiftPure2(f, _) {
    var ap = A;
    return [1, _,
        function (a, b) {
            var r = ap(f, [[1,a], [1,b], _]);
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

