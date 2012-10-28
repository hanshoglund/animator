
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

function aPrimGlobal(_) {
    return [1, _, 
        window
    ];
}
function aPrimObj(_) {
    return [1, _, 
        {}
    ];
}
function aPrimNull(_) {
    return [1, _, 
        null
    ];
}
function aPrimGet(type, name, obj, _) {
    // aInternalCheck(type, obj[name], "Animator: Type error");
    return [1, _, 
        obj[name]
    ];
}
function aPrimSet(type, name, obj, value, _) {
    // aInternalCheck(type, value, "Animator: Type error");
    obj[name] = value;
    return [1, _];
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

function aPrimCall0(f, _) {
    return [1, _, 
        f()
    ];
}   
function aPrimCall1(f, x, _) {
    return [1, _, 
        f(x)
    ];
}   
function aPrimCall2(f, x, y, _) {
    return [1, _, 
        f(x, y)
    ];
}   
function aPrimWrap2(f, _) {
    return [1, _, 
        function (x, y) {
            var r = A(f, [[1,x], [1,y], 0]);
            return r[2];
        }
    ];
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

