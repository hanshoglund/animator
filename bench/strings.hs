
bench = do
    let i = 4000

    -- windowConsoleLog "Concat Strings"
    -- let as = replicate i ("foo"::String)
    -- let a = fold as
    -- windowConsoleLog $ show a

    windowConsoleLog "Concat JsStrings"
    let bs = replicate i ("foo"::JsString)
    let b = fold bs
    logAny# $ unsafeCoerce b

    return ()
