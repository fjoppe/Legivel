module ErrorsAndWarnings

type MessageCode =
    |   Freeform    //  these are errors for which only a (freeform) string is given, ie not cancelable
    |   ErrPlainScalarRestrictedIndicator


//
//let messageStringForCode = function
//    |   ErrPlainScalarRestrictedIndicator  -> "Reserved indicators can't start a plain scalar."
//
    