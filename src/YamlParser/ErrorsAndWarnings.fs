module ErrorsAndWarnings

type MessageCode =
    |   Freeform    //  these are errors for which only a (freeform) string is given, ie not cancelable
    |   ErrPlainScalarRestrictedIndicator
    |   ErrTagKindMismatch


    