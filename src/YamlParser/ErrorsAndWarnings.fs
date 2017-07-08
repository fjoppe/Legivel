module ErrorsAndWarnings

type MessageCode =
    |   Freeform    //  these are errors for which only a (freeform) string is given, ie not cancelable
    //  the following errors are "cancelable", ie are returned as a potential error
    |   ErrPlainScalarRestrictedIndicator
    |   ErrTagKindMismatch
    |   ErrVerbatimTag
    |   ErrShorthandNamed
    |   ErrShorthandSecondary
    |   ErrAnchorSyntax
    |   ErrMapDuplicateKey


    