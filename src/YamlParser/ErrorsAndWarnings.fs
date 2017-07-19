module ErrorsAndWarnings

type MessageCode =
    |   Freeform    //  these are errors for which only a (freeform) string is given, ie not cancelable
    //  the following errors are "cancelable", ie are returned as a potential error
    |   ErrPlainScalarRestrictedIndicator
    |   ErrPlainScalarMultiLine
    |   ErrTagKindMismatch
    |   ErrVerbatimTag
    |   ErrVerbatimTagNoLocal
    |   ErrVerbatimTagIncorrectFormat
    |   ErrShorthandNamed
    |   ErrShorthandSecondary
    |   ErrAnchorSyntax
    |   ErrMapDuplicateKey
    |   ErrTagSyntax
    |   ErrTooManySpacesLiteral
    |   ErrTooLessIndentedLiteral
    |   ErrBadFormatLiteral
    |   ErrLengthExceeds1024
    |   ErrMissingDquote
    |   ErrDquoteIllegalChars
    |   ErrMissingSquote


    