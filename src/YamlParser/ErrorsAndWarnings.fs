module ErrorsAndWarnings

type MessageCode =
    |   Freeform    //  these are errors for which only a (freeform) string is given, ie not cancelable
    //  the following errors are "cancelable", ie are returned as a potential error
    |   ErrPlainScalarRestrictedIndicator
    |   ErrPlainScalarMultiLine
    |   ErrFoldedChompIndicator
    |   ErrTagKindMismatch
    |   ErrVerbatimTag
    |   ErrVerbatimTagNoLocal
    |   ErrVerbatimTagIncorrectFormat
    |   ErrShorthandNamed
    |   ErrShorthandSecondary
    |   ErrAnchorSyntax
    |   ErrAnchorNotExists
    |   ErrMapDuplicateKey
    |   ErrTagSyntax
    |   ErrTagBadFormat
    |   ErrTagConstraint
    |   ErrTooManySpacesLiteral
    |   ErrTooLessIndentedLiteral
    |   ErrBadFormatLiteral
    |   ErrLengthExceeds1024
    |   ErrMissingDquote
    |   ErrDquoteIllegalChars
    |   ErrMissingSquote
    |   ErrMissingMappingSymbol


    