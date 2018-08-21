module ErrorsAndWarnings

type MessageCode =
    //  these are errors for which only a (freeform) string is given, ie not cancelable
    |   Freeform                           = 0
    //  the following errors are "cancelable", ie are returned as a potential error
    |   ErrPlainScalarRestrictedIndicator   = 1
    |   ErrPlainScalarMultiLine             = 2
    |   ErrFoldedChompIndicator             = 3
    |   ErrTagKindMismatch                  = 4
    |   ErrVerbatimTag                      = 5
    |   ErrVerbatimTagNoLocal               = 6
    |   ErrVerbatimTagIncorrectFormat       = 7
    |   ErrShorthandNamed                   = 8
    |   ErrShorthandSecondary               = 9
    |   ErrAnchorSyntax                     = 10
    |   ErrAnchorNotExists                  = 11
    |   ErrMapDuplicateKey                  = 12
    |   ErrTagSyntax                        = 13
    |   ErrTagBadFormat                     = 14
    |   ErrTagConstraint                    = 15
    |   ErrTooManySpacesLiteral             = 16
    |   ErrTooLessIndentedLiteral           = 17
    |   ErrBadFormatLiteral                 = 18
    |   ErrLengthExceeds1024                = 19
    |   ErrMissingDquote                    = 20
    |   ErrDquoteIllegalChars               = 21
    |   ErrMissingSquote                    = 22
    |   ErrMissingMappingSymbol             = 23
    |   ErrTabCannotIndent                  = 24
    |   ErrIndentationError                 = 25


    