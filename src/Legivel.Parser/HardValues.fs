﻿module internal rec HardValues

#nowarn "40"

open System
open System.Text.RegularExpressions
open Legivel.Common
open Legivel.Internals.ParserMonads
open Legivel.TagResolution
open Legivel.Utilities.RegexDSL
open Legivel.RepresentationGraph
open Legivel.Internals
open ErrorsAndWarnings
open System.Diagnostics
open System.IO
open System.Collections.Generic

let ``start-of-line`` = RGP "^"
let ``end-of-file`` = RGP "\\z"


//  [1] http://www.yaml.org/spec/1.2/spec.html#c-printable
let ``c-printable`` = 
        RGO (
            "\u0009\u000a\u000d\u0020-\u007e" +   // 8 - bit, #x9 | #xA | #xD | [#x20-#x7E]
            "\u0085\u00a0-\ud7ff\ue000-\ufffd"    // 16- bit, #x85 | [#xA0-#xD7FF] | [#xE000-#xFFFD]
            )                                     //  32-bit -> currently not supported because .Net does not encode naturally. Yaml: [#x10000-#x10FFFF]


//  [2] http://www.yaml.org/spec/1.2/spec.html#nb-json
let ``nb-json`` =  RGO "\u0009\u0020-\uffff"

//  [3] http://www.yaml.org/spec/1.2/spec.html#c-byte-order-mark
let ``c-byte-order-mark`` = RGP "\ufeff"

//  [4] http://www.yaml.org/spec/1.2/spec.html#c-sequence-entry
let ``c-sequence-entry`` = RGP "-"

//  [5] http://www.yaml.org/spec/1.2/spec.html#c-mapping-key
let ``c-mapping-key`` = RGP "\\?"

//  [6] http://www.yaml.org/spec/1.2/spec.html#c-mapping-value
let ``c-mapping-value`` = RGP ":"

//  [7] http://www.yaml.org/spec/1.2/spec.html#c-collect-entry
let ``c-collect-entry`` = RGP ","

//  [8] http://www.yaml.org/spec/1.2/spec.html#c-sequence-start
let ``c-sequence-start`` = RGP "\["

//  [9] http://www.yaml.org/spec/1.2/spec.html#c-sequence-end
let ``c-sequence-end`` = RGP "\]"

//  [10]    http://www.yaml.org/spec/1.2/spec.html#c-mapping-start
let ``c-mapping-start`` = RGP "\{"

//  [11]    http://www.yaml.org/spec/1.2/spec.html#c-mapping-end
let ``c-mapping-end`` = RGP "\}"

//  [12]    http://www.yaml.org/spec/1.2/spec.html#c-comment
let ``c-comment`` = RGP "#"

//  [13]    http://www.yaml.org/spec/1.2/spec.html#c-anchor
let ``c-anchor`` = "&"

//  [14]    http://www.yaml.org/spec/1.2/spec.html#c-alias
let ``c-alias`` = "*"

//  [15]    http://www.yaml.org/spec/1.2/spec.html#c-tag
let ``c-tag`` = "!"

//  [16]    http://www.yaml.org/spec/1.2/spec.html#c-literal
let ``c-literal`` = "|"

//  [17]    http://www.yaml.org/spec/1.2/spec.html#c-folded
let ``c-folded`` = RGP ">"

//  [18]    http://www.yaml.org/spec/1.2/spec.html#c-single-quote
let ``c-single-quote`` = RGP "\'"

//  [19]    http://www.yaml.org/spec/1.2/spec.html#c-double-quote
let ``c-double-quote`` = RGP "\""

//  [20]    http://www.yaml.org/spec/1.2/spec.html#c-directive
let ``c-directive`` = "%"

//  [21]    http://www.yaml.org/spec/1.2/spec.html#c-reserved
let ``c-reserved`` = RGO "\u0040\u0060"

//  [22]    http://www.yaml.org/spec/1.2/spec.html#c-indicator
let ``c-indicator`` = RGO  "\-\?:,\[\]\{\}#&\*!\|>\'\"%@`" 

//  [23]    http://www.yaml.org/spec/1.2/spec.html#c-flow-indicator
let ``c-flow-indicator`` = RGO  @",\[\]\{\}" 

//  [24]    http://www.yaml.org/spec/1.2/spec.html#b-line-feed
let ``b-line-feed`` = RGP "\u000a"

//  [25]    http://www.yaml.org/spec/1.2/spec.html#b-carriage-return
let ``b-carriage-return`` = RGP "\u000d"

//  [i26]   http://www.yaml.org/spec/1.2/spec.html#b-char
let ``b-char`` = ``b-line-feed`` ||| ``b-carriage-return``

//  [27]    http://www.yaml.org/spec/1.2/spec.html#nb-char
let ``nb-char``  = ``c-printable`` - RGO "\u000a\u000d"


//  [28]    http://www.yaml.org/spec/1.2/spec.html#b-break
let ``b-break`` = 
        (``b-carriage-return`` + ``b-line-feed``) |||  //  DOS, Windows
        ``b-carriage-return``                          |||  //  MacOS upto 9.x
        ``b-line-feed``                                     //  UNIX, MacOS X

//  [29]    http://www.yaml.org/spec/1.2/spec.html#b-as-line-feed
let ``b-as-line-feed`` = ``b-break``

//  [30]    http://www.yaml.org/spec/1.2/spec.html#b-non-content
let ``b-non-content`` = ``b-break``

//  [31]    http://www.yaml.org/spec/1.2/spec.html#s-space
let ``s-space`` : string = "\u0020"  // space

//  [32]    http://www.yaml.org/spec/1.2/spec.html#s-tab
let ``s-tab`` = "\u0009"    // tab

//  [33]    http://www.yaml.org/spec/1.2/spec.html#s-white
let ``s-white`` = RGO(``s-space`` + ``s-tab``)

//  [34]    http://www.yaml.org/spec/1.2/spec.html#ns-char
let ``ns-char`` = ``nb-char`` - ``s-white``

//  [35]    http://www.yaml.org/spec/1.2/spec.html#ns-dec-digit
let ``ns-dec-digit`` = RGO "\u0030-\u0039"      //  0-9

//  [36]    http://www.yaml.org/spec/1.2/spec.html#ns-hex-digit
let ``ns-hex-digit`` =
    ``ns-dec-digit`` +
    RGO ("\u0041-\u0046")  +  //  A-F
    RGO ("\u0061-\u0066")     //  a-f

//  [37]    http://www.yaml.org/spec/1.2/spec.html#ns-ascii-letter
let ``ns-ascii-letter`` = 
    RGO ("\u0041-\u005A") +   //  A-Z
    RGO ("\u0061-\u007A")     //  a-z

//  [38]    http://www.yaml.org/spec/1.2/spec.html#ns-word-char
let ``ns-word-char`` =
    ``ns-dec-digit`` + (RGO @"\-") + ``ns-ascii-letter``

//  [39]    http://www.yaml.org/spec/1.2/spec.html#ns-uri-char
let ``ns-uri-char`` = 
    RGP @"%" + ``ns-hex-digit`` + ``ns-hex-digit``  |||
    RGO @"#;/?:@&=+$,_.!~*\'\(\)\[\]" + ``ns-word-char``

//  [40]    http://www.yaml.org/spec/1.2/spec.html#ns-tag-char
let ``ns-tag-char`` = 
    RGP @"%" + ``ns-hex-digit`` + ``ns-hex-digit``  |||
    (RGO @"#;/?:@&=+$_.~*\'\(\)" - ``c-flow-indicator``) + ``ns-word-char``

//  [41]    http://www.yaml.org/spec/1.2/spec.html#c-escape
let ``c-escape`` = RGP "\\\\"

//  [42]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-null
let ``ns-esc-null`` = RGP "0"

//  [43]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-bell
let ``ns-esc-bell`` = RGP "a"

//  [44]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-backspace
let ``ns-esc-backspace`` = RGP "b"

//  [45]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-horizontal-tab
let ``ns-esc-horizontal-tab`` = RGP "t"

//  [46]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-line-feed
let ``ns-esc-line-feed`` = RGP "n"

//  [47]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-vertical-tab
let ``ns-esc-vertical-tab`` = RGP "v"

//  [48]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-form-feed
let ``ns-esc-form-feed`` = RGP "f"

//  [49]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-carriage-return
let ``ns-esc-carriage-return`` = RGP "r"

//  [50]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-escape
let ``ns-esc-escape`` = RGP "e"

//  [51]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-space
let ``ns-esc-space`` = RGP "\u0020"

//  [52]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-double-quote
let ``ns-esc-double-quote`` = RGP "\""

//  [53]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-slash
let ``ns-esc-slash`` = RGP "/"

//  [54]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-backslash
let ``ns-esc-backslash`` = RGP "\\\\"

//  [55]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-next-line
let ``ns-esc-next-line`` = RGP "N"

//  [56]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-non-breaking-space
let ``ns-esc-non-breaking-space`` = RGP "_"

//  [57]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-line-separator
let ``ns-esc-line-separator`` = RGP "L"

//  [58]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-paragraph-separator
let ``ns-esc-paragraph-separator`` = RGP "P"

//  [59]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-8-bit
let ``ns-esc-8-bit`` = RGP "x" + Repeat(``ns-hex-digit``,2)

//  [60]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-16-bit
let ``ns-esc-16-bit`` = RGP "u" + Repeat(``ns-hex-digit``,4)

//  [61]    http://www.yaml.org/spec/1.2/spec.html#ns-esc-32-bit
let ``ns-esc-32-bit`` = RGP "U" + Repeat(``ns-hex-digit``,8) // currently not supported

//  [62]    http://www.yaml.org/spec/1.2/spec.html#c-ns-esc-char
let ``c-ns-esc-char`` = 
    RGP "\\\\" +
        (``ns-esc-null``             |||
            ``ns-esc-bell``             |||
            ``ns-esc-backspace``        |||
            ``ns-esc-horizontal-tab``   |||
            ``ns-esc-line-feed``        |||
            ``ns-esc-vertical-tab``     |||
            ``ns-esc-form-feed``        |||
            ``ns-esc-carriage-return``  |||
            ``ns-esc-escape``           |||
            ``ns-esc-space``            |||
            ``ns-esc-double-quote``     |||
            ``ns-esc-slash``            |||
            ``ns-esc-backslash``        |||
            ``ns-esc-next-line``        |||
            ``ns-esc-non-breaking-space``|||
            ``ns-esc-line-separator``   |||
            ``ns-esc-paragraph-separator``|||
            ``ns-esc-8-bit``            |||
            ``ns-esc-16-bit``           |||
            ``ns-esc-32-bit``)

//  [66]    http://www.yaml.org/spec/1.2/spec.html#s-separate-in-line
let ``s-separate-in-line`` = OOM(``s-white``) ||| ``start-of-line``

//  [72]    http://www.yaml.org/spec/1.2/spec.html#b-as-space
let ``b-as-space`` = ``b-break``

//  [75]    http://www.yaml.org/spec/1.2/spec.html#c-nb-comment-text
let ``c-nb-comment-text`` = RGP"#" + ZOM(``nb-char``)

//  [76]    http://www.yaml.org/spec/1.2/spec.html#b-comment
let ``b-comment`` = ``b-non-content`` ||| RGP "\\z" // EOF..

//  [77]    http://www.yaml.org/spec/1.2/spec.html#s-b-comment
let ``s-b-comment`` = OPT(``s-separate-in-line`` + OPT(``c-nb-comment-text``)) + ``b-comment`` 

//  [78]    http://www.yaml.org/spec/1.2/spec.html#l-comment
let ``l-comment`` = ``s-separate-in-line`` + OPT(``c-nb-comment-text``) + ``b-comment``

//  [79]    http://www.yaml.org/spec/1.2/spec.html#s-l-comments
let ``s-l-comments`` = (``s-b-comment`` ||| ``start-of-line``) + ZOM(``l-comment``)

//  [83]    http://www.yaml.org/spec/1.2/spec.html#ns-reserved-directive
let ``ns-reserved-directive`` = 
    ``ns-directive-name`` + ZOMNG(``s-separate-in-line`` + ``ns-directive-parameter``)

//  [84]    http://www.yaml.org/spec/1.2/spec.html#ns-directive-name
let ``ns-directive-name`` = OOM(``ns-char``)

//  [85]    http://www.yaml.org/spec/1.2/spec.html#ns-directive-parameter
let ``ns-directive-parameter`` = OOM(``ns-char``)

//  [86]    http://www.yaml.org/spec/1.2/spec.html#ns-yaml-directive
let ``ns-yaml-directive`` = RGP "YAML" + ``s-separate-in-line`` + GRP(``ns-yaml-version``)

//  [87]    http://www.yaml.org/spec/1.2/spec.html#ns-yaml-version
let ``ns-yaml-version`` = OOM(``ns-dec-digit``) + RGP "\\." + OOM(``ns-dec-digit``)

//  [88]    http://www.yaml.org/spec/1.2/spec.html#ns-tag-directive
let ``ns-tag-directive`` = 
    RGP "TAG" + ``s-separate-in-line`` + GRP(``c-tag-handle``) + ``s-separate-in-line`` + GRP(``ns-tag-prefix``)

//  [89]    http://www.yaml.org/spec/1.2/spec.html#c-tag-handle
let ``c-tag-handle`` = ``c-named-tag-handle`` ||| ``c-secondary-tag-handle`` ||| ``c-primary-tag-handle``

//  [90]    http://www.yaml.org/spec/1.2/spec.html#c-primary-tag-handle
let ``c-primary-tag-handle`` = RGP "!"

//  [91]    http://www.yaml.org/spec/1.2/spec.html#c-secondary-tag-handle
let ``c-secondary-tag-handle`` = RGP "!!"

//  [92]    http://www.yaml.org/spec/1.2/spec.html#c-named-tag-handle
let ``c-named-tag-handle`` = RGP "!" + OOM(``ns-word-char``) + RGP "!"

//  [93]    http://www.yaml.org/spec/1.2/spec.html#ns-tag-prefix
let ``ns-tag-prefix`` = ``c-ns-local-tag-prefix`` ||| ``ns-global-tag-prefix``

//  [94]    http://www.yaml.org/spec/1.2/spec.html#c-ns-local-tag-prefix
let ``c-ns-local-tag-prefix`` = RGP "!" + ZOM(``ns-uri-char``)

//  [95]    http://www.yaml.org/spec/1.2/spec.html#ns-global-tag-prefix
let ``ns-global-tag-prefix`` = ``ns-tag-char`` + ZOM(``ns-uri-char``)


//  [97]    http://www.yaml.org/spec/1.2/spec.html#c-ns-tag-property
let ``c-ns-tag-property`` = ``c-verbatim-tag`` ||| ``c-ns-shorthand-tag`` ||| ``c-non-specific-tag``

//  [98]    http://www.yaml.org/spec/1.2/spec.html#c-verbatim-tag
let ``c-verbatim-tag`` = RGP "!" + RGP "<" + OOM(``ns-uri-char``) + RGP ">"

//  [99]    http://www.yaml.org/spec/1.2/spec.html#c-ns-shorthand-tag
let ``c-ns-shorthand-tag`` = ``c-tag-handle`` + OOM(``ns-tag-char``)

//  [100]   http://www.yaml.org/spec/1.2/spec.html#c-non-specific-tag
let ``c-non-specific-tag`` = RGP "!"

//  [101]   http://www.yaml.org/spec/1.2/spec.html#c-ns-anchor-property
let ``c-ns-anchor-property`` = RGP "&" + ``ns-anchor-name``

//  [102]   http://www.yaml.org/spec/1.2/spec.html#ns-anchor-char
let ``ns-anchor-char`` =  ``ns-char`` - ``c-flow-indicator``

//  [103]   http://www.yaml.org/spec/1.2/spec.html#ns-anchor-name
let ``ns-anchor-name`` = OOM(``ns-anchor-char``)

//  [105]   http://www.yaml.org/spec/1.2/spec.html#e-scalar
let ``e-scalar`` = RGP String.Empty     // we'll see if this works..

//  [106]   http://www.yaml.org/spec/1.2/spec.html#e-node
let ``e-node`` = ``e-scalar``

//  [107]   http://www.yaml.org/spec/1.2/spec.html#nb-double-char
let ``nb-double-char`` = ``c-ns-esc-char`` ||| (``nb-json`` - RGO "\\\\\"")

//  [108]   http://www.yaml.org/spec/1.2/spec.html#ns-double-char
let ``ns-double-char`` = ``c-ns-esc-char`` |||  (``nb-json`` - RGO "\\\\\"" - ``s-white``)

//  [111]   http://www.yaml.org/spec/1.2/spec.html#nb-double-one-line
let ``nb-double-one-line`` = ZOM(``nb-double-char``)

//  [114]   http://www.yaml.org/spec/1.2/spec.html#nb-ns-double-in-line
let ``nb-ns-double-in-line`` = ZOM(ZOM(``s-white``) + ``ns-double-char``)


//  [117]    http://www.yaml.org/spec/1.2/spec.html#c-quoted-quote
let ``c-quoted-quote`` = ``c-single-quote`` + ``c-single-quote``

//  [118]   http://www.yaml.org/spec/1.2/spec.html#nb-single-char
let ``nb-single-char`` = ``c-quoted-quote`` ||| (``nb-json`` - ``c-single-quote``)

//  [119]   http://www.yaml.org/spec/1.2/spec.html#ns-single-char
let ``ns-single-char`` = // ``nb-single-char`` - ``s-white``
    ``c-quoted-quote`` ||| (``nb-json`` - ``c-single-quote`` - ``s-white``)

//  [122]   http://www.yaml.org/spec/1.2/spec.html#nb-single-one-line    
let ``nb-single-one-line`` = ZOM(``nb-single-char``)

//  [123]   http://www.yaml.org/spec/1.2/spec.html#nb-ns-single-in-line
let ``nb-ns-single-in-line`` = ZOM(ZOM(``s-white``) + ``ns-single-char``)

//  [128]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-safe-out
let ``ns-plain-safe-out`` = ``ns-char``

//  [129]   http://www.yaml.org/spec/1.2/spec.html#ns-plain-safe-in
let ``ns-plain-safe-in`` = ``ns-char`` - ``c-flow-indicator``

//  [163]   http://www.yaml.org/spec/1.2/spec.html#c-indentation-indicator(m)
let ``c-indentation-indicator`` = OPT(``ns-dec-digit``)

//  [164]   http://www.yaml.org/spec/1.2/spec.html#c-chomping-indicator(t)
let ``c-chomping-indicator`` = OPT(RGP "\\+" ||| ``c-sequence-entry``)


//  [202]   http://www.yaml.org/spec/1.2/spec.html#l-document-prefix
let ``l-document-prefix`` = OPT(``c-byte-order-mark``) + ZOM(``l-comment``)

//  [203]   http://www.yaml.org/spec/1.2/spec.html#c-directives-end
let ``c-directives-end`` = ``c-sequence-entry`` + ``c-sequence-entry`` + ``c-sequence-entry`` // RGP ("---", [Token.``c-directives-end``])


