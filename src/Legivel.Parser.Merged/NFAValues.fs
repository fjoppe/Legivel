module internal NFAValues

#nowarn "40"

open Legivel.Tokenizer
open Legivel.Utilities.RegexDSL


let ``ns-yaml-directive with comments`` = HardValues.``ns-yaml-directive`` + HardValues.``s-l-comments`` |> rgxToNFA

let ``ns-tag-directive with comments`` = HardValues.``ns-tag-directive``  + HardValues.``s-l-comments`` |> rgxToNFA

let ``ns-reserved-directive with comments`` = GRP(HardValues.``ns-reserved-directive``) + HardValues.``s-l-comments`` |> rgxToNFA

let ``NFA Percent`` = (RGP ("%", [Token.``t-percent``])) |> rgxToNFA
let ``NFA Ampersand`` = (RGP ("&", [Token.``t-ampersand``])) |> rgxToNFA

let ``NFA s-l-comments`` = HardValues.``s-l-comments`` |> rgxToNFA

let ``NFA ns-anchor-name`` = HardValues.``ns-anchor-name`` |> rgxToNFA
let ``NFA illegal ns-anchor-name`` = OOM(HardValues.``ns-char``) |> rgxToNFA

let private lsvt = RGP ("!", [Token.``t-quotationmark``]) + RGP ("<", [Token.``c-printable``])
let private rsvt = RGP (">", [Token.``t-gt``])
let ``NFA Verbatim`` = lsvt + GRP(OOM(HardValues.``ns-uri-char``)) + rsvt |> rgxToNFA
let ``NFA Illegal Verbatim`` = lsvt + OOM(HardValues.``ns-uri-char``) |> rgxToNFA
let ``NFA Illegal Verbatim No Local Tag`` = lsvt + RGP ("!", [Token.``t-quotationmark``]) + rsvt |> rgxToNFA
let ``NFA Shorthand Named`` = GRP(HardValues.``c-named-tag-handle``) + GRP(OOM(HardValues.``ns-tag-char``)) |> rgxToNFA
let ``NFA Illegal Shorthand Named`` = GRP(HardValues.``c-named-tag-handle``) |> rgxToNFA
let ``NFA Shorthand Secondary`` = HardValues.``c-secondary-tag-handle`` + GRP(OOM(HardValues.``ns-tag-char``)) |> rgxToNFA
let ``NFA Illegal Shorthand Secondary`` = HardValues.``c-secondary-tag-handle``  |> rgxToNFA
let ``NFA Shorthand Primary`` = HardValues.``c-primary-tag-handle``+ GRP(OOM(HardValues.``ns-tag-char``)) |> rgxToNFA
let ``NFA c-non-specific-tag`` =  HardValues.``c-non-specific-tag`` |> rgxToNFA

let ``NFA tagged uri`` = HardValues.``c-primary-tag-handle`` + OOM(HardValues.``ns-tag-char``) |> rgxToNFA

let ``NFA Asterisk`` = (RGP ("\\*", [Token.``t-asterisk``])) |> rgxToNFA

let ``NFA c-sequence-end`` = HardValues.``c-sequence-end`` |> rgxToNFA
let ``NFA c-mapping-end`` = HardValues.``c-mapping-end`` |> rgxToNFA
let ``NFA c-mapping-key`` = HardValues.``c-mapping-key`` |>rgxToNFA

let ``NFA c-mapping-value`` = HardValues.``c-mapping-value`` |> rgxToNFA

let ``NFA optional s-separate-in-line`` = (OPT(HardValues.``s-separate-in-line``)) |> rgxToNFA

let ``NFA indent chomp`` = GRP(HardValues.``c-indentation-indicator``) + GRP(HardValues.``c-chomping-indicator``) + HardValues.``s-b-comment`` |> rgxToNFA

let ``NFA chomp indent`` = GRP(HardValues.``c-chomping-indicator``) + GRP(HardValues.``c-indentation-indicator``) + HardValues.``s-b-comment`` |> rgxToNFA

let ``NFA illformed chomping``  = GRP(OOMNG(HardValues.``nb-char``)) + HardValues.``s-b-comment`` |> rgxToNFA

let ``NFA pipe`` = RGP ("\\|", [Token.``t-pipe``]) |> rgxToNFA

let ``NFA c-folded`` = HardValues.``c-folded`` |> rgxToNFA

let ``NFA hyphen`` = (RGP("-", [Token.``t-hyphen``])) |> rgxToNFA

let ``NFA ns-char`` = HardValues.``ns-char`` |> rgxToNFA

let ``NFA e-node s-l-comments`` = (HardValues.``e-node`` + HardValues.``s-l-comments``) |> rgxToNFA

let ``NFA ZOM s-space`` = ZOM(RGP(HardValues.``s-space``,[Token.``t-space``])) |> rgxToNFA

let ``NFA e-node`` = HardValues.``e-node`` |> rgxToNFA

let ``NFA c-forbidden`` = HardValues.``c-forbidden`` |>rgxToNFA

let ``NFA c-directives-end`` = HardValues.``c-directives-end`` |> rgxToNFA

let ``NFA l-document-prefix`` = (ZOM(HardValues.``l-document-prefix``)) |> rgxToNFA

let ``NFA l-document-suffix l-document-prefix`` = (OOM(HardValues.``l-document-suffix``) + ZOM(HardValues.``l-document-prefix``)) |> rgxToNFA


