(in-package #:montezuma)

;;https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references

;; The following ASCII table contains both ASCII control characters, ASCII printable characters and the extended ASCII character set ISO 8859-1,
;; also called ISO Latin1

(alexandria:define-constant *character-entities*
  '(
#|
    ("NUL" 0 "Null char")
    ("SOH" 1 "Start of Heading")
    ("STX" 2 "Start of Text")
    ("ETX" 3 "End of Text")
    ("EOT" 4 "End of Transmission")
    ("ENQ" 5 "Enquiry")
    ("ACK" 6 "Acknowledgment")
    ("BEL" 7 "Bell")
    ("BS" 8 "Back Space")
    ("HT" 9 "Horizontal Tab")
    ("LF" 10 "Line Feed")
    ("VT" 11 "Vertical Tab")
    ("FF" 12 "Form Feed")
    ("CR" 13 "Carriage Return")
    ("SO" 14 "Shift Out / X-On")
    ("SI" 15 "Shift In / X-Off")
    ("DLE" 16 "Data Line Escape")
    ("DC1" 17 "Device Control 1 (oft. XON)")
    ("DC2" 18 "Device Control 2")
    ("DC3" 19 "Device Control 3 (oft. XOFF)")
    ("DC4" 20 "Device Control 4")
    ("NAK" 21 "Negative Acknowledgement")
    ("SYN" 22 "Synchronous Idle")
    ("ETB" 23 "End of Transmit Block")
    ("CAN" 24 "Cancel")
    ("EM" 25 "End of Medium")
    ("SUB" 26 "Substitute")
    ("ESC" 27 "Escape")
    ("FS" 28 "File Separator")
    ("GS" 29 "Group Separator")
    ("RS" 30 "Record Separator")
    ("US" 31 "Unit Separator")
|#
    ("space" 32 "Space")
    ("excl" 33 "Exclamation")
    ("quot" 34 "Double quotes")
    ("number" 35 "Number")
    ("dollar" 36 "Dollar")
    ("percentage" 37 "Percentage")
    ("amp" 38 "Ampersand")
    ("squot" 39 "Single quote")
    ("lpar" 40 "parenthesis (or open bracket)")
    ("rpar" 41 "parenthesis (or close bracket)")
    ("ast" 42 "Asterisk (*)")
    ("plus" 43 "+")
    ("comma" 44 ",")
    ("minus" 45 "-")
    ("period" 46 "period, dot or full stop")
    ("slash" 47 "divide, forward slash")
    #| -9 |#
    ("colon" 58 "Colon")
    ("semicolon" 59 "Semicolon")
    ("lt" 60 "Less than (or open angled bracket)")
    ("eql" 61 "Equals")
    ("gt" 62 "Greater than (or close angled bracket)")
    ("quest" 63 "Question mark")
    ("atsym" 64 "At symbol")
    #| A-Z |#
    ("obracket" 91 "Opening bracket")
    ("backslash" 92 "Backslash")
    ("cbracket" 93 "Closing bracket")
    ("caret" 94 "Caret - circumflex, hat")
    ("underscore" 95 "Underscore")
    ("grave" 96 "Grave accent")
    #| a-z |#
    ("lcub" 123 "Opening brace")
    ("vbar" 124 "Vertical bar")
    ("rcub" 125 "Closing brace")
    ("tilde" 126 "Equivalency sign - tilde")
    ("del" 127 "Delete")
    ("euro" 128 "Euro sign")
    ;;(129 "")
    ("qlow" 130 "Single low-9 quotation mark")
    ("fhook" 131 "Latin small letter f with hook")
    ("dquot" 132 "Double low-9 quotation mark")
    ("hellip" 133 "Horizontal ellipsis")
    ("dagger" 134 "Dagger")
    ("ddagger" 135 "Double dagger")
    ("circumflex" 136 "Modifier letter circumflex accent")
    ("permill" 137 "Per mille sign")
    ("Scaron"138 "Latin capital letter S with caron")
    ("lsaquo" 139 "Single left-pointing angle quotation")
    ("OElig" 140 "Latin capital ligature OE")
    ;;(141))
    ("Zcaron" 142 "Latin captial letter Z with caron")
    ;;(143))
    ;;(144))
    ("lsquo" 145 "Left single quotation mark")
    ("rsquo" 146 "Right single quotation mark")
    ("ldquo" 147 "Left double quotation mark")
    ("rdquo" 148 "Right double quotation mark")
    ("bull" 149 "Bullet")
    ("ndash" 150 "En dash")
    ("mdash" 151 "Em dash")
    ("smtilde" 152 "Small tilde")
    ("trade" 153 "Trade mark sign")
    ("scaron" 154 "Latin small letter S with caron")
    ("rsaquo" 155 "Single right-pointing angle quotation mark")
    ("oelig" 156 "Latin small ligature oe")
    ;;(157))
    ("zcaron" 158 "Latin small letter z with caron")
    ("Yuml" 159 "Latin capital letter Y with diaeresis")
    ("nbsp" 160 "no-break space")
    ("iexcl" 161 "[[inverted question and exclamation marks|inverted exclamation mark]]")
    ("cent" 162 "[[Cent (currency)|cent sign]]")
    ("pound" 163 "[[pound sign]]")
    ("curren" 164 "[[Currency (typography)|currency sign]]")
    ("yen" 165 "[[Japanese yen|yen sign]] ''([[Chinese yuan|yuan]] sign)''")
    ("brvbar" 166 "broken bar ''(broken vertical bar)''")
    ("sect" 167 "[[section sign]]")
    ("uml" 168  "[[Trema (diacritic)|diaeresis]] ''(spacing diaeresis)''; see [[Germanic umlaut]]")
    ("copy" 169 "[[copyright symbol]]")
    ("ordf" 170 "feminine [[ordinal indicator]]")
    ("laquo" 171 "left-pointing double angle quotation mark ''(left pointing [[guillemet]])''")
    ("not" 172 "[[List of logic symbols|not sign]]")
    ("shy" 173 "[[soft hyphen]] ''(discretionary hyphen)''")
    ("reg" 174 "registered sign ''([[registered trademark symbol]])''")
    ("macr" 175 "[[macron]] ''(spacing macron, overline, APL overbar)''")
    ("deg" 176 "[[degree symbol]]")
    ("plusmn" 177 "[[plus-minus sign]] ''(plus-or-minus sign)''")
    ("sup2" 178 "superscript two ''(superscript digit two, squared)''")
    ("sup3" 179 "superscript three ''(superscript digit three, cubed)''")
    ("acute" 180 "[[acute accent]] ''(spacing acute)''")
    ("micro" 181 "micro sign")
    ("para" 182 "[[pilcrow]] sign ''(paragraph sign)''")
    ("middot" 183 "[[Interpunct|middle dot]] ''(Georgian comma, Greek middle dot)''")
    ("cedil" 184 "[[cedilla]] ''(spacing cedilla)''")
    ("sup1" 185 "superscript one ''(superscript digit one)''")
    ("ordm" 186 "masculine [[ordinal indicator]]")
    ("raquo" 187 "right-pointing double angle quotation mark ''(right pointing guillemet)''")
    ("frac14" 188 "vulgar fraction one quarter ''(fraction one quarter)''")
    ("frac12" 189 "vulgar fraction one half ''(fraction one half)''")
    ("frac34" 190 "vulgar fraction three quarters ''(fraction three quarters)''")
    ("iquest" 191 "[[Inverted question and exclamation marks|inverted question mark]] ''(turned question mark)''")
    ("Agrave" 192 "Latin capital letter A with [[grave accent]] ''(Latin capital letter A grave)''")
    ("Aacute" 193 "Latin capital letter A with [[acute accent]]")
    ("Acirc" 194 "Latin capital letter A with [[circumflex]]")
    ("Atilde" 195 "Latin capital letter A with [[tilde]]")
    ("Auml" 196 "Latin capital letter A with [[Trema (diacritic)|diaeresis]]")
    ("Aring" 197 "Latin capital letter A with ring above ''(Latin capital letter A ring)''")
    ("AElig" 198 "Latin capital letter AE ''(Latin capital ligature AE)''")
    ("Ccedil" 199 "Latin capital letter C with [[cedilla]]")
    ("Egrave" 200 "Latin capital letter E with [[grave accent]]")
    ("Eacute" 201 "Latin capital letter E with [[acute accent]]")
    ("Ecirc" 202 "Latin capital letter E with [[circumflex]]")
    ("Euml" 203 "Latin capital letter E with [[Trema (diacritic)|diaeresis]]")
    ("Igrave" 204 "Latin capital letter I with [[grave accent]]")
    ("Iacute" 205 "Latin capital letter I with [[acute accent]]")
    ("Icirc" 206 "Latin capital letter I with [[circumflex]]")
    ("Iuml" 207 "Latin capital letter I with [[Trema (diacritic)|diaeresis]]")
    ("Eth" 208 "Latin capital letter [[Eth]]")
    ("Ntilde" 209 "Latin capital letter N with [[tilde]]")
    ("Ograve" 210 "Latin capital letter O with [[grave accent]]")
    ("Oacute" 211 "Latin capital letter O with [[acute accent]]")
    ("Ocirc" 212 "Latin capital letter O with [[circumflex]]")
    ("Otilde" 213 "Latin capital letter O with [[tilde]]")
    ("Ouml" 214 "Latin capital letter O with [[Trema (diacritic)|diaeresis]]")
    ("times"  215 "[[multiplication sign]]")
    ("Oslash" 216 "Latin capital letter O with stroke ''(Latin capital letter O slash)''")
    ("Ugrave" 217 "Latin capital letter U with [[grave accent]]")
    ("Uacute" 218 "Latin capital letter U with [[acute accent]]")
    ("Ucirc" 219 "Latin capital letter U with [[circumflex]]")
    ("Uuml" 220 "Latin capital letter U with [[Trema (diacritic)|diaeresis]]")
    ("Yacute" 221 "Latin capital letter Y with [[acute accent]]")
    ("Thorn" 222 "Latin capital letter [[Thorn (letter)|THORN]]")
    ("szlig" 223 "Latin small letter sharp s ''(ess-zed)''; see German [[Eszett]]")
    ("agrave" 224 "Latin small letter a with [[grave accent]]")
    ("aacute" 225 "Latin small letter a with [[acute accent]]")
    ("acirc" 226 "Latin small letter a with [[circumflex]]")
    ("atilde" 227 "Latin small letter a with [[tilde]]")
    ("auml" 228 "Latin small letter a with [[Trema (diacritic)|diaeresis]]")
    ("aring" 229 "Latin small letter a with ring above")
    ("aelig" 230 "Latin small letter ae ''(Latin small ligature ae)''")
    ("ccedil" 231 "Latin small letter c with [[cedilla]]")
    ("egrave" 232 "Latin small letter e with [[grave accent]]")
    ("eacute" 233 "Latin small letter e with [[acute accent]]")
    ("ecirc" 234 "Latin small letter e with [[circumflex]]")
    ("euml" 235 "Latin small letter e with [[Trema (diacritic)|diaeresis]]")
    ("igrave" 236 "Latin small letter i with [[grave accent]]")
    ("iacute" 237 "Latin small letter i with [[acute accent]]")
    ("icirc" 238 "Latin small letter i with [[circumflex]]")
    ("iuml" 239 "Latin small letter i with [[Trema (diacritic)|diaeresis]]")
    ("eth" 240 "Latin small letter [[eth]]")
    ("ntilde" 241 "Latin small letter n with [[tilde]]")
    ("ograve" 242 "Latin small letter o with [[grave accent]]")
    ("oacute" 243 "Latin small letter o with [[acute accent]]")
    ("ocirc" 244 "Latin small letter o with [[circumflex]]")
    ("otilde" 245 "Latin small letter o with [[tilde]]")
    ("ouml" 246 "Latin small letter o with [[Trema (diacritic)|diaeresis]]")
    ("divide" 247 "division sign ''([[obelus]])''")
    ("oslash" 248 "Latin small letter o with stroke ''(Latin small letter o slash)''")
    ("ugrave" 249 "Latin small letter u with [[grave accent]]")
    ("uacute" 250 "Latin small letter u with [[acute accent]]")
    ("ucirc" 251 "Latin small letter u with [[circumflex]]")
    ("uuml" 252 "Latin small letter u with [[Trema (diacritic)|diaeresis]]")
    ("yacute" 253 "Latin small letter y with [[acute accent]]")
    ("thorn" 254 "Latin small letter [[Thorn (letter)|thorn]]")
    ("yuml" 255 "Latin small letter y with [[Trema (diacritic)|diaeresis]]")
    ("OElig" 338 "Latin capital ligature oe")
    ("oelig" 339 "Latin small ligature oe")
    ("Scaron" 352 "Latin capital letter s with [[caron]]")
    ("scaron" 353 "Latin small letter s with [[caron]]")
    ("Yuml" 376 "Latin capital letter y with [[Trema (diacritic)|diaeresis]]")
    ("fnof" 402 "Latin small letter f with hook ''([[Function (mathematics)#Notation|function]], florin)''")
    ("circ" 710 "modifier letter [[circumflex]] accent")
    ("tilde" 732 "small [[tilde]]")
    ("Alpha" 913 "Greek letter alpha")
    ("Beta" 914 "Greek capital letter Beta")
    ("Gamma" 915 "Greek capital letter Gamma")
    ("Delta" 916 "Greek capital letter Delta")
    ("Epsilon" 917 "Greek capital letter Epsilon")
    ("Zeta" 918 "Greek capital letter Zeta")
    ("Eta" 919 "Greek capital letter Eta")
    ("Theta" 920 "Greek capital letter Theta")
    ("Iota" 921 "Greek capital letter Iota")
    ("Kappa" 922 "Greek capital letter Kappa")
    ("Lambda" 923 "Greek capital letter Lambda")
    ("Mu" 924 "Greek capital letter Mu")
    ("Nu" 925 "Greek capital letter Nu")
    ("Xi" 926 "Greek capital letter Xi")
    ("Omicron" 927 "Greek capital letter Omicron")
    ("Pi" 928 "Greek capital letter Pi")
    ("Rho" 929 "Greek capital letter Rho")
    ("Sigma" 931 "Greek capital letter Sigma")
    ("Tau" 932 "Greek capital letter Tau")
    ("Upsilon" 933 "Greek capital letter Upsilon")
    ("Phi" 934 "Greek capital letter Phi")
    ("Chi" 935 "Greek capital letter Chi")
    ("Psi" 936 "Greek capital letter Psi")
    ("Omega" 937 "Greek capital letter Omega")
    ("alpha" 945 "Greek small letter alpha")
    ("beta" 946 "Greek small letter beta")
    ("gamma" 947 "Greek small letter gamma")
    ("delta" 948 "Greek small letter delta")
    ("epsilon" 949 "Greek small letter epsilon")
    ("zeta" 950 "Greek small letter zeta")
    ("eta" 951 "Greek small letter eta")
    ("theta" 952 "Greek small letter theta")
    ("iota" 953 "Greek small letter iota")
    ("kappa" 954 "Greek small letter kappa")
    ("lambda" 955 "Greek small letter lambda")
    ("mu" 956 "Greek small letter mu")
    ("nu" 957 "Greek small letter nu")
    ("xi" 958 "Greek small letter xi")
    ("omicron" 959 "Greek small letter omicron")
    ("pi" 960 "Greek small letter pi")
    ("rho" 961 "Greek small letter rho")
    ("sigmaf" 962 "Greek small letter final sigma")
    ("sigma" 963 "Greek small letter sigma")
    ("tau" 964 "Greek small letter tau")
    ("upsilon" 965 "Greek small letter upsilon")
    ("phi" 966 "Greek small letter phi")
    ("chi" 967 "Greek small letter chi")
    ("psi" 968 "Greek small letter psi")
    ("omega" 969 "Greek small letter omega")
    ("thetasym" 977 "Greek theta symbol")
    ("upsih" 978 "Greek Upsilon with hook symbol")
    ("piv" 982 "Greek pi symbol")
    ("ensp" 8194 "[[en (typography)|en space]]")
    ("emsp" 8195 "[[em (typography)|em space]]")
    ("thinsp" 8201 "[[thin space]]")
    ("zwnj" 8204 "[[zero-width non-joiner]]")
    ("zwj" 8205 "[[zero-width joiner]]")
    ("lrm" 8206 "[[left-to-right mark]]")
    ("rlm" 8207 "[[right-to-left mark]]")
    ("ndash" 8211 "[[en dash]]")
    ("mdash" 8212 "[[em dash]]")
    ("lsquo" 8216 "left single [[quotation mark]]")
    ("rsquo" 8217 "right single [[quotation mark]]")
    ("sbquo" 8218 "single low-9 [[quotation mark]]")
    ("ldquo" 8220 "left double [[quotation mark]]")
    ("rdquo" 8221 "right double [[quotation mark]]")
    ("bdquo" 8222 "double low-9 [[quotation mark]]")
    ("dagger" 8224 "[[Dagger (typography)|dagger, obelisk]]")
    ("Dagger" 8225 "[[Dagger (typography)|double dagger, double obelisk]]")
    ("bull" 8226 "[[Bullet (typography)|bullet]] ''(black small circle)''")
    ("hellip" 8230 "horizontal [[ellipsis]] ''(three dot leader)''")
    ("permil" 8240 "[[per mil]]le sign")
    ("prime" 8242 "[[Prime (symbol)|prime]] ''(minutes, feet)''")
    ("Prime" 8243 "[[Prime (symbol)|double prime]] ''(seconds, inches)''")
    ("lsaquo" 8249 "single left-pointing angle quotation mark")
    ("rsaquo" 8250 "single right-pointing angle quotation mark")
    ("oline" 8254 "[[overline]] ''(spacing overscore)''")
    ("frasl" 8260 "fraction slash ''([[Solidus (punctuation)|solidus]])''")
    ("euro" 8364 "[[euro sign]]")
    ("image" 8465 "black-letter capital I ''(imaginary part)''")
    ("weierp" 8472 "script capital P ''(power set, [[Weierstrass p]])''")
    ("real" 8476 "black-letter capital R ''(real part symbol)''")
    ("trade" 8482 "[[trademark symbol]]")
    ("alefsym" 8501 "[[Aleph number|alef symbol]] ''(first transfinite cardinal)''")
    ("larr" 8592 "leftwards arrow")
    ("uarr" 8593 "upwards arrow")
    ("rarr" 8594 "rightwards arrow")
    ("darr" 8595 "downwards arrow")
    ("harr" 8596 "left right arrow")
    ("crarr" 8629 "downwards arrow with corner leftwards ''(carriage return)''")
    ("lArr" 8656 "leftwards double arrow")
    ("uArr" 8657 "upwards double arrow")
    ("rArr" 8658 "rightwards double arrow")
    ("dArr" 8659 "downwards double arrow")
    ("hArr" 8660 "left right double arrow")
    ("forall" 8704 "[[Turned a|for all]]")
    ("part" 8706 "partial differential")
    ("exist" 8707 "[[List of logic symbols|there exists]]")
    ("empty" 8709 "[[empty set]] ''(null set)'']")
    ("nabla" 8711 "[[del]] or [[nabla symbol|nabla]] ''(vector differential operator)''")
    ("isin" 8712 "element of")
    ("notin" 8713 "not an element of")
    ("ni" 8715 "contains as member")
    ("prod" 8719 "Capital Pi notation|n-ary product]] ''(product sign)''")
    ("sum" 8721 "[[Series (mathematics)|n-ary summation]]")
    ("minus" 8722 "minus sign")
    ("lowast" 8727 "asterisk operator")
    ("prop" 8733 "proportional to")
    ("infin" 8734 "The infinity symbol|infinity]]")
    ("ang" 8736 "[[angle]]")
    ("radic" 8737 "Definition and notation|square root]] ''(radical sign)''")
    ("and" 8743 "[[List of logic symbols|logical and]] ''(wedge)''")
    ("or" 8744 "[[List of logic symbols|logical or]] ''(vee)''")
    ("cap" 8745 "[[Intersection (set theory)|intersection]] ''(cap)''")
    ("cup" 8746 "[[Union (set theory)|union]] ''(cup)''")
    ("int" 8747 "Terminology and notation|integral]]")
    ("there4" 8756 "[[therefore sign]]")
    ("sim" 8764 "tilde operator ''(varies with, similar to)''")
    ("cong" 8773 "congruent to")
    ("asymp" 8776 "almost equal to ''(asymptotic to)''")
    ("ne" 8800 "[[Inequation|not equal to]]")
    ("equiv" 8801 "identical to; sometimes used for 'equivalent to'")
    ("le" 8804 "less-than or equal to")
    ("ge" 8805 "greater-than or equal to")
    ("sub" 8834 "subset of")
    ("sup" 8835 "superset of")
    ("nsub" 8836 "not a subset of")
    ("sube" 8838 "subset of or equal to")
    ("supe" 8839 "superset of or equal to")
    ("oplus" 8853 "circled plus ''(direct sum)''")
    ("otimes" 8855 "circled times ''(vector product)''")
    ("perp" 8869 "[[up tack]] ''(orthogonal to, [[perpendicular]])''")
    ("sdot" 8901 "dot operator")
    ("lceil" 8968 "left ceiling ''(APL upstile)''")
    ("rceil" 8969 "right ceiling")
    ("lfloor" 8970 "left floor ''(APL downstile)''")
    ("rfloor" 8971 "right floor")
    ("lang" 9001 "left-pointing angle bracket ''(bra)''")
    ("rang" 9002 "right-pointing angle bracket ''(ket)''")
    ("loz" 9674 "[[lozenge]]")
    ("spades" 9824 "[[Suit (cards)|black spade suit]]")
    ("clubs" 9827 "[[Suit (cards)|black club suit]] ''(shamrock)''")
    ("hearts" 9829 "[[Suit (cards)|black heart suit]] ''(valentine)''")
    ("diams" 9830 "[[Suit (cards)|black diamond suit]]")
    )
  :test 'equalp)
#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *character-entity-table* (make-hash-table :test #'eq)) ;#'string=))
  (dolist (e *character-entities*)
    (setf (gethash (intern (string-upcase (car e))) *character-entity-table*) (cadr e))))
|#

(defun resolve-entities (word)
  ;; no ampersands, don't bother trying to resolve entities.
  ;;(unless (stringp word) (return-from resolve-entities word))
  (unless (find #\& word) (return-from resolve-entities word))

  ;; resolve hexadecimal specified entities
  (setf word
    (cl-ppcre:regex-replace-all
     "&#x([0-9a-fA-F]+);" word
     #'(lambda(target start end match-start match-end reg-starts reg-ends)
         (declare (ignorable reg-starts reg-ends))
         (let ((decoded (let ((*read-base* 16)) (read-from-string (subseq target (+ 3 match-start) (1- match-end))))))
           (format () "~a~a~a"  (subseq target 0 start) (code-char decoded) (subseq target end))))))

  ;; resolve decimal specified entities
  (setf word
    (cl-ppcre:regex-replace-all
     "&#([0-9]+);" word
     #'(lambda(target start end match-start match-end reg-starts reg-ends)
         (declare (ignorable reg-starts reg-ends))
         (let ((decoded (read-from-string (subseq target (+ 2 match-start) (1- match-end)))))
           (format () "~a~a~a" (subseq target 0 start) (code-char decoded) (subseq target end))))))
#|
  ;; resolve named entities
  (setf word
    (cl-ppcre:regex-replace-all
     "&([a-zA-Z]+);" word
     #'(lambda(target start end match-start match-end reg-starts reg-ends)
         (declare (ignorable reg-starts reg-ends))
         (let* ((entity (intern (string-upcase (subseq target (1+ match-start) (1- match-end)))))
                (ch (gethash entity *character-entity-table*)))
           (unless ch (error "unrecognised character entity: ~a" entity))
           (format () "~a~a~a" (subseq target 0 start) (code-char ch) (subseq target end))))))
|#

  ;; resolve named entities
  (setf word
    (cl-ppcre:regex-replace-all
     "&([a-zA-Z]+);" word
     #'(lambda(target start end match-start match-end reg-starts reg-ends)
         (declare (ignorable reg-starts reg-ends))
         (let* ((entity (subseq target (1+ match-start) (1- match-end))) ; discard & and semicolon
                (ch (cadr (assoc entity *character-entities* :test #'string=))))
           (unless ch (error "unrecognised character entity: ~a in ~s" entity word))
           (format () "~a~a~a" (subseq target 0 start) (code-char ch) (subseq target end))))))
  word)

(defun make-html-character-entities ()
  ;;(let ((as (make-adjustable-string 0)))
    (with-output-to-string (s)
      (format s "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">
<html style=\"font-size: 16px;\">
<head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
<title>Montezuma - Character Entities</title>
<link media=\"screen\" type=\"text/css\" href=\"./screen.css\" rel=\"stylesheet\">
<link media=\"print\" type=\"text/css\" href=\"./print.css\" rel=\"stylesheet\">
</head>
<body>
<div id=\"content\">
<h1>Montezuma - Character Entities</h1>

<h2 class=\"boxed\">Character Reference Overview</h2>
<p>The character entity information was obtained from: <a href=\"https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references\">https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references</a></p>
</div>

<p>A <i>numeric character reference</i> refers to a character by its Universal Character Set, it's Unicode <i>code point</i>, and uses the format</p>

<code>&amp;#</code><i>nnnn</i><code>;</code>

<p>or</p>

<code>&amp;#x</code><i>hhhh</i><code>;</code>

<p>where <i>nnnn</i> is the code point in decimal form, and <i>hhhh</i> is the code point in hexadecimal form.
The <i>x</i> must be lowercase in XML documents. The <i>nnnn</i> or <i>hhhh</i> may be any number of digits
and may include leading zeros. The <i>hhhh</i> may mix uppercase and lowercase, though uppercase is the usual style.</p>

<p>In contrast, a <i>character entity reference</i> refers to a character by the name of an <i>SGML entity</a></i> which
has the desired character as its <i>replacement text</i>. The entity must either be predefined (built into the markup language)
or explicitly declared in a \"Document Type Definition\" (DTD). The format is the same as for any entity reference:</p>

<code>&amp;</code><i>name</i><code>;</code>

<p>where <i>name</i> is the case-sensitive name of the entity. The semicolon is required.</p>
<table class=\"\">

<tr><td>Character</td><td>Name</td><td>Decimal</td><td>Hexadecimal</td><td>Description</td></tr>
")

      (dolist (c *character-entities*)
        (format s "<tr><td>&#~d;</td><td><code>&amp;~a;</code></td><td><code>&amp;#~a;</code></td><td><code>&amp;#x~x;</code></td><td>~a</td></tr>~%"
                (cadr c) ; html character
                (car c) ; name
                (cadr c) ; decimal
                (cadr c) ; hexadecimal
                (caddr c) ; description
                ))
      (format s "</table>
</body>
</html>")))


(defvar *escape-char-p*
  (lambda (char)
    (or (find char "<>&'\"")
        (> (char-code char) 127)))
  "Used by ESCAPE-STRING to test whether a character should be escaped.")

;;;; this code was reparented from who
(defun escape-string (string &key (test *escape-char-p*))
  (declare (optimize speed))
  "Escape all characters in STRING which pass TEST. This function is
not guaranteed to return a fresh string.  Note that you can pass NIL
for STRING which'll just be returned."
  (let ((first-pos (position-if test string)))
    (unless first-pos
      ;; nothing to do, just return STRING
      (return-from escape-string string))
    (with-output-to-string (s)
      (loop with len = (length string)
            for old-pos = 0 then (1+ pos)
            for pos = first-pos
            then (position-if test string :start old-pos)
            ;; now the characters from OLD-POS to (excluding) POS
            ;; don't have to be escaped while the next character has to
            for char = (and pos (char string pos))
            while pos
            do (write-sequence string s :start old-pos :end pos)
            (case char
              ((#\<)
               (write-sequence "&lt;" s))
              ((#\>)
               (write-sequence "&gt;" s))
              ((#\&)
               (write-sequence "&amp;" s))
              ((#\')
               (write-sequence "&#039;" s))
              ((#\")
               (write-sequence "&quot;" s))
              (otherwise
               (format s "&#~d;" (char-code char))))
            while (< (1+ pos) len)
            finally (unless pos
                      (write-sequence string s :start old-pos))))))
