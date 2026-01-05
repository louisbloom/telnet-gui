;; String Port Tests
;; Tests for string ports providing efficient character-by-character reading
(load "tests/test-helpers.lisp")

;; ===========================================
;; Creating String Ports
;; ===========================================
(define port (open-input-string "hello"))

(assert-true (string-port? port) "string-port? on port")

(assert-false (string-port? "hello") "string-port? on string")
(assert-false (string-port? nil) "string-port? on nil")
(assert-false (string-port? 42) "string-port? on number")

;; ===========================================
;; Reading Characters
;; ===========================================
(define p1 (open-input-string "abc"))

(assert-equal (port-read-char p1) #\a "port-read-char first")
(assert-equal (port-read-char p1) #\b "port-read-char second")
(assert-equal (port-read-char p1) #\c "port-read-char third")

(assert-nil (port-read-char p1) "port-read-char at EOF")
(assert-nil (port-read-char p1) "port-read-char past EOF")

;; ===========================================
;; Peeking Characters
;; ===========================================
(define p2 (open-input-string "xyz"))

(assert-equal (port-peek-char p2) #\x "port-peek-char first")
(assert-equal (port-peek-char p2) #\x "port-peek-char same again")
(assert-equal (port-read-char p2) #\x "port-read-char after peek")
(assert-equal (port-peek-char p2) #\y "port-peek-char second")

;; ===========================================
;; Position Tracking
;; ===========================================
(define p3 (open-input-string "test"))

(assert-equal (port-position p3) 0 "port-position at start")

(port-read-char p3)

(assert-equal (port-position p3) 1 "port-position after one read")

(port-read-char p3)
(port-read-char p3)

(assert-equal (port-position p3) 3 "port-position after three reads")

(port-peek-char p3) ; peek doesn't advance

(assert-equal (port-position p3) 3 "port-position unchanged after peek")

;; ===========================================
;; Source String
;; ===========================================
(define p4 (open-input-string "source"))

(assert-equal (port-source p4) "source" "port-source at start")

(port-read-char p4)
(port-read-char p4)

(assert-equal (port-source p4) "source" "port-source unchanged after reads")

;; ===========================================
;; EOF Detection
;; ===========================================
(define p5 (open-input-string "ab"))

(assert-false (port-eof? p5) "port-eof? at start")

(port-read-char p5)

(assert-false (port-eof? p5) "port-eof? after first read")

(port-read-char p5)

(assert-true (port-eof? p5) "port-eof? at end")

(port-read-char p5) ; read past EOF

(assert-true (port-eof? p5) "port-eof? past end")

;; ===========================================
;; Empty String
;; ===========================================
(define p6 (open-input-string ""))

(assert-true (port-eof? p6) "port-eof? on empty string")

(assert-nil (port-read-char p6) "port-read-char on empty string")
(assert-nil (port-peek-char p6) "port-peek-char on empty string")

(assert-equal (port-position p6) 0 "port-position on empty string")
(assert-equal (port-source p6) "" "port-source on empty string")

;; ===========================================
;; Multi-byte UTF-8 Characters
;; ===========================================
(define p7 (open-input-string "日本語"))

(assert-equal (port-read-char p7) #\u65e5 "port-read-char UTF-8 first")
(assert-equal (port-position p7) 1 "port-position counts chars not bytes")
(assert-equal (port-read-char p7) #\u672c "port-read-char UTF-8 second")
(assert-equal (port-read-char p7) #\u8a9e "port-read-char UTF-8 third")

(assert-true (port-eof? p7) "port-eof? after UTF-8")

;; ===========================================
;; Mixed ASCII and UTF-8
;; ===========================================
(define p8 (open-input-string "a日b"))

(assert-equal (port-read-char p8) #\a "mixed: ASCII first")
(assert-equal (port-read-char p8) #\u65e5 "mixed: UTF-8 middle")
(assert-equal (port-read-char p8) #\b "mixed: ASCII last")
(assert-equal (port-position p8) 3 "mixed: position is char count")

;; ===========================================
;; Whitespace and Special Characters
;; ===========================================
(define p9 (open-input-string "a b\tc\n"))

(assert-equal (port-read-char p9) #\a "special: first char")
(assert-equal (port-read-char p9) #\space "special: space")
(assert-equal (port-read-char p9) #\b "special: after space")
(assert-equal (port-read-char p9) #\tab "special: tab")
(assert-equal (port-read-char p9) #\c "special: after tab")
(assert-equal (port-read-char p9) #\newline "special: newline")

