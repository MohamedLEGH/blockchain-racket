#lang racket
(require racket/string)
(require racket/system)

(define (pretty-print-json json-str)
  ; for escaping '\' and '"' characters
  (define str-clean (string-replace json-str "\"" "\\\""))
  (system (string-append "echo \"" str-clean "\" | jq .")))

(provide pretty-print-json)