#lang racket
(require racket/string)
(require racket/system)

(define (pretty-print-json json_str)
  ; for escaping '\' and '"' characters
  (define str_clean (string-replace json_str "\"" "\\\""))
  (system (string-append "echo \"" str_clean "\" | jq .")))

(provide pretty-print-json)