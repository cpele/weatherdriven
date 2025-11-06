(define-module (cpele weatherdriven app))
(use-modules (json)
             (srfi srfi-19))

;; MVU program

(define (init)
  ;; To do: Init hash table not alist
  `((model
     . ((title . "Today's weather")
        ;; To do: Use plain date not date string
        (date . ,(date->string (current-date)))
        (forecast
         . ((hi
             . ((text . null)
                (temp-deg . null)))
            (lo
             . ((text . null)
                (temp-deg . null)))))))
    (effect . fetch)))

(define (view model)
  ;; To do: Convert hash table to alist
  ;; To do: Convert plain date to date string
  (scm->json-string model))

(define (update model message)
  "TBD"
  )
