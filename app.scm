(define-module (cpele weatherdriven app)
  #:export (init view update))
(use-modules (json)
             (srfi srfi-19)             ; Dates
             (srfi srfi-9 gnu)          ; Records
             (ice-9 match)
             (srfi srfi-1)              ; Regex
             (srfi srfi-197)            ; Pipelines
             )

;; Model

(define-immutable-record-type <weather>
  (make-weather temperature clouds precipitation severe-conditions)
  weather?
  (temperature weather-temperature)
  (clouds weather-clouds)
  (precipitation weather-precipitation)
  (severe-conditions weather-severe-conditions))

(define-immutable-record-type <prediction>
  (make-prediction weather issue-date effect-date probability)
  prediction?
  (weather prediction-weather)
  (issue-date prediction-issue-date)
  (effect-date prediction-effect-date)
  (probability prediction-probability))

(define-immutable-record-type <forecast>
  (make-forecast day prediction-hi prediction-lo)
  forecast?
  (day forecast-day)
  (prediction-hi forecast-prediction-hi)
  (prediction-lo forecast-prediction-lo))

(define-immutable-record-type <model>
  (make-model forecast)
  model?
  (forecast model-forecast))

(define-immutable-record-type <effect>
  (make-effect type payload)
  effect?
  (type effect-type)
  (payload effect-payload))

(define-immutable-record-type <change>
  (make-change model effect)
  change?
  (model change-model)
  (effect change-effect))

(define (init)
  (make-change
   (make-model
    (make-forecast
     'undefined                         ; Day
     'undefined                         ; Hi
     'undefined                         ; Lo
     ))
   (make-effect
    'init
    'undefined)))

;; View

(define (view model)
  (scm->json-string
   (chain model
          (model-forecast _)
          (view-forecast _))))

;; Update

(define (update model message)
  model)

(define (test-update)
  'TBD)
