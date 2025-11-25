# Immutable Records with Functional Setters: The Complete Answer

## TL;DR - Your Options

**There is no built-in Guile solution that provides BOTH automatic boilerplate generation AND functional setters.**

You must choose:

| Feature | SRFI-9 GNU | R6RS | Hybrid |
|---------|------------|------|--------|
| Auto-generated accessors | ❌ | ✅ | ✅ |
| Auto-generated constructor | ✅ | ✅ | ✅ |
| Auto-generated predicate | ✅ | ✅ | ✅ |
| Functional setters | ✅ | ❌ | 🟡 |
| Boilerplate | HIGH | LOW | MEDIUM |

## Detailed Analysis

### SRFI-9 GNU: Functional Setters, NO Auto-generation

```scheme
(use-modules (srfi srfi-9 gnu))

(define-immutable-record-type <person>
  (make-person name age email)
  person?
  (name person-name set-person-name)        ;; Must name ALL
  (age person-age set-person-age)            ;; 3 names per field
  (email person-email set-person-email))     ;; Repetitive!

(define p (make-person "Alice" 30 "a@example.com"))
(define p2 (set-person-age p 31))  ;; Functional setter
```

**Verdict:** HIGH boilerplate - must explicitly name every accessor and setter.

---

### R6RS Records: Auto-generation, NO Functional Setters

```scheme
(use-modules (rnrs records syntactic))

(define-record-type person
  (fields name age email))

;; Auto-generates: make-person, person?, person-name, person-age, person-email

(define p (make-person "Alice" 30 "a@example.com"))
;; No functional setters! Must write manually:
(define (with-age p new-age)
  (make-person (person-name p) new-age (person-email p)))
```

**Verdict:** LOW boilerplate for definition, but must manually implement functional setters.

**I confirmed:** SRFI-9 GNU's generic `set-field` does NOT work with R6RS records.

---

## Recommended Solutions

### Solution 1: R6RS + Manual Functional Setters (RECOMMENDED)

**Best for:** Records with few fields (< 5), prioritizing clean definitions

```scheme
(use-modules (rnrs records syntactic))

(define-record-type weather
  (fields temperature clouds precipitation severe-conditions))

;; Write functional setters as needed
(define (with-temperature w temp)
  (make-weather temp
                (weather-clouds w)
                (weather-precipitation w)
                (weather-severe-conditions w)))

(define (with-clouds w clouds)
  (make-weather (weather-temperature w)
                clouds
                (weather-precipitation w)
                (weather-severe-conditions w)))
```

**Pros:**
- Clean, minimal record definition
- Auto-generated accessors (weather-temperature, etc.)
- Only write setters you actually need

**Cons:**
- Must manually write each setter
- Error-prone with many fields

---

### Solution 2: R6RS + Setter Helper Macro

**Best for:** Many records or many fields

Create a macro to generate functional setters:

```scheme
(use-modules (rnrs records syntactic))

(define-syntax define-setter
  (syntax-rules ()
    ((define-setter record-type field-name accessor ...)
     (define (field-name rec new-val)
       (record-type
         (if (eq? 'accessor 'field-name)
             new-val
             (accessor rec))
         ...)))))

;; Or simpler: positional setter generator
(define-syntax define-field-setter
  (syntax-rules ()
    ((define-field-setter type-name constructor field-pos accessor ...)
     (define (set-field-pos rec new-val)
       (constructor
         (if (= 0 field-pos) new-val (accessor rec))
         ...)))))
```

This is complex but reusable.

---

### Solution 3: Accept SRFI-9 GNU Boilerplate

**Best for:** When you need named functional setters and don't mind verbosity

```scheme
(use-modules (srfi srfi-9 gnu))

(define-immutable-record-type <weather>
  (make-weather temperature clouds precipitation severe-conditions)
  weather?
  (temperature weather-temperature with-temperature)
  (clouds weather-clouds with-clouds)
  (precipitation weather-precipitation with-precipitation)
  (severe-conditions weather-severe-conditions with-severe-conditions))
```

**Pros:**
- Everything in one place
- Official Guile recommendation
- Functional setters included

**Cons:**
- 3 names per field (field, accessor, setter)
- Very repetitive

---

### Solution 4: Hybrid - R6RS Records, SRFI-9 Setters

**Best for:** Wanting some auto-generation with selected functional setters

```scheme
(use-modules (rnrs records syntactic))

;; Use R6RS for main definition (auto-generation)
(define-record-type weather
  (fields temperature clouds precipitation severe-conditions))

;; Import SRFI-9 GNU separately for records that need functional setters
(use-modules (srfi srfi-9 gnu))

(define-immutable-record-type <state>
  (make-state count status)
  state?
  (count state-count with-count)
  (status state-status with-status))
```

Mix and match based on needs.

---

## Practical Recommendation for weatherdriven

Given your MVU architecture and current R6RS usage:

### Option A: Stay with R6RS + Write Setters As Needed

```scheme
;; app.scm
(use-modules (rnrs records syntactic))

(define-record-type weather
  (fields temperature clouds precipitation severe-conditions))

(define-record-type prediction
  (fields weather issue-date effect-date probability))

;; Write functional setters only when you need them in update
(define (with-temperature w temp)
  (make-weather temp
                (weather-clouds w)
                (weather-precipitation w)
                (weather-severe-conditions w)))

;; In update function
(define (update model msg)
  (match msg
    (('set-temperature temp)
     (let* ((weather (model-weather model))
            (new-weather (with-temperature weather temp)))
       (with-weather model new-weather)))
    ...))
```

**Advantages for MVU:**
- Clean, readable record definitions
- Only write setters you actually use
- Easy to understand and maintain

---

### Option B: Switch to SRFI-9 GNU for Everything

```scheme
;; app.scm
(use-modules (srfi srfi-9 gnu))

(define-immutable-record-type <weather>
  (make-weather temperature clouds precipitation severe-conditions)
  weather?
  (temperature weather-temperature with-temperature)
  (clouds weather-clouds with-clouds)
  (precipitation weather-precipitation with-precipitation)
  (severe-conditions weather-severe-conditions with-severe-conditions))

(define-immutable-record-type <prediction>
  (make-prediction weather issue-date effect-date probability)
  prediction?
  (weather prediction-weather with-weather)
  (issue-date prediction-issue-date with-issue-date)
  (effect-date prediction-effect-date with-effect-date)
  (probability prediction-probability with-probability))
```

**Advantages for MVU:**
- All functional setters ready to use
- Immutability enforced
- Consistent pattern throughout

**Disadvantage:**
- More verbose definitions
- More names to track

---

## My Recommendation

**Use R6RS + manual functional setters (Option A).**

**Rationale:**
1. Your types are relatively small (4 fields max)
2. MVU pattern means you won't need setters for every field
3. Clean definitions are more maintainable
4. You only pay the cost for setters you actually use
5. Follows the Guile documentation's recommended R6RS approach

**Implementation pattern:**
```scheme
;; Define records with R6RS (minimal boilerplate)
(define-record-type weather (fields ...))

;; Write functional setters with descriptive names
(define (with-temperature weather temp) ...)
(define (with-clouds weather clouds) ...)

;; Or more general
(define (update-weather weather #:temperature [temp #f]
                                #:clouds [clouds #f]
                                ...)
  (make-weather
    (or temp (weather-temperature weather))
    (or clouds (weather-clouds weather))
    ...))
```

The boilerplate is manageable and the code remains clean and readable.
