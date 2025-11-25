# Record Types Comparison in Guile

## Summary

Based on the Guile documentation, here are your options for immutable records with functional setters:

### Best Option: SRFI-9 with GNU Extensions

**Module:** `(srfi srfi-9 gnu)`

**Recommendation:** This is the best choice for your needs because it:
- Provides `define-immutable-record-type` with automatic functional setters
- Generates all boilerplate automatically (constructor, predicate, accessors, functional setters)
- Has minimal ceremony compared to R6RS
- Is the recommended approach in Guile for most use cases
- Works with pattern matching

### Alternative: R6RS Records

**Module:** `(rnrs records syntactic (6))`

This provides more features (inheritance, protocols, etc.) but requires more explicit declarations and doesn't have built-in functional setters.

---

## Detailed Comparison

### 1. SRFI-9 with GNU Extensions (RECOMMENDED)

**Module:**
```scheme
(use-modules (srfi srfi-9 gnu))
```

**Syntax:**
```scheme
(define-immutable-record-type <person>
  (person age email address)
  person?
  (age     person-age set-person-age)
  (email   person-email set-person-email)
  (address person-address set-person-address))
```

**Features:**
- ✅ Immutable by default (cannot mutate even with `struct-set!`)
- ✅ Functional setters automatically generated
- ✅ Returns new record instance with updated field
- ✅ Simple, minimal syntax
- ✅ All boilerplate auto-generated (constructor, predicate, accessors, setters)
- ✅ Works with pattern matching
- ✅ Can use generic `set-field` and `set-fields` macros

**Example Usage:**
```scheme
(define rms (person 30 "rms@gnu.org" some-address))

;; Functional setter returns new instance
(define older-rms (set-person-age rms 60))

(person-age rms)       ;; ⇒ 30 (original unchanged)
(person-age older-rms) ;; ⇒ 60 (new instance)

;; Generic setter for any SRFI-9 record
(set-field rms (person-age) 60)

;; Update multiple fields efficiently
(set-fields rms
  ((person-age) 60)
  ((person-address address-street) "Temple Place"))
```

**Advantages:**
- Simple and concise
- Built-in functional setters
- Recommended by Guile documentation
- Minimal boilerplate

**Limitations:**
- No inheritance support
- No custom protocols
- No opaque/sealed types

---

### 2. R6RS Records

**Module:**
```scheme
(use-modules (rnrs records syntactic (6)))
```

**Syntax:**
```scheme
(define-record-type (person make-person person?)
  (fields (immutable age person-age)
          (immutable email person-email)
          (immutable address person-address)))
```

**Features:**
- ✅ Immutable fields (when declared as `immutable`)
- ✅ Inheritance support via `(parent parent-type)`
- ✅ Custom construction protocols
- ✅ Sealed and opaque type options
- ✅ Nongenerative UIDs
- ❌ No built-in functional setters

**Example Usage:**
```scheme
;; Simple record
(define-record-type (person make-person person?)
  (fields (immutable age person-age)
          (immutable email person-email)))

;; With auto-generated accessor names
(define-record-type person-record
  (fields age email))
;; Creates: person-record-age, person-record-email accessors

;; With inheritance
(define-record-type (employee make-employee employee?)
  (parent person)
  (fields (immutable salary employee-salary)))
```

**Field Specification Options:**
```scheme
;; Explicit names
(immutable field-name accessor-name)
(mutable field-name accessor-name mutator-name)

;; Auto-generated names (record-name-field-name)
(immutable field-name)
(mutable field-name)  ;; creates accessor and accessor-set!
field-name            ;; immutable with auto-generated accessor
```

**Record Clauses:**
- `(fields field-spec*)` - Define fields
- `(parent parent-name)` - Single inheritance
- `(protocol expression)` - Custom constructor protocol
- `(sealed sealed?)` - Prevent subtyping if true
- `(opaque opaque?)` - Hide record-type info if true
- `(nongenerative [uid])` - Non-generative type with optional UID

**Advantages:**
- Inheritance support
- More control over construction
- Standard R6RS (portable)
- Can be sealed/opaque

**Limitations:**
- More verbose
- No built-in functional setters (must write manually)
- More complex API

**Implementing Functional Setters with R6RS:**
You would need to write your own:
```scheme
(define (set-person-age person new-age)
  (make-person new-age
               (person-email person)
               (person-address person)))
```

---

### 3. Standard SRFI-9 (without GNU extensions)

**Module:**
```scheme
(use-modules (srfi srfi-9))
```

**Syntax:**
```scheme
(define-record-type <person>
  (make-person age email)
  person?
  (age person-age)      ;; immutable (no setter)
  (email person-email set-person-email!))  ;; mutable
```

**Features:**
- ✅ Simple syntax
- ✅ Can omit setter to make field immutable
- ❌ Setters are destructive (mutate in place)
- ❌ No functional setters

**Limitations:**
- Fields without setters are immutable, but...
- Fields WITH setters mutate destructively
- Must manually implement functional updates

---

## Recommendation for weatherdriven

Use **SRFI-9 with GNU extensions** (`(srfi srfi-9 gnu)`):

```scheme
(define-module (cpele weatherdriven models)
  #:use-module (srfi srfi-9 gnu)
  #:export (<weather> make-weather weather?
            weather-temperature set-weather-temperature
            weather-condition set-weather-condition

            <forecast> make-forecast forecast?
            forecast-location set-forecast-location
            forecast-predictions set-forecast-predictions))

(define-immutable-record-type <weather>
  (make-weather temperature condition)
  weather?
  (temperature weather-temperature set-weather-temperature)
  (condition weather-condition set-weather-condition))

(define-immutable-record-type <forecast>
  (make-forecast location predictions)
  forecast?
  (location forecast-location set-forecast-location)
  (predictions forecast-predictions set-forecast-predictions))
```

**Usage in MVU:**
```scheme
;; In update function
(define (update model msg)
  (match msg
    (('update-temperature new-temp)
     (let* ((weather (model-weather model))
            (updated-weather (set-weather-temperature weather new-temp)))
       (set-model-weather model updated-weather)))
    ...))

;; Or with generic set-fields
(set-fields model
  ((model-weather weather-temperature) new-temp))
```

## Additional Tools

### Generic Setters (work with any SRFI-9 record)

Available in `(srfi srfi-9 gnu)`:

```scheme
;; set-field: update one field
(set-field record (accessor-name) new-value)

;; set-fields: update multiple fields efficiently
(set-fields record
  ((accessor1) value1)
  ((accessor2) value2))

;; Nested updates
(set-fields record
  ((outer-accessor inner-accessor) nested-value))
```

### Custom Printers

```scheme
(set-record-type-printer! <weather>
  (lambda (record port)
    (format port "#<weather ~a °C, ~a>"
            (weather-temperature record)
            (weather-condition record))))
```

## References

- Guile Manual: SRFI-9 Records
- Guile Manual: rnrs records syntactic
- Your project: `app.scm` currently uses `(rnrs records syntactic)`
