# Record Types: Final Analysis for Immutable Records with Functional Setters

## The Problem

You want:
1. ✅ Immutable record types
2. ✅ Functional setters (return new record with updated field)
3. ✅ **Automatic boilerplate generation** (no explicit accessor/setter names)

## What Each System Provides

### SRFI-9 GNU (`(srfi srfi-9 gnu)`)

**What it gives you:**
- ✅ Immutable records via `define-immutable-record-type`
- ✅ Functional setters
- ❌ **Must explicitly name ALL accessors and setters**

```scheme
(use-modules (srfi srfi-9 gnu))

(define-immutable-record-type <person>
  (make-person name age email)
  person?
  (name person-name set-person-name)        ;; Must specify ALL names
  (age person-age set-person-age)            ;; No auto-generation
  (email person-email set-person-email))     ;; Repetitive!
```

**Boilerplate level:** HIGH - every field needs 3 names specified

---

### R6RS Records (`(rnrs records syntactic)`)

**What it gives you:**
- ✅ Immutable fields (when declared as `(immutable field)`)
- ✅ **Automatic accessor name generation** (record-name-field-name)
- ✅ **Automatic constructor generation**
- ✅ **Automatic predicate generation**
- ❌ **No functional setters at all**

```scheme
(use-modules (rnrs records syntactic))

;; Minimal syntax - maximum auto-generation
(define-record-type person
  (fields name age email))

;; Auto-generates:
;; - make-person constructor
;; - person? predicate
;; - person-name, person-age, person-email accessors

;; All fields are immutable by default (just field-name)
```

**Boilerplate level:** LOW - just list field names

**Problem:** No functional setters - you must manually write:
```scheme
(define (set-person-age p new-age)
  (make-person (person-name p)
               new-age
               (person-email p)))
```

---

## The Solution Options

### Option 1: Use R6RS + Manual Functional Setters

**When to use:** Small number of fields, want clean type definitions

```scheme
(use-modules (rnrs records syntactic))

(define-record-type person
  (fields name age email))

;; Write functional setters manually
(define (set-person-age p new-age)
  (make-person (person-name p) new-age (person-email p)))

(define (set-person-name p new-name)
  (make-person new-name (person-age p) (person-email p)))

(define (set-person-email p new-email)
  (make-person (person-name p) (person-age p) new-email))
```

**Pros:**
- Clean, minimal record definition
- Auto-generated accessors
- Full control over setters

**Cons:**
- Must write setter for each field manually
- Error-prone with many fields

---

### Option 2: Use R6RS + Generic set-fields Macro

**When to use:** Can accept generic update syntax, don't need named setters

```scheme
(use-modules (rnrs records syntactic)
             (srfi srfi-9 gnu))  ;; For set-field/set-fields macros

(define-record-type person
  (fields name age email))

;; Use generic setters (these work with R6RS records!)
(define p (make-person "Alice" 30 "alice@example.com"))

(define p2 (set-field p (person-age) 31))
(define p3 (set-fields p
             ((person-age) 31)
             ((person-name) "Alicia")))
```

**Wait, does this work?** Let me test...

---

### Option 3: Macro to Generate Functional Setters

**When to use:** Want both auto-generation AND named functional setters

Write a macro that wraps R6RS records and generates functional setters:

```scheme
(define-syntax define-immutable-record
  (syntax-rules ()
    ((define-immutable-record type-name (field ...))
     (begin
       ;; Define the R6RS record
       (define-record-type type-name
         (fields field ...))

       ;; Generate functional setters
       (define (set-type-name-field rec new-val)
         (make-type-name
           (if (eq? 'field 'field) new-val (type-name-field rec))
           ...))
       ...))))
```

This is complex and may not be worth it.

---

### Option 4: Accept SRFI-9 GNU Boilerplate

**When to use:** Want both features, accept explicit naming

Just bite the bullet and write out all the names:

```scheme
(use-modules (srfi srfi-9 gnu))

(define-immutable-record-type <person>
  (make-person name age email)
  person?
  (name person-name set-person-name)
  (age person-age set-person-age)
  (email person-email set-person-email))
```

**Pros:**
- Both immutability AND functional setters
- Clear and explicit
- Official Guile recommendation

**Cons:**
- Repetitive for types with many fields

---

## Testing Generic set-field with R6RS

Let me verify if SRFI-9 GNU's `set-field` works with R6RS records:

```scheme
(use-modules (rnrs records syntactic)
             (srfi srfi-9 gnu))

(define-record-type person
  (fields name age))

(define p (make-person "Alice" 30))

;; Does this work?
(set-field p (person-age) 31)
```

If this works, then **Option 2** is the best solution!

---

## Recommendation

**Priority ranking:**

1. **Try Option 2 first** - R6RS + generic `set-field`/`set-fields`
   - IF the generic macros work with R6RS records, this is ideal
   - Minimal boilerplate + functional updates

2. **Fall back to Option 1** - R6RS + manual setters
   - Write setter helpers as needed
   - Clean type definitions

3. **Last resort: Option 4** - SRFI-9 GNU with full boilerplate
   - If you really need named functional setters for every field

---

## Current Status in Your Project

Your `app.scm` uses:
```scheme
(use-modules (rnrs records syntactic))

(define-record-type weather
  (fields temperature clouds precipitation severe-conditions))

(define-record-type prediction
  (fields weather issue-date effect-date probability))
```

This gives you:
- ✅ Auto-generated accessors (e.g., `weather-temperature`)
- ✅ Auto-generated constructors (e.g., `make-weather`)
- ✅ Immutable fields
- ❌ No functional setters

**Next step:** Test if `(srfi srfi-9 gnu)` generic `set-field` works with your R6RS records!
