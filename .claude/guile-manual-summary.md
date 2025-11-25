# Guile 3.0.10 Reference Manual Summary

This document summarizes the GNU Guile Reference Manual available at `/opt/homebrew/share/info/guile.info*`.

## Manual Structure

The manual is split across 12 info files covering:

### Core Language Features
- **Scheme Basics**: Data types, procedures, expressions, closures, lexical scope
- **Data Types**: Booleans, numbers, characters, strings, symbols, keywords, pairs, lists, vectors, arrays, bytevectors
- **Procedures**: Lambda, optional arguments, higher-order functions, macros
- **Control Flow**: Conditionals, loops, continuations, exceptions, prompts

### Advanced Features
- **Module System**: Creating and using modules, R6RS libraries, declarative modules
- **Object System (GOOPS)**: Classes, instances, methods, generic functions, metaobject protocol
- **Foreign Function Interface**: Calling C libraries, foreign pointers, types, and structs
- **Concurrency**: Threads, mutexes, condition variables, atomics, futures, parallel forms

### I/O and Data Processing
- **Ports**: Binary/textual I/O, file ports, string ports, custom ports, encoding
- **Regular Expressions**: Pattern matching with regex
- **Parsing**: LALR(1) and PEG parsing
- **XML/SXML**: Reading, writing, and transforming XML using SXML representation

### System Programming
- **POSIX Interface**: File system, processes, signals, networking, terminals
- **Web Support**: URIs, HTTP client/server, headers, requests, responses
- **Internationalization**: Text collation, character case mapping, gettext support

### VM and Compilation
- **Virtual Machine**: Stack layout, instruction set, bytecode format, JIT compilation
- **Compiler Tower**: Tree-IL, CPS (Continuation-Passing Style), bytecode generation
- **Debugging**: Stack traces, traps, code coverage, GDB support

### Standard Libraries
- **SRFI Support**: Multiple SRFI implementations (1, 4, 9, 18, 19, 26, 27, 41, 69, 171, etc.)
- **R6RS/R7RS Support**: Compatibility with R6RS and R7RS standards
- **Utility Libraries**: Pattern matching, pretty printing, formatted output, queues, streams

## Key Sections by Topic

### For MVU Architecture (weatherdriven project)
- **Association Lists**: Section on alist operations (Dictionary Types)
- **Records**: SRFI-9 Records and R6RS records for data structures
- **Pattern Matching**: Using `(ice-9 match)` module
- **HTTP Server**: Web server implementation in (web server)
- **JSON**: Handling JSON (via dependencies/guile-json/)
- **Date/Time**: SRFI-19 for date operations

### Module Loading
- **Load Paths**: Section on configuring module search paths
- **Module System**: Creating and using modules with `define-module`
- **Character Encoding**: Handling source file encodings

### Error Handling
- **Exceptions**: Exception objects, raising/handling, throw/catch
- **Error Reporting**: Standard error handling procedures
- **Debugging**: Interactive debugging, stack traces

## Accessing Specific Topics

To look up specific topics in the manual:
```bash
# View full manual
info guile

# Search for specific topic
info guile "topic name"

# Common useful sections
info guile "Association Lists"
info guile "SRFI-9 Records"
info guile "Pattern Matching"
info guile "Web Server"
info guile "Exceptions"
```

## File Locations
- Main index: `/opt/homebrew/share/info/guile.info`
- Content files: `/opt/homebrew/share/info/guile.info-{1..11}`
- Source: `/opt/homebrew/Cellar/guile/3.0.10/share/info/`

## Version
GNU Guile 3.0.10 (September 25, 2024)

## License
GNU Free Documentation License, Version 1.3
