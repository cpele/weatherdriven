# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

weatherdriven is a weather application server written in GNU Guile Scheme. It implements a Model-View-Update (MVU) architecture pattern for managing application state and effects.

## Running the Application

The application uses GNU Guile Scheme. To start the server:

```scheme
guile -L . -l main.scm
```

Then evaluate `(start-server)` in the REPL to start the HTTP server on localhost:8080.

To stop the server: `(stop-server)`
To restart: `(restart-server)`

## Architecture

### MVU (Model-View-Update) Pattern

The application follows an MVU architecture defined in `(cpele weatherdriven app)`:

- **Model**: Application state stored as an association list containing the forecast model
- **View**: Converts the model to JSON for HTTP responses (`view` function in app.scm:44)
- **Update**: Processes messages to update the model (`update` function in app.scm:55)
- **Effects**: Side effects like fetching weather data, managed through an effects registry in main.scm:16

### Key Components

- `app.scm`: Contains MVU functions (init, view, update) and data structures
  - Record types for weather predictions using `rnrs records syntactic`
  - `init` function returns initial model and effect to run
  - `update` uses pattern matching to handle different message types

- `main.scm`: Server runtime and effects management
  - HTTP server on port 8080 using `(web server)`
  - Thread-based server lifecycle (start/stop/restart)
  - Effects registry that maps effect IDs to effect functions
  - Request handler that catches exceptions and returns JSON or error text

### Module System

The project uses Guile modules:
- Main module: `(cpele weatherdriven app)`
- Load path includes current directory (use `-L .` flag)
- Dependencies are in `dependencies/guile-json/` for JSON support

### Data Structures

- Association lists (alists) are used for model representation
- Record types defined for weather and prediction data
- Pattern matching with `(ice-9 match)` for message handling

## Development Patterns

### Module Loading

When running Guile, always include the load path flag:
```scheme
guile -L . -l main.scm
```

### Testing Functions

Test functions are defined inline (e.g., `test-update` in app.scm:63). Run them by loading the module and evaluating the test function in the REPL.

### Effect Management

Effects are registered in the `effects` alist in main.scm:16. Each effect:
- Takes a `dispatch` function parameter
- Calls `dispatch` with messages to trigger model updates
- Is invoked by the runtime when returned from `init` or `update`

### Date Handling

The codebase uses `(srfi srfi-19)` for date operations. Dates are formatted as strings for the model.
- Enable plan mode by default