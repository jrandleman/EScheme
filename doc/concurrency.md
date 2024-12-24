<!-- concurrency.md -->

# Concurrency in EScheme

### Describes EScheme's concurrency semantics!

## Concurrency System Overview

EScheme supports parallelism via true Java threads!

Its core literal syntax uses immutable data structures (including lists
and strings, in a departure from standard Scheme), however, objects
generated from classes can be mutated (meaning one could create mutable
lists via the object system!). Note that vectors and hashmaps are mutable
too.

In addition to Java threads, EScheme also supports JavaScript's `Promise`
concurrency paradigm. For synchronization, EScheme exposes Java reentrant
locks via `mutex`, and supports Clojure's `dosync` macro!

---

## Using Concurrent Primitives

- Spawn threads via the `thread` procedure.
- Create promises via the `promise` procedure.
- Get a reentrant lock via the `mutex` procedure.
- Check out the `help` function's `Procedures > Concurrency` section for more!

---

## Threading and Continuations

- Threads each have their own stack, and hence their own set of continuations.
- Continuations are delimited by the execution of the thread that created them.
  - Hence a child thread's continuation will never 'continue' back into the parent
    thread, rather, it will terminate where the child thread terminated.

---

## Threading and Dynamic Environments

- Each thread has a so-called 'dynamic environment', wherein a set of variable
  bindings is kept globally within the thread while being hidden from other threads.
  - Mutate a thread's dynamic environment via the `thread-define`, `thread-set!`,
    and `thread-get` macros!
- After querying for a variable that doesn't exist in a thread's dynamic environment,
  the 'meta thread''s dynamic environment is checked. If an entry is found in the
  meta thread's dynamic environment, a local copy is cached into the current thread's
  dynamic environment and the querying operation continues.
- This allows for us to have 'environment-global-thread-local' variables! State can
  be shared & operated upon by many procedures without having to lock, since each
  thread only ever operates on a local copy of the state!
  - This is used by `dynamic-wind` in order to maintain thread-local winds!
