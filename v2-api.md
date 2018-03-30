# Tufte v2 API

> This document describes the Tufte v1->v2 API changes

### tl;dr

The v2 API introduces a number of small but non-trivial **breaking API changes**.


### Motivations for API change

##### Extra flexibility

Before: Tufte provided results in the form of computed statistics maps. These could be merged, but merging was often expensive and/or lossy.

Now: Tufte provides results in the form of PStats objects that can be **cheaply and ~losslessly merged**, and **derefed** when/where a final computed statistics map is desired.

> The extra flexibility is particularly handy for advanced use cases like profiling across threads/systems, and/or over extended periods of time (e.g. in ongoing monitoring scenarios).

##### Percentiles

By delaying the computation of computed statistics until when/where desired, the possibility is opened to also compute more expensive statistics like **percentiles**.

The computational **cost is paid only on deref**, and can be paid on a separate thread/pool/etc.


### Migration guide

- If you use only `profile` with an out-the-box (println or timbre) handler, there are **no breaking changes for you**.

- If you use `profile` with any custom handlers, you'll need to update your handler fns:

  - Map keys given to handler fns have changed (See **2** below).
  - Stats map keys (as returned by @pstats) have changed (See **4** below).

- If you use `profiled`, see **1, 3, 4** below.


### Major changes (may be breaking)

1. `profiled` used to return `[<result> <?stats-map>]`, but will now return `[<result> <?pstats-obj>]`.

2. `profile` handlers used to receive keys: `:stats`, `:stats-str_`, but will now receive keys: `:pstats`, `:pstats-str_`.

3. Utils were renamed: `merge-stats` ->  `merge-pstats`, `format-stats` -> `format-pstats`.

4. The new (derefed) stats maps keys have changed: `#{:count :min :max :mean :mad :time}` -> `#{:n :min :max :mean :mad :sum :p50 :p90 :p95 :p99}`

### Other minor changes (usu. non-breaking)

1. Experimental accumulator utils were removed. The functionality they attempted to provide is now better provided natively by the new PStats objects.

2. Thread-local profiling and related utils were removed.

> Optimizations to the dynamic profiling code made dynamic profiling reasonably competitive with thread-local code. So as a simplification, the thread-local stuff was removed.
>
> Effectively, this means that `profiled` and `profile` calls now always behave as the older version would with `{:dynamic? true}` opts.
