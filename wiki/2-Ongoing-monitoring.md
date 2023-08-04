# Setup

One of Tufte's most valuable use cases is for monitoring the **ongoing performance** of production applications, etc.

The `pstats` objects generated from Tufte's `profiled` or `profile` calls are **~losslessly mergeable**.

This gives you a lot of flexibility re: integrating Tufte into your running applications.

As one example, suppose you have an HTTP application that you'd like to monitor+optimize for response times:

- Wrap each endpoint with a sampling call to `profile`. (E.g. with a Ring middleware).
- Your `profile` handler can accumulate (merge) `pstats` into a buffer.
- Every n minutes, drain the buffer and log endpoint performance to a db.
- Trigger alarms if any performance info (e.g. 95th percentile response times) are out of spec. The accumulated `pstats` info will also be helpful in quickly diagnosing a cause.

[`tufte/add-accumulating-handler!`](https://taoensso.github.io/tufte/taoensso.tufte.html#var-add-accumulating-handler.21) provides one common/convenient way to do this.

See the [example project](../tree/master/examples/clj) for more info.

# Performance

Tufte's designed specifically to support ongoing use in production, and is **highly optimized**: its overhead is on the order of a couple nanoseconds per wrapping.

If something's remotely worth profiling, Tufte's overhead should be completely insignificant.

Also, keep in mind that Tufte's [conditional profiling](./1-Getting-started#conditional-profiling) gives you complete control over if and when you do pay (however little) for profiling.