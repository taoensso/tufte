# Setup

One of Tufte's most valuable use cases is for monitoring the **ongoing performance** of production applications, etc.

The [`pstats`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:pstats-content) objects generated from Tufte's [`profiled`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#profiled) or [`profile`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#help:profile) calls are **~losslessly mergeable** with [`merge-pstats`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#merge-pstats).

This gives you a lot of flexibility re: integrating Tufte into your running applications.

As one example, suppose you have an HTTP application that you'd like to monitor+optimize for response times:

- Wrap each endpoint with a sampling call to `profile`. (E.g. with a Ring middleware).
- Your `profile` handler can accumulate (merge) `pstats` into a buffer.
- Every n minutes, drain the buffer and log endpoint performance to a db.
- Trigger alarms if any performance info (e.g. 95th percentile response times) are out of spec. The accumulated `pstats` info will also be helpful in quickly diagnosing a cause.

[`handler:accumulating`](https://cljdoc.org/d/com.taoensso/tufte/CURRENT/api/taoensso.tufte#handler:accumulating) provides one common/convenient way to do this.

See the [example project](../tree/master/examples/clj) for more info.

# Performance

Tufte's designed specifically to support ongoing use **in production**, and is **highly optimized**. If your code's remotely expensive enough to be worth profiling, then Tufte's overhead will likely be insignificant.

And Tufte's [conditional profiling](./1-Getting-started#conditional-profiling) gives you highly expressive control over what to profile, and when. This is useful both to minimize performance costs, and the production of unnecessary data that may be costly to store or analyze.