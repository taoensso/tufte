(ns taoensso.tufte.timbre
  "Handler for Timbre,
    Ref. <https://www.taoensso.com/timbre>."
  (:require
   [taoensso.encore         :as enc]
   [taoensso.encore.signals :as sigs]
   [taoensso.tufte          :as tufte]
   [taoensso.tufte.impl     :as impl]
   [taoensso.timbre         :as timbre]))

(defn handler:timbre
  "Alpha, subject to change.
  Returns a signal handler that:
    - Takes a Tufte profiling signal (map).
    - Uses Timbre to log the signal as a human-readable string.

  Options:
    `:timbre-level-fn` ---- (fn [tufte-level]) => timbre-level (default to constantly `:info`)
    `:format-pstats-opts` - Opts map provided to `format-pstats` (default nil)
    `:incl-keys` ---------- Subset of profiling signal keys to retain from those
                            otherwise excluded by default: #{:host :thread}"

  ([] (handler:timbre nil))
  ([{:keys [timbre-level-fn format-pstats-opts incl-keys]
     :or   {timbre-level-fn (fn [tufte-level] :info)}}]

   (let [nl enc/newline
         incl-host?   (contains? incl-keys :host)
         incl-thread? (contains? incl-keys :thread)]

     (fn a-handler:timbre [signal]
       (let [{:keys [inst ns coords, id level, #?@(:clj [host thread]), ctx data,
                     pstats format-pstats-fn]} signal]
         (timbre/log!
           {:loc      (when ns (let [[line column] coords] {:ns ns, :line line, :column column}))
            :level    (timbre-level-fn level)
            :instant  #?(:clj (enc/as-dt inst), :cljs inst)
            :msg-type :p
            :vargs
            (let [sb    (enc/str-builder)
                  s+spc (enc/sb-appender sb " ")]

              (s+spc "Tufte signal")
              (when id (s+spc (sigs/format-id ns id)))

              #?(:clj (when   (enc/and? host   incl-host?)   (enc/sb-append sb nl "   host: " (enc/pr-edn* host))))
              #?(:clj (when   (enc/and? thread incl-thread?) (enc/sb-append sb nl " thread: " (enc/pr-edn* thread))))
              (when-let [data (enc/not-empty-coll data)]     (enc/sb-append sb nl "   data: " (enc/pr-edn* data)))
              (when-let [ctx  (enc/not-empty-coll ctx)]      (enc/sb-append sb nl "    ctx: " (enc/pr-edn* ctx)))

              (enc/when-let [ff format-pstats-fn, formatted (ff pstats format-pstats-opts)]
                (enc/sb-append sb nl "<<< pstats <<<" nl formatted ">>> pstats >>>"))

              [(str sb)])}))))))

(comment ((handler:timbre) (assoc (#'tufte/dummy-signal) :data {:k1 :v1})))

(enc/deprecated
  (defn ^:no-doc ^:deprecated add-timbre-logging-handler!
    "Prefer (add-handler! <handler-id> (handler:timbre) <dispatch-opts>)."
    [{:keys [ns-pattern handler-id timbre-level]
      :or   {ns-pattern "*"
             handler-id   :timbre
             timbre-level :info}}]

    (let [timbre-level-fn (if (fn? timbre-level) timbre-level (fn [_] timbre-level))
          handler-fn      (handler:timbre {:timbre-level-fn timbre-level-fn})
          dispatch-opts
          (when    (and ns-pattern (not= ns-pattern "*"))
            {:ns-filter ns-pattern})]

      (tufte/add-handler! handler-id handler-fn dispatch-opts))))
