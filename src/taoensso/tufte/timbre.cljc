(ns taoensso.tufte.timbre
  "Tufte -> Timbre handler,
  Ref. <https://www.taoensso.com/timbre>."
  (:require
   [taoensso.encore         :as enc]
   [taoensso.encore.signals :as sigs]
   [taoensso.tufte          :as tufte]
   [taoensso.tufte.impl     :as impl]
   [taoensso.timbre         :as timbre]))

(defn handler:timbre
  "Alpha, subject to change!
  Returns a signal handler that:
    - Takes a Tufte profiling signal (map).
    - Uses Timbre to log the signal as a human-readable string.

  Options:
    `:level-fn` ----------- (fn [tufte-level]) => timbre-level (default `identity`).
    `:format-pstats-opts` - Opts map provided to `format-pstats` (default nil)"

  ([] (handler:timbre nil))
  ([{:keys [level-fn format-pstats-opts]
     :or   {level-fn identity}}]

   (fn a-handler:timbre [signal]
     (let [{:keys [inst ns coords, id level, #?@(:clj [host thread]), ctx data,
                   pstats format-pstats-fn]} signal]

       (timbre/log!
         {:loc      {:ns ns, :line (get coords 0)}
          :level    (level-fn level)
          :instant  #?(:clj (enc/as-dt inst), :cljs inst)
          :msg-type :p
          :tufte/pstats pstats ; -> log data map
          :vargs
          (let [nl  enc/newline
                sb (enc/str-builder)]

            (do      (enc/sb-append sb "Tufte pstats"))
            (when id (enc/sb-append sb " " (sigs/format-id ns id)))

            (when-let   [ff  format-pstats-fn]
              (when-let [fs (ff pstats (conj {:incl-newline? false} format-pstats-opts))]
                (enc/sb-append sb nl fs)))

            (when-let [data (enc/not-empty-coll data)] (enc/sb-append sb nl " data: " (enc/pr-edn* data)))
            (when-let [ctx  (enc/not-empty-coll ctx)]  (enc/sb-append sb nl "  ctx: " (enc/pr-edn* ctx)))

            [(str sb)])})))))

(comment
  ((handler:timbre)        (#'tufte/dummy-signal))
  ((handler:timbre) (assoc (#'tufte/dummy-signal) :data {:k1 :v1} :ctx {:k1 :v1})))

(enc/deprecated
  (defn ^:no-doc ^:deprecated add-timbre-logging-handler!
    "Prefer (add-handler! <handler-id> (handler:timbre) <dispatch-opts>)."
    [{:keys [ns-pattern handler-id timbre-level]
      :or   {ns-pattern   "*"
             handler-id   :timbre
             timbre-level :info}}]

    (let [timbre-level-fn (if (fn? timbre-level) timbre-level (fn [_] timbre-level))
          handler-fn      (handler:timbre {:level-fn timbre-level-fn})
          dispatch-opts
          (when    (and ns-pattern (not= ns-pattern "*"))
            {:ns-filter ns-pattern})]

      (tufte/add-handler! handler-id handler-fn dispatch-opts))))
