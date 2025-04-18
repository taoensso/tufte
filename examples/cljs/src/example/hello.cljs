(ns example.hello
  (:require [taoensso.tufte :as tufte]))

(tufte/add-handler! :my-console-handler (tufte/handler:console))

(defn get-x [] (+ 1 1))
(defn get-y [] (+ 2 2))

(tufte/profile {:level :info, :id ::my-profiling-id}
  (dotimes [_ 5]
    (tufte/p :get-x (get-x))
    (tufte/p :get-y (get-y))))
