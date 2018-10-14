(ns gov.nist.mm.util
  (:require [clojure.pprint :refer (pprint)]))

(defn ppp []
  (binding [clojure.pprint/*print-right-margin* 140]
    (pprint *1)))

(defn ppprint [arg]
  (binding [clojure.pprint/*print-right-margin* 140]
    (pprint arg)))
