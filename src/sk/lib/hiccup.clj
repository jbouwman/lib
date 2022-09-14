(ns sk.lib.hiccup
  (:require [clojure.java.io :as io]))

(defn walk [hiccup & [path]]
  (letfn [(mapcat-indexed [f hiccup]
            (apply concat (map-indexed f hiccup)))
          (walk-map [i [k hiccup]]
            (walk hiccup (conj (or path []) k)))
          (walk-coll [i hiccup]
            (walk hiccup (conj (or path []) i)))]
    (cond (map? hiccup)
          (mapcat-indexed walk-map hiccup)
          (coll? hiccup)
          (mapcat-indexed walk-coll hiccup)
          :else
          [[path hiccup]])))

(defn prefix? [path suffix]
  (when (= suffix (drop (- (count path) (count suffix)) path))
    (take (- (count path) (count suffix)) path)))

(defn find-records [rule data]
  (->> data
       (mapcat (fn [[path value]]
                 (reduce (fn [m [k suffix]]
                           (if-let [prefix (prefix? path suffix)]
                             (conj m [prefix k value])
                             m))
                         nil
                         rule)))
       (group-by first)
       vals
       (map (fn [record]
              (reduce (fn [m [_ k v]]
                        (assoc m k v))
                      {}
                      record)))
       (filter (fn [record]
                 (= (keys record) (keys rule))))))

(defn scrape [hiccup rule]
  (find-records rule hiccup))
