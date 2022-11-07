(ns sk.lib.module
  (:require [clojure.set :as set]
            [clojure.string :as str])
  (:import (clojure.lang IPersistentMap)))

;; Exceptions

(defmacro ex [message & [ex-data]]
  `(throw (ex-info ~message ~ex-data)))

(defmacro ex-when [condition & rest]
  `(when ~condition (ex ~@rest)))

(defmacro ex-unless [condition & rest]
  `(ex-when (not ~condition) ~@rest))

;; Types

(defprotocol Module
  (perform [_ message value])
  (responds? [_ message]))

(defn module? [x]
  (satisfies? Module x))

(defn -send [this message & [value]]
  (ex-unless this
             "Module is null" {:message message})
  (ex-unless (module? this)
             "Instance is not a Module"
             {:module this})
  (ex-unless (responds? this message)
             (format "Unknown message: %s/%s" (type this) message)
             {:message message
              :module this})
  (perform this message value))

(extend-type IPersistentMap
  Module
  (responds? [_ _]
    false))

(defmacro defmodule [class messages]
  `(extend-type ~class
     Module
     (perform [this# message# value#]
       (when-let [function# (~messages message#)]
         (function# this# value#)))
     (responds? [this# message#]
       (not (nil? (get ~messages message#))))))

(defrecord Assembly [system components config])

(def ^:dynamic active-config
  {})

(defn make
  [{:keys [components config]}]
  (->Assembly (atom nil) components (or config active-config)))

;; Map utilities

(defn mapf
  "Update the values in a map with a 1ary function."
  [f m]
  (->> m
       (map (fn [[k v]] [k (f v)]))
       (into {})))

(defn mkeys
  "Return the keys in map M whose values match predicate P"
  [m p]
  (reduce (fn [s [k v]] (cond-> s (p v) (conj k)))
          nil
          m))

(defn mapify
  "Coerce a set or map valued object to a map. Maps are unchanged,
  sets are converted to maps whose values are identical to their
  keys. For allowing #{:x} as shorthand for {:x :x} when all parameter
  names correspond to parameter value names."
  [value]
  (cond (or (map? value)
            (empty? value)) value
        (set? value) (into {} (map (fn [k] [k k]) value))
        :else
        (ex "Value is not a map or set" {:value value})))

(defn lifecycle
  [{:keys [init] :as props}]
  (let [hook (atom nil)]
    (reify
      Module
      (perform [_ message _]
        (case message
          :start
          (let [f (cond-> init (qualified-symbol? init) requiring-resolve)]
            (reset! hook (f props)))
          :stop
          (do
            (when (fn? @hook)
              (@hook))
            (reset! hook nil))))
      (responds? [_ message]
        (contains? #{:start :stop} message)))))

(defn component-constructor
  [{:keys [make] :or {make identity} :as component}]
  (let [function (cond-> make (qualified-symbol? make) requiring-resolve)]
    (ex-unless (ifn? function) "Illegal constructor" {:component component
                                                      :make make})
    function))

(defn component-properties
  [module k component config]
  (let [not-found (gensym)
        arg-default #(get-in component [:props %] not-found)
        config-required (merge (mapify (:config component))
                               (mapify (set (keys (:props component)))))
        props (merge (mapf #(get (:components module) % not-found)
                           (mapify (:deps component)))
                     (mapf #(get config % (arg-default %))
                           config-required))
        missing (map config-required (mkeys props (partial = not-found)))]
    (ex-unless (empty? missing) "Missing configuration"
               {:component k
                :missing missing})
    props))

(defn build-component
  [module k component config]
  ((component-constructor component)
   (component-properties module k component config)))

;; transitive dependencies and cycle detection

(defn read-cycles
  "Detect cycles in a multigraph beginning with path P. Map M is key -> coll."
  [p m]
  (let [pc (count p)
        [p1 & _] p
        pl (last p)]
    (or (and (< 1 pc)
             (= p1 pl) [p])
        (when (= (count (into #{} p)) pc)
          (mapcat #(read-cycles (conj p %) m) (get m pl))))))

(defn read-cycle
  "Report the shortest cycle in a multigraph."
  [p m]
  (first (sort-by count (read-cycles p m))))

(defn missing-dependencies
  [components]
  (let [dependencies (mapf #(set (vals (mapify (:deps %))))
                           components)]
    (set/difference (apply set/union (vals dependencies))
                    (into #{} (keys dependencies)))))

(defn activation-order
  [components]
  (let [missing (missing-dependencies components)]
    (ex-unless (empty? missing) "Missing dependencies" {:missing missing}))
  (let [dependencies (mapf #(set (vals (mapify (:deps %))))
                           components)
        cycle #(format "[%s]" (str/join ", " (read-cycle [%] dependencies)))]
    (loop [order nil]
      (let [started (set order)
            started? (partial contains? started)
            deps-started? #(every? started? (dependencies %))
            remaining (filter (comp not started?) (keys components))]
        (if (empty? remaining)
          (reverse order)
          (if-let [next (first (filter deps-started? remaining))]
            (recur (conj order next))
            (let [failed (first remaining)]
              (ex "Circular dependencies" {:component failed
                                           :cycle (cycle failed)}))))))))

(defn build
  [components config]
  (let [component-keys (activation-order components)]
    (reduce (fn [module component-key]
              (update module :components assoc component-key
                      (build-component module component-key
                                       (components component-key) config)))
            {:order component-keys}
            component-keys)))

(defn %broadcast
  [{:keys [components order]} message value reverse?]
  (reduce (fn [results component-key]
            (let [component (components component-key)]
              (if (responds? component message)
                (assoc results component-key
                       (-send component message value))
                results)))
          {}
          ((if reverse? reverse identity) order)))

;; Module behavior

(defn -broadcast
  [{:keys [system]} {:keys [message value]}]
  (%broadcast @system message value true))

(defn -get [{:keys [system]} k]
  (let [component (get-in @system [:components k])]
    (ex-unless component (format "Unknown component '%s'" k)
               {:key k})
    component))

(defn -start
  [{:keys [system components config]} _]
  (let [module (build components config)]
    (%broadcast module :start nil false)
    (reset! system module)))

(defn -stop
  [{:keys [system]} _]
  (%broadcast @system :stop nil true)
  (reset! system nil))

(defmodule Assembly
  {:broadcast #'-broadcast
   :get #'-get
   :start #'-start
   :stop #'-stop})

(defmacro with-module [[module components config] & body]
  `(binding [active-config ~config]
     (let [~module (make {:components ~components :config ~config})]
       (try
         (-send ~module :start)
         ~@body
         (finally
           (-send ~module :stop))))))
