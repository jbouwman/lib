(ns sk.lib.hiccup-test
  (:require [clojure.java.io :as io]
            [clojure.test :as t]
            [hickory.core :as hickory]
            [sk.lib.hiccup :as hiccup]))

;; Copied from https://www.wikidata.org/wiki/Wikidata:Database_reports/List_of_properties/all

(def wikidata-html
  "sk/lib/hiccup_test.html")

(def wikidata-property
  #"^P[0-9]+$")

;; Where to find ID and label fields, relative to a row element

(def wikidata-row
  {:id [3 2 2] :label [5 2]})

(def reader->walked
  (comp hiccup/walk hickory/as-hiccup hickory/parse slurp))

(t/deftest hiccup-test

  (t/is (= '([[:a] 1] [[:b] 2])
           (hiccup/walk {:a 1 :b 2} nil)))

  (t/is (= '([[7 :a] 1] [[7 :b] 2])
           (hiccup/walk {:a 1 :b 2} [7])))

  (let [data (reader->walked (io/resource wikidata-html))
        records (hiccup/scrape data wikidata-row)]
    (t/is (= 10280 (count records)))
    (t/is (every? #(re-find wikidata-property (:id %)) records))))


