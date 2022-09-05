(ns sk.lib.cat-test
  (:require [clojure.test :as t]
            [sk.lib.cat :as cat]))

;; https://plato.stanford.edu/entries/aristotle-categories/#TenFolDiv

(def aristotelian-cat-tree
  [:substance
   [:immobile]
   [:mobile  
    [:eternal]
    [:destructible  
     [:unensouled]
     [:ensouled  
      [:nonperceptive]
      [:perceptive  
       [:irrational]
       [:rational  
        [:cat]]]]]]])

(t/deftest cat-test
  (let [cats (cat/make aristotelian-cat-tree)]
    (t/is (= 12 (count (keys cats))))
    (t/is (cat/isa? cats :rational :ensouled))
    (t/is (not (cat/isa? cats :cat :immobile)))
    (t/is (cat/isa? cats :cat :cat))))
