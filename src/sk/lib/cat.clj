(ns sk.lib.cat)

;;
;; Categories describe what can be said of classes.
;;

(defn is [cats k]
  (update cats k #(or % {:key k})))

(defn sconj [s? x]
  (conj (or s? #{}) x))

(defn isa [cats k k']
  (-> (is (is cats k) k')
      (update-in [k :kinds] sconj k')
      (update-in [k' :kind] sconj k)))

(defn isa? [cats k k']
  (or (= k k')
      (let [kind (get-in cats [k :kind] #{})]
        (or (contains? kind k')
            (some #(isa? cats % k') kind)))))

(defn make
  ([cats [k & kinds]]
   (reduce #(-> (make % %2)
                (isa k (first %2)))
           (is cats k) kinds))
  ([defs]
   (make nil defs)))
