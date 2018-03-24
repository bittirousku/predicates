(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x))
  )

(defn less-than [n]
  (fn [x] (< x n))
  )

(defn equal-to [n]
  (fn [x] (== x n))
  )

(defn set->predicate [a-set]
  (fn [x] (contains? a-set x))
  )

(defn pred-and [pred1 pred2]
  (fn [x] (and (pred1 x) (pred2 x)))
  )

(defn pred-or [pred1 pred2]
  (fn [x] (or (pred1 x) (pred2 x)))
  )

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (cond
    (= string "") true
    (= string nil) true
    (every? whitespace? string) true
    :else false
    )
  )

(defn has-award? [book award]
  (if (contains? (:awards book) award)
    true
    false
    )
  )

(defn HAS-ALL-THE-AWARDS? [book awards]
  (every? (fn [award] (has-award? book award)) awards)
  )

(defn my-some [pred a-seq]
  (let [results (map pred a-seq)
        truths (filter boolean results)]
    (first truths)
    )
  )

(defn my-every? [pred a-seq]
  (let [results (map pred a-seq)] ;; (true false true true)
    (cond
      (empty? (filter (complement boolean) results)) true
      (empty? a-seq) true
      :else false
      )
    )
  )

(defn prime? [n]
  (let [is-divisible (fn [x] (= (mod n x) 0))]
    (not (some is-divisible (range 2 n)))
    )
  )

;^^
