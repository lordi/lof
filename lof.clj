; Laws of Form in Clojure

; inductive rules
(defn cross [e] [:cross e])
(defn ++ [e f] (cond (nil? e) f (nil? f) e true [e f]))

; atomic expressions
(def empty-cross (cross nil))
(def unmarked-space nil)

; helper functions
(defn cross? [e] (and (vector? e) (= (first e) :cross)))
(defn marked-space [e] (second e))

; apply axioms
(defn simplify [e]
  (cond
    (= e unmarked-space) unmarked-space
    (cross? e) (let [m (simplify (marked-space e))]
                 (if (cross? m) (marked-space m) (cross m)))
    (every? cross? e) (let [m1 (simplify (marked-space (first e)))
                            m2 (simplify (marked-space (second e)))]
                        (if (= m1 m2) (simplify (cross m1))
                          (++ (simplify (cross m1)) (cross m2))))))

;--

(defn simplify-and-print [e] (println e "->" (simplify e)))

(simplify-and-print empty-cross)
(simplify-and-print (cross empty-cross))
(simplify-and-print (++ empty-cross empty-cross))
(simplify-and-print (++ (cross empty-cross) empty-cross))


