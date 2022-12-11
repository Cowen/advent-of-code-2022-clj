(require '[clojure.string :as string])

(defn addx-get-x [addx-str] 
  (-> addx-str 
      (clojure.string/split #"\s+") 
      second 
      Integer/parseInt))

(defn run-instruction [register-vals instruction]
  (let [last-val (last register-vals)]
    (if (= instruction "noop")
        (conj register-vals last-val)
        (let [next-val (+ last-val (addx-get-x instruction))]
          (conj register-vals last-val next-val)))))

(defn run-all-instructions [instructions]
  (->> instructions
       (reduce run-instruction [1]))) ;; Register X always initializes with value 1

(defn part1-solver [input-instructions]
  (let [important-cycles #{20 60 100 140 180 220}
        cycle-val-indexes (->> important-cycles (map dec) set)]
    (->> (run-all-instructions input-instructions)
        (keep-indexed #(when (cycle-val-indexes %1) [(inc %1) %2]))
        (map #(apply * %))
        (reduce +))))

(defn read-instructions [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (line-seq rdr))))
