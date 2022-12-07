(use '[clojure.string :only [index-of]])

(defn solver [marker-length input]
  (let [uniq-chars-marker? (fn [s] (-> s set count (= marker-length)))
        marker-last-indexes (range marker-length (inc (count input)))]
    (->> marker-last-indexes
        (map #(subs input (- % marker-length) %))
        (filter uniq-chars-marker?) 
        first
        (index-of input)
        (+ marker-length))))

(def part1-solver (partial solver 4))

(def part2-solver (partial solver 14))
