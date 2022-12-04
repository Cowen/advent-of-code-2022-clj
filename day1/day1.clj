(require '[clojure.string :as str])

(defn raw-elf->calorie-total [^String raw-elf]
  (let [str-calories (str/split raw-elf #"\n")
       calories (mapv #(Integer/parseInt %) str-calories)]
  (reduce + calories)))

(defn raw-input->calorie-totals [raw-input]
  (let [raw-elves (str/split raw-input #"\n\n")]
        (mapv raw-elf->calorie-total raw-elves)))

(defn solution
  ([] (solution "data.txt"))
  ([input-file] (let [raw-input (slurp input-file)]
                  (->> raw-input
                       raw-input->calorie-totals
                       (reduce max)))))

(defn part2
  ([] (part2 "data.txt"))
  ([input-file] (let [raw-input (slurp input-file)]
                  (->> raw-input
                      raw-input->calorie-totals
                      sort
                      reverse
                      (take 3)
                      (reduce +)))))
