(require '[clojure.data.json :as json])

(defn interleave-elements [left-vec right-vec]
  (->> (interleave left-vec right-vec)
      (partition 2)))

(defn compare-elements [left right]
  "Returns :in-order or :out-of-order if conclusive result found, else nil to continue processing"
  (cond
    (every? integer? [left right]) 
    (cond
      (< left right) :in-order
      (> left right) :out-of-order
      :else nil)
    (every? vector? [left right]) 
    (let [conclusive-comparison (->> (interleave-elements left right)
                                     (map (partial apply compare-elements))
                                     (filter (comp not nil?))
                                     first)]
      (cond
        (not (nil? conclusive-comparison)) conclusive-comparison
        (< (count left) (count right)) :in-order
        (> (count left) (count right)) :out-of-order
        :else nil))
    (integer? left) (compare-elements [left] right)
    (integer? right) (compare-elements left [right])))

(defn read-input [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (doall (line-seq rdr))))

(defn input->pairs [input-seq]
  (->> input-seq
       (partition-by empty?)
       (filter #(not= % '("")))
       (map #(map json/read-str %))))

(defn part1-solver [input-filename]
  (->> (read-input input-filename)
      input->pairs
      (map (partial apply compare-elements))
      (keep-indexed #(when (= :in-order %2) (inc %1)))
      (apply +)))
