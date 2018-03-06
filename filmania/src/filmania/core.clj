(ns filmania.core
  (require [clojure.data.csv :as csv]
           [clojure.java.io :as io]
           [clojure.set :as s]))


(defn csv-seq
  "Retourne une séquence à partir d'un fichier CSV."
  [filename]
  (with-open [in-file (io/reader filename)]
    (doall
     (csv/read-csv in-file))))



(defn parse-movie
  "Construit un enregistrement de film depuis un entrée lue depuis CSV."
  [title-year genres]
  (let [r (re-seq #"(.+) \((\d\d\d\d)\)$" title-year)
        title (get (first r) 1)]
    (try
      (let [year (Integer/parseInt (get (first r) 2))]
        {:title title
         :year year
         :genres (set (filter #(not= % "(no genres listed)") (clojure.string/split genres #"\|")))})
      (catch Exception _ nil))))

(defn movie-map
  "Construit une map de films à partir d'un base en CSV."
  [csv]
  (reduce (fn [m [movie-id title-year genres]]
            (if-let [movie (parse-movie title-year genres)]
              (assoc m (Integer/parseInt movie-id) movie)
              m))
          {} csv))

;; Attention: gros fichier
(def movie-filename "resources/ml-latest-small/movies.csv")

(def movies (movie-map (rest (csv-seq movie-filename))))

(count movies)

(take 10 movies)

(count (filter #(contains? (get (nth % 1) :genres) "Sci-Fi") movies))

(count (filter #(contains? (get (nth % 1) :genres) "Romance") movies))

(defn all-genres [base]
  (if (seq base)
    (reduce s/union #{} (map #(get (nth % 1) :genres) base))
    #{}))

(defn films-by-genre [genre base]
  (if (seq base)
    (filter #(contains? (get (nth % 1) :genres) genre) base)))


(defn card-genres [base]
  (if (seq base)
    (into {} (map #(vec [% (count (films-by-genre % base))]) (all-genres base)))))

(filter #(= (nth % 1) (reduce max (vals (card-genres movies)))) (card-genres movies))

(filter #(= (nth % 1) (reduce min (vals (card-genres movies)))) (card-genres movies))

(defn get-map-id [s id]
  (loop [s s id id res (sorted-map)]
    (if (seq s)
      (if (= id (first (first s)))
        (recur (rest s) id (assoc res (Integer/parseInt (nth (first s) 1)) (Double/parseDouble (nth (first s) 2))))
        res)
      res)))


(defn parse-ratings [s]
  (if (seq s)
    (into (sorted-map) (lazy-seq (let [id (first (first s)) m (get-map-id s id)] (cons [(Integer/parseInt id) m] (parse-ratings (nthrest s (count m)))))))))



(def ratings (parse-ratings (rest (csv-seq "resources/ml-latest-small/ratings.csv"))))

(take 10 (get ratings 1))

(s/union (into #{} (keys movies)) (reduce s/union #{} (map #(into #{} (keys (val %))) ratings)))

(defn movie-avg-ratings []
  (let [s (into #{} (keys movies)) m
    (loop [tmp s res {}]
      (if (seq tmp)
        (recur (rest tmp) (assoc res (first tmp) [0 0]))
        res))]
    (loop [tmp ratings res m]
      (if (seq tmp)
          (recur (rest tmp)
                 (loop [rates (val (first tmp)) r res]
                    (if (seq rates)
                      (if (contains? s (key (first rates)))
                        (recur (rest rates) (assoc r (key (first rates)) [(+ (val (first rates)) (nth (get r (key (first rates))) 0)) (inc (nth (get r (key (first rates))) 1))]))
                        (recur (rest rates) r))
                      r)))
        res))))

(movie-avg-ratings)


