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

(csv-seq "resources/ml-latest-small/ratings.csv")
