(ns filmania.main
  (:use [filmania.core])
  (:use [clojure.string :only  (split)])
  (:gen-class))



(defn -main
  [& args]
  (println "Afficher le nombre de films dans la base : y/n");; question 1 partie 1
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (println (count movies)))
  (println "Afficher le nombre de films de science-fiction : y/n");; question 1 partie 1
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (println (count (filter #(contains? (get (nth % 1) :genres) "Sci-Fi") movies))))
  (println "Afficher le nombre de films de romance : y/n");; question 1 partie 1
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (println (count (filter #(contains? (get (nth % 1) :genres) "Romance") movies))))
  (println "Afficher tous les genres de films de la base : y/n");; question 2 partie 1
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (println (all-genres movies)))
  (println "Afficher tous les films d'un certain genre: y/n");; question 2 partie 1
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (do
      (println "Veuillez choisir un genre de film")
      (let [genre (split (read-line) #"\n")
            res (films-by-genre (first genre) movies)]
      (println res)
      (println "Total :"(count res)"films"))))
  (println "Afficher les genres de film avec leur cardinalité: y/n");; question 3 partie 1
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (println (card-genres movies)))
  (println "Afficher le genre le plus représenté: y/n");; question 3 partie 1
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (println (filter #(= (nth % 1) (reduce max (vals (card-genres movies)))) (card-genres movies))))
  (println "Afficher le genre le moins représenté: y/n");; question 3 partie 1
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (println (filter #(= (nth % 1) (reduce min (vals (card-genres movies)))) (card-genres movies))))
  (println "Afficher les 10 premières évaluations des utilisateurs: y/n");;question 1 partie 2
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (do
      (println "Format : [idFilm note]")
      (println (take 10 (get ratings 1)))))
  (println "Afficher les films les mieux notés: y/n");;question 2 partie 2
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (do
      (let [res (take-best-rated (/ (reduce + (vals average-ratings)) (count average-ratings)) (sort-by val > average-ratings))]
      (println res)
      (println "Total :"(count res)"films"))))
  (println "Afficher les films les moins bien notés: y/n");;question 2 partie 2
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (do
      (let [res (take-worst-rated (/ (reduce + (vals average-ratings)) (count average-ratings)) (sort-by val < average-ratings))]
      (println res)
      (println "Total :"(count res)"films"))))
  (println "Afficher la moyenne de la base de film: y/n");;question 2 partie 2
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (println (/ (reduce + (vals average-ratings)) (count average-ratings))))
  (println "Afficher les utilisateurs les plus sympatiques: y/n");;question 3 partie 2
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (let [res (take-friendly-users (/ (reduce + (vals average-ratings)) (count average-ratings)) (sort-by val > user-ratings))]
      (println res)
      (println "Total :"(count res)"utilisateurs")))
  (println "Afficher les utilisateurs les plus critiques: y/n");;question 3 partie 2
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (let [res (take-crit-users (/ (reduce + (vals average-ratings)) (count average-ratings)) (sort-by val < user-ratings))]
      (println res)
      (println "Total :"(count res)"utilisateurs")))
  (println "Afficher les films de science-fiction ayant la meilleur note: y/n");;question 3 partie 2
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (do
      (println "Format : [idFilm note]")
      (let [l (sort-by val > (filter #(contains? Sci-Fi (key %)) average-ratings))
            bestRate (second (first l))
            res (take-while #(= bestRate (nth % 1)) l)]
        (println res)
        (println "Total :"(count res)"films de science-fiction"))))
  (println "Afficher les films de science-fiction ayant la moins bonne note: y/n");;question 3 partie 2
  (if (= (nth (split (read-line) #"\n") 0) "y")
    (do
      (println "Format : [idFilm note]")
      (let [l (sort-by val < (filter #(contains? Sci-Fi (key %)) average-ratings))
            worstRate (second (first l))
            res (take-while #(= worstRate (nth % 1)) l)]
        (println res)
        (println "Total :"(count res)"films de science-fiction"))))
  )

