(ns schlussel.scratch
  (:require [incanter.core :refer :all :as ic]
            [incanter.datasets :refer :all as ds]
            [incanter.stats :refer :all :as st]
            [incanter.charts :refer :all :as ch]
            [incanter.io :refer :all :as io]
            [clojure.math.numeric-tower :as m]))


;; lx <- runif(nrPoints,min=1,max=5)  ;ly <- rnorm(nrPoints,sd=.2) + 0
(def line-data
  (map vector
       (sample-uniform nr-points :min 1 :max 5)
       (sample-normal nr-points :sd 2)))

;; distance metric

(def points [[0 3]
             [1 4]
             [5 0]
             [4 3]])

(->> (combo/combinations points 2)
     (map (partial apply euclidean-distance)))

(defn distance-matrix [dataset distance-fn]
  :TODO)


;; ---===< Incanter data sets >===---

(-> [7 8 9 3 4 13 0  0]
    sort)

(def iris
  (get-dataset :iris))

(col-names iris)
(nrow iris)

($ :Species iris)


(def cars (get-dataset :cars))
(view cars)
($ :speed cars)

(doto (scatter-plot [1 2 3] [4 5 6])
      view)

(with-data cars
  (def lm (linear-model ($ :dist) ($ :speed)))
  (doto (scatter-plot ($ :speed) ($ :dist))
    view
    ;(add-lines ($ :speed) (:fitted lm))
    ))
