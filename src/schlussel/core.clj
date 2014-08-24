(ns schlussel.core
  (:require [incanter.core :refer :all :as ic]
            [incanter.datasets :refer :all as ds]
            [incanter.stats :refer :all :as st]
            [incanter.charts :refer :all :as ch]
            [incanter.io :refer :all :as io]
            [clojure.math.numeric-tower :refer [floor ceil round] :as m]
            [clojure.math.combinatorics :as combo]
            [clj-ml.clusterers :refer :all :as cl]
            [clj-ml.data :refer :all :as cd]
            [clj-ml.distance-functions :refer :all :as dis]))

(def test-data(dataset ["id" "x" "y"]
                       (map vector
                            (->> (range 100)
                                 (map inc))
                            (sample-uniform 100)
                            (sample-uniform 100))))

;; Read data from .csv exported from R
(def data (-> "resources/key.csv"
              (read-dataset :header true)))

;; Display the key-shaped data as a scatter plot
(-> (scatter-plot :x :y :data data)
    view
    )

;; Display the key-shaped x coords in a histogram
(-> (histogram :x :nbins 50 :data data)
    ;;view
    )

;; Filter selection -> X coordinate
($ :x data)

;; ------------- ;;
;; Open covering ;;
;; ------------- ;;

(defn cover-uniform
  "Returns a covering expressed as value pairs in the dimension of the projection image."
  [resolution gain image]
  (let [min-val  (apply min image)
        max-val  (apply max image)
        size     ($= (max-val - min-val) / resolution)
        overlap  ($= size * gain)]
    (->> (range resolution)
         (map (fn [i] [($= min-val + (     i  * size)  - overlap)
                       ($= min-val + ((inc i) * size)  + overlap)])))))

;; check it out
(cover-uniform 10 0.2 (range 100))
(cover-uniform 15 0.8 ($ :x data))

(defn inverse-domain-from-values
  "Calculates the inverse domain from a dataset and a seq of covering value pairs."
  [data proj covering-values]
  (->> covering-values
       (map (fn [[l u]] ($where {proj {:gte l :lte u}} data)))))

;; Test it on the test-data dataset
(def image ($ :x test-data))
(->> (cover-uniform 10 0.2 image)
     (inverse-domain-from-values test-data :x))

(defn combine-uniform
  [dataset proj res gain]
  (->> ($ proj dataset)
       (cover-uniform res gain)
       (inverse-domain-from-values dataset proj)))

; (combine-uniform data :x 10 0.2)

(defn cover-balanced
  "Returns a covering expressed as indices."
  [resolution gain size]
  (let [bin-length ($= size / resolution)
        overlap    ($= bin-length * gain)]
    (->> (range resolution)
         (map (fn [i] [(max 0     (-> ($=       i  * bin-length - overlap) floor round))
                       (min size  (-> ($=  (inc i) * bin-length + overlap) ceil  round))])))))

;; check lengths of bins
(cover-balanced 10 0.2 100)
(->> (cover-balanced 10 0.2 100)
     (map (partial apply range))
     (map count))

(defn inverse-domain-from-indices
  "Calculates the inverse domain from a dataset ordered by the
   projection dimension and a seq of covering index pairs."
  [ordered-data covering-indices]
  (->> covering-indices
       (map (partial apply range))
       (map #($ % :all ordered-data))))

(defn combine-balanced
  [dataset proj res gain]
  (->> (nrow dataset)
       (cover-balanced res gain)
       (inverse-domain-from-indices ($order proj :asc dataset))))

; (combine-balanced data :x 10 0.2)

;; ---------------- ;;
;; Local Clustering ;;
;; ---------------- ;;

;; Distance metric f: Dataset -> Distance matrix (on which attributes? -> X and Y)
;; Do this in parallel with core.async Booyah!

(def features [:x :y])
(def cl-algo :k-means)
(def cl-args {:number-clusters 3
              :preserve-instances-order true})

(defn ml-data [features dataset]
  (->> (sel dataset :cols features)
       (to-vect)
       (make-dataset "tmo" features)))

(-> (make-clusterer cl-algo cl-args)
    type)

(defn do-cluster [cl-algo cl-args ml-data]
  (let [clusterer (make-clusterer cl-algo cl-args)]
    (clusterer-build clusterer ml-data)
    clusterer))

(def kmeans (do-cluster cl-algo cl-args (ml-data features test-data)))

(->> (.getAssignments kmeans)
     seq
     (map-indexed vector)
     (group-by second))

(clusterer-build (make-clusterer cl-algo cl-args) (ml-data features test-data))






























