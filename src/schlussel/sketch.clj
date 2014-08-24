(ns schlussel.sketch
  (:require [c2.core :refer [unify]]
            [c2.scale :as scale]))

;; https://www.youtube.com/watch?feature=player_detailpage&v=T83P3PVSy_8#t=510s

(let [width 500, bar-height 20
      data {"A" 1, "B" 2, "C" 4, "D" 3}
      s (scale/linear :domain [0 (apply max (vals data))]
                      :range [0 width])]
  [:div#bars
   (unify data (fn [[label val]]
                 [:div {:style {:height bar-height
                                :width (s val)
                                :background-color "gray"}}
                  [:span {:style {:color "white"}} label]]))])
