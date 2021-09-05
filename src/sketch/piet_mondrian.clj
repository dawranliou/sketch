(ns sketch.piet-mondrian
  (:require [sketch.core :as core]
            [quil.core :as q]))

(defn split-x [at {:keys [x y w h] :as square}]
  (if (< (core/random) 0.5)
    [square]
    [{:x x :y y :w (- at x) :h h}
     {:x at :y y :w (- (+ x w) at) :h h}]))

(defn split-y [at {:keys [x y w h] :as square}]
  (if (< (core/random) 0.5)
    [square]
    [{:x x :y y :w w :h (- at y)}
     {:x x :y at :w w :h (+ (- h at) y)}]))

(defn split-squares-with [squares {:keys [x y]}]
  (cond
    x (let [{to-split true
             no-split false}
            (group-by #(< (:x %) x (+ (:x %) (:w %))) squares)]
        (concat
          (mapcat (partial split-x x) to-split)
          no-split))
    y (let [{to-split true
             no-split false}
            (group-by #(< (:y %) y (+ (:y %) (:h %))) squares)]
        (concat
          (mapcat (partial split-y y) to-split)
          no-split))))

(defn draw []
  ;; (q/no-loop)
  (q/color-mode :hsb 360 100 100)
  (q/background 105 2 96)
  (q/stroke-weight (core/w 8/320))
  (let [step            (core/w 1/8)
        initial-squares [{:x 0 :y 0 :w core/*size* :h core/*size*}]
        squares         (into []
                              (reduce
                                (fn [squares split]
                                  (-> squares
                                      (split-squares-with {:x split})
                                      (split-squares-with {:y split})))
                                initial-squares
                                (range 0 core/*size* step)))
        colored-squares (-> squares
                            (assoc-in [(core/random-int (count squares)) :c]
                                      :red)
                            (assoc-in [(core/random-int (count squares)) :c]
                                      :blue)
                            (assoc-in [(core/random-int (count squares)) :c]
                                      :yellow))]
    #_
    (def -colored-squares colored-squares)
    (doseq [{:keys [x y w h c]} colored-squares]
      (case c
        :red    (q/fill 353 96 83)
        :blue   (q/fill 212 88 64)
        :yellow (q/fill 50 73 97)
        (q/fill 105 2 96))
      (q/rect x y w h))))

(defonce sketch
  (core/create-sketch {:draw #'draw}))

(comment
  (core/save sketch))
