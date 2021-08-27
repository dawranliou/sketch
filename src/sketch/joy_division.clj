(ns sketch.joy-division
  "A Quil sketch based on generative artistry's tutorial - Joy Division.
  See original post at https://generativeartistry.com/tutorials/joy-division/"
  (:require [quil.core :as q]
            [quil.applet :as qa]))

(def ^:dynamic *size* 640)

(defn w
  ([] (w 1.0))
  ([scale] (int (* *size* scale))))

(def ^:dynamic *random* clojure.core/rand)

(defn random
  ([] (random 1.0))
  ([n] (* (*random*) n)))

(defn setup []
  (q/frame-rate 1)
  (q/smooth)
  (q/color-mode :hsb 360 100 100 1.0))

(defn draw-line [line]
  (q/stroke-weight (w (/ 1 320)))
  (q/stroke-cap :square)
  (q/begin-shape)
  (apply q/vertex (first line))
  (let [pairs                 (partition 2 1 line)
        [[xn-1 yn-1] [xn yn]] (last pairs)]
    (doseq [[[x1 y1] [x2 y2]] (butlast pairs)
            :let              [cx (/ (+ x1 x2) 2)
                               cy (/ (+ y1 y2) 2)]]
      (q/quadratic-vertex x1 y1 cx cy))
    (q/quadratic-vertex xn-1 yn-1 xn yn))
  (q/end-shape))

(defn deviate-y [[x y]]
  (let [distance-to-center (Math/abs (- x (w 0.5)))
        variance           (max 0 (- (w 0.5) (w 0.156) distance-to-center))
        d                  (* (random) variance -0.5)]
    [x (+ y d)]))

(defn joy-division []
  (let [n     32
        step  (w (/ 1 n))
        lines (for [i (range (dec n))]
                (for [j (range (dec n))]
                  [(* (inc j) step) (* (inc i) step)]))]
    (doseq [line (drop (inc (/ n 10)) lines)]
      (->> line
           (into [] (map deviate-y))
           draw-line))))

(def ^:dynamic *seed* 1050879010481460)

(defn draw [size]
  (binding [*size*   size
            *random* (partial q/random 1.0)]
    (q/background 360)
    (q/random-seed *seed*)
    (joy-division)))

(defn save-and-quit [options]
  (let [draw (:draw options (fn []))
        wrap (fn []
               (draw)
               (q/no-loop)
               (q/save
                 (format "img-%s-%s-%s.png"
                         (System/currentTimeMillis)
                         *seed*
                         (q/width)))
               (q/exit))]
    (assoc options :draw wrap)))

(defn create-sketch
  ([] (create-sketch {}))
  ([{:keys [size once?]
     :or   {size  *size* ; 640px
            once? false}}]
   (q/sketch :title "Sketch"
             :size [size size]
             :setup setup
             :draw (partial draw size)
             :middleware (when once? [save-and-quit]))))

(defonce sketch (create-sketch))


(comment

  ;; Generate a new sketch
  (def sketch (create-sketch))

  ;; Save working image to disk
  (qa/with-applet sketch
    (q/save (format "img-%s-%s.png" (System/currentTimeMillis) *seed*)))

  ;; Generate a production image (10800 x 10800 pixels)
  (create-sketch {:size  10800
                  :once? true})
  )
