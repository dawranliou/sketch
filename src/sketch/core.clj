(ns sketch.core
  (:require [quil.core :as q]
            [quil.middleware :as qm]
            [quil.applet :as qa]))

(def ^:dynamic *size* 640)
(def ^:dynamic *seed* 1050879010481460)

(defn w [n]
  (* *size* n))

(defn random
  ([] (random 1.0))
  ([n] (* (q/random 1.0) n)))

;; Middlewares

(defn use-bindings [options]
  (let [draw (:draw options (fn []))]
    (assoc options :draw (fn []
                           (binding [*size* (q/width)]
                             (draw))))))

(defn use-seed [options]
  (let [draw (:draw options (fn []))]
    (assoc options :draw (fn []
                           (q/random-seed *seed*)
                           (draw)))))

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

(defn setup []
  (q/frame-rate 1)
  (q/smooth)
  (q/stroke-weight (w (/ 1 320))))

(defn create-sketch
  [{:keys [size once? draw]
    :or   {size *size*}}]
  (q/sketch :title "Sketch"
            :size [size size]
            :setup setup
            :draw draw
            :middleware (cond->> [qm/pause-on-error
                                  use-bindings
                                  use-seed]
                          once? (into [save-and-quit]))))

;; Save working image to disk
(defn save [sketch]
  (qa/with-applet sketch
    (q/save (format "img-%s-%s-%s.png"
                    (System/currentTimeMillis)
                    *seed*
                    (q/width)))))
