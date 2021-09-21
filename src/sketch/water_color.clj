(ns sketch.water-color
  (:require
   [sketch.core :as sketch]
   [quil.core :as q]
   [quil.middleware :as qm]))

(def seed 1050879010481460)

(defn middle [[x1 y1] [x2 y2]]
  [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)])

(defn distance [[x1 y1] [x2 y2]]
  (q/sqrt (/ (+ (q/sq (- x1 x2)) (q/sq (- y1 y2))) 2)))

(defn guassian-deviate [[x y] variance]
  [(+ x (* variance (q/random-gaussian)))
   (+ y (* variance (q/random-gaussian)))])

(defn polygon-deformation
  [pts]
  (let [generation (:gen (meta pts))]
    (reduce (fn [new-pts c]
              (if (= (last new-pts) c)
                new-pts
                (let [a  (last new-pts)
                      b  (middle a c)
                      d  (distance a c)
                      b' (guassian-deviate b (/ d 2.3))]
                  (conj new-pts a b' c))))
            (with-meta [(last pts)] {:gen (inc generation)})
            pts)))

(defn polygon [x y r n-pts]
  (let [angle (/ q/TWO-PI n-pts)]
    (with-meta
      (mapv
        #(vector (+ x (* r (q/cos %)))
                 (+ y (* r (q/sin %))))
        (range 0 q/TWO-PI angle))
      {:gen 0})))

(defn setup []
  (q/no-loop)
  (q/color-mode :hsb 360 100 100 1.0))

(defn fill-polygon [pts]
  (q/begin-shape)
  (doseq [[x y] pts]
    (q/vertex x y))
  (q/end-shape :close))

(defn draw []
  (q/background 360)
  (q/no-loop)
  (q/no-stroke)
  (q/random-seed seed)
  (q/with-translation [320 320]
    (let [pts  (polygon 0 0 200 10)
          base (-> (iterate polygon-deformation pts)
                   (nth 4))]
      (def -polygon polygon)
      (def -base base)
      (count -base)

      ;; Check the base polygon shape
      #_#_
      (q/fill 360 80 100 1)
      (fill-polygon base)

      (q/fill 360 100 100 0.04)
      (dotimes [n 30]
        (let [pts' (-> (iterate polygon-deformation base)
                       (nth 3))]
          (fill-polygon pts'))))))


#_
(.loop sketch)


(def sketch
  (q/sketch
    :size [640 640]
    :title "Sketch"
    :setup #'setup
    :draw #'draw
    :middleware [qm/pause-on-error]))


(comment

  (require '[quil.applet :as qa])

  (qa/with-applet sketch
    #_(guassian-deviate [100 100])
    (polygon-deformation -pts))

  (qa/with-applet sketch
    (q/save (format "img-%s-%s-%s.png"
                    (System/currentTimeMillis)
                    seed
                    (q/width))))

  )
