(ns sketch.circle-packging
  (:require [sketch.core :as core]
            [quil.core :as q]
            [quil.middleware :as qm]))

(defn collide? [{:keys [x y r]} circles]
  (some (fn [{x1 :x y1 :y r1 :r}]
          (>= (+ r1 r)
              (Math/sqrt
                (+ (* (- x1 x) (- x1 x))
                   (* (- y1 y) (- y1 y))))))
        circles))

(defn draw []
  (q/background 360)
  (q/no-fill)
  (q/stroke-weight (core/w 1/320))
  (let [min-radius    (core/w 1/320)
        max-radius    (core/w 50/320)
        total-circles 500
        max-attempts  500
        circle        (fn []
                        {:x (core/random (core/w 1))
                         :y (core/random (core/w 1))
                         :r min-radius})
        circles (loop [circles     []
                       attempt     0
                       safe-point? false
                       new-circle  (circle)]
                  (let [{:keys [r]} new-circle]
                    (cond
                      ;; Exit loop
                      (= total-circles (count circles)) circles
                      (= attempt max-attempts)          circles

                      ;; When a safe point found, try to pack a bigger circle
                      (and safe-point?
                           (<= r max-radius)
                           (not (collide? new-circle circles)))
                      (recur circles
                             attempt
                             true
                             (assoc new-circle :r (inc r)))
                      safe-point?
                      (recur (conj circles (assoc new-circle :r (dec r)))
                             0
                             false
                             (circle))

                      ;; Found a safe point to draw a circle
                      (not (collide? new-circle circles))
                      (recur circles
                             attempt
                             true
                             new-circle)
                      :else
                      (recur circles
                             (inc attempt)
                             false
                             (circle)))))]
    ;; Draw all the found circles
    (doseq [{:keys [x y r]} circles
            :let            [d (* 2 r)]]
      (q/arc x y d d 0 q/TWO-PI))))

(defonce sketch
  (core/create-sketch {:draw #'draw}))

(comment
  (core/save sketch)
  )
