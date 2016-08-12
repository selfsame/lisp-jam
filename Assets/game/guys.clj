(ns game.guys
  (:import ArcadiaState 
    [UnityEngine Ray])
  (:use
    arcadia.core arcadia.linear
    tween.core
    hard.core hard.input hard.physics hard.mesh
    pdfn.core
    game.constraints game.editor))

(defpdfn spawn)
(pdfn spawn [o ^:eye s]
  (let [eye (clone! :eye (v3+ (->v3 o) (local-direction o (v3 0 3 0))))]
    (set! (.rotation (.transform eye)) (rotation o))
    (state eye {
      :eye true 
      :hp 60 :max-hp 60
      :alert 0
      :target nil})))





(defn init [o]
  (let [s (state o)
        n (.name o)]
    (spawn o s)))

(def radar-v3s '(vertices (resource "icosphere42")))

(defn ball-update [o]
  (when (rand-nth [nil nil nil true])
    (let [

      pos (->v3 o)
      hits 
      (remove nil? 
        (for [v radar-v3s
              :let [ray (Ray. pos v)
                    hit (planet-hit ray)]] hit))
      points (map (prop* point) hits)
      rel-points (map #(v3- % pos ) points)
      inverse-points (map 
        #(v3* (.normalized %) 
              (pow2  (/ 8 (+ 1 (Mathf/Log (.magnitude %)))))) rel-points)
      
      gravity (v3div (reduce v3+ inverse-points) (max 1 (count points)))]
      (set-state! o :gravity gravity)))
    (do 
      (let [body (->rigidbody o)
            gfv3 (clamp-v3 (v3* (state o :gravity) 20) -1000 1000)]
      (global-force! body  (.x gfv3)(.y gfv3)(.z gfv3))
      (torque! body  0 0 60))))

(defn ball-gizmos [o]
  (let [pos (->v3 o)]
    (gizmo-color (color 1 0 0))
    (gizmo-ray pos (v3* (state o :gravity) 10))))

(defn ball-test [v]
  (let [ball (clone! :ball v)]
    (state! ball {
      :ball true 
      :gravity (v3 0)
      :body true
      :hp 200})
    (hook+ ball :update #'game.guys/ball-update)
    ;(hook+ ball :on-draw-gizmos #'game.core/ball-gizmos)
    ball))

#_(defn init-ball [o]
  (let [sc (?f 2.0 8.0)
        ball (ball-test (v3+ (->v3 o) (local-direction o (->v3 0 (* sc 0.5) 0))))]
    (local-scale! ball (v3 sc))
    (rotate! ball (rotation o))) )

'(state! (the eye-spawn) 
  {:eye-spawn true})


