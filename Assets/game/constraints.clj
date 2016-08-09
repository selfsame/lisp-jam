(ns game.constraints
  (:import ArcadiaState 
    [UnityEngine Ray])
  (:require tween.pool)
  (:use
    arcadia.core arcadia.linear
    tween.core
    hard.core hard.input hard.mesh hard.physics
    pdfn.core
    game.editor))

(tween.pool/def-pool 300 Ray origin direction)

(defn from-to-rotation [from to]
  (UnityEngine.Quaternion/FromToRotation from to))

(defn constrain-to-planet [o]
  (let [o-speed (or (state o :speed) 0.2)
        orbit-height (or (state o :hover-distance) 3.0)
        ray (Ray. (->v3 o) (local-direction o (v3 0 -1 0)))
        hits (ray-hits ray)
        hit (first (filter #(planet? (->go (.collider %))) hits))]
    (!Ray ray)
    (when hit 
      (let [pos-lerp-rat  (+ 0.05 (* 0.5 0.5 0.5 0.5 0.5 o-speed))
            rot-lerp-rat  (+ 0.12 (* 0.5 0.5 0.5 0.5 0.5 o-speed))
            norm-ray      (Ray. (.point hit) (.normal hit))
            orbit-point   (.GetPoint norm-ray orbit-height)
            fwd           (.forward (.transform o))
            ship-rotation (.rotation (.transform o))
            good-quat 
            (Quaternion/LookRotation 
              (v3- fwd 
                   (v3* (.normal hit) 
                        (Vector3/Dot fwd (.normal hit)))) 
              (.normal hit))
            target-rotation (from-to-rotation (Vector3/up) (.normal hit))] 
        (!Ray norm-ray)
        (rotation! o (Quaternion/Lerp ship-rotation good-quat rot-lerp-rat))
        (set! (.position (.transform o)) 
              (lerp o orbit-point pos-lerp-rat)) ))))

