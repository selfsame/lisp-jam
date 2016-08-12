(ns game.constraints
  (:import ArcadiaState 
    [UnityEngine Ray Physics RaycastHit]
    [Collections])
  (:require tween.pool)
  (:use
    arcadia.core arcadia.linear
    tween.core
    hard.core hard.input hard.mesh hard.physics
    pdfn.core
    game.editor
    clojure.pprint))


(tween.pool/def-pool 300 Ray origin direction)

(def hit-buff (make-array UnityEngine.RaycastHit 40))

(defn raycast-non-alloc [
  ^Vector3 origin 
  ^Vector3 direction 
  ^|UnityEngine.RaycastHit[]| results]
  (Physics/RaycastNonAlloc origin direction results) )





(defn from-to-rotation [from to]
  (UnityEngine.Quaternion/FromToRotation from to))

(defn constrain-to-planet [^UnityEngine.GameObject o]
  (let [o-speed  (or (state o :speed) 0.2)
        orbit-height (or (state o :hover-distance) 3.0)
        hit-count (raycast-non-alloc (->v3 o) (local-direction o (v3 0 -1 0)) hit-buff)
        hit (if (pos? hit-count) (aget hit-buff 0))]

    (when hit 
      (let [pos-lerp-rat  (+ 0.05 (* 0.5 0.5 0.5 0.5 0.5 o-speed))
            rot-lerp-rat   (+ 0.09 (* 0.5 0.5 0.5 0.5 0.5 o-speed))
            norm-ray      (Ray. (.point hit) (.normal hit))
                          ;(v3+ (.point hit) (v3* (.normal hit) orbit-height))
            orbit-point   (.GetPoint norm-ray orbit-height)
            fwd           (.forward (.transform o))
            ship-rotation (.rotation (.transform o))
            good-quat 
            (Quaternion/LookRotation 
              (v3- fwd 
                   (v3* (.normal hit) 
                        (Vector3/Dot fwd (.normal hit)))) 
              (.normal hit))] 
        (rotation! o (Quaternion/Lerp ship-rotation good-quat rot-lerp-rat))
        (set! (.position (.transform o)) 
              (lerp o orbit-point pos-lerp-rat)) ))))



'[(tween.pool/stats <>Ray)]
