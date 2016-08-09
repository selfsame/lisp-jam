(ns user
  (:import [UnityEngine Physics] ArcadiaState)
  (:use 
    arcadia.core arcadia.linear 
    hard.core hard.physics
    clojure.pprint)
  (:require [arcadia.introspection :as intro]))

(defn save-properties [t]
  (spit (str "types/" t ".edn")  
    (apply str (map #(str % "\n") 
      (intro/properties t)))) true)

'(mapv save-properties [
  UnityEngine.Transform
  UnityEngine.Camera
  UnityEngine.Rigidbody
  UnityEngine.SphereCollider
  UnityEngine.TextMesh
  UnityEngine.Light
  UnityEngine.MeshRenderer
  UnityEngine.BoxCollider
  ArcadiaState])