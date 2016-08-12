(ns user
  (:import [UnityEngine Physics] ArcadiaState
    RenderSettings)
  (:use 
    arcadia.core arcadia.linear 
    hard.core hard.physics)
  (:require [arcadia.introspection :as intro]))

(def RECORD (atom {}))

(def valuable-type? 
  (set (map (comp symbol str) [
    UnityEngine.Color
    System.Boolean
    System.String
    System.Int32
    System.Single
    UnityEngine.Vector3
    UnityEngine.Quaternion])))

(defn record-props [t]
  (count (get t 
    (swap! RECORD assoc (symbol (str t)) 
    (into {} 
    (mapv 
      #(let [[t n] (vec (re-seq #"[^ ]+" (str %)))] 
        (if true #_(value-type-filter t)
            {(keyword n) (symbol (str t))} {})) 
      (intro/properties t)))))))

(defmacro cloner* [t]
  (let [m (filter (comp valuable-type? last) (get @RECORD t))
        gets (map #(symbol (str "." (subs (str (first %)) 1))) m)
        model (gensym 'a)
        base (gensym 'b)]
  `(~'fn [~model ~base]
    ~@(map (fn [g] 
      (list 'set! (list g base) (list g model))) gets))))

(defmacro edner* [t]
  (let [m (filter (comp valuable-type? last) (get @RECORD t))
        gets (map #(symbol (str "." (subs (str (first %)) 1))) m)
        model (gensym 'a)]
  `(~'fn [~model]
    ~(into {:T t} 
      (map (fn [g k] {k (list g model)}) gets (map first m))))))

'(mapv record-props [
  UnityEngine.Transform
  UnityEngine.Camera
  UnityEngine.Rigidbody
  UnityEngine.SphereCollider
  UnityEngine.TextMesh
  UnityEngine.Light
  UnityEngine.MeshRenderer
  UnityEngine.BoxCollider
  ArcadiaState
  UnityEngine.RenderSettings
  ])

'(pprint (macroexpand-1 '(cloner* UnityEngine.Camera)))

'(pprint (macroexpand-1 '(edner* UnityEngine.Light)))



(defn render-settings [] 
  (object-typed UnityEngine.RenderSettings))

'((edner* UnityEngine.Light) (.* (the sun)>Light))

'(spit (str "Assets/typedata/" t ".edn")  
    (apply str ))'