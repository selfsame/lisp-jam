(ns squirt
  (:import [UnityEngine Physics] 
    ArcadiaState RenderSettings
    ArcadiaBehaviour StartHook UpdateHook OnTriggerEnterHook
    [System.Reflection Assembly
            AssemblyName MemberInfo
            PropertyInfo FieldInfo])
  (:use 
    arcadia.core arcadia.linear 
    hard.core hard.physics
    clojure.pprint)
  (:require clojure.reflect))

(def RECORD (atom {}))

(def uninteresting? #{:hideFlags :hierarchyCapacity :hasChanged :useGUILayout})

(defn prop->map [p]
  (let [[t n] (vec (re-seq #"[^ ]+" (str p)))] 
  {(keyword n) (symbol (str t))}))

(defn- setable-property? [^PropertyInfo p]
  (boolean
    (and
      (not (= "Item" (.Name p)))
      (.CanRead p)
      (.CanWrite p))))

(defn- setable-properties [type-or-typesym]
  (let [^System.MonoType t type-or-typesym
        property->map (var-get #'clojure.reflect/property->map)]
      (filter setable-property?
        (.GetProperties t
          (enum-or BindingFlags/Instance BindingFlags/Public)))))

(defn- setable-field? [^FieldInfo f]
  (boolean (and (.IsPublic f) (not (.IsStatic f)))))

(defn- setable-fields [type-or-typesym]
  (let [^System.Type t type-or-typesym
        field->map (var-get #'clojure.reflect/field->map)]
    (for [^FieldInfo f (.GetFields t)
          :when (setable-field? f)] f)))

(defn- setables [type-or-typesym]
  (let [t type-or-typesym]
    (into {} 
      (mapcat prop->map 
        (setable-fields t)
        (setable-properties t)))))

(defn record-props [t]
  (count (swap! RECORD assoc (symbol (str t)) 
    (into {} (setables t)))))

(defmacro cloner* [t]
  (let [m (dissoc (get @RECORD t) :name)
        gets (map #(symbol (str "." (subs (str (first %)) 1))) m)
        model (gensym 'a)
        base (gensym 'b)]
  `(~'fn [~model ~base]
    ~@(map (fn [g] 
      (list 'set! (list g base) (list g model))) gets))))

(defn edner* [t]
  (record-props (eval t))
  (let [m (get @RECORD t {})
        m (dissoc m :name)
        gets (map #(symbol (str "." (subs (str (first %)) 1))) m)
        model (gensym 'a)]
  (eval 
    `(~'fn [~model]
      ~(into {::type t} 
        (remove (comp uninteresting? first) 
          (map (fn [g k] [k (list g model)]) gets (map first m))))))))

(defn ->graph [o]
  (let [cs (cmpts o UnityEngine.Component)]
    (into {}
      (map #(vector (hash %) ((edner* (symbol (str (type %)))) %)) cs))))

'(pprint (->graph (resource "star")))








(record-props UpdateHook)

(get @RECORD 'ArcadiaState)




'(pprint (filter (comp valuable-type? last)(get @RECORD 'UnityEngine.Transform)))

'(pprint (macroexpand-1 '(cloner* UnityEngine.Light)))

'(pprint (macroexpand-1 '(edner* UnityEngine.Light)))



(defn render-settings [] 
  (object-typed UnityEngine.RenderSettings))

'((edner* UnityEngine.Light) (.* (the sun)>Light))

'(spit (str "Assets/typedata/" t ".edn")  
    (apply str ))'