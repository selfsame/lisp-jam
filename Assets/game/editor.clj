(ns game.editor
  (:import ArcadiaState)
  (:use
    arcadia.core arcadia.linear
    tween.core
    hard.core hard.input hard.mesh hard.physics
    pdfn.core
    clojure.pprint))

(def LEVELS 
  (atom (try (read-string (slurp "levels.edn")) 
             (catch Exception e {}))))

(def ASSETS (atom [:ball-spawn :tree-spawn :eye-spawn :rocket-spawn]))


(def status (atom "dev"))

(defn status! 
  ([] (status! @status))
  ([s] (when-let [ui (the status)] 
    (set! (.* ui>Text.text) (str "status: " (reset! status s))))))

(defn planet? [o] (re-find #"planet" (.name o)))

(defn planet-hit [ray]
  (let [hits (ray-hits ray)]
    (first (filter #(planet? (->go (.collider %))) hits))))

(defn save-planet []
  (let [planet (the #"planet.*")
        pk (keyword (.name planet))]
    (spit "levels.edn" 
      (prn-str
        (swap! LEVELS assoc pk
          (mapv 
            (fn [o] {
              :type (keyword (.name o))
              :position (->v3 o)
              :rotation (.* o>Transform.rotation)})
            (remove 
              (comp #{"spawn"} (prop* name))
              (children planet))))))
    (log ['saved pk (count (get @LEVELS pk))])))


(defn editor-update [o]
  (cond 
    (key-down? "tab") (do (swap! ASSETS (comp flatten (juxt rest first))) (status! (str (first @ASSETS))))
    (key-down? "p") (save-planet))
  (when (mouse-down?) 
    (when-let [hit (planet-hit (mouse-ray))]
      (let [planet (->go (.collider hit))
            spawn (clone! (first @ASSETS) (.point hit))] 
        (set! 
          (.rotation (.transform spawn)) 
          (Quaternion/LookRotation (.normal hit)))
        (rotate! spawn (v3 90 0 0))
        (parent! spawn planet)))))

(defn editor-gizmos [o]
  (when-let [hit (planet-hit (mouse-ray))]
    (gizmo-color (color 1 0 1))
    (gizmo-point (.point hit) 1.0)
    (gizmo-ray (.point hit) (v3* (.normal hit) 3))))