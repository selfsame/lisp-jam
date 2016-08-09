(ns game.core
  (:import ArcadiaState)
  (:use
    arcadia.core
    arcadia.linear
    tween.core
    hard.core
    hard.input
    hard.mesh
    hard.physics
    pdfn.core
    clojure.pprint))


(def LEVELS 
  (atom (try (read-string (slurp "levels.edn")) 
             (catch Exception e {}))))

(def ASSETS (atom [:ball-spawn :tree-spawn :obstacle]))

(def edit-obj (atom :ball-spawn))

(declare fire-bullet make-level impact)

(def status (atom "dev"))

(defn status! 
  ([] (status! @status))
  ([s] (when-let [ui (the status)] 
    (set! (.* ui>Text.text) (str "status: " (reset! status s))))))


(def orbit-height 2.0)
(def speed        0.5)
(def bullet-speed 2.0)


(deftween [:trail-renderer :time] [this]
  {:base (.GetComponent this UnityEngine.TrailRenderer)
   :get (.time this)
   :tag System.Single})





(defn from-to-rotation [from to]
  (UnityEngine.Quaternion/FromToRotation from to))

(defn set-rotation! [a b] 
  (set! (.rotation (.transform a)) (.rotation (.transform b))))

(defn planet? [o] (re-find #"planet" (.name o)))

(defn ->data-haver [o]
  (when o 
    (try 
      (or ({{} nil} (state o) o)
          (->data-haver (parent o)))
      (catch Exception e nil))))






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

(defn planet-hit [ray]
  (let [hits (ray-hits ray)]
    (first (filter #(planet? (->go (.collider %))) hits))))

(defn update-mouse [o]
  (cond 
    (key-down? "escape") (make-level)
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

(defn mouse-gizmos [o]
  (when-let [hit (planet-hit (mouse-ray))]
    (gizmo-color (color 1 0 1))
    (gizmo-point (.point hit) 1.0)
    (gizmo-ray (.point hit) (v3* (.normal hit) 3))))













(defn ship-keys [o]
  (if (key? "a") (rotate! o (v3 0 -1.8 0)))
  (if (key? "d") (rotate! o (v3 0 1.8 0)))
  (if (key? "w") (update-state! o :speed #(min (state o :max-speed) (+ % 0.1))))
  (if (key? "s") (update-state! o :speed #(max 0 (- % 0.1)))))


(defn constrain-to-planet [o]
  (let [o-speed (or (state o :speed) 0.2)
        ray (Ray. (->v3 o) (local-direction o (v3 0 -1 0)))
        hits (ray-hits ray)
        hit (first (filter #(planet? (->go (.collider %))) hits))]
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
        (set! (.rotation (.transform o)) 
              (Quaternion/Lerp ship-rotation good-quat rot-lerp-rat))
        (set! (.position (.transform o)) 
              (lerp o orbit-point pos-lerp-rat)) ))))  

(defn move [o]
  (let [sp       (or (state o :speed) speed)
        last-pos (or (state o :last-pos) (->v3 o))
        desired  (v3+ (->v3 o) (local-direction o (v3 sp 0 0)))
        crv-mod  (- sp (.magnitude (v3- desired last-pos)))
        fixed    (v3+ (->v3 o) (local-direction o (v3 (+ sp crv-mod) 0 0)))]
    (position! o fixed)
    #_(if (= "ship" (.name o)) 
      (log (- sp (.magnitude (v3- desired last-pos)))))
    (set-state! o :last-pos (->v3 o))))

(defn update-ship [o]
  (constrain-to-planet o)
  (ship-keys o)
  (move o))

(defn update-bullet [o]
  (when-not (state o :disabled)
    (constrain-to-planet o)
    (move o)
    (let [heading (local-direction o (v3 1 0 0))
          ray (Ray. (->v3 o) heading)
          hits (filter #(cmpt (gobj (.collider %)) ArcadiaState) (ray-hits ray (state o :speed)))]
       (mapv
        #(impact o (state o) (.collider %) (state (.collider %)) %) hits))))

(defn splode 
  ([p] (splode p 1))
  ([p bigness]
    (let [base (clone! :empty (->v3 p))]
      (dorun (for [i (range (inc (or bigness 5)))
                   :let [rp (?sphere 2.0)
                         fire (clone! :fire)]]
        (do 
          (parent! fire base)
          (position! fire (->v3 base))
          (set! (.rotation (.transform fire)) (?rotation))
          (set! (.localScale (.transform fire)) (v3 0.2))
          (timeline* 
            (tween {
              :local {:scale (v3 2.0 2.0 2.0)
                      :position rp}
              :material {:color (color 1 1 1 0.3)}} fire 0.3 :pow3)
            ;(wait (?f 0 0.2))
            (tween {:local {
              :scale (v3 0)}}     fire  0.5 :pow3)))))
      (timeline* (wait 1.0) #(do (destroy! base) nil))
      base)))

(defpdfn die)
(pdfn die [a as] 
  (timeline* 
    (tween {:local {:scale (v3 0)}} a 0.5)
    #(destroy! a)))
(pdfn die [a ^:ship as] (make-level))


(defpdfn trigger-dispatch)
(pdfn trigger-dispatch [a as b bs] (log [(.name a) (.name b)]))
(pdfn trigger-dispatch [a ^:damage as b ^:hp bs]
  (splode a (int (* 0.1 (state a :damage))))
  (update-state! b :hp #(- % (state a :damage)))
  (destroy! a)
  (if (neg? (state b :hp))
      (die b bs)))

(pdfn trigger-dispatch [a ^:damage as b ^:body bs])

(pdfn trigger-dispatch [a ^:ship as b ^:obstacle bs]
  (make-level))

(defn on-trigger [o c]
  (let [other (.gameObject c)
        [a b] (mapv ->data-haver [o other])]
    (trigger-dispatch a (state a) b (state b))))

(defpdfn impact)
(pdfn impact [a as b bs h] nil)
(pdfn impact [a ^:force as b ^:body bs h]
  (let [body (->rigidbody b)
        fv (v3* (v3- (->v3 b) (.point h)) 100) 
        fx (splode a (int (* 0.1 (state a :damage))))]
        (position! fx (.point h))
        (parent! fx b)
        (position! (parent! a fx) (.point h))
  (global-force! body (.x fv) (.y fv) (.z fv))
  (set-state! a :disabled true)
  (destroy! (children a))
  (timeline*
    (tween {:trail-renderer {:time 0.0}} a 0.5))))


;(set-state! (the obstacle) :obstacle true)

(defn fire-bullet [o]
  (let [side (:gun-toggle (update-state! o :gun-toggle #(* -1 %)))
        bullet (clone! :bullet (v3+ (->v3 o) (local-direction o (v3 3 0 (* 1.0 side)))))]
    (set! (.rotation (.transform bullet)) (.rotation (.transform o)))
    (state! bullet {
      :bullet true
      :speed bullet-speed
      :damage 20
      :force true})
    (hook+ bullet :update #'game.core/update-bullet)
    (hook+ (first (children bullet)) :on-trigger-enter #'game.core/on-trigger)
    (timeline*
      (wait 10.0)
      (tween {:local {:scale (v3 0 0 0)}
              :trail-renderer {:time 0.0}} bullet 0.6)
      #(destroy! bullet))))

(defn update-hud [o]
  (let [health-rect-tform (cmpt (the health-bar) UnityEngine.RectTransform)]
    (set! (.sizeDelta health-rect-tform)  (v2 (state o :hp) 17))))

(defn update-camera [o]
  (let [target (the cam-target)]
    (position! o (lerp (->v3 o) (->v3 target) 0.2))
    (set! (.rotation (.transform o)) 
          (Quaternion/Lerp (.rotation (.transform o)) 
                           (.rotation (.transform target)) 
                           0.15))))



(defn populate-level []
  (status!)
  (let [planet (the #"planet.*")
        pk (keyword (.name planet))
        level (get @LEVELS pk)]
    (mapv 
      (fn [o]
        (set! 
          (.rotation (.transform 
            (parent! (clone! (:type o) (:position o)) planet))) 
          (:rotation o)))
      level)
    nil))


(defn make-level []
  (clear-cloned!)
  (clone! :sun1)
  (clone! :sun2)
  (clone! :EventSystem)
  (clone! :Canvas)
  (let [p (clone! :planet2)
        spawn (first (children p))
        s (clone! :ship (->v3 spawn))
        cam (clone! :camera)] 
  (set-rotation! s spawn)
  (state! s {
    :speed 0.5
    :max-speed 1.0
    :gun-toggle 1
    :ship true
    :hp 200
    :max-hp 200})
  (timeline* (wait 0.0) #(populate-level))
  (timeline* :loop 
    #(if (key? "space")
         (do (fire-bullet s) false)
         true)
    (wait 0.2))
  (hook+ s :update #'game.core/update-ship)
  (hook+ s :update #'game.core/update-hud)
  (hook+ cam :update #'game.core/update-camera)
  (hook+ cam :on-draw-gizmos #'game.core/mouse-gizmos)
  (hook+ cam :update #'game.core/update-mouse)
  (hook+ s :on-trigger-enter #'game.core/on-trigger)))









(status! "alt gravity")

(make-level)

(defn inverse-hit-point+ [a b]
  (.point b))

(def radar-v3s
  (vertices (resource "icosphere42")))


(defn ball-update [o]
  (let [pos (->v3 o)
    hits 
    (vec (remove nil? 
        (for [v radar-v3s]
          (planet-hit (Ray. pos v)))))
    points (map (prop* point) hits)
    rel-points (map #(v3- % pos ) points)
    inverse-points (map 
      #(v3* (.normalized %) 
            (pow2  (/ 8 (+ 1 (Mathf/Log (.magnitude %)))))) rel-points)
    
    gravity (v3div (reduce v3+ inverse-points) (max 1 (count points))) ]
    (set-state! o :gravity gravity)
    (apply global-force! (cons (->rigidbody o)  (->vec (clamp-v3 (v3* gravity 10) -1000 1000))))
    (torque! (->rigidbody o) 0 0 60 )))

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
    (hook+ ball :update #'game.core/ball-update)
    ;(hook+ ball :on-draw-gizmos #'game.core/ball-gizmos)
    ball))

(defn init-ball [o]
  (let [sc (?f 2.0 8.0)
        ball (ball-test (v3+ (->v3 o) (local-direction o (->v3 0 (* sc 0.5) 0))))]
    (local-scale! ball (v3 sc))
    (set! (.rotation (.transform ball)) (.rotation (.transform o)))))

(defn init-spawn [o]
  (let [obj (clone! (or (state o :obj) :empty) (->v3 o))
        sc (or (?f (or (state o :scale-min) 1) (or (state o :scale-max) 1)))]
    (set! (.rotation (.transform obj)) (.rotation (.transform o)))
    (if (state o :scale-min)
      (local-scale! obj (?f (or (state o :scale-min) 1) (or (state o :scale-max) 1))))
    (if (state o :rand-y)
      (rotate! obj (v3 0 (?f 360) 0)))))

'(make-level)

'(state! (the tree-spawn) {
  :obj :tree
  :scale-min 0.8
  :scale-max 1.5
  :rand-y true}) 
'(hook+ (the tree-spawn) :start #'game.core/init-spawn)
'(clear-cloned!)

;TODO
'([x] camera system)
'([x] collisions
  ([x] collision dispatch pdfn)
  ([x] generic :hp :bullet method))
'([/] spawn points
  ([x] levels.edn data for planets)
  ([x] in game editor)
  ([ ] cycle spawn type))
'([ ] fix planetoid constraints
  ([ ] step algo based on speed))
'([ ] obstacles)
'([ ] enemies)
'([ ] menu)
'([/] HUD
  ([x] lifebar)
  ([ ] :score :level-time :enemies-left))
'([ ] sfx
  ([ ] game events)
  ([ ] music))