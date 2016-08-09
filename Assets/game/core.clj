(ns game.core
  (:import ArcadiaState 
    [UnityEngine Ray])
  (:require tween.pool)
  (:use
    arcadia.core arcadia.linear
    tween.core
    hard.core hard.input hard.mesh hard.physics
    pdfn.core
    game.editor
    game.constraints))

(declare fire-bullet make-level impact)

(def orbit-height 2.0)
(def speed        0.5)
(def bullet-speed 2.0)
  
(deftween [:trail-renderer :time] [this]
  {:base (.GetComponent this UnityEngine.TrailRenderer)
   :get (.time this)
   :tag System.Single})

(defn ->data-haver [o]
  (when o 
    (try (or ({{} nil} (state o) o)
             (->data-haver (parent o)))
         (catch Exception e nil))))





(defn ship-keys [o]
  (if (key-down? "escape") (make-level))
  (if (key? "a") (rotate! o (v3 0 -1.8 0)))
  (if (key? "d") (rotate! o (v3 0 1.8 0)))
  (if (key? "w") (update-state! o :speed #(min (state o :max-speed) (+ % 0.1))))
  (if (key? "s") (update-state! o :speed #(max 0 (- % 0.1)))))




(defn move [o]
  (let [sp       (or (state o :speed) speed)
        last-pos (or (state o :last-pos) (->v3 o))
        desired  (v3+ (->v3 o) (local-direction o (v3 sp 0 0)))
        crv-mod  (- sp (.magnitude (v3- desired last-pos)))
        fixed    (v3+ (->v3 o) (local-direction o (v3 (+ sp crv-mod) 0 0)))]
    (position! o fixed)
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
          (rotation! fire (?rotation))
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




(defn fire-bullet [o]
  (let [side (:gun-toggle (update-state! o :gun-toggle #(* -1 %)))
        bullet (clone! :bullet (v3+ (->v3 o) (local-direction o (v3 3 0 (* 1.0 side)))))]
    (set! (.rotation (.transform bullet)) (.rotation (.transform o)))
    (state! bullet {
      :bullet true
      :speed bullet-speed
      :damage 20
      :force true
      :hover-distance 3.0})
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
    (rotation! o (Quaternion/Lerp (rotation o) (rotation target) 0.15))))



(defn populate-level []
  (let [planet (the #"planet.*")
        pk (keyword (.name planet))
        level (get @LEVELS pk)]
    (dorun (map 
      (fn [o]
        (rotation! (parent! (clone! (:type o) (:position o)) planet) (:rotation o))) 
      level)) nil))


(defn make-level []
  (clear-cloned!)
  (clone! :sun)
  (clone! :EventSystem)
  (clone! :Canvas)
  (let [p (clone! :planet2)
        spawn (first (children p))
        s (clone! :ship (->v3 spawn))
        cam (clone! :camera)] 
  (rotation! s (rotation spawn))
  (state! s {
    :ship true
    :speed 0.5
    :max-speed 1.0
    :hover-distance 3.0
    :gun-toggle 1
    :hp 200
    :max-hp 200})
  (timeline* (wait 0) (populate-level))
  (timeline* :loop 
    #(if (key? "space")
         (do (fire-bullet s) false) true)
    (wait 0.2))
  (hook+ s :update           #'game.core/update-ship)
  (hook+ s :update           #'game.core/update-hud)
  
  (hook+ cam :on-draw-gizmos #'game.editor/editor-gizmos)
  (hook+ cam :update         #'game.editor/editor-update)
  (hook+ cam :update         #'game.core/update-camera)
  (hook+ s :on-trigger-enter #'game.core/on-trigger)))







(def radar-v3s (vertices (resource "icosphere42")))

(defn ball-update [o]
  (when (rand-nth [nil nil nil true])
    (let [

      pos (->v3 o)
      hits 
      (remove nil? 
        (for [v radar-v3s
              :let [ray (Ray pos v)
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



(make-level)




'(state! (the tree-spawn) {
  :obj :tree
  :scale-min 0.8
  :scale-max 1.5
  :rand-y true}) 
'(hook+ (Selection/activeGameObject) :start #'game.core/init-spawn)
'(clear-cloned!)