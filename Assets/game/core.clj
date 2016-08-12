(ns game.core
  (:import ArcadiaState 
    [UnityEngine Ray Input])
  (:require tween.pool)
  (:use
    arcadia.core arcadia.linear
    tween.core
    hard.core hard.input hard.mesh hard.physics
    pdfn.core
    game.editor
    game.constraints
    game.guys
    game.world))

(declare fire-bullet make-level impact)

(def orbit-height 8.0)
(def speed        0.5)
(def bullet-speed 1.6)

(defonce current-level (atom :planet1))
  
(deftween [:trail-renderer :time] [this]
  {:base (.GetComponent this UnityEngine.TrailRenderer)
   :get (.time this)
   :tag System.Single})

(deftween [:camera :field-of-view] [this]
  {:base (.GetComponent this UnityEngine.Camera)
   :get (.fieldOfView this)
   :tag System.Single})

(defn ->data-haver [o]
  (when o 
    (try (or ({{} nil} (state o) o)
             (->data-haver (parent o)))
         (catch Exception e nil))))



(defn rocket-bank [o n]
  #_(let [rk (get (children o) 1)]
    ;(rotate! rk (v3 0 0 (∆ (* n 35))))
    #_(rotate! eng (local-direction eng (v3 0 0 n))) ) )

(defn ship-keys [o]
  (if (key-down? "escape") (make-level))
  (if (and (key-down? "p") (key? "j")) (make-level :planet4))
  (if (and (key-down? "p") (key? "j")(key? "a")) (make-level :planet5))
  (if (and (key-down? "p") (key? "j")(key? "l")) (make-level :planet6))
  (if (key? "a") (do (rocket-bank o -2) (rotate! o (v3 0 (∆ (* 35 -1.8)) 0))))
  (if (key? "d") (do (rocket-bank o 2) (rotate! o (v3 0 (∆ (* 35 1.8)) 0))))
  (if (key? "w") (update-state! o :speed #(min (state o :max-speed) 
    (+ % (∆ (* 35 0.01 ))))))
  (if (key? "s") (update-state! o :speed #(max 0.2 (- % (∆ (* 35 0.01)))))))

(defn ai-keys [o]
  (if (< -0.5 (sin (* Time/time 0.6))) (do (rocket-bank o -2) (rotate! o (v3 0 -1.8 0))))
  (if (> 0.5 (cos (* Time/time 1.1))) (do (rocket-bank o 2) (rotate! o (v3 0 1.8 0))))
  (if true (update-state! o :speed #(min (state o :max-speed) (+ % 0.1))))
  (if (< -0.3 (cos (* 0.2 Time/time)) )  (update-state! o :speed #(max 0 (- % 0.1)))))


(defn move [^GameObject o]
  (let [opos     (.position (.transform o))
        sp       (∆ (* 35 (or (state o :speed) speed)))
        last-pos (or (state o :last-pos) opos)
        desired  (v3+ opos (local-direction o (v3 sp 0 0)))
        crv-mod  (- sp (.magnitude (v3- desired last-pos)))
        fixed    (v3+ opos (local-direction o (v3 (+ sp crv-mod) 0 0)))]
    (set! (.position (.transform o)) fixed)
    (set-state! o :last-pos (>v3 o))))

(defn update-ship [^GameObject o]
  (constrain-to-planet o)
  (if (state o :ai) (ai-keys o) (ship-keys o))
  (when-let [^UnityEngine.Light light (.GetComponentInChildren o UnityEngine.Light)]
    (set! (.intensity light) (float (or (state o :speed) 0.0))))
  (move o))

(defn update-bullet [o]
  (when-not (state o :disabled)
    (constrain-to-planet o)
    (move o)
    (let [heading (local-direction o (v3 1 0 0))
          ray (Ray. (>v3 o) heading)
          hits (filter #(cmpt (gobj (.collider %)) ArcadiaState) (ray-hits ray (state o :speed)))]
       (mapv
        #(impact o (state o) (.collider %) (state (.collider %)) %) hits))))





(defn spin [o] (rotate! o (v3 0 (∆ 62) 0)))

(defn gate-update [o]
  (if (the star)
    (set-state! o :active false)
    (do 
      (set! (.* (child-named o "center")>Light.intensity) (float 1.0))
      (set-state! o :active true))))


(defn splode 
  ([p] (splode p 1))
  ([p bigness]
    (let [base (clone! :empty (>v3 p))]
      (dorun (for [i (range (inc (or bigness 5)))
                   :let [rp (?sphere 2.0)
                         fire (clone! :fire)]]
        (do 
          (parent! fire base)
          (position! fire (>v3 base))
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
(pdfn trigger-dispatch [a as b bs] ;(log [(.name a) (.name b)])
  )
(pdfn trigger-dispatch [a ^:damage as b ^:hp bs]
  (splode a (int (* 0.1 (state a :damage))))
  (update-state! b :hp #(- % (state a :damage)))
  (destroy! a)
  (if (neg? (state b :hp))
      (die b bs)))

(pdfn trigger-dispatch [a ^:damage as b ^:body bs])

(pdfn trigger-dispatch [a ^:ship as b ^:star bs] (destroy b))
(pdfn trigger-dispatch [a ^:ship as b ^:gate bs] 
  (if (:active bs) 
    (timeline* (wait 0) (make-level (:gate bs)))))

(defn on-trigger [o c]
  (let [other (.gameObject c)
        [a b] (mapv ->data-haver [o other])]
    (trigger-dispatch a (state a) b (state b))))

(defpdfn impact)
(pdfn impact [a as b bs h] nil)
(pdfn impact [a ^:force as b ^:body bs h]
  (let [body (->rigidbody b)
        fv (v3* (v3- (>v3 b) (.point h)) 100) 
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
        bullet (clone! :bullet (v3+ (>v3 o) (local-direction o (v3 10 0 (* 1.0 side)))))]
    (rotation! bullet (rotation o))
    (state! bullet {
      :bullet true
      :speed bullet-speed
      :damage 20
      :force true
      :hover-distance 3.0})
    (hook+ bullet :update #'game.core/update-bullet)
    (hook+ bullet :update #'game.core/update-bullet)
    (hook+ (first (children bullet)) :on-trigger-enter #'game.core/on-trigger)
    (timeline*
      (wait 10.0)
      (tween {:local {:scale (v3 0 0 0)}
              :trail-renderer {:time 0.0}} bullet 0.6)
      #(destroy! bullet))))


(defonce clock (atom 0))

(defn update-hud [o]
  (let [health-rect-tform (cmpt (the health-bar) UnityEngine.RectTransform)]
    #_(set! (.sizeDelta health-rect-tform)  (v2 (state o :hp) 17))
    Time/deltaTime
    (swap! clock #(+ % Time/deltaTime))
    (set! (.* (the time)>Text.text) (str  (int (/ @clock 60)) ":" (int (mod @clock 60))))
    (set! (.* (the stars)>Text.text) (str  " " (count (every star))))))

(defn update-camera [o]
  (let [target (the cam-target)]
    (position! o (Vector3/Lerp (>v3 o) (>v3 target) (∆ (* 35 0.17))))
    (rotation! o (Quaternion/Lerp (rotation o) (rotation target) (∆ (* 35 0.15))))))



(defn populate-level [planet]
  (let [planet (the #"planet.*")
        pk (keyword (.name planet))
        level (get @LEVELS pk)]
    (dorun (map 
      (fn [o]

        (try 
          (state! 
            (rotation! 
              (parent! (clone! (:type o) (:position o)) planet) 
              (:rotation o))
            (:state o))
          (catch Exception e (log e)))) 
      level)) nil))

'(local-position (the cam-target))
'(local-rotation (the cam-target))

(def cam-settings {
  :planet1 "[#unity/Vector3 [-3.9 40.9 0.0]
#unity/Quaternion [0.3861523 0.5923567 -0.3861524 0.5923566]]"
:planet4 "[#unity/Vector3 [-19.0 22.4 0.0]
#unity/Quaternion [0.2031946 0.6772828 -0.2031946 0.6772828]]"
:planet6 "[#unity/Vector3 [-21.03 5.68 0.0]
#unity/Quaternion [0.01628914 0.7069191 -0.01628914 0.7069191]]"
:planet5 "[#unity/Vector3 [-16.97 3.6 0.0] #unity/Quaternion [-0.01561059 0.7069345 0.01561059 0.7069345]]"})


(defn cam-config [k]
  (when-let [d (get cam-settings k)]
    ((fn [[p q]] (local-position! (the cam-target) p)
      (local-rotation! (the cam-target) q))
      (read-string d))))

'(make-level :planet3)




(defn make-level 
  ([] (make-level @current-level))
  ([k] 
    (reset! current-level k)
    (clear-cloned!)
    (clone! :EventSystem)
    (clone! :Canvas)
    (let [p (clone! k)
          pstate (state p)
          spawn (child-named p "spawn")
          s (clone! :ship (>v3 spawn))
          cam (clone! :camera)] 
    (populate-level p)
    (rotation! s (rotation spawn))
    (timeline* (wait 0) (if (cam-settings k) (cam-config k)))
    (skyball (:sky pstate :miami))
    (state! s {
      :ship true
      :speed 0
      :max-speed 0.6
      :hover-distance 7.5
      :gun-toggle 1
      :hp 200
      :max-hp 200})
    (timeline* :loop 
      #(if (key? "space")
           (do (fire-bullet s) false) true)
      (wait 0.2))
    (hook+ s :update           #'game.core/update-ship)
    (hook+ s :update           #'game.core/update-hud)
    
    ;(hook+ cam :on-draw-gizmos #'game.editor/editor-gizmos)
    (hook+ cam :update         #'game.editor/editor-update)
    (hook+ cam :update         #'game.core/update-camera)
    (hook+ s :on-trigger-enter #'game.core/on-trigger))))



(defn init-spawn [o]
  (let [obj (clone! (or (state o :obj) :empty) (>v3 o))
        sc (or (?f (or (state o :scale-min) 1) (or (state o :scale-max) 1)))]
    (rotation! obj (rotation o))
    (if (state o :scale-min)
      (local-scale! obj (?f (or (state o :scale-min) 1) (or (state o :scale-max) 1))))
    (if (state o :rand-y)
      (rotate! obj (v3 0 (?f 360) 0)))))



(defn intro-screen [_]
  (clear-cloned!)
  (skyball :meridian)
  (let [scene   (clone! :introscene)
        cam (child-named scene "camera")]

  (reset! clock 0)
  
  (timeline* 
    ;(tween {:camera {:field-of-view 55}} cam 3.0 {:out :pow3})
    #(not (Input/anyKeyDown))
    (tween {:camera {:field-of-view 178}} cam 1.5 {:in :pow3})
    #(do (make-level :planet1) false))))

'(intro-sscreen nil)


'(set-state!  (Selection/activeGameObject) :ai true)

'(state! (the tree-spawn) {
  :obj :tree
  :scale-min 0.8
  :scale-max 1.5
  :rand-y true}) 
'(hook+ (Selection/activeGameObject) :start #'game.core/intro-screen)
'(set-state! (Selection/activeGameObject) :star true)
'(clear-cloned!)




