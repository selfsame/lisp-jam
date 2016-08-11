(ns game.world
  (:import ArcadiaState 
    [UnityEngine Color])
  (:use
    arcadia.core arcadia.linear
    tween.core hard.seed
    hard.core hard.mesh))

(deftween [:camera :field-of-view] [this]
  {:base (.GetComponent this UnityEngine.Camera)
   :get (.fieldOfView this)
   :tag System.Single})

(def last-sky (atom nil))

(defn gradiate 
  ([o] (gradiate o (mapv color (sort #(< (first %1) (first %2)) 
    (repeatedly 6 #(vector (?f 0.2 0.9) (?f 0.1 0.8) (?f 0.1 0.9)))))))
  ([o six-colors] 
    (let [skydome o
            [c1 c2 c3 c4 c5 c6] six-colors
            [m1 m2 m3] (vec (shuffle [(srand 3)(srand 3)(srand 3)]))]
        (reset! last-sky [c1 c2 c3 c4 c5 c6])
        (vertex-colors! skydome 
          (fn [x y z i] 
            (Color/Lerp c5
              (Color/Lerp c1 
                (Color/Lerp c6 c3 (sin (* y m3)))
                (cos (* x m2)))
              (cos (* z m1))))) o)))

(defn make-skydome []
  (let [rv (v3 (?f -0.2 0.1) (?f -0.2 0.3)(?f -0.2 0.2))]
  (hook+ (gradiate (clone! :skyball)) :update #(rotate! % rv))))

'(do (destroy! (the skyball))
  (make-skydome))

'(gradiate (clone! :skyball) @last-sky)

'(str @last-sky)

(def skies 
{:yell-purp "[#unity/Color [0.4563513 0.2887135 0.7213883 1.0] #unity/Color [0.3304492 0.2411222 0.1774861 1.0] #unity/Color [0.5286844 0.2351193 0.5412424 1.0] #unity/Color [0.8495363 0.7919118 0.1993009 1.0] #unity/Color [0.8068133 0.7054008 0.4996326 1.0] #unity/Color [0.6943583 0.6883557 0.4009431 1.0]]"
 :pastelgoth "[#unity/Color [0.4412065 0.4248923 0.535843 1.0] #unity/Color [0.6254358 0.4549651 0.6161992 1.0] #unity/Color [0.8647524 0.3533525 0.6874495 1.0] #unity/Color [0.7044883 0.1843295 0.5332503 1.0] #unity/Color [0.3881265 0.1464254 0.5707632 1.0] #unity/Color [0.5693207 0.713671 0.619423 1.0]]"
 :bird   "[#unity/Color [0.3796766 0.7346345 0.5604312 1.0] #unity/Color [0.4430125 0.463688 0.6244465 1.0] #unity/Color [0.852449 0.2169022 0.2610638 1.0] #unity/Color [0.5274823 0.6050904 0.3482536 1.0] #unity/Color [0.2037919 0.3281526 0.6660516 1.0] #unity/Color [0.536961 0.5579552 0.247919 1.0]]"
 :purpor "[#unity/Color [0.3153701 0.1060666 0.5641489 1.0] #unity/Color [0.3772687 0.1547806 0.3860488 1.0] #unity/Color [0.3798962 0.3075268 0.5152587 1.0] #unity/Color [0.7116494 0.7764431 0.200876 1.0] #unity/Color [0.6701339 0.4613301 0.2734218 1.0] #unity/Color [0.2899934 0.3594173 0.3434699 1.0]]"
 :reddo "[#unity/Color [0.399708 0.3665132 0.3015406 1.0] #unity/Color [0.5733199 0.7329649 0.2372955 1.0] #unity/Color [0.6347872 0.6335507 0.208861 1.0] #unity/Color [0.6884009 0.7705879 0.6305042 1.0] #unity/Color [0.5559539 0.3712607 0.5942253 1.0] #unity/Color [0.6056182 0.1190551 0.2285255 1.0]]"
 :miami "[#unity/Color [0.3766273 0.4599797 0.6414964 1.0] #unity/Color [0.333058 0.4921878 0.301102 1.0] #unity/Color [0.366755 0.2422009 0.4001856 1.0] #unity/Color [0.7340484 0.5258282 0.3596563 1.0] #unity/Color [0.8188054 0.5585465 0.3806493 1.0] #unity/Color [0.8716728 0.6193385 0.4500059 1.0]]"
 :meridian "[#unity/Color [0.5920917 0.514935 0.4503812 1.0] #unity/Color [0.8809944 0.6498094 0.3638338 1.0] #unity/Color [0.7224531 0.3912195 0.3957265 1.0] #unity/Color [0.7923608 0.4158091 0.438444 1.0] #unity/Color [0.5639502 0.69959 0.5733853 1.0] #unity/Color [0.2641351 0.4099448 0.5798451 1.0]]"})

'(skyball :bird)

(defn skyball 
  ([] (gradiate (clone! :skyball)))
  ([k] (gradiate (clone! :skyball) (read-string (get skies k (:miami skies))))))