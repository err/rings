;;rings.clj
;;eric caspary
;;Spring 2010
(ns rings.core
  (:use [rosado.processing]
        [rosado.processing.applet])
  (:require [rings.tuio :as tuio]))

;;vars
(def *framerate* 30)
(def *width* 1680)
(def *height* 1050)
(def *max-age* (* 2 *framerate*)) ;;maximum age of a ring
(def *max-r* 90) ;(* 2 *framerate*)) ;;maximum radius of a ring
(def *min-r* 20)

;;atoms
(def *mouse-position* (atom [0 0]))
(def *mouse-button* (atom -1))
					; -1 - not-pressed
					;  0 - not-pressed
					;  1 - pressed

;;refs
(def *time*      (ref  0))
(def *counter*   (ref  0))
(def *growers*   (ref {}))
(def *shrinkers* (ref []))

;;structs
(defstruct ring :x :y :r :t :dir :original-dir)
(defstruct rgb-color :r :g :b :a)
(defstruct hsb-color :h :s :b :a)


;;TUIO
(def *tuio-port* 3333)
(def *tuio* (tuio/client *tuio-port*))

(defn reset-entity-counter []
  (dosync (ref-set *counter* 0)))

(defn new-id []
  (let [id @*counter*]
    (dosync (alter *counter* inc))
    id)) 

(defn age
  "Returns the age of an object.
   Assumes the object to be a struct w/ a key :t"
  ([obj]
     (let [t @*time*]
       (abs (- (:t obj) t))))
  ([obj time]
     (abs (- (:t obj) time))))

(defn expired? [obj]
  (> (age obj) *max-age*))

(defn ring-size [ring]
  "Returns the radius of a ring object. 
   At present, the radius is a function of the ring's age."
  (let [age (age ring)
	dir (:dir ring)]
    [(+ *min-r* age) (- *max-r* (* 2 age))]
    ;; (if (= dir 'grow)
    ;;   age
    ;;   (- *max-r* (* 2 age)))
    ))

(defn new-ring [x y dir]
  ;; returns [ring color] vec
  (let [t @*time*]
    [{:x x :y y :t t :dir dir :original-dir dir}
     {:h (rand 1.0) :s 0.5 :b 0.75}
     {:h (rand 1.0) :s 0.5 :b 0.75}]))

(defn remove-rings [m]
  "removes old rings from m (hash-map)"
  (into {}
	(take 80 (filter (fn [[id [ring c1 c2]]] (not (expired? ring))) (seq m)))))

(defn many-rings []
  (zipmap (repeatedly new-id)
	  (map (fn [[x y]] (new-ring x y 'grow))
	       (for [x (range 0 *width*  (+ 10 *max-r*))
		     y (range 0 *height* (+ 10 *max-r*))]
		 [x y]))))

(defn automagically-add-ring []
  "A new ring is added automatically every somany frames,
   given that the mouse button is depressed."
  (let [t @*time*
	b @*mouse-button*
	rx (rand *width*)
	ry (rand *height*)
	[mx my] @*mouse-position*
	[x y] (if (pos? b) [mx my] [rx ry] )]
    ;; (when (zero? (mod t 60))
    ;;   (dosync
    ;;    (alter *growers* into (many-rings))))
    (cond (pos? b) ;mouse-button-on
 	  (do
 	    (dosync
 	     (alter *growers* assoc (new-id) (new-ring mx my 'grow)) 
 ;; (alter *shrinkers* conj (new-ring mx my 'shrink))
 	     )
 	     ;; (when (zero? (mod t 10))
 	     ;;   (dosync
 	     ;; 	(alter *growers* assoc (new-id) (new-ring rx ry 'grow))))
 	    )
 	  ;; (zero? (mod t 10))
 	  ;; (do
 	  ;;   (dosync
 	  ;;    (alter *growers* assoc (new-id) (new-ring rx ry 'grow)) ))
 	  )
    ))

(defn update []
  "Updates the world model.
   Warning: side effects below!"
  (automagically-add-ring)
  (dosync
   (alter *growers* remove-rings)
   ;; (println "frame-count: " (frame-count))
   ;; (alter *shrinkers* remove-rings)
   ))

(defn random-ring-color [color-mode]
  (if (= color-mode 'hsb)
    {:h (rand 1.0) :s 0.5 :b 0.75}
    {:r (rand 256) :g (rand 256) :b (rand 256)}))

;;alpha 0 -> transparent
;;alpha 1 -> opaque
(defn ring-alpha [{dir :original-dir :as ring} r]
  (let [max-r *max-r*]
    (if (= dir 'grow)
      (- 1 (/ r max-r)) ;;growers' opacity is inversely proportional to their radius
      (/ r max-r))))

(defn draw-ring [x y r1 r2 c1 c2 c3 a]
  (fill-float   c1 c2 c3 a)
  (stroke-float c1 c2 c3 a)
  (stroke-weight (max (/ r1 4) 0))
  (ellipse 0 0 r1 r2))

(defn draw-ring-cluster [[ring color1 color2]]
  (when ring
    (let [{x :x, y :y, t :t, dir :dir} ring
	  {h1 :h, s1 :s, b1 :b} color1
	  {h2 :h, s2 :s, b2 :b} color2
	  [r1 r2] (ring-size ring)
	  a (max (- 0.9  (/ (age ring) *max-age*)) 0)
	  time (mod @*time* 16)]
      (color-mode HSB 1.0)
      (ellipse-mode CENTER)
      (with-translation [x y]
	(with-rotation [(mod @*time* 2)]
	  (draw-ring 0 0 r1 r2 h1 s1 b1 a)
	  (draw-ring 0 0 r2 r1 h2 s2 b2 a)
	  (with-rotation [(* TWO_PI (/ (age ring) *max-age*))]
	    (with-translation [r1 r1]
	      (draw-ring 0 0 (/ r1 2) (/ r2 2) h1 s1 b1 a)
	      (draw-ring 0 0 (/ r2 2) (/ r1 2) h2 s2 b2 a)))
	  (with-rotation [(* TWO_PI (/ (age ring) *max-age*))]
	    (with-translation [(- r1) r1]
	      (draw-ring 0 0 (/ r1 2) (/ r2 2) h1 s1 b1 a)
	      (draw-ring 0 0 (/ r2 2) (/ r1 2) h2 s2 b2 a)))
	  (with-rotation [(* TWO_PI (/ (age ring) *max-age*))]
	    (with-translation [r1 (- r1)]
	      (draw-ring 0 0 (/ r1 2) (/ r2 2) h1 s1 b1 a)
	      (draw-ring 0 0 (/ r2 2) (/ r1 2) h2 s2 b2 a)))
	  (with-rotation [(* TWO_PI (/ (age ring) *max-age*))]
	    (with-translation [(- r1) (- r1)]
	      (draw-ring 0 0 (/ r1 2) (/ r2 2) h1 s1 b1 a)
	      (draw-ring 0 0 (/ r2 2) (/ r1 2) h2 s2 b2 a))))))))


(defn draw []

  (color-mode HSB 1.0)
  (background-float 0.75 0.5 0.25)
;  (color-mode RGB 256)
;  (background-float 200 200 255) ;;Ken's color

  (dorun (map draw-ring-cluster (vals @*growers*)))
;  (doall (map draw-orbital @*growers-orb*))

;  (doall (map draw-orbital @*shrinkers-orb*))  
  (dosync (alter *time* inc))
  ;; (let [[x y] @*mouse-position*]
  ;;   (color-mode RGB)
  ;;   (fill-float 10 100 100)
  ;;   (rect x y 10 10))

  (update)
  )

;;;; ORBITAL - an object which moves in a circle about a fixed-point
;;   
;;  such objects will rotate in a specific ROTATION-DIRECTION about a FIXED-POINT
;;  always remaining a certain distance away.. this being the raidius
;;
;;
;; (comment
;;   (defstruct orbital :center :radius :direction :point :theta :d-theta :t)

;;   (def *d-theta* (/ TWO_PI 60))
;;   (defn d-theta [x y]
;;     "Returns the angular velocity of an orbital centered about point (x,y)."
;;     ;;right now we're just relying on a constant
;;     *d-theta*)

;;   (defn new-orbital [h k dir expanding?]
;;     ;;returns [orb color] vec
;;     (let [t @*time*
;; 	  theta (random TWO_PI)
;; 	  point (point-from-theta h k 0 theta) ]
;;       [(struct-map orbital :center [h k] :radius 0
;; 		   :direction dir :point point :expanding? expanding?
;; 		   :theta theta :d-theta (d-theta h k) :t t) 
;;        (struct-map hsb-color :h (rand 1.0) :s 0.5 :b 0.75)]))

;;   (defn point-from-theta [h k r theta]
;;     (let [x (+ h (* r (cos theta)))
;; 	  y (+ k (* r (sin theta)))]
;;       [x y]))

;;   (defn theta-from-point [h k r x y ]
;;     (let [theta (acos (/ (abs (- x h)) r))]
;;       theta))

;;   (defn orbital-radius [radius expanding?]
;;     (if ))

;;   (defn next-orbital-position [orb]
;;     (let [time @*time*
;; 	  {rad     :radius
;; 	   dir     :direction
;; 	   expand  :expanding
;; 	   theta   :theta
;; 	   d-theta :d-theta
;; 	   [h k]   :center
;; 	   [x y]   :point} orb
;; 	  new-theta (if (= dir 'ccw) (+ theta d-theta) (- theta d-theta))
;; 	  new-radius (orbital-radius rad expand)
;; 	  [x-new y-new] (point-from-theta h k rad theta-new) ]
;;       [x-new y-new]))

;;   (defn random-point-on-circle [h k r]
;;     "returns a point laying randomly on
;;    the  perimeter of  circle centered at (h,k) with radius r."
;;     (let [theta (random TWO_PI)
;; 	  point (point-from-theta h k r theta)]
;; 					;oR.x = oP.x + (o.x - oP.x) * cos(theta) - (o.y - oP.y) * sin(theta)
;; 					;oR.y = oP.y + (o.x - oP.x) * sin (theta) + (o.y - oP.y) * cos (theta) `
;;       point))

;;   (comment
;;     (let [h 200
;; 	  k 200
;; 	  r 100
;; 	  theta (rand TWO_PI)
;; 	  d-theta (/ TWO_PI 60)
;; 	  point (point-from-theta h k r theta)]
;;       (def *orbital* (ref [(struct-map orbital :center [h k] :radius r
;; 				       :direction 'ccw :theta theta :d-theta d-theta 
;; 				       :point point) ;(random-point-on-circle 200 200 100)
;; 			   (struct-map hsb-color :h (rand) :s 0.5 :b 0.75)]))))

;;   (defn draw-ring-orb-pair [[ring ring-color] [orb orb-color]]
;;     )


;;   (defn draw-orbital [[orb color]]
;;     (let [{[ctr-x ctr-y] :center
;; 	   [orb-x orb-y] :point
;; 	   r :radius} orb
;; 	   {h :h s :s b :b} color]
;;       (color-mode HSB 1.0)
;;       (ellipse-mode RADIUS)
;;       (fill-float h s b 1.0)
;;       (ellipse orb-x orb-y 10 10)))

;;   (defn update-orbital [[orb color]]
;;     "Warning: Side effects below!
;;    Certain fields the ref to orb is altered."
;;     (let [pos (next-orbital-position orb)
;; 	  {dir :direction
;; 	   theta :theta
;; 	   d-theta :d-theta} orb
;; 	  theta-new (if (= dir 'ccw) (+ theta d-theta) (- theta d-theta))
;; 	  new-orb (assoc orb :point pos
;; 			 :theta theta-new)]
;;       (dosync (ref-set *orbital* [new-orb color]))))

;;   x(defn draw []
;;     (color-mode RGB 256)
;;     (background-float 200 200 255) ;;Ken's color
;;     (fill-float 10 10 220)
;; 					;  (ellipse (+ 100 (first (:pos (first @*orbital*))))
;; 					;	   (+ 100 (second (:pos (first @*orbital*))))
;; 					;	   10 10)
;;     (let [[h k] (:center (first @*orbital*))]
;;       (fill-float 50 60 70)
;;       (ellipse h k 4 4))
;;     (draw-orbital @*orbital*)
;;     (dosync (alter *time* inc))
;;     (update-orbital @*orbital*)
;;     ;;  (let [[x y] @*mouse-position*]
;; 					;    (color-mode RGB)
;; 					;    (fill-float 10 100 100)
;; 					;    (rect x y 10 10))

;; 					;  (update)
;;     ))



(defn mouse-moved [evt]
  (let [x (.getX evt)
	y (.getY evt)]
    (reset! *mouse-position* [x y])))

(defn mouse-pressed [evt]
  (let [x (.getX evt)
	y (.getY evt)
	b @*mouse-button*]
   (reset! *mouse-button*  (* -1 b))
   (dosync
    (alter *growers* assoc (new-id) (new-ring x y 'grow)
	                   (new-id) (new-ring (- x (* 2 *max-r*)) (- y (* 2 *max-r*)) 'grow)
			   (new-id) (new-ring (+ x (* 2 *max-r*)) (+ y (* 2 *max-r*)) 'grow)
			   (new-id) (new-ring (- x (* 2 *max-r*)) (+ y (* 2 *max-r*)) 'grow)
			   (new-id) (new-ring (+ x (* 2 *max-r*)) (- y (* 2 *max-r*)) 'grow))
    ;; (alter *shrinkers* conj (new-ring x y 'shrink))
    )))

(defn mouse-released [evt]
  (let [x (.getX evt)
	y (.getY evt)
	b @*mouse-button*]	
    (reset! *mouse-button* (* -1 b))))

(defn mouse-dragged [evt]
  (let [x (.getX evt)
	y (.getY evt)]
   (reset! *mouse-position* [x y])))

;(defn key-pressed [evt]
;  (let [key (.get


(defn init-tuio-client []
  (try (tuio/connect! *tuio*)
       (doto *tuio*
	 (tuio/on-add-cursor! curs
			      (dosync
			       (alter *growers* assoc (new-id)
				      (new-ring (.getScreenX curs *width*) (.getScreenY curs *height*) 'grow))
			       ;; (alter *shrinkers* conj (new-ring x y 'shrink))
			       ))

	 (tuio/on-update-cursor! curs
				 (dosync
				  (alter *growers* assoc (new-id)
					 (new-ring (.getScreenX curs *width*) (.getScreenY curs *height*) 'grow))
				  ;; (alter *shrinkers* conj (new-ring x y 'shrink))
				  )))
	 
	  ;; (tuio/on-remove-cursor! curs
          ;;  (do (println "remove cursor")
	  ;;      (dosync
	  ;; 	(alter *cursors* dissoc (.getCursorID curs)))))

	 ;; (tuio/on-refresh! tobj (println tobj))
       ;;)
       (catch Exception e (.printStackTrace e))))


 (defn kill-tuio []
   (tuio/disconnect! *tuio*)
   (println "tuio client disconnected"))


(defn setup []
  "Runs once."
  (smooth)
  (background-float 200 200 255)
  (framerate *framerate*)
  (init-tuio-client)
  (reset-entity-counter))


(defapplet rings :title "Rings!"
  :setup setup :draw draw
  :size [*width* *height*]
  :mouse-moved mouse-moved
  :mouse-pressed mouse-pressed
  :mouse-released mouse-released
  :mouse-dragged mouse-dragged)
 ; :KEY-PRESSED KEY-PRESSED)

(run rings :interactive)
;; (stop rings)