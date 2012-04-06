(ns rings.tuio
  (:import [TUIO TuioClient]
       [TUIO TuioContainer]
       [TUIO TuioCursor]
       [TUIO TuioListener]
       [TUIO TuioObject]
       [TUIO TuioPoint]
       [TUIO TuioTime]))

(defmacro on-add-object! [client data cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject
	 [d#]
	 (let [~data d#]))
        (updateTuioObject [d#] )
        (removeTuioObject [d#] )
        (addTuioCursor [d#] )
        (updateTuioCursor [d#] )
        (removeTuioCursor [d#] )
        (refresh [d#]))))

(defmacro on-update-object! [client data cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject [d#] )
        (updateTuioObject
	 [d#]
	 (let [~data d#] ~cb)
	 ;; (println "\nx: " (.getScreenX d# 1024)
	 ;; 	  " y: " (.getScreenY d# 768))
	 )
        (removeTuioObject [d#] )
        (addTuioCursor [d#] )
        (updateTuioCursor [d#] )
        (removeTuioCursor [d#] )
        (refresh [d#]))))

(defmacro on-remove-object! [client data cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject [d#] )
        (updateTuioObject [d#] )
        (removeTuioObject
	 [d#]
	 (let [~data d#] ~cb))
        (addTuioCursor [d#] )
        (updateTuioCursor [d#] )
        (removeTuioCursor [d#] )
        (refresh [d#]))))

(defmacro on-add-cursor! [client data cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject [d#] )
        (updateTuioObject [d#] )
        (removeTuioObject [d#] )
      
	(addTuioCursor [d#]
         (let [~data d#] ~cb))

        (updateTuioCursor [d#] )
        (removeTuioCursor [d#] )
        (refresh [d#]))))

(defmacro on-update-cursor! [client data cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject [d#] )
        (updateTuioObject [d#] )
        (removeTuioObject [d#] )
        (addTuioCursor [d#] )
        (updateTuioCursor
	 [d#]
         (let [~data d#] ~cb))
        (removeTuioCursor [d#] )
        (refresh [d#]))))

(defmacro on-remove-cursor! [client data cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject [d#] )
        (updateTuioObject [d#] )
        (removeTuioObject [d#] )
        (addTuioCursor [d#] )
        (updateTuioCursor [d#] )
        (removeTuioCursor
	 [d#]
	 (let [~data d#] ~cb))
        (refresh [d#]))))

(defmacro on-refresh! [client data cb]
  `(. ~client addTuioListener
      (proxy [TuioListener] []
        (addTuioObject [d#] )
        (updateTuioObject [d#] )
        (removeTuioObject [d#] )
        (addTuioCursor [d#] )
        (updateTuioCursor [d#] )
        (removeTuioCursor [d#]  )
        (refresh
	 [d#]
	 (let [~data d#] ~cb)))))

(defn remove-all-listeners! [c]
  (.removeAllTuioListeners c))

(defn client  
  ([] (TuioClient.))
  ([port] (TuioClient. port)))

(defn connect!
  [c] (.connect c))

(defn disconnect!
  [c] (.disconnect c))

(defn connected? [c]
  (.isConnected? c))

(defn cursors [c]
  (.getTuioCursors c))

(defn objects [c]
  (.getTuioObjects c))

(defn cursor
  "Gets a TUIO cursor on ID."
  [c id]
  (first (filter (fn [c]
                   (let [id2 (.getCursorID c)]
                     (= id2 id)))
                 (cursors c))))


(comment
  (def c (client))

  (doto c
    (on-add-object! #(println "add object"))
    (on-update-object! #(println "update object"))
    (on-remove-object! #(println "remove object"))
    (on-add-cursor! #(println "add cursor"))
    (on-update-cursor! ;; #(println "\nx: " (.getScreenX d# 1024)
		       ;; 		 " y: " (.getScreenY d# 768))
		        (println "update cursor"))
    (on-remove-cursor! #(println "remove cursor"))
    (on-refresh! #(println "refresh")))

  (connect! c)
  (disconnect! c))

