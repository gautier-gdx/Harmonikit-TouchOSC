(ns testosc.core
  (:use overtone.live)
  (:use overtone.inst.piano))

(zero-conf-on)

(definst kick [volume 1.0]
  (let [body-freq (* 220 (env-gen (lin 0.01 0 0.3 1) :action NO-ACTION))
        body (sin-osc body-freq)
        
        pop-freq (+ 20 (* 400 (env-gen (lin 0.01 0 0.1 1) :action NO-ACTION)))
        pop  (sin-osc pop-freq)
        
        env  (env-gen (perc 0.001 1.5) :action FREE)
        ]
    (* env (+ body pop))))

(definst hat []
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* 0.7 src env)))

(definst bass [freq 50 volume 1.0 wah 1.0]
  (let [son (saw freq)
        son (* son wah)
        son (clip:ar son 0 1)]
    (* volume son)))

(definst beep [freq 1100]
  (let [src (sin-osc freq)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* 2 src env)))

(definst dubstep [note 25 sweep-rate 4 vol 0 hi? 0 sweep? 0 decimate? 0]
  (let [trig (coin-gate 0.5 (impulse:kr 2))
        freq (midicps note)
        sweep (lin-exp (lf-saw sweep-rate) -1 1 40 5000)
        son (->
             (mix (lf-saw (* freq [0.99 1 1.01])))
             (lpf sweep)
             (normalizer))
        son (+ son (bpf son 2000 2))
        son (select hi? [son (* 4 (hpf son 1000))])
        son (select sweep? [son (* 4 (hpf son sweep))])
        son (select decimate? [son (round son 0.1)])
        son (tanh (* son 5))
        son (+ son (* 0.3 (g-verb son 10 0.1 0.7)))]
    (* vol son)))

(defonce server (atom nil))

(defn start-osc []
  (swap! server (fn [old-server]
                  (if (not (nil? old-server))
                    old-server
                    (osc-server 44100 "Phil's overtone")))))

(defn register-debug-listener []
  (osc-listen @server (fn [msg] (println msg)) :debug))

(defn remove-debug-listener []
  (osc-rm-listener @server :debug))

(defmacro simple-handler [args body]
  `(fn [msg#]
     (let [~args (:args msg#)]
       ~body)))

(defn make-piano-handler [note]
  (simple-handler [arg]
                  (when (not (zero? arg))
                    (piano note))))

(def piano-map
  (let [middle-b 59
        indices (range 1 13)
        path-from (fn [index] (str "/1/push" index))
        note-from (fn [index] (+ middle-b index))]
    (zipmap
     (map path-from indices) (map (comp make-piano-handler note-from) indices))))

(defn register-handlers [handler-map]
  (doseq [[path handler] handler-map]
    (osc-handle @server path handler))
  @server)

(defonce sequencer-state (atom (vec (repeat 8 (vec (repeat 8 0))))))

(defonce beep-freq (atom 1100))

(defonce dubstep-note (atom 25))
(defonce sweep-rate (atom 4))
(defonce sequencer-on? (atom false))

(def metro (metronome 256))

(def inst-map
  {0 {1 kick}
   1 {1 hat}
   2 {1 #(beep @beep-freq)}
   3 dubstep
   4 {1 #(ctl bass :volume 1) 0 #(ctl bass :volume 0)}})

(defn sequencer [beat notes]
  (when @sequencer-on?
    (at (metro beat)
        (let [mod-beat (mod beat 8)]
          (doseq [[index inst] inst-map]
            (let [val  (get-in @notes [index mod-beat])
                  inst (get inst val (fn [] nil))]
              (inst)))))
    (apply-at (metro (inc beat)) #'sequencer (inc beat) [notes])))

(defn sequencer-off []
  (reset! sequencer-on? false)
  (kill bass)
  (kill dubstep)
  )

(defn sequencer-on []
  (bass :volume 0)
  (reset! sequencer-on? true)
  (sequencer (metro) sequencer-state))

(defn make-sequencer-handler [inst beat]
  (simple-handler [arg]
                  (swap! sequencer-state
                         assoc-in [inst beat] (int arg))))

(def ^{:private true} multitoggle-map-entries
  (let [path-from (fn [x y] (str "/1/multitoggle1/" (inc x) "/" (inc y)))]
    (for [x (range 8)
          y (range 8)]
      [(path-from x y) (make-sequencer-handler x y)])))

(def sequencer-map
  (into
   {"/1/toggle13" (simple-handler [arg] (if (zero? arg) (sequencer-off) (sequencer-on)))
    "/1/fader1"  (simple-handler [arg] (reset! beep-freq (scale-range arg 0 1 250 1500)))
    "/1/fader2"  (simple-handler [arg] (ctl dubstep :note (round-to (scale-range arg 0 1 20 50) 1)))
    "/1/fader3"  (simple-handler [arg] (ctl dubstep :sweep-rate (scale-range arg 0 1 1 16)))
    "/1/toggle1" (simple-handler [arg] (ctl dubstep :hi? arg))
    "/1/toggle2" (simple-handler [arg] (ctl dubstep :sweep? arg))
    "/1/toggle3" (simple-handler [arg] (ctl dubstep :decimate? arg))
    "/1/fader5"  (simple-handler [arg]
                                 (let [new-bpm (scale-range arg 0 1 160 500)]
                                   (metro :bpm new-bpm)))
    "/1/xy2"      (simple-handler [arg1 arg2]
                               (do
                                 (ctl bass :freq (scale-range arg1 0 1 40 250))
                                 (ctl bass :wah  (scale-range arg2 0 1 1.5 30))))}
   multitoggle-map-entries))

(start-osc)
(register-handlers sequencer-map)
