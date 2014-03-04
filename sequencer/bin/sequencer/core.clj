(ns sequencer.core

 (:use overtone.live)
  (:use overtone.inst.drum)
  (:use osc-multitoggle.core))

(def server (osc-server 44100 "osc-clj"))

(zero-conf-on)
(osc-listen server (fn [msg] (println msg)) :debug)
(def rows 6)
(def columns 16)

(add-multitoggle-handler server 2 "multitoggle" rows columns)

(def one-twenty-bpm (metronome 240))

(defn play-column [layout widget col]
  (let [activedrums (get-active-in-row layout widget col)]
    (doseq [drum activedrums]
      (case drum
        "1" (kick)
        "2" (snare)
        "3" (kick2)
        "4" (kick3)
        "5" (open-hat)
        "6" (closed-hat)))))

(defn looper [nome]
  (let [beat (nome)
        beat-at (mod beat columns)]
    (at (nome beat) (play-column "2" "multitoggle" (str (+ beat-at 1))))
    (apply-at (nome beat) looper nome [])))

(looper one-twenty-bpm)