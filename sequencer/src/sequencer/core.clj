(ns sequencer.core

 (:use overtone.live)
  (:use overtone.inst.drum)
  (:use osc-multitoggle.core))

(def server (osc-server 44100 "osc-clj"));;on defini un serveur osc, le port de sorti 44100 est arbitraire

(zero-conf-on);; on rend visible le serveur 
(osc-listen server (fn [msg] (println msg)) :debug) ;;sert a afficher toute les interraction entre les deux appareil communiquants a partir du serveur
;; on defini le nombre de ligne et de colonne de notre séquenceur
(def rows 6)
(def columns 16)
;; on defini le controle du multitoggle de touchosc, on pourrait le faire nous meme comme dans myinstrument.clj mais la biblioteque osc en a un deja tout fait 
(add-multitoggle-handler server 2 "multitoggle" rows columns)
;;on defini un metronome a un tempo de 120
(def one-twenty-bpm (metronome 240))
;;on joue la colonne on regarde si l'instrument est acitivé sur la colonne, si oui on le joue N.B: les chiffre e bleu correspondent aux id des boutons dans touchosc
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


;; on creer une boucle recursive qui s'appelle une fois la colonne jouée
(defn looper [nome]
  (let [beat (nome)
        beat-at (mod beat columns)]
    (at (nome beat) (play-column "2" "multitoggle" (str (+ beat-at 1))));; au tic du metronome on joue la colonne et on incremente la colonne pour pouvoir jouer la suivante
    (apply-at (nome beat) looper nome [])));; apply-at sert a faire la recursion a un instant t, ici apres un tic de metronome

(looper one-twenty-bpm)