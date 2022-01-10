(ns de.zalf.berest.core.climate.algo)

(defn potential-evaporation-turc-wendling
  [globrad-Jpcm2 tavg & {:keys [fk] :or {fk 1}}]
  (/ (* (+ globrad-Jpcm2 (* 93 fk)) (+ tavg 22)) (* 150 (+ tavg 123))))

(defn climatic-water-balance-turc-wendling
  [precip-mm globrad-Jpcm2 tavg & {:keys [fk] :or {fk 1}}]
  (- precip-mm (potential-evaporation-turc-wendling globrad-Jpcm2 tavg :fk fk)))

