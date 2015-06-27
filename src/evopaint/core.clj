(ns evopaint.core
  (:import [javax.swing JFrame JPanel]
           [java.awt Dimension Color Graphics2D]
           java.io.File
           java.awt.image.BufferedImage
           javax.imageio.ImageIO))

(def width 400)
(def height 267)
(def population (atom nil))
(def reference-image (atom nil))
(def fittest (atom nil))

(set! *warn-on-reflection* true)

(defn load-image [path]
  (ImageIO/read (File. path)))

(defn create-image [w h]
  (BufferedImage. w h BufferedImage/TYPE_INT_ARGB))

(defn random-color []
  (take 4 (repeatedly #(rand-int 255))))

(defn random-gene []
  (let [r (+ 5 (rand-int 20))]
    {:x (- (rand-int (+ width r)) r)
     :y (- (rand-int (+ height r)) r)
     :r r
     :c (random-color)}))

(defn gene-sequence [n]
  (vec (take n (repeatedly #(random-gene)))))

(defn initial-population [n m]
  (vec (take n (repeatedly #(gene-sequence m)))))

(defn reproduce
  [father mother]
  (vec (map #(rand-nth [%1 %2 %3]) mother father (repeatedly random-gene))))

(defn render-gene
  [^Graphics2D g2d gene]
  (.setColor g2d
    (Color. (nth (:c gene) 0)
            (nth (:c gene) 1)
            (nth (:c gene) 2)
            (nth (:c gene) 3)))
  (.fillOval g2d (:x gene)
                 (:y gene)
                 (:r gene)
                 (:r gene)))

(defn render-genes
  [genes ^Graphics2D g2d]
    (.clearRect g2d 0 0 width height)
    (doseq [gene genes]
      (render-gene g2d gene)))

(defn create-panel []
  (proxy [JPanel] []
    (paintComponent [g2d]
      (render-genes @fittest g2d))))

(defn create-frame
  [panel]
  (doto (new JFrame)
    (.add panel)
    .pack
    .show))

(defn diff-color
  [^long a ^long b]
    (+
      (Math/abs (- (bit-and b 0xFF) (bit-and a 0xFF)))
      (Math/abs (- (bit-and (bit-shift-right b 8) 0xFF) (bit-and (bit-shift-right a 8) 0xFF)))
      (Math/abs (- (bit-and (bit-shift-right b 16) 0xFF) (bit-and (bit-shift-right a 16) 0xFF)))
      (Math/abs (- (bit-and (bit-shift-right b 24) 0xFF) (bit-and (bit-shift-right a 24) 0xFF)))))

(defn compare-images
  [^BufferedImage a ^BufferedImage b]
  (reduce +
    (map #(let [i (mod % width)
                j (quot % width)]
            (diff-color (.getRGB a i j) (.getRGB b i j)))
         (range 0 (* width height) 5))))

(defn score-fitness
  [genes]
  (let [i (create-image width height)]
    (render-genes genes (.createGraphics i))
    (compare-images @reference-image i)))

(defn breed [pop]
  (concat pop (map #(reproduce % (rand-nth pop)) pop)))

(defn prescore
  [pop]
  (pmap #(hash-map :score (score-fitness %) :genes %) pop))

(defn evolve-generation []
  (reset! population (->> @population
                          (breed)
                          (prescore)
                          (sort-by :score)
                          (take 50)
                          (map :genes))))

(defn start [panel]
  (future
    (while true
      (time (evolve-generation))
      (reset! fittest (last @population))
      (.repaint panel))))

(defn -main [& args]
  (reset! reference-image (load-image "/tmp/image.jpg"))
  (reset! population (initial-population 50 1000))

  (let [panel (create-panel)]
    (.setPreferredSize panel (Dimension. width height))
    (create-frame panel)
    (start panel)))
