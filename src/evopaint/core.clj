(ns evopaint.core
  (:import [javax.swing JFrame JPanel]
           [java.awt Dimension Color Graphics2D]
           java.io.File
           java.awt.image.BufferedImage
           javax.imageio.ImageIO))

(def width 432)
(def height 288)
(def population (atom nil))
(def reference-image (atom nil))
(def fittest (atom nil))

(set! *warn-on-reflection* true)

(defn load-image [path]
  (ImageIO/read (File. path)))

(defn create-image [w h]
  (BufferedImage. w h BufferedImage/TYPE_INT_ARGB))

(defn random-color []
  (vec (take 4 (repeatedly #(rand-int 255)))))

(defn random-gene []
  {:x (rand-int width)
   :y (rand-int height)
   :r (+ 5 (rand-int 55))
   :c (random-color)})

(defn gene-sequence [n]
  (vec (take n (repeatedly #(random-gene)))))

(defn initial-population [n m]
  (vec (take n (repeatedly #(gene-sequence m)))))

(defn cosmic-rays
  [genes]
  (conj (assoc genes (rand-int (- (count genes) 1)) (random-gene))
        (random-gene)))

(defn reproduce
  [father mother]
  (cosmic-rays (vec (map #(rand-nth [%1 %2]) mother father))))

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
         (range (* width height)))))

(defn score-fitness
  [genes]
  (let [i (create-image width height)]
    (render-genes genes (.createGraphics i))
    (time (compare-images @reference-image i))
    (compare-images @reference-image i)))

(defn breed [pop]
  (concat pop (map #(reproduce % (rand-nth pop)) pop)))

(defn prescore
  [pop]
  (map #(hash-map :score (score-fitness %) :genes %) pop))

(defn evolve-generation []
  (reset! population (map :genes (take 20 (sort-by :score (prescore (breed @population)))))))

(defn start [panel]
  (future
    (while true
      (evolve-generation)
      (reset! fittest (last @population))
      (.repaint panel))))

(defn -main [& args]
  (reset! reference-image (load-image "/tmp/image.jpg"))
  (reset! population (initial-population 20 10))

  (let [panel (create-panel)]
    (.setPreferredSize panel (Dimension. width height))
    (create-frame panel)
    (start panel)))
