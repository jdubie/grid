(ns jnitest.core
  (:require
    [jnitest.snake :as snake]
    [jnitest.agent :as agent]
    [clojure.java.io :as io])
  (:import
    [java.net InetAddress DatagramPacket DatagramSocket]
    [javax.imageio ImageIO]
    [java.awt Color Image]
    [java.awt.image BufferedImage AffineTransformOp]
    [java.awt.geom AffineTransform]))

(defn ->32x32
  [path]
  (let [sourceImage (ImageIO/read (io/file path))
        tx (AffineTransform.)
        _ (.rotate tx
                   -1.5708
                   (/ (.getWidth sourceImage) 2)
                   (/ (.getHeight sourceImage) 2))
        op (AffineTransformOp. tx AffineTransformOp/TYPE_BILINEAR)
        sourceImage (.filter op sourceImage nil)
        thumbnail (.getScaledInstance sourceImage 32 -1 Image/SCALE_SMOOTH)
        bufferedThumbnail (BufferedImage.
                            (.getWidth thumbnail nil)
                            (.getHeight thumbnail nil)
                            BufferedImage/TYPE_INT_RGB)]
    (.drawImage (.getGraphics bufferedThumbnail) thumbnail 0 0 nil)
    bufferedThumbnail))

(defonce client (DatagramSocket. 3002))
(defonce ip (InetAddress/getByName "192.168.0.18"))

(defn send-buf
  [buf]
  (let [packet (DatagramPacket. buf (* 32 32 3) ip 3001)]
    (.send client packet)))

(defn send-matrix
  [matrix]
  (send-buf (byte-array (flatten matrix))))

(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
    [(+ dx x) (+ dy y)]))

(defn step [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(defn ->matrix
  [f]
  (map
    (fn [x]
      (map
        (fn [y]
          (f x y))
        (range 32)))
    (range 32)))

(defn run-game
  []
  (doseq [b (take 1000 (iterate step (set (take (/ (* 32 32) 2) (repeatedly #(-> [(rand-int 32) (rand-int 32)]))))))]
    (send-matrix (->matrix (fn [x y] (if (contains? b [x y])
                                       [255 0 0]
                                       [0 0 0]))))
    (Thread/sleep 100)))

(defn img-path->matrix
  [path]
  (let [i (->32x32 path)]
    (->matrix (fn [x y]
                (let [c (Color. (.getRGB i x y))]
                  [(.getRed c) (.getGreen c) (.getBlue c)])))))

(defn send-img
  [path]
  (send-matrix (img-path->matrix path)))

(defn random-matrix
  []
  (->matrix (fn [_ _]
              [(rand-int 256)
               (rand-int 256)
               (rand-int 256)])))

(defn degree->color
  [d]
  (let [radians (* Math/PI (/ d 360))]
    (->> (range 3)
         (map (fn [i] (* i (/ (* 2 Math/PI) 3))))
         (map #(+ % radians))
         (map #(Math/sin %))
         (map #(+ % 1))
         (map #(/ % 2))
         (map #(* % 255))
         (map int))))

(defn rainbox-matrix
  ([] (rainbox-matrix 0))
  ([x']
   (->matrix (fn [x _]
               (degree->color (/ (* 2 360 (+ x x')) 32))))))

(defn send-random
  []
  (send-matrix (random-matrix)))

(defn rainbox-animation
  []
  (doseq [x (range 100)]
    (Thread/sleep 10)
    (send-matrix (rainbox-matrix x))))

(defn s
  []
  (loop [state (snake/init {:width 32 :height 32})]
    (Thread/sleep 10)
    (send-matrix (snake/render state))
    (if (:dead? state)
      (recur (snake/init {:width 32 :height 32}))
      (recur (snake/tick state (agent/best-action state))))))

(defn clear
  []
  (send-buf (byte-array (repeat (* 32 32 3) 0))))