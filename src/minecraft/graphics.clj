(ns minecraft.graphics
  (:import (java.awt Graphics
                     GraphicsConfiguration
                     GraphicsDevice
                     GraphicsEnvironment
                     image.VolatileImage
                     Toolkit)
           (javax.swing ImageIcon))
  (:gen-class))
(def ^{:tag Toolkit} toolkit-default (Toolkit/getDefaultToolkit))
(def ^{:tag GraphicsEnvironment :private true} graphics-environment
  (GraphicsEnvironment/getLocalGraphicsEnvironment))
(def ^{:tag GraphicsDevice :private true} screen-device-default
  (.getDefaultScreenDevice graphics-environment))
(def ^{:tag GraphicsConfiguration :private true} graphics-configuration
  (.getDefaultConfiguration screen-device-default))
;;(.setFullScreenWindow screen-device-default frame)
(def ^{:tag ImageIcon :private true} resource-image
  (ImageIcon. "./resources/resource.png"))
(def ^{:tag VolatileImage} resource-cache
  (.createCompatibleVolatileImage graphics-configuration
                                  (.getIconWidth resource-image)
                                  (.getIconHeight resource-image)))
(def ^{:tag Graphics} resource-cache-graphics
  (.getGraphics resource-cache))
(doto resource-cache-graphics
  ;;build cache
  (.drawImage (.getImage resource-image) 0 0 nil nil))
