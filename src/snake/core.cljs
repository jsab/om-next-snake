(ns ^:figwheel-always snake.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.next :as om :refer-macros [defui]]
            [om.core :as omo]
            [sablono.core :refer-macros [html]]
            [cljs.core.async :refer [put! <! chan]]
            [clojure.set :refer [map-invert]]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

(def KEY {:escape 27
          :enter 13
          :up 38
          :down 40
          :left 37
          :right 39})

(def arrows [:up :down :left :right])

(defonce rows 20)
(defonce cols 20)

(defn key<-coord [{:keys [x y]}]
  (+ x (* y cols)))

(defn coord<-key [key]
  {:x (mod key cols)
   :y (int (/ key cols))})

(defn random-coord []
  {:x (rand-int cols)
   :y (rand-int rows)})

(defn create-board []
  (apply merge {} (repeatedly 10 #(hash-map (key<-coord (random-coord)) :food))))

(defonce app-state (atom {:screen :title
                          :engine nil
                          :board (create-board)
                          :snake {:pieces (list {:x 10 :y 10} {:x 9 :y 10})
                                  :direction :right}}))

(defn read
  [{:keys [state] :as env} key params]
  (let [st @state]
    (if-let [[_ v] (find st key)]
      {:value v}
      {:value :not-found})))


(defn next-position [position direction]
  (case direction
    :right (update-in position [:x] #(mod (inc %) cols))
    :down  (update-in position [:y] #(mod (inc %) rows))
    :left  (update-in position [:x] #(mod (+ cols (dec %)) cols))
    :up    (update-in position [:y] #(mod (+ rows (dec %)) rows))))


(defn move-snake [state]
  (let [s (:snake @state)
        next (next-position (first (:pieces s)) (:direction s))
        key (key<-coord next)
        kind (get (:board @state) key)]
    (case kind
      :food (swap! state assoc
                   :snake (assoc s :pieces (conj (:pieces s) next))
                   :board (dissoc (:board @state) key))
      (swap! state assoc :snake
             (assoc s :pieces (conj (butlast (:pieces s)) next))))
    ))

(defmulti mutate om/dispatch)

(defmethod mutate :default
  [_ _ _] {:quote true})

(defmethod mutate 'change-screen
  [{:keys [state]} _ {:keys [screen]}]
  {:action #(swap! state assoc :screen screen)})

(defmethod mutate 'start
  [{:keys [state]} _ {:keys [engine]}]
  {:action #(swap! state assoc :engine engine)})

(defmethod mutate 'move
  [{:keys [state]} _ _]
  {:action #(move-snake state)})

(defmethod mutate 'turn
  [{:keys [state]} _ {:keys [direction]}]
  {:action #(do (swap! state assoc-in [:snake :direction] direction))})


(defn title-screen [c]
  [:a.title-box
   {:on-click #(do (om/transact! c '[(change-screen {:screen :game})])
                   (om/transact! c `[(~'start {:engine ~(. js/window setInterval
                                                           (fn [] (om/transact! c '[(move)])) 300)})]))}
   [:.start-game "Start Game!"]])


(defn style<-key [k]
  (let [{:keys [x y]} (coord<-key k)]
    {:style {:left (str (* (/ 100 rows) x) "vw")
             :top (str (* (/ 100 cols) y) "vh")}}))

(defui Snake
  static om/IQuery
  (query [this]
    [:snake])

  Object
  (render [this]
    (let [{:keys [snake] :as props} (om/props this)]
      (html [:.snake
             (for [p (:pieces snake)]
               [:.snake-piece (style<-key (key<-coord p))])]))))

(def snake (om/factory Snake))

(defui Board
  static om/IQuery
  (query [this]
    [:board])

  Object
  (render [this]
    (let [{:keys [board] :as props} (om/props this)]
      (html [:.board
             (for [[k v] board]
               [:.food (style<-key k)])]))))

(def board (om/factory Board))

(defn key-down-game [c e]
  (when-let [key ((map-invert (select-keys KEY arrows)) (.-keyCode e))]
    (om/transact! c `[(~'turn {:direction ~key})])))


(defui Game
  static om/IQueryParams
  (params [this]
    {:snake (om/get-query Snake)
     :board (om/get-query Board)})

  static om/IQuery
  (query [this]
    '[:screen :board :snake])

  Object
  (componentDidMount [this]
    (. js/window addEventListener "keydown" #(key-down-game this %)))

  (componentWillUnmount [this]
    (. js/window removeEventListener "keydown" #(key-down-game this %)))

  (render [this]
    (let [props (om/props this)]
      (html (case (:screen props)
              :title (title-screen this)
              :game [:.board-and-snake
                     (board props)
                     (snake props)]
              :end [:.end "END"])))))


(def reconciler
  (om/reconciler
   {:state app-state
    :parser (om/parser {:read read :mutate mutate})}))


(om/add-root! reconciler
              Game (. js/document (getElementById "app")))




(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
