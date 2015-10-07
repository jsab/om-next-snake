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

(defn random-coord []
  {:x (rand-int cols)
   :y (rand-int rows)})

(defn new-random-coords [n old]
  (let [old (into #{} old)]
    (loop [result #{}]
      (if (= n (count result))
        result
        (let [candidate (random-coord)]
          (if (old candidate)
            (recur result)
            (recur (conj result candidate))))))))

(defn create-board []
  (let [obstacles (new-random-coords 10 #{})
        food (new-random-coords 10 obstacles)]
    (merge
     (zipmap obstacles (repeat :obstacle))
     (zipmap food (repeat :food)))))


(defonce app-state (atom {:screen :title
                          :engine nil
                          :board nil
                          :snake nil
                          :direction :right
                          :dead nil
                          :turns []}))

(defn init-game [state]
  (swap! state assoc
         :board (create-board)
         :snake (list {:x 10 :y 10} {:x 9 :y 10})))

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


(defn valid-turn? [old new]
  ((case old
     (:right :left) #{:up :down}
     (:up :down) #{:right :left}) new))


(defn turn [state]
  (let [old (:direction @state)]
    (when-let [new (last (:turns @state))]
      (when (valid-turn? old new)
        (swap! state assoc
               :direction new
               :turns (butlast (:turns @state)))))))


(defn move [state]
  (turn state)
  (let [s (:snake @state)
        next (next-position (first s) (:direction @state))
        all-elems (merge (:board @state) (zipmap s (repeat :snake)))
        kind (get all-elems next)]
    (case kind
      (:snake
       :obstacle) (do (swap! state update-in [:engine] js/clearInterval)
                      (swap! state assoc
                             :dead kind
                             :screen :title))
      :food (swap! state assoc
                   :snake (conj s next)
                   :board (assoc (dissoc (:board @state) next)
                                 (first (new-random-coords 1 (keys all-elems))) :food))
      (swap! state assoc :snake (conj (butlast s) next)))))

(defmulti mutate om/dispatch)

(defmethod mutate :default
  [_ _ _] {:quote true})

(defmethod mutate 'change-screen
  [{:keys [state]} _ {:keys [screen]}]
  {:action #(swap! state assoc :screen screen)})

(defmethod mutate 'start
  [{:keys [state]} _ {:keys [engine]}]
  {:action #(do
              (init-game state)
              (swap! state assoc :engine engine))})

(defmethod mutate 'move
  [{:keys [state]} _ _]
  {:action #(move state)})

(defmethod mutate 'turn
  [{:keys [state]} _ {:keys [direction]}]
  {:action #(when (valid-turn? (or (first (:turns @state)) (:direction @state)) direction)
              (println "STACKING TURN")
              (swap! state update-in [:turns] conj direction))})


(defn title-screen [c dead]
  [:.title-screen
   (when dead [:.dead (case dead
                        :snake "You ate yourleft."
                        :obstacle "You crashed into an obstacle.")])
   [:a.title-box
    {:on-click #(do (om/transact! c '[(change-screen {:screen :game})])
                    (om/transact! c `[(~'start {:engine ~(. js/window setInterval

                                                            (fn [] (om/transact! c '[(move)])) 300)})]))}
    [:.start-game (if dead
                    "Try Again!"
                    "Start Game!")]]])


(defn style<-coord [{:keys [x y]}]
  {:style {:left (str (* (/ 100 rows) x) "vw")
           :top (str (* (/ 100 cols) y) "vh")}})

(defui Snake
  static om/IQuery
  (query [this]
    [:snake])

  Object
  (render [this]
    (let [{:keys [snake] :as props} (om/props this)]
      (html [:.snake
             (for [p snake]
               [:.snake-piece (style<-coord p)])]))))

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
               [:div (merge {:class (name v)} (style<-coord k))])]))))

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
    '[:screen :board :snake :dead])

  Object
  (componentDidMount [this]
    (. js/window addEventListener "keydown" #(key-down-game this %)))

  (componentWillUnmount [this]
    (. js/window removeEventListener "keydown" #(key-down-game this %)))

  (render [this]
    (let [{:keys [dead] :as props} (om/props this)]
      (html (case (:screen props)
              :title (title-screen this dead)
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
