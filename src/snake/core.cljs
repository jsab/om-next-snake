(ns ^:figwheel-always snake.core
  (:require [om.next :as om :refer-macros [defui]]
            [sablono.core :refer-macros [html]]
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
    {:food food
     :obstacles obstacles}))

(defn create-snake []
  {:pieces (list {:x 10 :y 10} {:x 9 :y 10})})


(defonce app-state (atom {:screen :title
                          :engine nil
                          :board nil
                          :snake nil
                          :direction :right
                          :dead nil
                          :turns []}))

(declare reconciler)

(defn start-engine! []
  (swap! app-state update-in [:engine]
         #(if-not %
            (. js/window setInterval
               (fn [] (om/transact! reconciler '[(move)])) 300)
            %)))

(defn stop-engine! []
  (swap! app-state update-in [:engine]
         #(if %
            (js/clearInterval %)
            %)))


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
  (let [s (:pieces (:snake @state))
        next (next-position (first s) (:direction @state))
        all-elems (merge (zipmap (:food (:board @state)) (repeat :food))
                         (zipmap (:obstacles (:board @state)) (repeat :obstacle))
                         (zipmap s (repeat :snake)))
        kind (get all-elems next)]
    (case kind
      (:snake
       :obstacle) (do (stop-engine!)
                      (swap! state assoc
                             :dead kind
                             :screen :title))
      :food (swap! state #(-> % (assoc-in [:snake :pieces] (conj s next))
                                (assoc-in [:board :food] (conj (disj (:food (:board @state)) next)
                                                               (first (new-random-coords 1 (keys all-elems)))))))
      (swap! state assoc-in [:snake :pieces] (conj (butlast s) next)))))


(defmulti mutate om/dispatch)


(defmethod mutate :default
  [_ _ _] {:quote true})


(defmethod mutate 'change-screen
  [{:keys [state]} _ {:keys [screen]}]
  {:action #(swap! state assoc :screen screen)})


(defmethod mutate 'new-game
  [{:keys [state]} _ _]
  {:action #(do (swap! state assoc
                       :board (create-board)
                       :snake (create-snake)
                       :direction :right
                       :screen :game)
                (start-engine!))})


(defmethod mutate 'move
  [{:keys [state]} _ _]
  {:action #(move state)})

(defmethod mutate 'turn
  [{:keys [state]} _ {:keys [direction]}]
  {:action #(when (valid-turn? (or (first (:turns @state)) (:direction @state)) direction)
              (swap! state update-in [:turns] conj direction))})


(def reconciler
  (om/reconciler
   {:state app-state
    :parser (om/parser {:read read :mutate mutate})}))


(defn title-screen [c dead]
  [:.title-screen
   (when dead [:.dead (case dead
                        :snake "You ate yourleft."
                        :obstacle "You crashed into an obstacle.")])
   [:a.title-box
    {:on-click #(om/transact! c '[(new-game)])}
    [:.start-game (if dead
                    "Try Again!"
                    "Start Game!")]]])


(defui Piece
  static om/IQuery
  (query [this]
    '[:x :y])

  Object
  (render [this]
    (let [{:keys [x y]} (om/props this)]
      (html [:.piece {:style {:left (str (* (/ 100 rows) x) "vw")
                              :top (str (* (/ 100 cols) y) "vh")}}]))))

(def piece (om/factory Piece))


(defui Snake
  static om/IQueryParams
  (params [this]
    {:piece (om/get-query Piece)})

  static om/IQuery
  (query [this]
    '[{:pieces ?piece}])

  Object
  (render [this]
    (let [{:keys [pieces]} (om/props this)]
      (html [:.snake (map piece pieces)]))))

(def snake (om/factory Snake))


(defui Board
  static om/IQueryParams
  (params [this]
          {:piece (om/get-query Piece)})

  static om/IQuery
  (query [this]
    '[{:food [?piece]} {:obstacles [?piece]}])

  Object
  (render [this]
    (let [{:keys [food obstacles]} (om/props this)]
      (html [:.board
             [:.food (map piece food)]
             [:.obstacles (map piece obstacles)]]))))

(def board (om/factory Board))


(defn key-down-game [c e]
  (when-let [key ((map-invert (select-keys KEY arrows)) (.-keyCode e))]
    (om/transact! c `[(~'turn {:direction ~key})]))
  (when (= (:enter KEY) (.-keyCode e))
    (om/transact! c '[(new-game)])))


(defui Game
  static om/IQueryParams
  (params [this]
    {:snake (om/get-query Snake)
     :board (om/get-query Board)})

  static om/IQuery
  (query [this]
    '[:screen :dead {:board ?board} {:snake ?snake}])

  Object
  (componentDidMount [this]
    (. js/window addEventListener "keydown" #(key-down-game this %)))

  (componentWillUnmount [this]
    (. js/window removeEventListener "keydown" #(key-down-game this %)))

  (render [this]
    (let [props (om/props this)]
      (html (case (:screen props)
              :title (title-screen this (:dead props))
              :game [:.board-and-snake
                     (board (:board props))
                     (snake (:snake props))]
              :end [:.end "END"])))))


(om/add-root! reconciler
              Game (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
