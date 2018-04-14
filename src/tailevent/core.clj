(ns tailevent.core)

(def white (map str [1 1 1]))

(def state (atom {:units {}}))

;;

(defn get-unit
  [state unit]
  (get-in state [:units unit]))

(defn unit-sent-poll
  [state unit]
  (when-not (get-unit state unit)
    (println (str "Unit: '" unit "' joined for the first time.")))
  (update state :units assoc unit
          {:last-poll (System/currentTimeMillis)}))

(defn units-that-timed-out
  [state timeout-millis]
  )

(defn remove-timed-out-units
  [state timeout-millis]
  (let [epoch           (System/currentTimeMillis)
        timed-out-units (filter #(> (- epoch (:last-poll (get (:units state) %)))
                                    timeout-millis)
                                (keys (:units state)))]
    (doseq [unit timed-out-units]
      (println (str "Unit: '" unit "' timed out. Removing it."))
      (let [alias (get-in state [:units unit :alias])]
        (when alias
          (println (str "Alias: '" alias "' removed.")))))
    (assoc state :units (apply dissoc (:units state) timed-out-units))))

(defn alias->unit
  [state alias]
  (let [units (:units state)]
    (first (filter #(= (get-in units [% :alias]) alias) (keys units)))))

(defn create-alias
  [state alias unit-name]
  (let [unit (get-unit state unit-name)]
    (if unit
      (do
        (if (:alias unit)
          (println (str "Renaming " (:alias unit) " -> " alias))
          (println (str "Settings alias of unit: '" unit-name "' to: '" alias "'"))
          )
        (let [new-state (assoc-in state [:units unit-name :alias] alias)
              old-unit-name (alias->unit state alias)]
          (if old-unit-name
            (do
              (println (str "Removing alias of unit: '" old-unit-name "'"))
              (assoc-in new-state [:units old-unit-name :alias] nil))
            new-state)))
      (do
        (println (str "Warning: Unit: '" unit "' does not exist."))
        state))))

(defn first-free-unit
  [state]
  (first (remove #(get-in (:units state) [% :alias]) (keys (:units state)))))

(defn create-alias-next-free-unit
  [state alias]
  (let [unit (first-free-unit state)]
    (if unit (create-alias state alias unit)
         (do
           (println (str "Warning: No free units for: '" alias '"."))
           state))))

;;

(defn set-color
  [state alias color]
  (let [unit (alias->unit state alias)]
    (if unit
      (do (println (str "Setting color of unit with alias: '" alias "' to: " color))
        (assoc-in state [:units unit :color] color))
      (do (println (str "No such alias: '" alias "'")) state))))

(defn poll-color
  [state alias]
  (let [unit (alias->unit state alias)]
    (if unit (or (get-in state [:units unit :color]) white)
        (println (str "No such alias: '" alias "'")))))


;;


(def -example-led 	"[15:46:45 INFO]: [@pluizer] :LED richard 1 0 0")
(def -example-koppel 	"[15:46:45 INFO]: [@pluizer] :KOPPEL richard")

(defn led-command
  [context alias r g b]
  (set-color (:state context) alias [r g b]))

(defn koppel-command
  [context alias]
  (create-alias-next-free-unit (:state context) alias))

(def -commands {:led    led-command
                :koppel koppel-command})

;;

(defn tail-file
  [filename event-fn continue?-fn interval]
  (let [reader (java.io.BufferedReader. (java.io.FileReader. filename))]
    (future
      (loop []
        (let [line (.readLine reader)]
          (when line (event-fn line))
          (Thread/sleep interval)
          (when-not (continue?-fn line) (recur)))))))

(defn get-command
  [line commands]
  (let [[_ user body] (re-matches #"\[.*\]: \[@*(.*)\] :\s*(.*)"
                                  (clojure.string/lower-case line))
        body          (clojure.string/split body #" ")
        fn-name       (first body)
        args          (rest body)]
    {:user  user
     :state state
     :fn    ((keyword fn-name) commands)
     :args  args}))

(defn command->fn
  [command]
  (when (:fn command)
    (fn [state]
      (apply (:fn command)
             (cons (assoc command :state state) (:args command))))))

;;

(def must-run true)

(defn start-polling
  []
  (tail-file "/tmp/log"
             #(do
                (println %)
                (reset! state ((command->fn (get-command % -commands)) @state)))
             (fn [_] must-run)
             100))
