(ns adventofcode.day7
  "Advent of Code 2015 Day 7 - 'Some Assembly Required'"
  (:require [adventofcode.core :refer [get-lines]]
            [clojure.edn :as edn]
            [clojure.tools.cli :as cli :refer [parse-opts]])
  (:import  [java.io IOException]))


;; For complete puzzle description, http://adventofcode.com/2015/day/7
;; The copy of the description is also found in /resources/puzzle/day7.txt

;; Default input file contains 339 random instructions explained below.
(def input-file-path "resources/input/day7.txt")


;; "Each wire has an identifier (some lowercase letters) and
;;  can carry a 16-bit signal (a number from 0 to 65535)."
(def max-signal 65535)

;; "A signal is provided to each wire by a gate,
;;  another wire, or some specific value."
(defn value-signal?
  "Given signal as Symbol or Long,
   return true if it is a number; false, otherwise."
  [signal]
  (number? signal))

;; Other things to keep in mind:
;; "Each wire can only get a signal from one source,
;;  but can provide its signal to multiple destinations."
;; "A gate provides no signal until all of its inputs have a signal."


;; --- Example: Simple Circuit ---

;; For example, here is a simple circuit:

(def simple-circuit

  '(;; 123 -> x means that the signal 123 is provided to wire x.
    "123 -> x"
    "456 -> y"

    ;; x AND y -> d means that the bitwise AND of wire x and wire y is provided
    ;; to wire d.
    "x AND y -> d"

    ;; x OR y -> e means that the bitwise OR of wire x and wire y is provided
    ;; to wire e.
    "x OR y -> e"

    ;; x LSHIFT 2 -> f means that the value from wire x is left-shifted by 2
    ;; and then provided to wire f.
    "x LSHIFT 2 -> f"

    ;; y RSHIFT 2 -> g means that the value from wire y is right-shifted by 2
    ;; and then provided to wire g.
    "y RSHIFT 2 -> g"

    ;; NOT e -> f means that the bitwise complement of the value from wire e is
    "NOT x -> h"
    "NOT y -> i"))


;; "After it (simple-circuit) is run, these are the signals on the wires:"
(def simple-circuit-signals
  {'d 72
   'e 507
   'f 492
   'g 114
   'h 65412
   'i 65079
   'x 123
   'y 456})

;; --- Prep: Instructions to Circuits (Wire-Signal Pair) ---

(defn add-wire-signal-pair
  "Given circuits as a map of wire signal kv pair
   and circuit instruction as string,
   return the circuits with the new wire-signal kv pair
   based on the given instruction."
  [circuits instruction]
  (let [;; read an instruction wrapped by parens
        ;; as a list of words that are either symbols or long
        words      (edn/read-string (str "(" instruction ")"))

        ;; Symbol for "(signal) connect to (wire)" is ->
        connect-to '->

        ;; wire is the first word after connect-to Symbol in the instruction
        ;; list of words (e.g. x in (a AND b -> x))
        wire       (first (rest (drop-while #(not= connect-to %) words)))

        ;; a signal is what is provided to the wire and
        ;; a list of words up to '->
        ;; (e.g. (123) in (123 '-> 'x) or ('a 'AND 'b) in ('a 'AND 'b '-> 'x))
        signal     (take-while #(not= connect-to %) words)]

    ;; If the main function accepts an input file other than provided
    ;; by the puzzle, validating its format such as existence of ->
    ;; may be made here or right after reading the input file.
    ;; (Future Wish List: Look into clojure.spec)
    ;; (when (or (empty? words) (empty? signal) ... )
    ;;   (throw (Exception. "Invalid signal and/or wire format.")))

    ;; add a new wire-signal pair to the circuits
    ;; so that a signal can be looked up by a wire
    (assoc circuits wire signal)))


(defn all-circuits
  "Given instructions as a map of wire as Symbol and signal
   as a list of Symbols or numbers,
   return a map of wire-signal kv pair where a wire is an identifier and
   value is a list of strings which represents either gate, another wire,
   or a signal.
   (e.g. ('123 -> x' 'a AND b -> y') returns {'x 123 'y ('a 'AND 'b)}"
  [instructions]
  (reduce add-wire-signal-pair {} instructions))


;; --- Step 1. Find Value Signals ---

(defn process-value-signal
  "This function takes two vectors. First, a vector of maps that are (solved)
   signals and (unsolved) circuits, respectively.  Second, a vector of wire
   as Symbol and signal as a list of words that are either Symbol or numeric
   value. It returns a single numeric value that is a signal connected to
   the given wire.
   (All maps consists of wire as key and signal as value.)"
  [[signals circuits] [wire signal]]
  (let [word1 (first signal)]

    ;; if signal is just a single word which represents value signal
    (if (and (= (count signal) 1) (value-signal? word1))

      ;; add this wire-signal pair to (solved) signals and
      ;; remove from (unsolved) circuits
      [(conj signals {wire word1}) (dissoc circuits wire)]

      ;; otherwise, return a vector of signals and circuits as they are
      [signals circuits])))


;; --- Step 2. Apply Value Signals ---

;; Possible bitwise operations are NOT, AND, OR, LSHIFT, and RSHIFT
(def op
  {;; bitwise operation NOT for unsigned integers is
   ;; maximum value (e.g. 2^16) - a given integer
   'NOT    (fn [n] (- max-signal n))
   ;; equivalent to (fn [n] (+ max-signal (bit-not n) 1))

   ;; bitwise operation AND (takes two values/wires)
   'AND    bit-and

   ;; bitwise operation OR (takes two values/wires)
   'OR     bit-or

   ;; bitwise operation LSHIFT (takes value/wire and a number of bits)
   'LSHIFT bit-shift-left

   ;; bitwise operation RSHIFT (takes value/wire and a number of bits)
   'RSHIFT bit-shift-right})

(defn get-identifiers
  "Given instruction as a list of words that are either Symbol or numeric value,
  return a sequence of identifiers."
  [instruction]
  (remove #(or (op %) (value-signal? %)) instruction))

(defn signal->value-signal
  "Given signal as a list of one to three words that represent either gate or
   numeric value (i.e. assumption: no wire is included), return a value signal."
  [[word1 & [word2 word3]]]

  ;; if the first word is a value signal
  (if (value-signal? word1)

    ;; if the second word exists
    (if word2

      ;; it is a bitwise operation with two operands
      ;; compute value signal from this gate and two value signals
      ((op word2) word1 word3)

      ;; else it is already a value signal
      ;; (i.e. a wire has a numeric value signal)
      word1)

    ;; if the first word is not a value signal,
    ;; it is NOT operation with a single operand
    ;; compute value signal from this gate and a value signal
    ((op word1) word2)))

(defn apply-value-signals*
  "This function takes two vectors. First, a vector of maps that are (solved)
   signals and (unsolved) circuits, respectively.  Second, a vector of wire
   as Symbol and signal as a list of words that are either Symbol or numeric
   value. It applies value signals into any applicable identifiers in signal
   first. If the signal still contains at least one identifier, it updates
   circuits with the new signal.  If not, it adds the new signal to (solved)
   signals and removes it from (unsolved) circuit. It then returns a a vector
   of signals and circuits with the same format as the first argument.
   (All maps consist of wire as key (Symbol) and signal (a list of Symbols
   or number) as value.)"
  [[signals circuits] [wire signal]]
  ;; new signal is one all the value signals are applied to a given signal
  (let [new-signal (map #(if-let [new-val (signals %)] new-val %) signal)]

    ;; if no value signals were applied to any Symbols in this signal list
    (if (= new-signal signal)

      ;; return both signals and circuits as they are
      [signals circuits]

      ;; if new signal list has at least one identifier
      ;; (i.e. not a value signal or a symbol)
      (if (seq (get-identifiers new-signal))

        ;; update the circuits with the new signal
        [signals (assoc circuits wire new-signal)]

        ;; else new signal is ready to be connected to the wire and
        ;; added to signals and remove the wire from the circuits list
        [(conj signals {wire (signal->value-signal new-signal)})
         (dissoc circuits wire)]))))

(defn apply-value-signals
  "Given a vector of maps that represent signals and circuits,
   return a vector of maps such that signals and circuits are updated
   as follows:
     1. All values in signals are applied to any Symbol/wire in each
        circuit where applicable.
     2. If the circuit becomes a value signal, move the circuit from
        circuits to signals.
     3. If part of the circuit becomes a value signal, update circuits."
  [[signals circuits]]
  (reduce apply-value-signals* [signals circuits] circuits))


;; --- Assemble Circuits ---

(defn assemble-circuits
  "Given (unsolved) circuits that is a map of wire and signal as kv pair,
   return all circuits are assembled, have signals, and are lit!"
  [circuits]

  ;; Initially pass an empty signals (i.e. no lit wires) and
  ;; the given circuits to assemble
  (loop [[signals circuits] [{} circuits]]

    ;; Repeat the following process until the circuits is empty
    (if (seq circuits)

      ;; First, find all wires with value signals without a logic gate
      ;; and move them to the signals (solved) from the circuits (unsolved)
      ;; Second, apply the found value signals/wires to the remaining circuits
      (recur (apply-value-signals
              (reduce process-value-signal [signals circuits] circuits)))
      ;; When no more ciircuits left to assemble,
      ;; return the solved circuits with signals.
      signals)))


(def cli-options
  [["-e" "--example EXAMPLE" "Example"
    :id :example]
   ["-p" "--part PART" "Part"
    :id :part
    :parse-fn #(Integer/parseInt %)
    :validate [#{1 2} "Must be either 1 or 2."]]])


(defn part2
  "Given lit signals from Part 1 and original circuits,
   return the lit signals to solve Part 2"
  [all-signals all-circuits]
       ;; "take the signal you got on wire a"
  (->> (all-signals 'a)
       ;; (the format of instructions for signal is a list of Symbol or number)
       list
       ;; "override wire b to that signal and reset the other wires
       ;;  (including wire a)"
       (assoc all-circuits 'b)
       ;; re-assemble circuits
       assemble-circuits))


(defn -main
  "This is the entry point of Advent of Code 2015 Day 7 solutions.
   It can take optional arguments as follows:
      -e with any value to solve example circuits and print
      -p 1 to solve Part 1 of the puzzle and print
      -p 2 to solve Part 2 of the puzzle and print
   No arguments will return the entire lit signals for Part 1."
  [& args]
  (try
    (let [{{:keys [example part]} :options} (cli/parse-opts args cli-options)
          instructions                      (if example
                                              simple-circuit
                                              (get-lines input-file-path))
          all-circuits                      (all-circuits instructions)
          all-signals                       (assemble-circuits all-circuits)]

      (case part
        ;; --- Part One ---
        ;; "what signal is ultimately provided to wire a?"
        1 (println "Part 1: Signal value" (all-signals 'a) "is provided to wire a")

        ;; --- Part Two ---
        ;; "Now, take the signal you got on wire a, override wire b
        ;;  to that signal, and reset the other wires (including wire a).
        ;;  What new signal is ultimately provided to wire a?"
        2 (println "Part 2: Signal value" ((part2 all-signals all-circuits) 'a)
                   "is provided to wire a")

        ;; Prints the entire signals either for example or Part 1
        (println "All wires are assembled as follows:" all-signals)))

    (catch IOException e (println "IOException:" (.getMessage e)))
    (catch Exception e (println "Exception:" (.getMessage e)))))

;; Wish List: answer check?
