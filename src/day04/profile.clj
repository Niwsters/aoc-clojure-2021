(ns day04.profile
  (:require [day04.input]
            [day04.part1 :as part1]
            [taoensso.tufte :as tufte]))

;; We'll request to send `profile` stats to `println`:
(tufte/add-basic-println-handler! {})

;;; Let's define a couple dummy fns to simulate doing some expensive work
;; How do these fns perform? Let's check:

(tufte/profile ; Profile any `p` forms called during body execution
  {} ; Profiling options; we'll use the defaults for now
  (tufte/p (part1/part1 day04.input/input)))
