;; You are given the following information, but you may prefer to do some
;; research for yourself.
;;
;; 1 Jan 1900 was a Monday.
;; Thirty days has September,
;; April, June and November.
;; All the rest have thirty-one,
;; Saving February alone,
;; Which has twenty-eight, rain or shine.
;; And on leap years, twenty-nine.
;;
;; A leap year occurs on any year evenly divisible by 4, but not on a century
;; unless it is divisible by 400.
;; How many Sundays fell on the first of the month during the twentieth century
;; (1 Jan 1901 to 31 Dec 2000)?

(load "util")

(defn euler-19 [years]
  (count
    (filter #(= 1 %)
            (for [year years
                  month (range 0 12)]
              (.get (java.util.GregorianCalendar. year month 1)
                    java.util.Calendar/DAY_OF_WEEK)))))

(run (euler-19 (range 1900 (inc 2000))))
