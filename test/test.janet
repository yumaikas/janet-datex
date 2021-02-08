# Tests for datex
(import ../datex :as "dt")
(use testament)

(exercise! []
  # This is the 22nd of January, 2008, a Tuesday
  (def mytime (os/mktime { :year 2008 :month 0 :month-day 21 }))
  (def weekday (dt/weekday-str mytime))

  (deftest weekday-str 
    (assert-equal "tuesday" weekday))

  (deftest weekday-str-2
    (assert-equal "friday"
                  (as-> (os/mktime { :year 2021 :month 0 :month-day 28 }) it
                        (dt/weekday-str it))))

  (deftest parse-ymdstr
    (assert-equal (os/mktime { :year 2021 :month 0 :month-day 29 }) 
                  (dt/parse-ymdstr "2021-1-30")))
  (deftest parse-ymdstr-failure
    (assert-equal nil (dt/parse-ymdstr "2021-A-30")))

  (deftest to-ymdstr
    (assert-equal "2021-1-30"
                  (as-> (os/mktime { :year 2021 :month 0 :month-day 29 }) it
                        (os/date it)
                        (dt/to-ymdstr it))))

  (deftest add-1-day-works 
    (assert-matches {:year 2021 :month 0 :month-day 30 :hours 0 }
                    (as-> (os/mktime { :year 2021 :month 0 :month-day 29 }) it
                          (dt/add-time it :days 1)
                          (os/date it))))

  # DEFERRED: the dt/next api is on hold for now, 
  # since something seems to be off about it, and I can't quite figure out what
  # (def one-day-forward (dt/next "tuesday" :local mytime))
  # (def one-year-forward (dt/next "year" :local mytime))

  #(deftest can-add-one-day
  #  (assert-matches { :year 2008 :month 0 :month-day 22 } (os/date one-day-forward :local)))

  # (deftest can-go-next-year
  # (assert-matches { :year 2009 :month 0 :month-day 0 } (os/date one-year-forward)))

# @task[Create kvx with at least a way to filter a dict down to a given set of keys |status: to-do]
# @task[Get more date parsing stuff in place.]
)
