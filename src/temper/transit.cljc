(ns temper.transit
  (:require [cognitect.transit :as transit] ; brought in by cljs
            [time-literals.read-write]
            #?@(:cljs [; FIXME unattractive copy-paste from time-literals.read-write
                       [java.time :refer [Period
                                          LocalDate
                                          LocalDateTime
                                          ZonedDateTime
                                          OffsetTime
                                          Instant
                                          OffsetDateTime
                                          ZoneId
                                          DayOfWeek
                                          LocalTime
                                          Month
                                          Duration
                                          Year
                                          YearMonth]]]))
  #?(:clj (:import (java.time Period
                              LocalDate
                              LocalDateTime
                              ZonedDateTime
                              OffsetTime
                              Instant
                              OffsetDateTime
                              ZoneId
                              DayOfWeek
                              LocalTime
                              Month
                              Duration
                              Year
                              YearMonth))))

(def time-classes
  {'period Period
   'date LocalDate
   'date-time LocalDateTime
   'zoned-date-time ZonedDateTime
   'offset-time OffsetTime
   'instant Instant
   'offset-date-time OffsetDateTime
   'time LocalTime
   'duration Duration
   'year Year
   'year-month YearMonth
   'zone ZoneId
   'day-of-week DayOfWeek
   'month Month})

(def write-handlers "Handle tick aka java.time clasess."
  {:handlers
   (into {} (for [[tick-class host-class] time-classes]
              [host-class (transit/write-handler (constantly (name tick-class)) str)]))})

(def read-handlers "Handle tick aka java.time classes."
  {:handlers
   (into {} (for [[sym fun] time-literals.read-write/tags]
              [(name sym) (transit/read-handler fun)]))})   ; omit "time/" for brevity