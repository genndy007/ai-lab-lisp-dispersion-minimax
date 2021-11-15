; 1. Вам потрібно по вашим статистичним даним порахувати математичне 
; сподівання кількості часу та дисперсію очок. 

(import [pandas :as pd])

(setv path "data.csv")
(setv data (pd.read_csv path))


(setv time (get data "time"))
(print "Ozhidanie:" (.mean time))

(setv score (get data "score"))
(print "Dispersiya:" (.var score))

