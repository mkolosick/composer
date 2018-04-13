#lang s-exp "../lib/music.rkt"

(chord-names
 I [0 4 7]
 I [0 4]
 V7/IV [0 4 7 10]
 ii [2 3 7]
 ii6 [5 4 9]
 V [7 4 7]
 V7 [7 4 7 10]
 V/V [2 4 7]
 iii [4 3 7]
 I6 [4 3 8]
 I6 [4 8]
 viio [11 3 6]
 viio6 [2 3 9]
 vi [9 3 7]
 IV [5 4 7])

(progressions
 (I (ii I iii ii6 viio6 vi V/V))
 (vi V/V)
 (V (I V7/IV V I))
 (ii V)
 (I6 ii6)
 (V/V V)
 (iii V)
 (V7/IV IV)
 (viio6 I6)
 (ii6 V)
 (V7 I))

(pivots
 (V/V V)
 (V7/IV V7))