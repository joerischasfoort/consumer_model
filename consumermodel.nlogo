turtles-own [preference beta gamma price service quality alpha1 alpha2 alpha3 needtoshop choice]

globals [order priceS serviceS qualityS conc avgclustering avgorder avgnoshop avgred avgblue avggreen]


;;;;;;;;;;;;;;;;;;;;;;;;
; experimental settings
; - Beta expresses the chance of randomly going shopping to avoid getting stuck at one store
;   Beta ranges between 0 (purely random selection) to 2 (deterministic shopping)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  clear-all
  reset-ticks
  set priceS (list pricered priceblue pricegreen)
  set serviceS (list servicered serviceblue servicegreen)
  set qualityS (list qualityred qualityblue qualitygreen)
; here price, sevice and quality are set for redmarket, bluemarket and greenmarket
; values range from 0.0 to 1.0, the higher the score, the better it is (vector preferences)
; higher levels indicate lower price, better service and better quality
  set-default-shape turtles "circle"
  let teller 0
  ;; make the initial network of two turtles and an edge
  make-node nobody        ;; first node, unattached
  make-node turtle 0      ;; second node, attached to first node

  repeat (nragents - 2) [make-node find-partner ] ; thus 200 turtles are generated in a social network
  ask links [ set color gray ]
  set avgclustering 0
  set avgorder 0
  set avgnoshop 0
  set avgred 0
  set avgblue 0
  set avggreen 0
  setup-agents
end

to setup-agents
; inirialization of the value of the agents
let help 0
  ask turtles [
  set gamma perceptual_noise
;  set gamma 0.1 + random-float 0.4
  set preference [0.1 0.1 0.1]
  set alpha1 random-float 1.0
  set alpha2 random-float 1.0
  set alpha3 random-float 1.0
  set help alpha1 + alpha2 + alpha3
  set alpha1 alpha1 / help
  set alpha2 alpha2 / help
  set alpha3 alpha3 / help

  set needtoshop 0
  set beta exploration]
end

;; used for creating a new node
to make-node [old-node]
  crt 1
  [
    set color red
    if old-node != nobody
      [ create-link-with old-node [ set color green ]
        ;; position the new node near its partner
        move-to old-node
        fd 8
      ]
  ]
end

;; The idea behind the code is a bit tricky to understand.
;; Basically we take the sum of the degrees (number of connections)
;; of the turtles, and that's how many "tickets" we have in our lottery.
;; Then we pick a random "ticket" (a random number).  Then we step
;; through the turtles to figure out which node holds the winning ticket.
to-report find-partner
  let total random-float sum [count link-neighbors] of turtles
  let partner nobody
  ask turtles
  [
    let nc count link-neighbors
    ;; if there's no winner yet...
    if partner = nobody
    [
      ifelse nc > total
        [ set partner self ]
        [ set total total - nc ]
    ]
  ]
  report partner
end
;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;

to go
; now just one product, in future a portfolio of products
; each agent have same products and speed of using it (same family size)
  ask turtles [
   set needtoshop needtoshop + random-poisson 10
   ifelse needtoshop > shoppingthreshold [
   consumer
   set needtoshop 0][set color yellow]
  ]
  wom
  tick
  plotresults
  set priceS (list pricered priceblue pricegreen)
  set serviceS (list servicered serviceblue servicegreen)
  set qualityS (list qualityred qualityblue qualitygreen)
  if ticks > 100000 [stop]
end

to consumer
let sumP 0
let teller 0
let randomP 0
let prefcons 0
let utility 0

    ;; chose seller
    set sumP 0
    set teller 0
    while [teller < 3]
    [
      set sumP sumP + e ^ (beta * (item teller preference))
      set teller teller + 1
    ]

    set randomP random-float sumP
    set sumP e ^ (beta * (item 0 preference))
    set choice 0

    while [sumP <= randomP and choice < 2]
    [
      set choice choice + 1
      set sumP sumP + e ^ (beta * (item choice preference))
    ]
    if choice = 0 [set color red]
    if choice = 1 [set color blue]
    if choice = 2 [set color green]

    set teller 0
    while [teller < 3]
    [
      set prefcons item teller preference
      set utility item teller priceS * alpha1 + item teller serviceS * alpha2 + item teller qualityS * alpha3
      ifelse (choice = teller) [
        set prefcons prefcons * (1 - gamma) + utility][
        set prefcons prefcons * (1 - gamma)]
      set preference replace-item teller preference prefcons
      set teller teller + 1
    ]
end

to wom
; word of mouth
; note that I assume that people with more neighbors are more influenced since I assume the sum of preferences. An alternative would be to scale it to the relative preference by dividing by the number of neighbors
let teller 0
let prefdum 0
let neighborpreference 0
  ask turtles [
    set teller 0
    while [teller < 3]
    [
      set prefdum item teller preference
      set neighborpreference 0
      ask link-neighbors [ set neighborpreference neighborpreference + item teller preference ]
      set neighborpreference neighborpreference / count link-neighbors
      set prefdum prefdum * (1 - norminfluence) + norminfluence * neighborpreference
      set preference replace-item teller preference prefdum
      set teller teller + 1
    ]
  ]
end

to plotresults
let prefs 0
let sprefs 0
let teller 0

  set order 0
  ask turtles [
    set prefs 0
    set sprefs 0
    set teller 0
    while [teller < 3]
    [
      set prefs prefs + item teller preference * item teller preference
      set sprefs sprefs + item teller preference
      set teller teller + 1
    ]
    set sprefs sprefs * sprefs
    set order order + prefs / sprefs
  ]
  set order order / nragents



  let actchoice 0
  let nrc 0
  let nrn 0
  ask turtles [
    set actchoice choice
    ask link-neighbors [
      set nrn nrn + 1
      if choice = actchoice [set nrc nrc + 1]]
  ]
  set conc nrc / nrn
  set-current-plot "clustering"
  plot conc

  set-current-plot "marketshares"
  set-current-plot-pen "no shopping"
  plot count turtles with [color = yellow]
  set-current-plot-pen "red"
  plot count turtles with [color = red]
  set-current-plot-pen "blue"
  plot count turtles with [color = blue]
  set-current-plot-pen "green"
  plot count turtles with [color = green]
  if ticks > 10 [
    set avgclustering avgclustering + 0.002 * conc
    set avgorder avgorder + 0.002 * order
    set avgnoshop avgnoshop + 0.002 * count turtles with [color = yellow]
    set avgred avgred + 0.002 * count turtles with [color = red]
    set avgblue avgblue + 0.002 * count turtles with [color = blue]
    set avggreen avggreen + 0.002 * count turtles with [color = green]
  ]
end

to layout
  ;; the number 3 here is arbitrary; more repetitions slows down the
  ;; model, but too few gives poor layouts
  repeat 3 [
    ;; the more turtles we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
    display  ;; for smooth animation
  ]
  ;; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end
@#$#@#$#@
GRAPHICS-WINDOW
181
33
701
574
25
25
10.0
1
10
1
1
1
0
0
0
1
-25
25
-25
25
1
1
1
ticks
30.0

BUTTON
6
25
72
58
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
5
104
82
137
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
6
156
91
189
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
11
326
90
371
# of nodes
count turtles
3
1
11

SLIDER
6
542
178
575
perceptual_noise
perceptual_noise
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
6
462
178
495
exploration
exploration
0
2
1
0.01
1
NIL
HORIZONTAL

SLIDER
10
380
182
413
nragents
nragents
0
1000
210
1
1
NIL
HORIZONTAL

BUTTON
5
64
76
97
NIL
layout
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
7
421
180
454
shoppingthreshold
shoppingthreshold
0
100
0
1
1
NIL
HORIZONTAL

SLIDER
6
504
178
537
norminfluence
norminfluence
0
1
0.75
0.01
1
NIL
HORIZONTAL

PLOT
708
34
1260
202
marketshares
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"no shopping" 1.0 0 -1184463 true "" ""
"red" 1.0 0 -2674135 true "" ""
"blue" 1.0 0 -13345367 true "" ""
"green" 1.0 0 -10899396 true "" ""

SLIDER
709
451
842
484
pricered
pricered
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
708
489
842
522
priceblue
priceblue
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
706
529
840
562
pricegreen
pricegreen
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
848
452
973
485
servicered
servicered
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
848
489
973
522
serviceblue
serviceblue
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
848
530
975
563
servicegreen
servicegreen
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
980
451
1105
484
qualityred
qualityred
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
980
489
1106
522
qualityblue
qualityblue
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
981
530
1106
563
qualitygreen
qualitygreen
0
1
0.5
0.1
1
NIL
HORIZONTAL

PLOT
709
210
1258
360
clustering
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

@#$#@#$#@
## WHAT IS IT?

## HOW IT WORKS

## HOW TO USE IT

## THINGS TO NOTICE

## THINGS TO TRY

## EXTENDING THE MODEL
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3
@#$#@#$#@
set layout? false
set plot? false
setup repeat 300 [ go ]
repeat 100 [ layout ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment 1" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1001"/>
    <metric>avgclustering</metric>
    <metric>avgorder</metric>
    <metric>avgnoshop</metric>
    <metric>avgred</metric>
    <metric>avgblue</metric>
    <metric>avggreen</metric>
    <enumeratedValueSet variable="nragents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pricegreen">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="serviceblue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="servicered">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="servicegreen">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qualityblue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perceptual_noise">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="priceblue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shoppingthreshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qualityred">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qualitygreen">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norminfluence">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pricered">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment 3" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
  set pricered 0.5
  set pricegreen 0.5
  set priceblue 0.5
  set servicered 0.5
  set servicegreen 0.5
  set serviceblue 0.5
  set qualityred 0.5
  set qualitygreen 0.5
  set qualityblue 0.5</setup>
    <go>if ticks = 100 [ set pricered 0.7]
  if ticks = 200 [ set pricered 0.5]
  ask turtles [
   set needtoshop needtoshop + random-poisson 10
   ifelse needtoshop &gt; shoppingthreshold [
   consumer
   set needtoshop 0][set color yellow]
  ]
  wom
  tick
  plotresults
  set priceS (list pricered priceblue pricegreen)
  set serviceS (list servicered serviceblue servicegreen)
  set qualityS (list qualityred qualityblue qualitygreen)
  if ticks &gt; 1000 [stop]</go>
    <timeLimit steps="1001"/>
    <metric>avgclustering</metric>
    <metric>avgorder</metric>
    <metric>avgnoshop</metric>
    <metric>avgred</metric>
    <metric>avgblue</metric>
    <metric>avggreen</metric>
    <enumeratedValueSet variable="nragents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pricegreen">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="serviceblue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="servicered">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="servicegreen">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qualityblue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perceptual_noise">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="priceblue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shoppingthreshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qualityred">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qualitygreen">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norminfluence">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pricered">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment 2" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1001"/>
    <metric>avgclustering</metric>
    <metric>avgorder</metric>
    <metric>avgnoshop</metric>
    <metric>avgred</metric>
    <metric>avgblue</metric>
    <metric>avggreen</metric>
    <enumeratedValueSet variable="nragents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pricegreen">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="serviceblue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="servicered">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="servicegreen">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qualityblue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perceptual_noise">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="priceblue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shoppingthreshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qualityred">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qualitygreen">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norminfluence">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pricered">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment 4" repetitions="1" runMetricsEveryStep="false">
    <setup>setup
  set pricered 0.5
  set pricegreen 0.5
  set priceblue 0.5
  set servicered 0.5
  set servicegreen 0.5
  set serviceblue 0.5
  set qualityred 0.5
  set qualitygreen 0.5
  set qualityblue 0.5</setup>
    <go>if ticks = 100 [ set pricered 0.7]
  if ticks = 200 [ set pricered 0.5]
  ask turtles [
   set needtoshop needtoshop + random-poisson 10
   ifelse needtoshop &gt; shoppingthreshold [
   consumer
   set needtoshop 0][set color yellow]
  ]
  wom
  tick
  plotresults
  set priceS (list pricered priceblue pricegreen)
  set serviceS (list servicered serviceblue servicegreen)
  set qualityS (list qualityred qualityblue qualitygreen)
  if ticks &gt; 1000 [stop]</go>
    <timeLimit steps="1001"/>
    <metric>avgclustering</metric>
    <metric>avgorder</metric>
    <metric>avgnoshop</metric>
    <metric>avgred</metric>
    <metric>avgblue</metric>
    <metric>avggreen</metric>
    <enumeratedValueSet variable="nragents">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="exploration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pricegreen">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="serviceblue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="servicered">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="servicegreen">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qualityblue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="perceptual_noise">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="priceblue">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shoppingthreshold">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qualityred">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="qualitygreen">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="norminfluence">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pricered">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
