;
; Snake for 6502
;
; change direction: W A S D
;

define appleL    $00    ; screen location of apple, low byte
define appleH    $01    ; screen location of apple, high byte

define headL    $10    ; screen location of snake head, low
define headH    $11    ; screen location of snake head, high

define bodyStart $12 ; start of snake body byte pairs
define snakeDir  $02 ; direction (possible values below)
define snakeLen  $03 ; snake length, in bytes

; directions (each using a separate bit)
define movingUp     1
define movingRight  2
define movingDown   4
define movingLeft   8

; ASCII values of keys controlling the snake
define ASCII_w      $77
define ASCII_a      $61
define ASCII_s      $73
define ASCII_d      $64

; system variables
define sysRandom    $fe
define sysLastKey   $ff

;
; main program
;

  jsr init
  jsr loop


init:
  jsr initSnake
  jsr generateApplePosition
  rts


initSnake:
  lda #movingRight ; starting direction
  sta snakeDir

  lda #4 ; start length (2 segments)
  sta snakeLen

  lda #$11
  sta headL

  lda #$10
  sta bodyStart

  lda #$0f
  sta $14 ; body segment 1 hmmm

  lda #$04
  sta headH
  sta $13 ; body segment 1
  sta $15 ; body segment 2
  rts

generateApplePosition:
  ; load a new random byte into $00
  lda sysRandom
  sta appleL

  ; load a new  random number from  2 to 5 into $01
  lda sysRandom
  and #$03  ; mask out lowest 2 bits
  clc
  adc #2
  sta appleH

  rts


loop:
  jsr readKeys
  jsr checkCollision
  jsr updateSnake
  jsr drawApple
  jsr drawSnake
  jsr spinWheels
  jmp loop


readKeys:
  lda sysLastKey
  cmp #ASCII_w
  beq upKey
  cmp #ASCII_d
  beq rightKey
  cmp #ASCII_s
  beq downKey
  cmp #ASCII_a
  beq leftKey
  rts

upKey:
  lda #movingDown
  bit snakeDir
  bne illegalMove

  lda #movingUp
  sta snakeDir
  rts

rightKey:
  lda #movingLeft
  bit snakeDir
  bne illegalMove

  lda #movingRight
  sta snakeDir
  rts

downKey:
  lda #movingUp
  bit snakeDir
  bne illegalMove

  lda #movingDown
  sta snakeDir
  rts

leftKey:
  lda #movingRight
  bit snakeDir
  bne illegalMove
  lda #movingLeft
  sta snakeDir
  rts

illegalMove:
  rts


checkCollision:
  jsr checkAppleCollision
  jsr checkSnakeCollision
  rts

checkAppleCollision:
  lda appleL
  cmp headL
  bne doneCheckingAppleCollision
  lda appleH
  cmp headH
  bne doneCheckingAppleCollision

  ; eat apple
  inc snakeLen
  inc snakeLen
  jsr generateApplePosition
doneCheckingAppleCollision:
  rts


checkSnakeCollision:
  ldx #2 ; start with 2nd segment
snakeCollisionLoop:
  lda headL,x
  cmp headL
  bne continueCollisionLoop
maybeCollided:
  lda headH,x
  cmp headH
  beq didCollide
continueCollisionLoop:
  inx
  inx
  cpx snakeLen ; got to last section with no collision
  beq didntCollide
  jmp snakeCollisionLoop
didCollide:
  jmp gameOver
didntCollide:
  rts


updateSnake:
  ldx snakeLen
  dex
  txa
updateLoop:
  lda headL,x
  sta bodyStart,x
  dex
  bpl updateLoop

  lda snakeDir
  lsr
  bcs up
  lsr
  bcs right
  lsr
  bcs down
  lsr
  bcs left
up:
  lda headL
  sec
  sbc #$20
  sta headL
  bcc upup
  rts
upup:
  dec headH
  lda #$1
  cmp headH
  beq collision
  rts
right:
  inc headL
  lda #$1f
  bit headL
  beq collision
  rts
down:
  lda headL
  clc
  adc #$20
  sta headL
  bcs downdown
  rts
downdown:
  inc headH
  lda #$6
  cmp headH
  beq collision
  rts
left:
  dec headL
  lda headL
  and #$1f
  cmp #$1f
  beq collision
  rts
collision:
  jmp gameOver


drawApple:
  ldy #0
  lda sysRandom
  sta (appleL),y
  rts


drawSnake:
  ldx snakeLen
  lda #0
  sta (headL,x) ; erase end of tail

  ldx #0
  lda #1
  sta (headL,x) ; paint head
  rts

spinWheels:
  ldx #0
spinLoop:
  nop
  nop
  dex
  bne spinLoop
  rts


gameOver:
  brk

