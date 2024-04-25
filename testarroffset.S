# this is just to test if the array can be accessed correctly
.data
.align 2
arr: .word 8
arr_body: .space 32

.text

main:
  la   t0, arr_body
  li   t1, 42
  sw   t1, 4(t0)
  # now try another way to access the array
  li   t6, 4
  lw   t5, arr_body(t6) # expect 42
  # test if the values are the same
  beq  t1, t5, done
  j    fail

