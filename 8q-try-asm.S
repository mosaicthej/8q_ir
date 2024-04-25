# these are assumptions for existing global arrays:
.data
.align 2
arr_row: .word 8
arr_row_body: .space 32

.align 2
arr_col: .word 8
arr_col_body: .space 32

.align 2
arr_diag1: .word 15
arr_diag1_body: .space 60

.align 2
arr_diag2: .word 15
arr_diag2_body: .space 60

# above should be defined globally in main

.text
# basic block coalesce:
# 1. _f_try_if_0_fi -> _f_try_fn_end
# 2. _f_try_loop_fi_0 -> _f_try_fn_end
# 3. _f_try_loop_body_0 -> _f_try_if_1_header_0
# 4. _f_try_loop_first_0 -> _f_try_loop_test_0
# 5. _f_try_loop_last_0 -> _f_try_if_fin_1


li   t3, 8 # const N
# func_try is called from outside
.globl func_try
func_try: # preamble, coalesce with _f_try_if_0_header
  # save ra, fp, param and s1
  # 4 words per frame
  _f_try_preamble:
    addi sp, sp, -16
    sw   ra, 12(sp)
    sw   fp, 8(sp)
    addi fp, sp, 8 # fp1 <- sp1+8 = sp0-8
    sw   a0, -4(fp) # param c <-> _t_try_arg_c
    sw   s1, -8(fp) # s1 <- _t_try_r_0

  _f_try_if_0_header:
    # li   t3, 8 # const N. THIS BETTER ALWAYS BE THE CASE
    beq  a0, t0, _f_try_if_0_then

    _f_try_if_0_else:
      li   s1, 0 # _t_try_r_0 <- 0; loop init.
    
      _f_try_loop_test_0:
        # will just load consts JIT... (if t3 is evicted)
        # 1 cycle instruction does not really matter...
        blt  t3, s1, _f_try_fn_end # _f_try_loop_fi_0

        _f_try_if_1_header_0: # if !arr_row[r]
          # this colaesces with _f_try_loop_body_0
          slli t0, s1, 2 # t0 <- s1 << 2: _t_row_r_offset
          la   t1, arr_row_body # t1 <- row_body
          add  t6, t1, t0 # _t_row_r_mem
          lw   t0, 0(t6) # _t_row_r_val
          beqz t0, _f_try_if_fin_1

        _f_try_if_2_header_0: !arr_diag1[r+c]
          add  t0, a0, s1 # t0 <- c + r : _t_diag1_rc
          slli t0, t0, 2 # t0 <- (c + r) << 2: _t_diag1_rc_offset
          la   t1, arr_diag1_body # t1 <- diag1_body
          add  t5, t1, t0 # _t_diag1_rc_mem
          lw   t0, 0(t5) # _t_diag1_rc_val
          beqz t0, _f_try_if_fin_1

        _f_try_if_3_header_0: !arr_diag2[r-c+7]
          sub  t0, s1, a0 # t0 <- r - c : _t_rmc
          addi t0, t0, 7 # t0 <- r - c + 7 : _t_rp7mc
          slli t0, t0, 2 # t0 <- (r - c + 7) << 2: _t_rp7mc_offset
          la   t1, arr_diag2_body # t1 <- diag2_body
          add  t4, t1, t0 # _t_rp7mc_mem
          lw   t0, 0(t4) # _t_rp7mc_val
          beqz t0, _f_try_if_fin_1

          _f_try_loop_body_0:
            # row[r] = true; diag1[r+c] = true; diag2[r+7-c] = true
            # col[c] = r
            # try(c+1)
            # row[r] = false; diag1[r+c] = false; diag2[r+7-c] = false
            li   t0, 1 # _t_true
            sw   t0, 0(t6) # row[r] = true
            sw   t0, 0(t5) # diag1[r+c] = true
            sw   t0, 0(t4) # diag2[r+7-c] = true
            
            slli t0, s1, 2 # t0 <- s1 << 2: _t_col_c_offset
            la   t1, arr_col_body # t1 <- col_body
            add  t2, t1, t0 # _t_col_c_mem
            sw   a0, 0(t2) # col[c] = r

            addi a0, a0, 1 # c++
            call func_try
            lw   a0, -4(fp) # restore c

            # now, recompute the mem addresses
            slli t0, s1, 2 # t0 <- s1 << 2: _t_row_r_offset
            la   t1, arr_row_body # t1 <- row_body
            add  t6, t1, t0 # _t_row_r_mem
            sw   zero, 0(t6) # _t_row_r_val

            add  t0, a0, s1 # t0 <- c + r : _t_diag1_rc
            slli t0, t0, 2 # t0 <- (c + r) << 2: _t_diag1_rc_offset
            la   t1, arr_diag1_body # t1 <- diag1_body
            add  t5, t1, t0 # _t_diag1_rc_mem
            sw   zero, 0(t5) # _t_diag1_rc_val

            sub  t0, s1, a0 # t0 <- r - c : _t_rmc
            addi t0, t0, 7 # t0 <- r - c + 7 : _t_rp7mc
            slli t0, t0, 2 # t0 <- (r - c + 7) << 2: _t_rp7mc_offset
            la   t1, arr_diag2_body # t1 <- diag2_body
            add  t4, t1, t0 # _t_rp7mc_mem
            sw   zero, 0(t4) # _t_rp7mc_val

        _f_try_if_fin_1:
          # this coalesces with _f_try_loop_last_0
          addi s1, s1, 1 # r++
          j    _f_try_loop_test_0


      _f_try_if_0_then:
      call func_printBoard

  _f_try_fn_end:
    # postamble
    lw   s1, -8(fp)
    lw   a0, -4(fp)
    lw   ra, 12(sp)
    lw   fp, 8(sp)
    addi sp, sp, 16
    ret # jr ra, or 
    # JUMP(label "%__SECRET_INTERNAL_DONT_SET_THIS_OR_YOU_WILL_BE_FIRED")JUMP(label "%__SECRET_INTERNAL_DONT_SET_THIS_OR_YOU_WILL_BE_FIRED")
