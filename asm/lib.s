.global print_hw
.global print_int

hello_world: .ascii "Hello, World!\n"
.equ hw_size, 14

.lcomm itoa_buffer, 22
.equ itoa_buffer_size, 22

print_hw:
  li a7, 64
  li a0, 1
  la a1, hello_world
  li a2, hw_size
  ecall
  ret

itoa:
  addi sp, sp, -16
  sd s0, (sp)
  sd ra, 8(sp)

  la t2, itoa_buffer
  bgez a0, before_digit_loop
  li t0, 45
  sb t0, (t2)
  addi t2, t2, 1
  neg a0, a0

before_digit_loop:
  add t0, a0, zero
  li t1, 10
  li t3, 0

start_digit_loop:
  div t0, t0, t1
  addi t3, t3, 1
  beqz t0, end_digit_loop
  j start_digit_loop

end_digit_loop:
	add t2, t2, t3
  li t0, 10
  sb t0, (t2)
  li t5, 0
  add t5, t5, t3
	addi t2, t2, -1

start_string_creation_loop:
  rem t0, a0, t1
  div a0, a0, t1
  addi t4, t0, 48
  sb t4, (t2)
  addi t3, t3, -1
  addi t2, t2, -1
  beqz t3, end_string_creation_loop
	j start_string_creation_loop
end_string_creation_loop:
  ld s0, (sp)
  ld ra, 8(sp)
  addi sp, sp, 16
  ret

print_int:
  addi sp, sp, -16
  sd s0, (sp)
  sd ra, 8(sp)

  call itoa
  li a7, 64
  li a0, 1
  la a1, itoa_buffer
  li a2, 0
	add a2, a2, t5
  ecall

  ld s0, (sp)
  ld ra, 8(sp)
  addi sp, sp, 16
  ret
