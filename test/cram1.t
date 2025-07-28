This test require RISCV64 toolchain, specifically:
 - riscv64-linux-gnu-as
 - riscv64-linux-gnu-ld

  $ test_program () { \
  > dune exec compiler temp_source object > /dev/null; \
  > riscv64-linux-gnu-as -march=rv64gc object -o main_temp.o && riscv64-linux-gnu-as -march=rv64gc $INSIDE_DUNE/../../asm/lib.s -o lib_temp.o && riscv64-linux-gnu-ld main_temp.o lib_temp.o; \
  > qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./a.out
  > }

  $ echo 'main() { return 0; }' > temp_source
  $ test_program

  $ echo 'main() { return 143; }' > temp_source
  $ test_program
  [143]

  $ echo 'main() { n = 1 + 2 * 3 + 7 / 7; return n; }' > temp_source
  $ test_program
  [8]

  $ echo 'main() { n = 0; if (1 == 1) { n = 1; } return n; }' > temp_source
  $ test_program
  [1]

  $ echo 'main() { n = 0; while (n < 143) { n = n + 1; } return n; }' > temp_source
  $ test_program
  [143]

  $ echo 'extern print_int(n); main() { n = 1; print_int(n); return 0; }' > temp_source
  $ test_program
  1

  $ echo 'extern print_hw(); main() { print_hw(); return 0; }' > temp_source
  $ test_program
  Hello, World!

  $ echo 'extern print_int(n); main() { a=0; b=1; n=7; while (n>1) {b=a+b;a=b-a;n=n-1;} print_int(a); return 0; }' > temp_source
  $ test_program
  1

  $ echo 'extern print_int(n); main() { acc=1; n=6; while (n>1)  {acc=acc*n; n=n-1; } print_int(acc); return 0; }' > temp_source
  $ test_program
  720

  $ echo 'exit() {return 143 ; } main(){ var = exit(); return var; }' > temp_source
  $ test_program
  [143]
