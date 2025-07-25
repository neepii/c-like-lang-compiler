  $ test_program () { \
  > dune exec compiler temp_source object > /dev/null; \
  > riscv64-linux-gnu-as -march=rv64gc object -o /tmp/main_temp.o && riscv64-linux-gnu-as -march=rv64gc ~/compiler/asm/lib.s -o /tmp/lib_temp.o && riscv64-linux-gnu-ld /tmp/main_temp.o /tmp/lib_temp.o; \
  > ./a.out
  > }

  $ echo 'main() { return 0; }' > temp_source
  $ test_program

  $ echo 'extern print_int(n); main() { n = 1; print_int(n); return 0; }' > temp_source
  $ test_program
  1

  $ echo 'extern print_hw(); main() { print_hw(); return 0; }' > temp_source
  $ test_program
  Hello, World!

  $ echo 'extern print_int(n); main() { a=0; b=1; n=7; while (n>1) {b=a+b;a=b-a;n=n-1;} print_int(a); return 0; }' > temp_source
  $ test_program
  8

  $ echo 'extern print_int(n); main() { acc=1; n=6; while (n>1)  {acc=acc*n; n=n-1; } print_int(acc); return 0; }' > temp_source
  $ test_program
  720

  $ echo 'exit(n) {return n ; } main(){ exit(143); }' > temp_source
  $ test_program
  [143]
	[143]
