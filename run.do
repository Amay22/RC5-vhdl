cd C:/Users/Amay/Desktop/Testbench
vlib work
vcom rc5tb1.vhd
vcom rc5_pkg.vhd
vcom fpgarc5complete1.vhd
vcom a.vhd
quit -sim
vcom rc5tb1.vhd
vsim work.rc5tb1
add wave clock rc5Val tempVal key_rdy do_rdy key_vld clr
run 5000


.main clear 