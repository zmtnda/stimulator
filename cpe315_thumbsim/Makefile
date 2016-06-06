all:
	g++ -g main.cpp decode.cpp thumbsim_driver.cpp parse.cpp execute.cpp -o thumbsim

run: all
	./thumbsim -i -d -s -c 256 -f inputs/fib.sim > myFib.out
	vimdiff myFib.out outputs/fib.completeoutput

l: all
	lldb ./thumbsim -- -i -d -s -c 256 -f inputs/fib.sim

s: all
	./thumbsim -i -d -s -c 256 -f inputs/shang.O2.sim > shang2.out

sl: all
	lldb ./thumbsim -- -i -d -s -c 256 -f inputs/shang.O2.sim > shang2.out

clean:
	rm -rf ./*.o ./thumbsim
