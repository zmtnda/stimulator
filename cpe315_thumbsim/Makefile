all:
	g++ -g main.cpp decode.cpp thumbsim_driver.cpp parse.cpp execute.cpp -o thumbsim

run: all
	./thumbsim -i -d -s -c 256 -f inputs/fib.sim > myFib.out
	vimdiff myFib.out outputs/fib.completeoutput

l: all
	lldb ./thumbsim -- -i -d -c 256 -f inputs/fib.sim

clean:
	rm -rf ./*.o ./thumbsim
