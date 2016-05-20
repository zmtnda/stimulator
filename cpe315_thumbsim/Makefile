all:
	g++ -g main.cpp decode.cpp thumbsim_driver.cpp parse.cpp execute.cpp -o thumbsim

run: all
	./thumbsim -i -d -f inputs/fib.sim

l: all
	lldb ./thumbsim -- -i -d -f inputs/fib.sim

clean:
	rm -rf ./*.o ./thumbsim
