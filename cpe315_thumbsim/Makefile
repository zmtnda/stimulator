all:
	g++ -g main.cpp decode.cpp thumbsim_driver.cpp parse.cpp execute.cpp -o thumbsim

run: all
	./thumbsim -i -d -f inputs/fib.sim > myFib.out
	vimdiff myFib.out outputs/fib.out

l: all
	lldb ./thumbsim -- -i -d -f inputs/fib.sim

b: all
	./thumbsim -i -d -f inputs/byteTest.sim

lb: all
	lldb ./thumbsim -- -i -d -f inputs/byteTest.sim

clean:
	rm -rf ./*.o ./thumbsim
