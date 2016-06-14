build/tsar.index : build/tsar.out
	LinkGrammar -o build/tsar build/tsar.out

build :
	mkdir build

build/tsar.out : build
	cpp -nostdinc -CC -P -I .. -I $(dicts_dir) -o build/tsar.out tsar.dict

.PHONY : clean
clean :
	rm -rf build
