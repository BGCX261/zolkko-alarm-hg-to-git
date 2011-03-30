cd `dirname $0`
make
erl.exe -pa $PWD/ebin $PWD/deps/*/ebin -s smokehouse

rem -mnesia dir '"db"'
