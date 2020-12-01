all: day1
day1:
	runhaskell day1.hs < day1.input
watch:
	while inotifywait -e close_write day$(day).hs; do make day$(day); done
