-- wordcount

main = interact wordCount
	where wordCount input = show (length (input))

