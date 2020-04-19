all:
	# Needs Pandoc > 2.7 ----> https://stackoverflow.com/a/31778080/10728610
	pandoc -t revealjs -s -o index.html README.md -V revealjs-url=./reveal.js --slide-level=2 -V theme=simple
