DIST_ARCHIVE=icfp-e75ce0b1-020d-43e5-bd51-793ecd5f990b.tar.gz

clean:
	rm -f $(DIST_ARCHIVE) dist/punter

.DUMMY: clean

$(DIST_ARCHIVE): dist/punter dist/src
	cd dist && tar cvfz ../$(DIST_ARCHIVE) .

dist: clean $(DIST_ARCHIVE)

dist/punter:
	stack setup
	stack build
	stack install --local-bin-path dist
	mv dist/icfpc2017 dist/punter

dist/src:
	rm -rf dist/src
	git clone . dist/src
	rm -rf dist/src/.git


