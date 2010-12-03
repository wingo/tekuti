moddir=$(prefix)/share/guile/site

SOURCES =					\
	tekuti/base64.scm			\
	tekuti/boot.scm				\
	tekuti/cache.scm			\
	tekuti/comment.scm			\
	tekuti/config.scm			\
	tekuti/filters.scm			\
	tekuti/git.scm				\
	tekuti/index.scm			\
	tekuti/match-bind.scm			\
	tekuti/mod-lisp.scm			\
	tekuti/page-helpers.scm			\
	tekuti/page.scm				\
	tekuti/post.scm				\
	tekuti/request.scm			\
	tekuti/tags.scm				\
	tekuti/template.scm			\
	tekuti/url.scm				\
	tekuti/util.scm				\
	tekuti/web.scm

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(GOBJECTS)
EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)

CLEANFILES = $(GOBJECTS)

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(top_builddir)/env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"
