.PHONY: serve build draft

serve:
	bb blog.clj serve

build:
	bb blog.clj build

draft:
	bb blog.clj new "$(filter-out $@,$(MAKECMDGOALS))"

%:
	@:
