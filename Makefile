.PHONY: serve build draft send send-unsent

serve:
	bb blog.clj serve

build:
	bb blog.clj build

draft:
	bb blog.clj new "$(filter-out $@,$(MAKECMDGOALS))"

send:
	bb email.clj send "$(filter-out $@,$(MAKECMDGOALS))"

send-unsent:
	bb email.clj send-unsent

%:
	@:
