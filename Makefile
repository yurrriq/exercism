.PHONY: site

site: ; emacsclient -e '(org-publish-project "exercism-site")'

force:
	rm ~/.org-timestamps/exercism.cache
	find .. -name '*.org' | xargs touch
