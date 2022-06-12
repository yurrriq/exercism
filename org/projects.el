(add-to-list
 'org-publish-project-alist
 '("exercism"
   :base-directory "~/src/github.com/yurrriq/exercism/"
   :base-extension "org"
   :publishing-directory "~/src/github.com/yurrriq/exercism/_site/"
   :exclude "\\(README.org\\|setup.org\\|.direnv\\)"
   :recursive t
   :auto-sitemap t
   :sitemap-filename "index.org"
   :sitemap-title "My Exercism.io Solutions"
   :export-creator-info nil
   :export-author-info nil
   :table-of-contents t
   :section-numbers nil
   :html-doctype "html5"
   :html-postamble "<script src=\"https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.min.js\"></script>"
   :style-include-default nil
   :publishing-function org-html-publish-to-html))

(add-to-list
 'org-publish-project-alist
 '("exercism-site"
   :base-directory "~/src/github.com/yurrriq/exercism/"
   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
   :publishing-directory "~/src/github.com/yurrriq/exercism/_site/"
   :recursive t
   :publishing-function org-publish-attachment
   :components ("exercism")))
