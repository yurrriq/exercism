(add-to-list
 'org-publish-project-alist
 '("exercism"
   :base-directory "~/src/yurrriq/exercism/"
   :base-extension "org"
   :publishing-directory "~/src/yurrriq/exercism/_site/"
   :exclude "\\(README\\|setup\\).org"
   :recursive t
   :auto-sitemap t
   :sitemap-filename "index.org"
   :sitemap-title "My Exercism.io Solutions"
   :export-creator-info nil
   :export-author-info nil
   :table-of-contents t
   :section-numbers nil
   :html-doctype "html5"
   :html-postamble "<script src=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js\"></script>"
   :style-include-default nil
   :publishing-function org-html-publish-to-html))

(add-to-list
 'org-publish-project-alist
 '("exercism-site"
   :base-directory "~/src/yurrriq/exercism/"
   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
   :publishing-directory "~/src/yurrriq/exercism/_site/"
   :recursive t
   :publishing-function org-publish-attachment
   :components ("exercism")))
