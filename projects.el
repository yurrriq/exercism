(unless (assoc "exercism" org-publish-project-alist)
  (add-to-list
   'org-publish-project-alist
   '("exercism"
     :base-directory "~/src/yurrriq/exercism/"
     :base-extension "org"
     :publishing-directory "~/src/yurrriq/exercism/_site/"
     :exlude ""
     :recursive t
     :auto-sitemap t
     :sitemap-fileame "sitemap.org"
     :sitemap-title "Sitemap"
     :export-creator-info nil
     :export-author-info nil
     :table-of-contents t
     :section-numbers nil
     :html-postamble "<p class=\"postamble\">Last update %d.</p>"
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
     :components ("exercism"))))
