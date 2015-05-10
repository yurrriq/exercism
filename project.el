(unless (assoc "Exercism.io: Erlang" org-publish-project-alist)
  (add-to-list
   'org-publish-project-alist
   '("Exercism.io: Erlang"
     :base-directory "~/src/yurrriq/exercism/erlang/"
     :base-extension "org"
     :exclude "setup.org"
     :recursive t
     :publishing-directory "~/src/yurrriq/exercism/erlang/public_html/"
     :publishing-function org-html-publish-to-html)))
