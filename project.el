(unless (assoc "Exercism.io: Haskell" org-publish-project-alist)
  (add-to-list
   'org-publish-project-alist
   '("Exercism.io: Haskell"
     :base-directory "~/src/yurrriq/exercism/haskell/"
     :base-extension "org"
     :exclude "\\(setup\\|README\\).org"
     :recursive t
     :publishing-directory "~/src/yurrriq/exercism/haskell/public_html/"
     :publishing-function org-html-publish-to-html)))
