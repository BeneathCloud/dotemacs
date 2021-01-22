;; Straght.el disable package.el
(setq package-enable-at-startup nil)

;; Snails.el workaround on MacOS
;; (if (featurep 'cocoa)
;;     (progn
;;       (setq ns-use-native-fullscreen nil)
;;       (setq ns-use-fullscreen-animation nil)

;;       (set-frame-parameter (selected-frame) 'fullscreen 'maximized)

;;       (run-at-time "2sec" nil
;;                    (lambda ()
;;                      (toggle-frame-fullscreen)
;;                      )))
;;   (custom-set-variables
;;    '(initial-frame-alist (quote ((fullscreen . maximized))))))
