;;; agc-perspective.el --- Setup perspective for quick navigation

;; Copyright (C) 2017 Burin Choomnuan

;; Author: Burin Choomnuan <agilecreativity@gmail.com>
;; URL: https://github.com/agilecreativity/agc-perspective.el
;; Version: 0.1.0
;; Keywords: navigation
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4")(perspective-el "20160609.1444"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Setup the perspective for quick navigation to the project
;;
;; See documentation on https://github.com/agilecreativity/git-init.el/README.org

;; agc-perspective.el
;; https://github.com/credmp/emacs-config/blob/master/loader.org#project-mappings
;; Setup perspectives, or workspaces, to switch between
(use-package perspective
  :ensure t
  :config
  ;; Enable perspective mode
  (persp-mode t)
  (defmacro custom-persp (name &rest body)
    `(let ((initialize (not (gethash ,name perspectives-hash)))
           (current-perspective persp-curr))
       (persp-switch ,name)
       (when initialize ,@body)
       (setq persp-last current-perspective)))

  ;; Jump to last perspective
  (defun custom-persp-last ()
    (interactive)
    (persp-switch (persp-name persp-last)))
  (define-key persp-mode-map (kbd "C-x p -") 'custom-persp-last)

  (defun custom-persp/emacs ()
    (interactive)
    (custom-persp "emacs"
                  (find-file "~/.emacs.d/init.el")))
  (define-key persp-mode-map (kbd "C-x p e") 'custom-persp/emacs)

  (progn
    (defun custom-persp/ansible ()
      (interactive)
      (custom-persp "ansible"
                    (find-file "~/projects/devops/ansible-dotfiles--agilecreativity/roles/dotfiles/tasks/awesome.yml")))
    (define-key persp-mode-map (kbd "C-x p a") 'custom-persp/ansible))

  ;; TODO: is it possible to pass the parameter to avoid duplicate the code?
  ;; e.g. custom-persp/client-sba-app vs custom-persp/client-sba-aut(omation)
  (progn
    (defun custom-persp/client-app ()
      (interactive)
      (custom-persp "client-app"
                    (find-file "~/projects/client/sba-app")))
    (define-key persp-mode-map (kbd "C-x p ca") 'custom-persp/client-app))

  (progn
    (defun custom-persp/client-jen ()
      (interactive)
      (custom-persp "client-jenkins"
                    (find-file "~/projects/client/jenkins/jenkins/scripts")))
    (define-key persp-mode-map (kbd "C-x p cj") 'custom-persp/client-jen))

  (progn
    (defun custom-persp/client-aut ()
      (interactive)
      (custom-persp "client-automation"
                    (find-file "~/projects/client/sba-automation")))
    (define-key persp-mode-map (kbd "C-x p ct") 'custom-persp/client-aut))

  (progn
    (defun custom-persp/spikes ()
      (interactive)
      (custom-persp "spikes"
                    (find-file "~/projects/")))
    (define-key persp-mode-map (kbd "C-x p s") 'custom-persp/spikes))

  ;; Project of the moment (potm)
  (progn
    (defun custom-persp/potm ()
      (interactive)
      (custom-persp "potm"
                    (find-file "~/staging/hn-reader/src/clj/com/agilecreativity/hn_reader/")))
    (define-key persp-mode-map (kbd "C-x p p") 'custom-persp/potm)))

(provide 'agc-perspective)
