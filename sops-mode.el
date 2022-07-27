;;; sops-mode.el --- minor mode to managing secrets using mozilla's sops tool

;; Copyright (C) 2022 August Feng

;; Author: August Feng <augustfengd@gmail.com>
;; Version: 0.1
;; Keywords: processes
;; URL: https://github.com/augustfengd/sops-mode


;;; Commentary:

;; This package provides the ability to decrypt a sops secrets and some more in
;; the future.

(defcustom sops-executable
  "sops"
  "The sops executable."
  :group 'sops
  :type 'string)

(defun sops-decrypt ()
  "decrypt sops file in current file."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save))
  (let ((cmd sops-executable)
        (args (list "-d" (file-truename (buffer-file-name))))
        (output (get-buffer-create "*decrypted*")))
    (with-current-buffer output
      (setq buffer-read-only nil)
      (erase-buffer)
      (if (zerop (apply #'call-process cmd nil t nil args))
          (progn
            (json-mode)
            (view-mode))
        (compilation-mode nil))
      (display-buffer output '(nil (allow-no-window . t))))))

(defun sops-encrypt ()
  "encrypt file using sops."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save))
  (let ((cmd sops-executable)
        (args (list "-e" (file-truename (buffer-file-name))))
        (output (get-buffer-create "*encrypted*")))
    (with-current-buffer output
      (setq buffer-read-only nil)
      (erase-buffer)
      (if (zerop (apply #'call-process cmd nil t nil args))
          (progn
            (json-mode)
            (view-mode))
        (compilation-mode nil))
      (display-buffer output '(nil (allow-no-window . t))))))

;;;###autoload
(define-minor-mode sops-mode
  "encrypt/decrypt files with sops."
  :lighter " sops"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map  (kbd "C-c C-d") 'sops-decrypt)
            (define-key map  (kbd "C-c C-c") 'sops-encrypt)
            map))

;;;###autoload
(add-hook 'json-mode-hook 'sops-mode)

(provide 'sops-mode)
;;; sops-mode.el ends here
