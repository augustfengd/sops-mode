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

(defun sops-decrypt (filename)
  "decrypt sops file in current file."
  (let ((args (list "-d" filename)))
    (with-output-to-string
      (with-current-buffer standard-output
        (apply #'call-process sops-executable nil t nil args)))))

(defun sops-encrypt (filename)
  "encrypt file in current sops."
  (let ((args (list "-e" filename)))
    (with-output-to-string
      (with-current-buffer standard-output
        (apply #'call-process sops-executable nil t nil args)))))

(defun sops-decrypt-and-display ()
  "decrypt sops file in current file."
  (interactive)
  (let ((content (sops-decrypt (buffer-file-name)))
        (buffer (get-buffer-create "*decrypted*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert content)
      (json-mode)
      (view-mode)
      (display-buffer buffer '(nil (allow-no-window . t))))))

(defun sops-encrypt-and-display ()
  "decrypt sops file in current file."
  (interactive)
  (let ((content (sops-encrypt (buffer-file-name)))
        (buffer (get-buffer-create "*encrypted*")))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert content)
      (json-mode)
      (view-mode)
      (display-buffer buffer '(nil (allow-no-window . t))))))

;;;###autoload
(define-minor-mode sops-mode
  "encrypt/decrypt files with sops."
  :lighter " sops"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-d") 'sops-decrypt-and-display)
            (define-key map (kbd "C-c C-e") 'sops-encrypt-and-display)
            map))

;;;###autoload
(add-hook 'json-mode-hook 'sops-mode)

(provide 'sops-mode)
;;; sops-mode.el ends here
