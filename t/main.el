(require 'sops-mode)

(ert-deftest ert-test-encrypt ()
  (should (string-equal (sops-decrypt "t/data/secrets.enc.json")
                        (with-temp-buffer
                          (insert-file-contents "t/data/secrets.json")
                          (buffer-string)))))
