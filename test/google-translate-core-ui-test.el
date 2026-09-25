;; (ert-deftest test-google-translate-request-words-fixtures ()
;;   (dolist (file (f-files google-translate-test/word-fixture-path))
;;     (let ((fixture (th-google-translate-load-fixture file)))
;;       (th-google-translate-request-fixture fixture)
;;       ;; assertions are skipped. In case of no errors assume that test pass.
;; )))

(ert-deftest test-google-translate-language-abbreviation/English/en ()
  (should
   (string-equal
    (google-translate-language-abbreviation "English")
    "en")))

(ert-deftest test-google-translate-language-abbreviation/Detect-Language/auto ()
  (should
   (string-equal
    (google-translate-language-abbreviation "Detect language")
    "auto")))

(ert-deftest test-google-translate-language-display-name/auto/unspecified ()
  (should
   (string-equal
    (google-translate-language-display-name "auto")
    "unspecified language")))

(ert-deftest test-google-translate-language-display-name/en/English ()
  (should
   (string-equal
    (google-translate-language-display-name "en")
    "English")))

(ert-deftest test-google-translate--translation-title/source-auto/detected ()
  (should
   (string-equal
    "Translate from English (detected) to Russian:\n"
    (google-translate--translation-title (make-gtos :source-language "auto"
                                                    :target-language "ru"
                                                    :auto-detected-language "en")
                                         "Translate from %s to %s:\n"))))

(ert-deftest test-google-translate--translation-title/source-auto/detected-nil ()
  (should
   (string-equal
    "Translate from English to Russian:\n"
    (google-translate--translation-title (make-gtos :source-language "en"
                                                    :target-language "ru"
                                                    :auto-detected-language nil)
                                         "Translate from %s to %s:\n"))))

(ert-deftest test-google-translate--text-phonetic/do-not-show-phonetic ()
  (should
   (string-equal
    ""
    (google-translate--text-phonetic
     (make-gtos :text-phonetic "phonetic") "%s"))))

(ert-deftest test-google-translate--text-phonetic/show-phonetic-but-empty ()
  (setq google-translate-show-phonetic t)
  (should
   (string-equal
    ""
    (google-translate--text-phonetic (make-gtos :text-phonetic "") "%s")))
  (setq google-translate-show-phonetic nil))

(ert-deftest test-google-translate--text-phonetic/show-phonetic ()
  (setq google-translate-show-phonetic t)
  (should
   (string-equal
    "phonetic"
    (google-translate--text-phonetic
     (make-gtos :text-phonetic "phonetic") "%s")))
  (setq google-translate-show-phonetic nil))

(ert-deftest test-google-translate--translated-text ()
  (should
   (string-equal
    "translation"
    (google-translate--translated-text
     (make-gtos :translation "translation") "%s"))))

(ert-deftest test-google-translate--suggestion ()
  (should
   (string-equal
    "\nDid you mean: suggest\n"
    (google-translate--suggestion (make-gtos
                                   :suggestion "suggest"
                                   :source-language "en"
                                   :target-language "ru")))))

(ert-deftest test-google-translate--suggestion-action ()
  (with-temp-buffer
    (let* ((suggestion "suggestion")
           (source-language "en")
           (target-language "ru")
           (button (insert-text-button "Foo"
                                       'action 'test
                                       'suggestion suggestion
                                       'source-language source-language
                                       'target-language target-language)))
      (with-mock
       (mock (google-translate-translate source-language
                                         target-language
                                         suggestion))
       (google-translate--suggestion-action button)))))

(ert-deftest test-google-translate--translation-phonetic/do-not-show-phonetic ()
  (should
   (string-equal
    ""
    (google-translate--translation-phonetic
     (make-gtos :translation-phonetic "phonetic") "%s"))))

(ert-deftest test-google-translate--translation-phonetic/show-phonetic-but-empty ()
  (setq google-translate-show-phonetic t)
  (should
   (string-equal
    ""
    (google-translate--translation-phonetic
     (make-gtos :translation-phonetic "") "%s")))
  (setq google-translate-show-phonetic nil))

(ert-deftest test-google-translate--translation-phonetic/show-phonetic ()
  (setq google-translate-show-phonetic t)
  (should
   (string-equal
    "phonetic"
    (google-translate--translation-phonetic
     (make-gtos :translation-phonetic "phonetic") "%s")))
  (setq google-translate-show-phonetic nil))

(ert-deftest test-google-translate-read-source-language/detect-language ()
  (with-mock
   (stub google-translate-completing-read => "Detect language")
   (should
    (string-equal
     (google-translate-read-source-language)
     "auto"))))

(ert-deftest test-google-translate-read-source-language/english ()
  (with-mock
   (stub google-translate-completing-read => "English")
   (should
    (string-equal
     (google-translate-read-source-language)
     "en"))))

(ert-deftest test-google-translate-listen-program-args/keeps-http-connection ()
  (let ((args (eval (car (get 'google-translate-listen-program-args
                              'standard-value)))))
    (should (member "-autoexit" args))
    (should (equal (cadr (member "-multiple_requests" args)) "1"))))

(ert-deftest test-google-translate--play-urls/one-process-per-url-in-order ()
  ;; The chain outlives the let: later urls must reuse the first command.
  (let ((log (make-temp-file "google-translate-listen")))
    (unwind-protect
        (progn
          (let ((google-translate-listen-program "sh")
                (google-translate-listen-program-args
                 (list "-c" (format "echo \"$0\" >> %s"
                                    (shell-quote-argument log)))))
            (google-translate--play-urls '("url1" "url2")))
          (with-timeout (5 (ert-fail (format "played: %S" (f-read log))))
            (while (not (equal (f-read log) "url1\nurl2\n"))
              (accept-process-output nil 0.05))))
      (delete-file log))))

(defun th-google-translate-exited-process (program &rest args)
  "Run PROGRAM with ARGS and return its process once it has exited."
  (let ((process (apply 'start-process "google-translate-test" nil
                        program args)))
    (while (process-live-p process)
      (accept-process-output process 0.05))
    process))

(ert-deftest test-google-translate--play-next-url/plays-remaining-urls ()
  (let ((process (th-google-translate-exited-process "true" "url1")))
    (process-put process 'google-translate-pending-urls '("url2"))
    (with-mock
     (mock (google-translate--play-urls '("url2") '("true")))
     (google-translate--play-next-url process "finished\n"))))

(ert-deftest test-google-translate--play-next-url/stops-when-playback-fails ()
  (let ((process (th-google-translate-exited-process "false" "url1")))
    (process-put process 'google-translate-pending-urls '("url2"))
    (with-mock
     (not-called google-translate--play-urls)
     (google-translate--play-next-url process "exited abnormally\n"))))
