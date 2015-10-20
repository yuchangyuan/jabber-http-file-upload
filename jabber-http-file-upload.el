;;; jabber-http-file-upload.el ---

;; Copyright (C) 2015 Yu Changyuan

;; Author: Yu Changyuan <reivzy@gmail.com>
;; Keywords: comm, extensions

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

;; This package add XEP-0363 (see http://xmpp.org/extensions/xep-0363.html)
;; support to jabber.el.
;;
;; The code should work with Conversation on Android, using Prosody
;; as server(with mod_http_upload enabled).
;;
;; Usage:
;;   M-x jabber-http-file-upload

;;; Code:

(require 'jabber)
(require 'url)
(require 'mailcap)

(defun jabber-http-file-upload-done-cb (status data)
  (let ((to  (plist-get data :to))
        (jc  (plist-get data :jc))
        (get (plist-get data :get))
        (buffer (plist-get data :buffer)))
    (with-current-buffer buffer
      (if (eq jabber-buffer-connection jc)
          (jabber-chat-send jabber-buffer-connection get)
        (jabber-send-message jc to nil get "chat")))))

(defun jabber-http-file-upload-request-cb (jc xml-data data)
  (let* ((mime (plist-get data :mime))
         (file (plist-get data :file))
         (put (nth 1 (cdar (jabber-xml-get-children (jabber-iq-query xml-data)
                                                    'put))))
         (get (nth 1 (cdar (jabber-xml-get-children (jabber-iq-query xml-data)
                                                    'get))))
         (to (plist-get data :to))
         (buffer (plist-get data :buffer))
         (url-request-method "PUT")
         (url-request-extra-headers `(("Content-Type" . ,mime)))
         (url-request-data (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-substring-no-properties (point-min)
                                                             (point-max)))))
    (message "put %s\nget %s" put get)
    (url-retrieve put
                  #'jabber-http-file-upload-done-cb
                  (list `(:jc ,jc :to ,to :get ,get :buffer ,buffer)))))

(defun jabber-http-file-upload-request (jc to data)
  (let* ((file (plist-get data :file))
         (size (nth 7 (file-attributes file)))
         (name (file-name-nondirectory file))
         (ext  (file-name-extension file))
         (mime (cdr (assoc (concat "." ext) mailcap-mime-extensions))))
    (plist-put data :mime mime)
    (jabber-send-iq jc to "get"
                    `(request ((xmlns . urn:xmpp:http:upload))
                              (filename () ,name)
                              (size () ,(number-to-string size))
                              (content-type () ,mime))
                    #'jabber-http-file-upload-request-cb data
                    #'jabber-process-data "Http update request failed")))


(defun jabber-http-file-upload-disco-info-cb (jc xml-data data)
  (let ((res nil))
    (setq res (mapcar (lambda (node)
                      (when (eq (jabber-xml-node-name node) 'feature)
                        (jabber-xml-get-attribute node 'var)))
                    (jabber-xml-node-children
                     (jabber-iq-query xml-data))))
    (if (not (member "urn:xmpp:http:upload" res))
        (jabber-http-file-upload-disco-info jc data)
      (jabber-http-file-upload-request
       jc (jabber-xml-get-attribute xml-data 'from) data)
      )))

(defun jabber-http-file-upload-disco-info (jc data)
  (let ((items1 (plist-get data :items1)))
    (when items1
      ;; remove current elem
      (plist-put data :items1 (cdr items1))
      (jabber-send-iq jc (car items1) "get"
                      '(query ((xmlns
                                . "http://jabber.org/protocol/disco#info")))
                      #'jabber-http-file-upload-disco-info-cb data
                      #'jabber-process-data "Info discovery failed"))))

(defun jabber-http-file-upload-disco-items-cb (jc xml-data data)
  (let ((item (jabber-xml-path xml-data '(query item))))
    (when item
      (let ((jids (cadr item))
            (items0 (plist-get data :items0)))
        (mapc (lambda (x)
                (when (and x
                           (eq (car x) 'jid))
                  (setq items0 (cons (cdr x) items0))))
              jids)
        (plist-put data :items0 items0)))
    (jabber-http-file-upload-disco-items jc data)))

(defun jabber-http-file-upload-disco-items (jc data)
  "Use disco service to find all items, deep first search"
  (let ((items0 (plist-get data :items0))
        (items1 (plist-get data :items1)))
    (when items0
      (plist-put data :items0 (cdr items0))
      (plist-put data :items1 (cons (car items0) items1))
      (jabber-send-iq jc (car items0) "get"
                      '(query ((xmlns
                                . "http://jabber.org/protocol/disco#items")))
                      #'jabber-http-file-upload-disco-items-cb data
                      #'jabber-process-data "Item discovery failed"))
    (unless items0
      (jabber-http-file-upload-disco-info jc data))))

(defun jabber-http-file-upload (jc to file)
  "Share file with xmpp http upload extension XEP-0363."
  (interactive (list (or (and (memq jabber-buffer-connection
                                     jabber-connections)
                               jabber-buffer-connection)
                         (jabber-read-account))
                     (or jabber-chatting-with
                         (jabber-read-jid-completing "Share file with: "))
                     (ido-read-file-name "file: ")))
  (let* ((server (plist-get (fsm-get-state-data jc) :server))
         (data `(:to ,to :file ,file :buffer ,(current-buffer)
                     :items0 (,server) :items1 ())))
    (jabber-http-file-upload-disco-items jc data)))

(provide 'jabber-http-file-upload)
;;; jabber-http-file-upload.el ends here
