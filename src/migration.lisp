(in-package :migration-user)

(setf *db-connection-parameters*
'("planet_git" "gitui" "oenRTe90u" "localhost"))

(def-query-migration 1 "drop keys table"
  :execute "DROP TABLE keys")

(def-query-migration 2 "add location column to login"
  :execute "ALTER TABLE login ADD COLUMN location text")

;; version 3

;; ALTER TABLE login ADD UNIQUE (username)
;; ALTER TABLE email ADD UNIQUE (email)
;; ALTER TABLE email ADD CONSTRAINT email_fk FOREIGN KEY (user_id) REFERENCES login (id) MATCH FULL
;; ALTER TABLE key ADD CONSTRAINT key_fk FOREIGN KEY (user_id) REFERENCES login (id) MATCH FULL
