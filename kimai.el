;;; kimai-tracker.el --- Suivi de temps avec Kimai via Emacs -*- lexical-binding: t; -*-

;; Auteur : DarkBuffalo <db@gnu.re>
;; URL : https://github.com/DarkBuffalo/kimai.el
;; Version : 1.0
;; Package-Requires: ((emacs "25.1")(request "0.3.2"))
;; Licence : MIT

;;; Description :
;; Ce fichier permet de démarrer et d'arrêter le suivi de temps sur un serveur
;; Kimai directement depuis Emacs. Vous pouvez sélectionner dynamiquement un
;; projet, une activité, et fournir une description pour chaque suivi.

;;; Configuration :
;; Avant d'utiliser ce fichier, configurez les variables suivantes :
;; - `kimai-server-url` : URL de votre serveur Kimai (ex. https://votre-serveur.tld/api).
;; - `kimai-api-token` : Jeton d'authentification API Kimai.
;; - `kimai-username` : Votre nom d'utilisateur Kimai.

;;; Usage :
;; 1. Chargez ce fichier avec `M-x load-file`.
;; 2. Utilisez `M-x kimai-start-tracking` pour démarrer un suivi de temps.
;;    Sélectionnez un projet, une activité et entrez une description.
;; 3. Utilisez `M-x kimai-stop-tracking` pour arrêter le suivi en cours.

;;; Code :

;; Dépendances nécessaires
(require 'request)
(require 'json)
(require 'calendar)

;; Définir un groupe de personnalisation
(defgroup kimai nil
  "Suivi de temps avec Kimai via Emacs."
  :group 'tools
  :prefix "kimai-")

;; Variables personnalisables
(defcustom kimai-server-url "https://votre-serveur-kimai.tld/api"
  "URL de l'API Kimai."
  :type 'string
  :group 'kimai)

(defcustom kimai-api-token "votre-jeton-api"
  "Jeton d'authentification pour l'API Kimai."
  :type 'string
  :group 'kimai)

(defcustom kimai-username "votre-nom-utilisateur"
  "Nom d'utilisateur utilisé pour Kimai."
  :type 'string
  :group 'kimai)


;; Variables pour le suivi
(defvar kimai-active-timer-id nil)


(defun kimai-check-config ()
  "Vérifie si les variables de configuration Kimai sont correctement remplies.
Affiche un message d'avertissement si une variable est vide."
  (unless (and (stringp kimai-server-url) (not (string-empty-p kimai-server-url)))
    (user-error "La variable `kimai-server-url` n'est pas configurée"))
  (unless (and (stringp kimai-api-token) (not (string-empty-p kimai-api-token)))
    (user-error "La variable `kimai-api-token` n'est pas configurée"))
  (unless (and (stringp kimai-username) (not (string-empty-p kimai-username)))
    (user-error "La variable `kimai-username` n'est pas configurée")))



(defun kimai-api-request (endpoint method &optional data)
  "Envoie une requête à l'API Kimai.
ENDPOINT est le chemin de l'API.
METHOD est la méthode HTTP (\"GET\", \"POST\", etc.).
DATA est un dictionnaire à convertir en JSON pour le corps de la requête.
Retourne le corps JSON de la réponse ou signale une erreur en cas d'échec."
  (kimai-check-config)
  (let* ((url (concat kimai-server-url endpoint))
         (response (request url
                     :type method
                     :headers `(("X-AUTH-TOKEN" . ,kimai-api-token)
                                ("X-AUTH-USER" . ,kimai-username)
                                ("Content-Type" . "application/json"))
                     :data (when data (json-encode data))
                     :parser 'json-read
                     :sync t)))
    (if (request-response-error-thrown response)
        (progn
          (message "Requête échouée. URL : %s" url)
          (message "Données envoyées : %s" (when data (json-encode data)))
          (message "Code de réponse HTTP : %s" (request-response-status-code response))
          (message "Corps de la réponse : %s" (request-response-data response))
          (error "Erreur de l'API Kimai : %s" (request-response-error-thrown response)))
      (request-response-data response))))


;;; modeline
(defvar kimai-tracking-start-time nil
  "The start time of the current tracking session in seconds since epoch.")

(defvar kimai-tracking-active nil
  "Whether a tracking session is currently active.")

(defvar kimai-mode-line-string ""
  "String displayed in the mode-line for Kimai.")

(defun kimai-update-mode-line ()
  "Update the mode-line with the elapsed tracking time."
  (if kimai-tracking-active
      (let* ((elapsed (float-time (time-subtract (current-time) kimai-tracking-start-time)))
             (hours (floor (/ elapsed 3600)))
             (minutes (mod (floor (/ elapsed 60)) 60)))
        (setq kimai-mode-line-string
              (format " Kimai: %02d:%02d " hours minutes)))
    (setq kimai-mode-line-string "")))

(unless (memq 'kimai-mode-line-string global-mode-string)
  (setq global-mode-string (append global-mode-string '(kimai-mode-line-string))))

;;; end modeline here



(defun kimai-fetch-customers ()
  "Récupère la liste des clients depuis Kimai."
  (kimai-api-request "/customers" "GET"))


(defun kimai-fetch-projects (customer-id)
  "Récupère la liste des projets depuis Kimai."
  (kimai-api-request (concat "/projects?customer=" (number-to-string customer-id)) "GET"))


(defun kimai-fetch-activities (project-id)
  "Récupère la liste des activités depuis Kimai."
  (kimai-api-request (concat "/activities?project=" (number-to-string project-id)) "GET"))

(defun kimai-prompt-for-customer ()
  "Invite l'utilisateur à sélectionner un client."
  (let* ((customers (kimai-fetch-customers))
         (customer-names (mapcar (lambda (c) (cons (cdr (assoc 'name c)) (cdr (assoc 'id c)))) customers)))
    (cdr (assoc (completing-read "Sélectionnez un client: " customer-names) customer-names))))

(defun kimai-prompt-for-project (customer-id)
  "Invite l'utilisateur à sélectionner un projet."
  (let* ((projects (kimai-fetch-projects customer-id))
         (project-names (mapcar (lambda (p) (cons (cdr (assoc 'name p)) (cdr (assoc 'id p)))) projects)))
    (cdr (assoc (completing-read "Sélectionnez un projet: " project-names) project-names))))

(defun kimai-prompt-for-activity (project-id)
  "Invite l'utilisateur à sélectionner une activité."
  (let* ((activities (kimai-fetch-activities project-id))
         (activity-names (mapcar (lambda (a) (cons (cdr (assoc 'name a)) (cdr (assoc 'id a)))) activities)))
    (cdr (assoc (completing-read "Sélectionnez une activité: " activity-names) activity-names))))

;;;###autoload
(defun kimai-start-tracking ()
  "Démarre un suivi de temps pour un projet et une activité spécifiques."
  (interactive)
  (let* ((customer-id (kimai-prompt-for-customer))
         (project-id (kimai-prompt-for-project customer-id))
         (activity-id (kimai-prompt-for-activity project-id))
         (description (read-string "Entrez une description: "))
         (response (kimai-api-request "/timesheets" "POST"
                                      `(("project" . ,project-id)
                                        ("activity" . ,activity-id)
                                        ("description" . ,description)))))
    (if (assoc 'id response)
        (progn
          (setq kimai-active-timer-id (cdr (assoc 'id response)))
          (message "Suivi de temps démarré avec ID: %s" kimai-active-timer-id)
          (setq kimai-tracking-start-time (current-time)) ;; modeline
          (setq kimai-tracking-active t) ;;modeline
          (run-with-timer 1 1 #'kimai-update-mode-line))
      (message "Erreur lors du démarrage du suivi de temps: %s" response))))

;;;###autoload
(defun kimai-stop-tracking ()
  "Arrête le suivi de temps actif."
  (interactive)
  (if kimai-active-timer-id
      (let ((response (kimai-api-request (format "/timesheets/%s/stop" kimai-active-timer-id) "PATCH")))
        (if (assoc 'id response)
            (progn
              ;; timeline
              (setq kimai-tracking-start-time nil)
              (setq kimai-tracking-active nil)
              (kimai-update-mode-line)
              ;; message arret
              (message "Suivi de temps arrêté pour l'ID: %s" kimai-active-timer-id)
              (setq kimai-active-timer-id nil))
          (message "Erreur lors de l'arrêt du suivi de temps: %s" response)))
    (message "Aucun suivi de temps actif.")))




;;; ** Rapport
(defvar kimai-projects-cache nil
  "Cache contenant tous les projets récupérés depuis l'API Kimai.")

(defvar kimai-activities-cache nil
  "Cache contenant toutes les activits récupérées depuis l'API Kimai.")

(defun kimai-load-projects-and-activities ()
  "Charge les projets et activités depuis l'API Kimai et les met en cache."
  (setq kimai-projects-cache (kimai-api-request "/projects" "GET"))
  (setq kimai-activities-cache (kimai-api-request "/activities" "GET")))

(defun kimai-get-project-name (project-id)
  "Retourne le nom du projet correspondant à PROJECT-ID depuis le cache."
  (cdr (assoc 'name (seq-find (lambda (p) (= (cdr (assoc 'id p)) project-id)) kimai-projects-cache))))

(defun kimai-get-activity-name (activity-id)
  "Retourne le nom de l'activité correspondant à ACTIVITY-ID depuis le cache."
  (cdr (assoc 'name (seq-find (lambda (a) (= (cdr (assoc 'id a)) activity-id)) kimai-activities-cache))))

(defun kimai-last-month-period ()
  "Retourne une liste '(start end) correspondant au début et à la fin du mois dernier."
  (let* ((current-date (decode-time))
         (year (nth 5 current-date))
         (month (nth 4 current-date))
         (last-month (if (= month 1) 12 (1- month)))
         (last-month-year (if (= month 1) (1- year) year))
         (start (format "%04d-%02d-01" last-month-year last-month))
         (end (format "%04d-%02d-%02d" last-month-year last-month
                      (calendar-last-day-of-month last-month last-month-year))))
    (list start end)))

(defun kimai-get-time-entries-last-month ()
  "Récupère les entrées de temps de Kimai pour le mois dernier."
  (let* ((period (kimai-last-month-period))
         (start (car period))
         (end (cadr period)))
    (kimai-get-time-entries start end)))

(defun kimai-get-time-entries (start end)
  "Récupère les entrées de temps de Kimai entre START et END (format YYYY-MM-DD)."
  (kimai-api-request (format "/timesheets?begin=%sT00:00:00&end=%sT23:59:59" start end) "GET"))

(defun kimai-format-duration (seconds)
  "Convertit une durée en SECONDS en format hh:mm."
  (let ((hours (/ seconds 3600))
        (minutes (% (/ seconds 60) 60)))
    (format "%02d:%02d" hours minutes)))

(defun kimai-get-project-name (project-id)
  "Retourne le nom du projet correspondant à PROJECT-ID depuis l'API Kimai."
  (let ((projects (kimai-api-request "/projects" "GET")))
    (cdr (assoc 'name (seq-find (lambda (p) (= (cdr (assoc 'id p)) project-id)) projects)))))

(defun kimai-get-activity-name (activity-id)
  "Retourne le nom de l'activité correspondant à ACTIVITY-ID depuis l'API Kimai."
  (let ((activities (kimai-api-request "/activities" "GET")))
    (cdr (assoc 'name (seq-find (lambda (a) (= (cdr (assoc 'id a)) activity-id)) activities)))))


(defun kimai-group-by-project (data)
  "Retourne une table associant chaque projet (nom)  une liste d'entrées de temps."
  (let ((project-table (make-hash-table :test 'equal)))
    ;; Convertir en liste normale si c'est un vecteur
    (when (vectorp data)
      (setq data (append data nil)))

    (dolist (entry data)
      (let* ((project-id (cdr (assoc 'project entry)))
             (project-name (kimai-get-project-name project-id)) ;; Récupérer le nom du projet
             (entries (gethash project-name project-table '())))
        (puthash project-name (cons entry entries) project-table)))
    project-table))



(defun kimai-generate-org-report (start end)
  "Génère un rapport Org-mode avec un tableau par projet pour les entrées de temps entre START et END."
  (let* ((data (kimai-get-time-entries start end))
         (buffer (get-buffer-create "*Kimai Org Report*"))
         (project-table (kimai-group-by-project data)))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert (format "* Rapport Kimai du %s au %s\n\n" start end))
      (maphash
       (lambda (project entries)
         (insert (format "** %s\n\n" project))
         (insert "| Date       | Début      | Fin        | Durée  | Activité       | Description           |\n")
         (insert "|------------+-----------+-----------+--------+---------------+----------------------|\n")
         (dolist (entry entries)
           (let* ((begin (cdr (assoc 'begin entry)))
                  (end (cdr (assoc 'end entry)))
                  (duration (kimai-format-duration (cdr (assoc 'duration entry))))
                  (activity (or (kimai-get-activity-name (cdr (assoc 'activity entry))) "N/A"))
                  (description (or (cdr (assoc 'description entry)) "Aucune description")))
             (insert (format "| %s | %s | %s | %s | %s | %s |\n"
                             (substring begin 0 10)
                             (substring begin 11 19)
                             (substring end 11 19)
                             duration
                             activity
                             description))))
         (insert "\n"))
       project-table)
      (unless (> (hash-table-count project-table) 0)
        (insert "** Aucun projet trouvé\n\n"))
      (insert "\n"))
    (switch-to-buffer buffer)))



;;;###autoload
(defun kimai-month-report ()
  "Month report by project in org Buffer."
  (interactive)
  (let* ((period (kimai-last-month-period))
         (start (car period))
         (end (cadr period)))
    (kimai-generate-org-report start end)))



  (provide 'kimai)
;;; kimai.el ends here
