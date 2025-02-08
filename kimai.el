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
(require 'org)

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


;; Variables pour le suivi et le cache
(defvar kimai-active-timer-id nil)
(defvar kimai-projects-cache nil "Cache contenant tous les projets récupérés depuis l'API Kimai.")
(defvar kimai-activities-cache nil "Cache contenant toutes les activits récupérées depuis l'API Kimai.")
(defvar kimai-cache-timeout 3600 "Dure de validité du cache en secondes")
(defvar kimai-cache-last-update nil "Timestamp de la dernire mise à jour du cache")
(defvar kimai-update-timer nil "Timer pour la mise à jour de la modeline")

;; Variables pour la modeline
(defvar kimai-tracking-start-time nil
  "The start time of the current tracking session in seconds since epoch.")
(defvar kimai-tracking-active nil
  "Whether a tracking session is currently active.")
(defvar kimai-mode-line-string ""
  "String displayed in the mode-line for Kimai.")

;; Fonctions de gestion du cache
(defun kimai-cache-expired-p ()
  "Vérifie si le cache a expiré."
  (or (null kimai-cache-last-update)
      (> (- (float-time) kimai-cache-last-update) kimai-cache-timeout)))

(defun kimai-refresh-cache ()
  "Rafrachit le cache des projets et activités."
  (setq kimai-projects-cache (kimai-api-request "/projects" "GET")
        kimai-activities-cache (kimai-api-request "/activities" "GET")
        kimai-cache-last-update (float-time)))

(defun kimai-ensure-cache ()
  "S'assure que le cache est à jour."
  (when (kimai-cache-expired-p)
    (kimai-refresh-cache)))


(defun kimai-check-config ()
  "Vérifie si les variables de configuration Kimai sont correctement remplies.
Affiche un message d'avertissement si une variable est vide."
  (unless (and (stringp kimai-server-url) (not (string-empty-p kimai-server-url)))
    (user-error "La variable `kimai-server-url` n'est pas configurée"))
  (unless (and (stringp kimai-api-token) (not (string-empty-p kimai-api-token)))
    (user-error "La variable `kimai-api-token` n'est pas configurée"))
  (unless (and (stringp kimai-username) (not (string-empty-p kimai-username)))
    (user-error "La variable `kimai-username` n'est pas configurée")))

;; Ajouter une fonction pour gérer les erreurs HTTP de manière plus élegante
(defun kimai-handle-api-error (response)
  "Gere les erreurs API de manire plus detaille."
  (let ((status (request-response-status-code response))
        (error-msg (request-response-error-thrown response)))
    (pcase status
      (401 (error "Erreur d'authentification. Vérifiez vos identifiants"))
      (403 (error "Accès non autorise"))
      (404 (error "Ressource non trouve"))
      (_ (error "Erreur API (%s): %s" status error-msg)))))

(defun kimai-api-request (endpoint method &optional data)
  "Envoie une requète à l'API Kimai."
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
        (kimai-handle-api-error response)
      (request-response-data response))))

;; Fonctions de formatage
(defun kimai-format-date (date-string)
  "Formate une date ISO en format lisible."
  (format-time-string "%Y-%m-%d"
                      (date-to-time date-string)))

(defun kimai-format-time (time-string)
  "Formate une heure ISO en format lisible."
  (format-time-string "%H:%M:%S"
                      (date-to-time time-string)))

(defun kimai-format-duration (seconds)
  "Convertit une durée en SECONDS en format hh:mm."
  (let ((hours (/ seconds 3600))
        (minutes (% (/ seconds 60) 60)))
    (format "%02d:%02d" hours minutes)))

;;; modeline
;; Gestion de la modeline
(defun kimai-start-modeline-updates ()
  "Démarre les mises à jour de la modeline."
  (when kimai-update-timer
    (cancel-timer kimai-update-timer))
  (setq kimai-update-timer
        (run-with-timer 0 60 #'kimai-update-mode-line)))

(defun kimai-stop-modeline-updates ()
  "Arrète les mises à jour de la modeline."
  (when kimai-update-timer
    (cancel-timer kimai-update-timer)
    (setq kimai-update-timer nil)))

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
  "Retourne une liste correspondant au début et à la fin du mois dernier."
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

(defun kimai-validate-date-range (start end)
  "Valide une plage de dates."
  (let ((start-time (date-to-time start))
        (end-time (date-to-time end)))
    (when (time-less-p end-time start-time)
      (error "La date de fin doit être postérieure à la date de début"))))

(defun kimai-get-time-entries (start end)
  "Récupere les entres de temps de Kimai entre START et END (format YYYY-MM-DD)."
  (kimai-validate-date-range start end)
  (kimai-api-request (format "/timesheets?begin=%sT00:00:00&end=%sT23:59:59" start end) "GET"))

(defun kimai-group-by-project (data)
  "Optimisation du groupement par projet avec cache."
  (kimai-ensure-cache)
  (let ((project-table (make-hash-table :test 'equal)))
    (seq-do
     (lambda (entry)
       (let* ((project-id (cdr (assoc 'project entry)))
              (project-name (or (kimai-get-project-name project-id) "Inconnu"))
              (entries (gethash project-name project-table '())))
         (puthash project-name
                  (cons entry entries)
                  project-table)))
     data)
    project-table))


(defun kimai-generate-org-report (start end)
  "Génere un rapport Org-mode détaillé pour les entres de temps entre START et END.
Le rapport inclut des statistiques par projet et des totaux globaux."
  (let* ((data (kimai-get-time-entries start end))
         (buffer (get-buffer-create "*Kimai Org Report*"))
         (project-table (kimai-group-by-project data))
         (total-duration 0)
         (total-projects 0))

    ;; Assurons-nous que le cache est  jour
    (kimai-ensure-cache)

    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)

      ;; En-tte du rapport
      (insert (format "#+TITLE: Rapport Kimai du %s au %s\n" start end))
      (insert (format "#+DATE: Généré le %s\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert (format "#+AUTHOR: %s\n\n" kimai-username))

      ;; Traitement par projet
      (insert "* Projets\n\n")
      (maphash
       (lambda (project entries)
         (setq total-projects (1+ total-projects))
         (let ((project-duration 0))
           ;; En-tete du projet
           (insert (format "** %s\n\n" project))

           ;; Tableau des entrées
           (insert "|---|\n")
           (insert "| Date | Début | Fin | Durée | Activité | Description |\n")
           (insert "|---|\n")

           ;; Trier les entrées par date
           (setq entries
                 (sort entries
                       (lambda (a b)
                         (string< (cdr (assoc 'begin a))
                                (cdr (assoc 'begin b))))))

           ;; Ajouter chaque entrée
           (dolist (entry entries)
             (let* ((begin (cdr (assoc 'begin entry)))
                    (end (cdr (assoc 'end entry)))
                    (duration (cdr (assoc 'duration entry)))
                    (activity-id (cdr (assoc 'activity entry)))
                    (activity-name (or (kimai-get-activity-name activity-id) "N/A"))
                    (description (or (cdr (assoc 'description entry)) ""))
                    (formatted-duration (kimai-format-duration duration)))

               ;; Accumuler les durées
               (setq project-duration (+ project-duration duration))
               (setq total-duration (+ total-duration duration))

               ;; Insérer la ligne du tableau
               (insert (format "| %s | %s | %s | %s | %s | %s |\n"
                             (kimai-format-date begin)
                             (kimai-format-time begin)
                             (kimai-format-time end)
                             formatted-duration
                             activity-name
                             description))))

           ;; Rsum du projet
           (insert "|---|\n\n")
           (insert (format "*** Résumé du projet\n"))
           (insert (format "- Nombre d'entrées : %d\n" (length entries)))
           (insert (format "- Durée totale : %s\n\n"
                         (kimai-format-duration project-duration)))))
       project-table)

      ;; Statistiques globales
      (insert "* Statistiques globales\n\n")
      (insert (format "- Nombre total de projets : %d\n" total-projects))
      (insert (format "- Temps total enregistré : %s\n"
                     (kimai-format-duration total-duration)))

      ;; Ajouter des proprits pour org-mode
      (goto-char (point-min))
      (insert "#+OPTIONS: toc:2 num:nil\n")
      (insert "#+STARTUP: overview indent\n\n"))

    ;; Afficher le buffer et activer org-mode
    (with-current-buffer buffer
      (org-mode)
      (org-table-map-tables 'org-table-align)
      (goto-char (point-min)))

    (switch-to-buffer buffer)))

;;;###autoload
(defun kimai-month-report ()
  "Month report by project in org Buffer."
  (interactive)
  (let* ((period (kimai-last-month-period))
         (start (car period))
         (end (cadr period)))
    (kimai-generate-org-report start end)))

;;; Dynamic block

;; Fonction utilitaire pour convertir un vecteur en liste
(defun kimai-vector-to-list (vector-data)
  "Convertit un vecteur de données Kimai en liste.
VECTOR-DATA est le vecteur à convertir."
  (if (vectorp vector-data)
      (append vector-data nil)
    vector-data))

(defun org-dblock-write:kimai-report (params)
  "écrire un rapport Kimai dans un dynamic block.
Les paramêtres acceptés sont :
:start-date  Date de début (format YYYY-MM-DD)
:end-date    Date de fin (format YYYY-MM-DD)
:projects    Liste des projets (optionnel)
:activities  Liste des activités (optionnel)
:format      Format de sortie ('table ou 'list, par défaut 'table)"

 ;; S'assurer que le cache est chargé
  (unless (and (bound-and-true-p kimai-projects-cache)
               (bound-and-true-p kimai-activities-cache))
    (kimai-load-projects-and-activities))

  (let* ((start (or (plist-get params :start-date)
                    (format-time-string "%Y-%m-01")))
         (end (or (plist-get params :end-date)
                  (format-time-string "%Y-%m-%d")))
         (format-type (or (plist-get params :format) 'table))
         (projects (plist-get params :projects))
         (activities (plist-get params :activities))
         (entries (kimai-vector-to-list (kimai-get-time-entries start end)))
         (total-duration 0))

    (when projects
      (setq entries
            (seq-filter
             (lambda (entry)
               (member (kimai-get-project-name (cdr (assoc 'project entry)))
                       projects))
             entries)))

    (when activities
      (setq entries
            (seq-filter
             (lambda (entry)
               (member (kimai-get-activity-name (cdr (assoc 'activity entry)))
                       activities))
             entries)))

    (insert (format "Rapport Kimai du %s au %s\n" start end))

    (pcase format-type
      ('table
       (insert "| Date | Projet | Activité | Début | Fin | Durée | Description |\n")
       (insert "|------+--------+----------+-------+-----+--------+-------------|\n")
       (dolist (entry (sort entries
                            (lambda (a b)
                              (string< (cdr (assoc 'begin a))
                                       (cdr (assoc 'begin b))))))
         (let* ((begin (cdr (assoc 'begin entry)))
                (end (cdr (assoc 'end entry)))
                (duration (cdr (assoc 'duration entry)))
                (project (kimai-get-project-name (cdr (assoc 'project entry))))
                (activity (kimai-get-activity-name (cdr (assoc 'activity entry))))
                (description (or (cdr (assoc 'description entry)) "")))
           (setq total-duration (+ total-duration duration))
           (insert (format "| %s | %s | %s | %s | %s | %s | %s |\n"
                           (kimai-format-date begin)
                           project
                           activity
                           (kimai-format-time begin)
                           (kimai-format-time end)
                           (kimai-format-duration duration)
                           description))))
       (insert "|------+--------+----------+-------+-----+--------+-------------|\n")
       (insert (format "| Total | | | | | %s | |\n"
                       (kimai-format-duration total-duration))))

      ('list
       (dolist (entry (sort entries
                            (lambda (a b)
                              (string< (cdr (assoc 'begin a))
                                       (cdr (assoc 'begin b))))))
         (let* ((begin (cdr (assoc 'begin entry)))
                (end (cdr (assoc 'end entry)))
                (duration (cdr (assoc 'duration entry)))
                (project (kimai-get-project-name (cdr (assoc 'project entry))))
                (activity (kimai-get-activity-name (cdr (assoc 'activity entry))))
                (description (or (cdr (assoc 'description entry)) "")))
           (setq total-duration (+ total-duration duration))
           (insert (format "- %s :: %s / %s (%s)\n  %s - %s\n  %s\n"
                           (kimai-format-date begin)
                           project
                           activity
                           (kimai-format-duration duration)
                           (kimai-format-time begin)
                           (kimai-format-time end)
                           description))))
       (insert (format "\nDurée totale : %s\n" (kimai-format-duration total-duration)))))))

;;;###autoload
(defun kimai-insert-report-block ()
  "Insérer un nouveau dynamic block pour un rapport Kimai."
  (interactive)
  (let* ((start-date (read-string "Date de début (YYYY-MM-DD): "
                                  (format-time-string "%Y-%m-01")))
         (end-date (read-string "Date de fin (YYYY-MM-DD): "
                                (format-time-string "%Y-%m-%d")))
         (format-type (completing-read "Format (table/list): "
                                       '("table" "list")
                                       nil t nil nil "table")))
    (org-create-dblock
     `(:name "kimai-report"
             :start-date ,start-date
             :end-date ,end-date
             :format ,(intern format-type)))))


(provide 'kimai)
;;; kimai.el ends here
