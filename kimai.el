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



(defun kimai-fetch-projects ()
  "Récupère la liste des projets depuis Kimai."
  (kimai-api-request "/projects" "GET"))


(defun kimai-fetch-activities ()
  "Récupère la liste des activités depuis Kimai."
  (kimai-api-request "/activities" "GET"))

(defun kimai-prompt-for-project ()
  "Invite l'utilisateur à sélectionner un projet."
  (let* ((projects (kimai-fetch-projects))
         (project-names (mapcar (lambda (p) (cons (cdr (assoc 'name p)) (cdr (assoc 'id p)))) projects)))
    (cdr (assoc (completing-read "Sélectionnez un projet: " project-names) project-names))))

(defun kimai-prompt-for-activity ()
  "Invite l'utilisateur à sélectionner une activité."
  (let* ((activities (kimai-fetch-activities))
         (activity-names (mapcar (lambda (a) (cons (cdr (assoc 'name a)) (cdr (assoc 'id a)))) activities)))
    (cdr (assoc (completing-read "Sélectionnez une activité: " activity-names) activity-names))))

;;;###autoload
(defun kimai-start-tracking ()
  "Démarre un suivi de temps pour un projet et une activité spécifiques."
  (interactive)
  (let* ((project-id (kimai-prompt-for-project))
         (activity-id (kimai-prompt-for-activity))
         (description (read-string "Entrez une description: "))
         (response (kimai-api-request "/timesheets" "POST"
                                      `(("project" . ,project-id)
                                        ("activity" . ,activity-id)
                                        ("description" . ,description)))))
    (if (assoc 'id response)
        (progn
          (setq kimai-active-timer-id (cdr (assoc 'id response)))
          (message "Suivi de temps démarré avec ID: %s" kimai-active-timer-id))
      (message "Erreur lors du démarrage du suivi de temps: %s" response))))

;;;###autoload
(defun kimai-stop-tracking ()
  "Arrête le suivi de temps actif."
  (interactive)
  (if kimai-active-timer-id
      (let ((response (kimai-api-request (format "/timesheets/%s/stop" kimai-active-timer-id) "PATCH")))
        (if (assoc 'id response)
            (progn
              (message "Suivi de temps arrêté pour l'ID: %s" kimai-active-timer-id)
              (setq kimai-active-timer-id nil))
          (message "Erreur lors de l'arrêt du suivi de temps: %s" response)))
    (message "Aucun suivi de temps actif.")))

(provide 'kimai)
;;; kimai.el ends here
