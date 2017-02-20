(defvar prodigy-dependency-services nil
  "An alist of the service name and some properties that aid in
  managing service dependency.")

(defvar prodigy-dependency-service-status-change-hook nil
  "A hook for listening in service status changes.")

(defconst prodigy-dependency-condition-prefix "prodigy-dependency-condition"
  "A symbol prefix for easier debugging.")


(defun prodigy-set-service-waiting (service)
  "Set SERVICE to `waiting'"
  (unless (-any-p
           (lambda (status)
             (equal (plist-get status :id) 'waiting))
           prodigy-status-list)
    (prodigy-define-status :id 'waiting :face 'prodigy-yellow-face))
  (prodigy-set-status service 'waiting))


(defun prodigy-service-will-p (service action)
  "Check if SERVICE has an ACTION entry."
  (-if-let (dependency-props (cdr (assoc (plist-get service :name) prodigy-dependency-services)))
      (equal (plist-get dependency-props :action) action)
    nil))

(defun prodigy-service-will-start-p (service)
  "Check if SERVICE has an dependency start entry."
  (prodigy-service-will-p service 'start ))

(defun prodigy-service-will-stop-p (service)
  "Check if SERVICE has an dependency stop entry."
  (prodigy-service-will-p service 'stop))

(defun prodigy-service-conditions-statisfied-p (service)
  "Check if SERVICE conditions are satisfied."
  (-if-let (dependency-props (cdr (assoc (plist-get service :name) prodigy-dependency-services)))
      (-every-p #'funcall (plist-get dependency-props :conditions))
    nil))


(defun prodigy-update-service-dependencies ()
  "Trigger service dependencies."
  (-each
      prodigy-dependency-services
    (lambda (dependency-pair)
      (lexical-let* ((service-name (car dependency-pair))
          (service (prodigy-find-service service-name))
          (service-props (cdr dependency-pair))
          (action (plist-get service-props :action))
          (args (plist-get service-props :args))
          (conditions (plist-get service-props :conditions)))
        (when (and action
                 (-every-p #'funcall conditions))
          (pcase action
            ('start
             ;; (message "[trigger-start] %s" service-name)
             (apply #'prodigy-start-service service args))
            ('stop
             ;; (message "[trigger-stop] %s" service-name)
             (apply #'prodigy-stop-service service args))))))))


(defun prodigy-service-add-to-dependents (service)
  "Guarantee SERVICE exist in `prodigy-dependency-services'."
  (lexical-let ((service-name (plist-get service :name)))
    (unless (assoc service-name prodigy-dependency-services)
      (push (cons service-name nil) prodigy-dependency-services))))

(defun prodigy-service-remove-from-dependents (service)
  "Remove SERVICE properties in `prodigy-dependency-services'."
  (lexical-let ((service-name (plist-get service :name)))
    (-when-let (service-entry (assoc service-name prodigy-dependency-services))
      (setcdr service-entry nil)
      (when (equal (plist-get service :status) 'waiting)
        (prodigy-set-status service nil)))))

(defun prodigy-service-dependents (service)
  "Find all services who depends on SERVICE."
  (lexical-let ((service-name (plist-get service :name)))
    (-select
     (lambda (this-service)
       (-when-let (dependency-pairs (plist-get this-service :depends-on))
         (-any
          (lambda (dependency-pair)
            (pcase-let ((`(,this-service-name . _) dependency-pair))
              (equal this-service-name service-name)))
          dependency-pairs)))
     prodigy-services)))

(defun prodigy-service-dependencies (service)
  "Given a SERVICE, return the services it depends on."
  (-when-let (dependency-pairs (plist-get service :depends-on))
    (-map (-compose #'prodigy-find-service #'car)
          dependency-pairs)))


(defun prodigy-create-service-status-condition (depend-property)
  "Create a start condition based on DEPEND-PROPERTY."
  (lexical-let* ((service-name (car depend-property))
      (target-status (cdr depend-property))
      (function-symbol
       (intern (format "%s--%s--%s--started-p"
                       prodigy-dependency-condition-prefix
                       service-name
                       (symbol-name target-status))))
      (status-reached nil))
    (fset
     function-symbol
     (lambda (&rest _args)
       (lexical-let* ((this-service (prodigy-find-service service-name))
           (this-status (plist-get this-service :status)))
         ;; (message "[check] %s %s %s" service-name this-status target-status)
         (unless status-reached
           (when (equal this-status target-status)
             (setq status-reached t)
             (remove-hook 'prodigy-dependency-service-status-change-hook function-symbol)))
         (and
          (prodigy-service-started-p this-service)
          status-reached))))
    (add-hook 'prodigy-dependency-service-status-change-hook function-symbol)
    function-symbol))

(defun prodigy-create-service-stop-condition (service)
  "Create a stop condition based on SERVICE."
  (lexical-let* ((service-name (plist-get service :name))
      (function-symbol
       (intern (format "%s--%s--stopped-p"
                       prodigy-dependency-condition-prefix
                       service-name))))
    (fset
     function-symbol
     (lambda (&rest _args)
       (lexical-let* ((this-service (prodigy-find-service service-name)))
         (or (not (prodigy-service-started-p this-service))
            (prodigy-service-stopping-p this-service)))))
    function-symbol))

(defun prodigy-start-service-dependency (starter service &optional callback)
  "Start a managed SERVICE with dependencies.

If SERVICE has a `:depends-on' property, it starts its
dependencies first before this one."
  (prodigy-service-add-to-dependents service)
  (-if-let (dependency-entries (plist-get service :depends-on))
      (unless (prodigy-service-started-p service)
        (lexical-let ((service-name (plist-get service :name)))
          (if (prodigy-service-will-start-p service)
              (if (prodigy-service-conditions-statisfied-p service)
                  (prog1
                      (apply starter service callback)
                    (prodigy-service-remove-from-dependents service))
                (-each
                    (prodigy-service-dependencies service)
                  #'prodigy-start-service))
            (lexical-let ((new-props
                 (list :action 'start
                    :args (list callback)
                    :dependencies (-map #'car dependency-entries)
                    :conditions (-map #'prodigy-create-service-status-condition dependency-entries))))
              (prog1
                  nil
                (setcdr (assoc service-name prodigy-dependency-services) new-props)
                (prodigy-set-service-waiting service)
                (-each
                    (prodigy-service-dependencies service)
                  #'prodigy-start-service)
                (prodigy-update-service-dependencies))))))
    (funcall starter service callback)))

(defun prodigy-stop-service-dependency (stopper service &optional force callback)
  "Stop a managed SERVICE with dependencies.

If SERVICE has a `:depends-on' property, it stops this service
first, then its dependencies and finally its service dependent on
this one."
  (prodigy-service-add-to-dependents service)
  (-if-let (dependent-services (prodigy-service-dependents service))
      (unless (prodigy-service-stopping-p service)
        (lexical-let ((service-name (plist-get service :name)))
          (if (prodigy-service-will-stop-p service)
              (if (prodigy-service-conditions-statisfied-p service)
                  (prog1
                      (apply stopper service force callback)
                    (prodigy-service-remove-from-dependents service))
                (-each
                    dependent-services
                  #'prodigy-stop-service))
            (lexical-let ((new-props
                 (list :action 'stop
                    :args (list force callback)
                    :dependents (-map (lambda (dependent-service)
                                        (plist-get dependent-service :name))
                                      dependent-services)
                    :conditions (-map #'prodigy-create-service-stop-condition dependent-services))))
              (prog1
                  nil
                (setcdr (assoc service-name prodigy-dependency-services) new-props)
                (prodigy-set-service-waiting service)
                (-each
                    dependent-services
                  #'prodigy-stop-service)
                (prodigy-update-service-dependencies))))))
    (funcall stopper service force callback)))

(defun prodigy-set-status-dependency (service status)
  "Listen to SERVICE STATUS changes

If a depedency is waiting on a status change."
  ;; (message "[state-change] %s %s %s" (plist-get service :name) (plist-get service :status) status)
  (run-hook-with-args 'prodigy-service-status-change-hook service status)
  (prodigy-update-service-dependencies))

(advice-add 'prodigy-start-service :around #'prodigy-start-service-dependency)
(advice-add 'prodigy-stop-service :around #'prodigy-stop-service-dependency)
(advice-add 'prodigy-set-status :after #'prodigy-set-status-dependency)