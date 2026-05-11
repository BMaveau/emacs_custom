;;; project --- Gathering tasks in Roam files and creating overview table in their project file
;;; Commentary:
;;; Fetches the projects with project--projects


;;; Code:
(require 'org-roam)
(require 'org-roam-dailies)

(defun proroam--projects ()
  "Find all node ids with a Project tag."
  (let* ((project-ids (flatten-tree (org-roam-db-query [:select [node-id] :from tags :where (= tag "Project")] )))
		 (projects (mapcar (lambda (n) (nth 0 (org-roam-db-query [:select [id file title] :from nodes :where (= id $s1)] n)))
						   project-ids))
		 (projects (mapcar (lambda (p)
							 (with-current-buffer
								 (generate-new-buffer "customroam")
							   (insert-file-contents (nth 1 p))
							   (goto-char (point-min))
							   (append p (list (if (search-forward-regexp "^- habitat-id: \\([0-9-]+\\)$" nil t) (match-string 1) "no-id")))))
						   projects)))
	projects
	)
  )

(defun proroam--tags (tag-to-find)
  "Find all node ids with a TAG-TO-FIND.
The files with the TAG-TO-FIND should contain a line at the top with the
associated project.  returns a list of list, with the innermost list
containing the project id, the file id where TAG-TO-FIND was found, filename and
title of the task."
  (let ((tasks ())
		(projects (proroam--projects)))
	(setq tasks (flatten-tree (org-roam-db-query [:select [node-id] :from tags :where (= tag "Task")] )))
	(setq tasks
		  (mapcar (lambda (n)
					(nth 0 (org-roam-db-query [:select [id file title] :from nodes :where (= id $s1)] n)))
				  tasks
				  ))
	(setq tasks (mapcar (lambda (n)
						  (with-current-buffer (generate-new-buffer "customroam")
							(insert-file-contents (nth 1 n))
							(goto-char (point-min))
							(if (search-forward-regexp "^- Project: \\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]$" nil t)
								(cons (match-string 1) n)
							  nil)))
						tasks)
		  )
	)
  )



(defun proroam--reminders ()
  "Find all node ids with a Reminder tag.
Returns a list of list, with the innermost list containing the
project id, the task id, filename and title of the task."
  (proroam--tags "Task")
  )


(defun proroam--tasks ()
  "Find all node ids with a Task tag.
Returns a list of list, with the innermost list containing the
project id, the task id, filename and title of the task."
  (proroam--tags "Task")
  (let ((tasks ())
		(projects (proroam--projects)))
	(setq tasks (flatten-tree (org-roam-db-query [:select [node-id] :from tags :where (= tag "Task")] )))
	(setq tasks
		  (mapcar (lambda (n)
					(nth 0 (org-roam-db-query [:select [id file title] :from nodes :where (= id $s1)] n)))
				  tasks
				  ))
	(setq tasks (mapcar (lambda (n)
						  (with-current-buffer (generate-new-buffer "customroam")
							(insert-file-contents (nth 1 n))
							(goto-char (point-min))
							(if (search-forward-regexp "^- Project: \\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]$" nil t)
								(cons (match-string 1) n)
							  nil)))
						tasks)
		  )
	)
  )

(defun proroam--find-project (pid)
  "Find the project with the corresponding PID."
  (let ((found nil)
		(project nil)
		(projects (proroam--projects)))
	(while (and (not found) projects)
	  (if (not (string= pid (nth 0 (car projects))))
		  (setq projects (cdr projects))
		(setq project (car projects))
		(setq found t)
		)
	  )
	project
	)
  )


(defun proroam-get-status (task)
  "Decides if the TASK is finished or ongoing."
  (with-current-buffer (find-file-noselect (nth 2 task))
	(let ((status ""))
	  (setq status (if (seq-contains-p (org-map-entries '(or (org-entry-get nil "TODO") "DONE")) "TODO") "TODO" "DONE"))
	  (put-text-property 0 4 'font-lock-face 'org-todo status)
	  status
	  ))
  )

(defun proroam-get-effort (task)
  "Sums the total estimated effort of the TASK."
  (with-current-buffer (find-file-noselect (nth 2 task))
	(org-duration-from-minutes (-sum (org-map-entries '(org-duration-to-minutes (or (org-entry-get nil "EFFORT") "")))))
	)
  )

(defun proroam-get-tspent (task)
  "Gets the total time spent on TASK"
  (with-current-buffer (find-file-noselect (nth 2 task))
	(org-clock-sum)
	(org-duration-from-minutes org-clock-file-total-minutes)
	)
  )

(defun proroam-get-deadline (task)
  (with-current-buffer (find-file-noselect (nth 2 task))
	(let ((deadline (org-map-entries '(lambda () (if (string= (org-entry-get nil "TODO") "TODO") (org-entry-get nil "DEADLINE") nil)))))
	  (-reduce-from (lambda (cum it) (cond ((and cum it (date< cum it)) cum) ((and cum it) it) (cum) (it))) nil deadline)
	  )
	)
  )

(defun date< (a b)
  (let* ((lhs (parse-time-string a))
		 (rhs (parse-time-string b))
		 (comp (-zip-with (lambda (a b) (value< a b))  lhs rhs)))
	(cond ((/= (nth 5 lhs) (nth 5 rhs)) (nth 5 comp))
		  ((/= (nth 4 lhs) (nth 4 rhs)) (nth 4 comp))
		  ((/= (nth 3 lhs) (nth 3 rhs)) (nth 3 comp))
		  (t nil))
	)
  )



;; (defun test-parsing ()
;;   (let ((todo t)
;; 		(effort 0)
;; 		(tspent 0))
;; 	(setq todo (or (org-entry-get nil "TODO") "DONE"))
;; 	(setq effort (org-duration-to-minutes (or (org-entry-get nil "EFFORT") "")))
;; 	(setq tspent (org-duration-to-minutes (or (org-entry-get nil "CLOCKSUM") "")))
;; 	(list todo effort tspent)
;; 	))

(setq proroam-table-format
	  '(("Task" (lambda (task) (format "[[id:%s][%s]]" (nth 1 task ) (nth 3 task))))
		("Status" proroam-get-status)
		("Timing" proroam-get-tspent)
		("Effort" proroam-get-effort))
	  ;; 	  "The formatting of the task overview table for every project.
	  ;; Consists of a list of list with the innermost list
	  ;; containing as car the column name and as cdr a function which
	  ;; takes the task as argument and returns the content of that cell
	  ;; as string."
	  )


;; Write table with tasks per project
;; Create table per project

(defun proroam--write-task-table ()
  "Writes in every project file the task overview table."
  (mapc (lambda (p)
		  (with-current-buffer (find-file-noselect (nth 1 p))
			(goto-char (point-min))
			(if (search-forward "* Overview Task" nil t)
				(progn
				  (message "%d" (point))
				  (delete-region (point) (point-max)))
			  (progn
				(goto-char (point-max))
				(insert "* Overview Task")))
			(insert "\n\n")
			(goto-char (point-max))
			(org-table-create (format "%dx2" (length proroam-table-format)))
			
			(dolist (header (mapcar 'car proroam-table-format))
			  (org-table-next-field)
			  (insert header))
			(dolist (ta (proroam--tasks))
			  (when  (string= (nth 0 ta) (nth 0 p))
				(dolist (field (mapcar 'cadr proroam-table-format))
				  (org-table-next-field)
				  (insert (funcall field ta)))
				)
			  )
			(org-table-align)
			))
		(proroam--projects))
  )

(defun proroam--meeting-time (date pro-time)
  (let ((rdir (expand-file-name org-roam-dailies-directory org-roam-directory))
		(projects '())
		(durations "")
		(filename (concat date ".org" )))
	(when (file-exists-p (file-name-concat rdir filename))
	  (with-current-buffer (find-file-noselect (file-name-concat rdir filename))
		(goto-char (point-min))
		(setq projects (org-map-entries '(or (org-entry-get nil "PROJECT") "None")))
		(setq durations (org-map-entries (lambda () (/ (org-clock-sum-current-item) 60.0))))
		)
	  )
	(when (-contains? projects "None")
	  (insert (format "%s contains unassigned meetings\n" filename)))
	(dotimes (idx (length projects))
	  (let ((task-time (nth idx durations))
			(project-id (nth idx projects))
			(prev-time 0))
		(unless (string= project-id "None")
		  (setq project-id (substring project-id 5 41))
		  (setq prev-time (or (plist-get pro-time project-id 'string=) 0))
		  (when (> task-time 0)
			(setq pro-time (plist-put pro-time project-id (+ prev-time task-time) 'string=)))
		  )
		)
	  )
	pro-time
	)
  )

(defun proroam-timesheet (week-number)
  (let ( (pro-time '())
		 (sdate '())
		 (fdate '()))
	(setq sdate (parse-time-string (format "2026-W%.2d" week-number)))
	(setq fdate (decoded-time-add  sdate '(nil nil nil 7)))
	(let ((date sdate))
	  (dotimes (days 7)
		(setq pro-time (proroam--meeting-time (format "%d-%.2d-%.2d" (nth 5 date) (nth 4 date) (nth 3 date)) pro-time))
		(setq date (decoded-time-add date '(nil nil nil 1)))
		)
	  )
	
	(setq sdate (format "%d-%.2d-%.2d" (nth 5 sdate) (nth 4 sdate) (nth 3 sdate)))
	(setq fdate (format "%d-%.2d-%.2d" (nth 5 fdate) (nth 4 fdate) (nth 3 fdate)))
	(dolist (task (proroam--tasks))
	  (when (nth 2 task)
		(with-current-buffer (find-file-noselect (nth 2 task))
		  (let ((task-time (/ (org-clock-sum sdate fdate) 60.0))
				(prev-time (or (plist-get pro-time (nth 0 task) 'string=) 0)))
			(when (> task-time 0)
			  (setq pro-time (plist-put pro-time (nth 0 task) (+ prev-time task-time) 'string=)))
			)
		  )
		)
	  )
	(dolist (project (proroam--projects))
	  (with-current-buffer (find-file-noselect (nth 1 project))
		(let ((task-time (/ (org-clock-sum sdate fdate) 60.0))
			  (prev-time (or (plist-get pro-time (nth 0 project) 'string=) 0)))
		  (when (> task-time 0)
			(setq pro-time (plist-put pro-time (nth 0 project) (+ prev-time task-time) 'string=)))
		  )
		)
	  )
	;; Build list of (project-id . hours) pairs
	(let ((project-hours '())
		  (daily-schedule '())  ;; Will hold list of days, each day is list of (project-id . hours)
		  (total 0))
	  
	  ;; Collect all project hours
	  (dotimes (idx (/ (length pro-time) 2))
		(let ((pid (nth (* 2 idx) pro-time))
			  (hours (nth (+ (* 2 idx) 1) pro-time)))
		  (push (cons pid hours) project-hours)))
	  
	  ;; Sort by hours descending to fill with bigger blocks first
	  (setq project-hours (sort project-hours (lambda (a b) (> (cdr a) (cdr b)))))
	  
	  ;; Initialize 5 days with empty lists
	  (dotimes (i 5)
		(push '() daily-schedule))
	  (setq daily-schedule (nreverse daily-schedule))
	  
	  ;; Distribute hours across days, filling up to 7.8h per day
	  (dolist (proj project-hours)
		(let ((remaining-hours (cdr proj))
			  (proj-id (car proj))
			  (day-idx 0))
		  (while (and (> remaining-hours 0) (< day-idx 5))
			(let* ((day-total (apply '+ (mapcar 'cdr (nth day-idx daily-schedule))))
				   (day-space (- 7.8 day-total))
				   (to-add (min remaining-hours day-space)))
			  (when (> to-add 0)
				(setf (nth day-idx daily-schedule)
					  (cons (cons proj-id to-add) (nth day-idx daily-schedule)))
				(setq remaining-hours (- remaining-hours to-add)))
			  (setq day-idx (1+ day-idx))))))
	  
	  ;; Write header line with tabs
	  (insert "Type\tStatus\tProject No.\tProject Description\tApprover ID\tProject Task No.\tDescription\tMon\tTue\tWed\tThu\tFri\n")
	  
	  ;; Write table rows for each project
	  (dotimes (idx (/ (length pro-time) 2))
		(let* ((proj-id (nth (* 2 idx) pro-time))
			   (total-hours (nth (+ (* 2 idx) 1) pro-time))
			   (project (proroam--find-project proj-id))
			   (habitat-id (nth 3 project))
			   (project-no (if (string= habitat-id "no-id") (nth 2 project) habitat-id))
			   (project-desc (nth 2 project)))
		  ;; Type, Status, Project No., Project Description
		  (insert (format "Project\tSubmitted\t%s\t%s\t\t\t\t" project-no project-desc))
		  ;; Hours for each day (Mon-Fri)
		  (dotimes (day 5)
			(let ((day-hours (cdr (assoc proj-id (nth day daily-schedule) 'string=))))
			  (insert (if day-hours (replace-regexp-in-string "\\." "," (format "%.1f" day-hours)) ""))
			  (insert "\t")))
		  ;; Total
		  ;; (insert (replace-regexp-in-string "\\." "," (format "%.1f" total-hours)))
		  (insert "\n")
		  ))))
  )

;; Type	Status	Project No.	Project Description	Approver ID	Project Task No.	Description	1 Mon	2 Tue	3 Wed	4 Thu	5 Fri	6 Sat	7 Sun	Total
;; Project	Submitted	HR001	Holiday	STEFAAN.WIJNANTS	1	General					7,8			7,8
;; Project	Submitted	2022-0587	Feed4Flex_SBO	LAURENT.DECLERCQ	602	T3.4: Border of line layout design of workplace considering ergonomics and safety	5							5
;; Project	Submitted	2024-0286	OptiBATT_IRVA	ANKIT.SURTI	303	T1.3 Link to topology optimization toolset (CodesignS)	2,8	7,8	5					15,6
;; Project	Submitted	2025-2939	CodesignS_Project_Writing_2025	SOFIE.BURGGRAEVE	422	2025-4188 DAPS5,0			2,8	2,8				5,6
;; Project	Submitted	2024-1453	Flexline_IRVA	DORIEN.REYNDERS	WP2	WP2: Supporting Model and Optimization routines				5				5


(defun proroam-start-meeting (meeting-name project)
  "Start a meeting.
Insert an entry in the diary of today with header MEETING-NAME.  The
property PROJECT with value PROJECT is added and the clock is started."
  (interactive
   (list
	(read-string "Meeting name: " nil nil "Placeholder")
	(completing-read "Select a project: " (mapcar (lambda (p) (nth 2 p)) (proroam--projects)) nil t)
	))
  (setq project (seq-find (lambda (p) (string= (nth 2 p) project)) (proroam--projects)))
  (org-roam-dailies-goto-today)
  (goto-char (point-max))
  (insert (format "* %s\n" meeting-name))
  (org-set-property "PROJECT" (format "[[id:%s][%s]]" (nth 0 project) (nth 2 project)))
  (org-clock-in))

;; org-map-entries to loop over all headings
;; org-entry-get nil "TODO" to get the TODO status
;; (org-duration-from-minutes (org-clock-sum "[2024-11-21]" "[2024-11-22]")) to get the duration from a to b
;; (org-entry-get nil "EFFORT") get effort estimation
;; (org-entry-get nil "TODO") get TODO status
;; (org-clock-sum) -> set property CLOKCSUM -> determine diff

;; (defun test-parsing ()
;;   (let ((todo t)
;; 		(effort 0))
;; 	(setq todo (org-entry-get nil "TODO"))
;; 	(setq effort ((org-duration-to-minutes (org-entry-get nil "EFFORT")))
;; 	(list todo effort)
;; 	))
;; (with-current-buffer (find-file-noselect (nth 2 (nth 6 (proroam--tasks))))
;;   (org-map-entries 'test-parsing))

;; (let ( (pro-time '())
;; 	   (sdate "[2025-02-24]")
;; 	   (fdate "[2025-03-03]"))
;;   (dolist (task (proroam--tasks))
;; 	(when (nth 2 task)
;; 	  (with-current-buffer (find-file-noselect (nth 2 task))
;; 		(let ((task-time (/ (org-clock-sum sdate fdate) 60))
;; 			  (prev-time (or (plist-get pro-time (nth 0 task) 'string=) 0)))
;; 		  (when (> task-time 0)
;; 		  (setq pro-time (plist-put pro-time (nth 0 task) (+ prev-time task-time) 'string=)))
;; 		  )
;; 		)
;; 	  )
;; 	)
;;   (dolist (project (proroam--projects))
;; 	(with-current-buffer (find-file-noselect (nth 1 project))
;; 	  (let ((task-time (/ (org-clock-sum sdate fdate) 60.0))
;; 			(prev-time (or (plist-get pro-time (nth 0 project) 'string=) 0)))
;; 		(when (> task-time 0)
;; 			(setq pro-time (plist-put pro-time (nth 0 project) (+ prev-time task-time) 'string=)))
;; 		  )
;; 		)
;; 	)
;;   (dotimes (idx (/ (length pro-time) 2))
;; 	(insert (concat (nth 2 (proroam--find-project (nth (* 2 idx) pro-time))) "\t" (format "%.2f" (nth (+ (* 2 idx) 1) pro-time)) "\n"))
;; 	)
;;   )

;; (setq tasks (proroam--tasks))
;; (setq ta (seq-find (lambda (ta) (string= (nth 1 ta) "7c282dc5-4fe4-4b99-b188-5ad1130336e7")) tasks))
;; (setq deadline (proroam-get-deadline ta))
;; (-reduce-from (lambda (cum it) (cond ((and cum it) cum) (cum) (it))) nil deadline)

(provide 'proroam)
;;; project.el ends here



