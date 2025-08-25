;;; treemacs-autoadjust.el  -*- lexical-binding: t -*-
;; Automatically adjusts the Treemacs sidebar width based on its content,
;; up to a configurable maximum percentage of the frame width, and respecting
;; a minimum character width.

;;; How to use:
;;
;; Enable the mode by adding the following to your Emacs configuration
;; (after ensuring this file is in your load-path):
;;
;;   (require 'treemacs-autoadjust)
;;   (treemacs-autoadjust-mode 1)
;;
;; Alternatively, enable it interactively with `M-x treemacs-autoadjust-mode`.
;;
;; The maximum width of the Treemacs window is controlled by the
;; `treemacs-max-width-ratio` customizable variable. The default value of 1
;; means it can take up to 100% of the frame width if the content requires it.
;; A value of 0.5, for example, means the Treemacs window will not exceed 50%
;; of the frame's width.
;;
;; The minimum width of the Treemacs window is controlled by the
;; `treemacs-min-width-chars` customizable variable. The default value is 20
;; characters.
;;
;; You can customize these variables using `M-x customize-group RET treemacs RET`.

;; Written by: thinkyfish (thinkyfish@gmail.com, github.com/thinkyfish)
;; released under MIT License and GPLv3.

;;; Custom Widget Types for Validation

(require 'wid-edit) ; Ensure widget editing functions are available

(define-widget 'treemacs-width-ratio-type 'float
  "Float value above 0.0 and less than or equal to 1.0"
  :value 1.0
  :validate (lambda (widget)
              (let ((value (widget-value widget)))
                (if (and (floatp value) (> value 0.0) (<= value 1.0))
					nil
                  (progn
				  (widget-put widget :error "Value must be above 0.0 and less than or equal to 1.0.")
				  widget))))
  :group 'treemacs)

(define-widget 'treemacs-min-width-type 'integer
  "Integer value greater than 1."
  :validate (lambda (widget)
              (let ((value (widget-value widget)))
                (if (and (integerp value) (> value 1))
					nil
                  	(progn
						(widget-put widget :error "Value must be an integer greater than 1.")
                  		widget)
                )))
  :group 'treemacs)


;;; Customizable Variables

(defcustom treemacs-max-width-ratio 1.0
  "The maximum width of the Treemacs window as a ratio of the frame width.
For example, 0.50 means the Treemacs window will not exceed 50% of
the frame's width. Must be between 0.0 and 1.0."
  :type 'treemacs-width-ratio-type
  :group 'treemacs)

(defcustom treemacs-min-width-chars 20
  "The minimum width of the Treemacs window in characters.
Must be greater than 1."
  :type 'treemacs-min-width-type
  :group 'treemacs)

;;; Suppress warnings.  Could also use autoload but I didn't
;; want to mess with other files.
(defun treemacs-get-local-window ())

;;; Core Logic

(defun treemacs-fit-window-to-buffer ()
  ;; we cannot use the built in fit-window-to buffer,
  ;; because of the treemacs buffer type.
  ;; this based on the built-in fit-window-to-buffer because
  ;; it uses C functions to determine the width of
  ;; of the buffer, saving performance. 
  "Adjust size of treemacs sidebar to display its buffer's contents exactly.
Respects `treemacs-min-width-chars` and `treemacs-max-width-ratio`."
  (interactive)
  (when-let* ((window (treemacs-get-local-window)))
  (let* (
		 (frame (window-frame window))
		 (pixelwise window-resize-pixelwise)
         (char-width (frame-char-width frame))
		 (char-height (frame-char-height frame))
         (total-width (window-size window t pixelwise))
         (user-min-width ; Minimum size based on user setting
           (if pixelwise
               (* char-width treemacs-min-width-chars)
             treemacs-min-width-chars))
         (min-width (max (window-min-size window nil window pixelwise) user-min-width)) ; Final minimum width
         (max-width (round (* (frame-width frame) treemacs-max-width-ratio))) ; Compute custom width


	     ;; When fitting horizontally, assume that WINDOW's
	     ;; start position remains unaltered. WINDOW can't get
	     ;; wider than its frame's pixel width, its height
	     ;; remains unaltered. Calculate required width based on content.
	     (width (+ (car (window-text-pixel-size
						 window (window-start window) nil
						 (frame-pixel-width (window-frame window))
						 ;; Add one line-height to assure that
						 ;; we're on the safe side. This
						 ;; overshoots when the first line below
						 ;; the bottom is wider than the window.
						 (* (window-body-height window pixelwise)
							(if pixelwise 1 char-height))))
                   (- (* total-width (if pixelwise 1 char-width))
                      (window-body-width window t)))))
	(unless pixelwise
	  (setq width (/ (+ width char-width -1) char-width)))
    ;; Clamp the calculated width between min-width and max-width
    (setq width (max min-width (min max-width width)))
	
	;; resize only if we aren't already the right size
	(unless (= width total-width)  
	  (window-preserve-size window t)
	  (window-resize-no-error
       window (- width total-width) t window pixelwise)))))


(defun advice-add* (functions where advice-function)
  "Add ADVICE-FUNCTION to each function in FUNCTIONS at position WHERE.
WHERE should be a keyword like :after, :before, :override, etc."
  (dolist (func functions)
    (advice-add func where advice-function)))


(defun advice-remove* (functions advice-function)
  "Remove ADVICE-FUNCTION from each function in FUNCTIONS."
  (dolist (func functions)
    (advice-remove func advice-function)))


(defun treemacs-autoadjust-noparam ()
  (treemacs-fit-window-to-buffer))


(defun treemacs-autoadjust-1param (param)
  (ignore param) ;; throw away a single parameter
  (treemacs-fit-window-to-buffer))


(defun treemacs-autoadjust-opparam (&optional param)
  (ignore param) ;; throw away an optional parameter
  (treemacs-fit-window-to-buffer))


(defun treemacs-autoadjust-onscroll (window _display-start)
  "Adjust Treemacs width when its window scrolls.
Intended for use in `window-scroll-functions`. Checks if WINDOW
is the Treemacs window and calls `treemacs-fit-window-to-buffer`."
  (and (eq window (treemacs-get-local-window))
	   (treemacs-fit-window-to-buffer)))

;;;###autoload
(define-minor-mode treemacs-autoadjust-mode
  "Minor mode to automatically adjust Treemacs window width.
Respects `treemacs-min-width-chars` and `treemacs-max-width-ratio`."
  :init-value nil
  ;; :lighter " TrAdjust"
  :global t  ; Make it global so it affects Treemacs whenever it runs
  :group 'treemacs
  ;; we need to advise these separately because emacs buggs if we
  ;; try to use (&optional) or (interactive)
  (let* ((advise-1param  '(treemacs-leftclick-action
						   treemacs-single-click-expand-action
						   treemacs-rightclick-menu
						   treemacs-collapse-parent-node))
		 (advise-noparam '(treemacs--follow
						   treemacs-rename-file))
		 (advise-opparam '(treemacs
						   treemacs-collapse-all-projects
						   treemacs-COLLAPSE-action
						   treemacs-previous-line
						   treemacs-next-line
						   treemacs-TAB-action
						   treemacs-RET-action)))

	(if treemacs-autoadjust-mode
		;; Mode enabled: add advice and hooks
		(progn
          (message "Treemacs auto-adjust mode enabled.")
          (advice-add* advise-1param :after #'treemacs-autoadjust-1param)
		  (advice-add* advise-noparam :after #'treemacs-autoadjust-noparam)
		  (advice-add* advise-opparam :after #'treemacs-autoadjust-opparam)
          ;; Add hook globally; the hook function itself checks the window.
          (add-hook 'window-scroll-functions #'treemacs-autoadjust-onscroll)
		  (treemacs-fit-window-to-buffer))
	  ;; Mode disabled: remove advice and hooks
	  (progn
		(message "Treemacs auto-adjust mode disabled.")
		(advice-remove* advise-1param #'treemacs-autoadjust-1param)
		(advice-remove* advise-noparam #'treemacs-autoadjust-noparam)
		(advice-remove* advise-opparam #'treemacs-autoadjust-opparam)
        ;; Remove the global hook.
        (remove-hook 'window-scroll-functions #'treemacs-autoadjust-onscroll)
		))))


(provide 'treemacs-autoadjust)
;;(package-generate-autoloads "treemacs" "~/.emacs.d/elpa/treemacs-20250412.130045/")
