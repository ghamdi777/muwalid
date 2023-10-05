(ql:quickload :cl-svg)

(require :simple-rgb)
(require :tecgraf-libs)
(require :serapeum)
(require :dexador)
(require :jonathan)
(require :cl-base64)
(require :skippy)
(require :iup)

(load "colors.lisp")

(defconstant *color-types*
  '("Light"
    "Dark"
    "Random"))

(defconstant *google-api-uri* "secret")
(defconstant *google-api-key* "secret")
(defconstant *google-search-key* "secret")

;;; @AUX

(defun alist-get (key alist)
  (cdr (assoc key alist :test #'equal)))

(defun randsel (list)
  (nth
   (random (length list)) list))

(defun randv (a b)
  (+ (mod (random b) (- b a)) a))

(defun randvn (a b)
  (+ (mod (- (random (1+ (* 2 b))) b) (- b a)) a))

;;; @COLORS

(defun xmlrgb (rgb)
  (apply #'format (nconc (list nil "rgb(~S, ~S, ~S)") (mapcar #'round rgb))))

(defun complrgb (rgb)
  (coerce
   (simple-rgb:complement-rgb (apply #'simple-rgb:rgb (mapcar #'round rgb)))
   'list))

(defun darkrgb (rgb &optional (a 0.5))
  (coerce
   (simple-rgb:darken-rgb (apply #'simple-rgb:rgb (mapcar #'round rgb)) :alpha a)
   'list))

(defun lightrgb (rgb &optional (a 0.5))
  (coerce
   (simple-rgb:lighten-rgb (apply #'simple-rgb:rgb (mapcar #'round rgb)) :alpha a)
   'list))

(defun darkcolors ()
  (loop :for color :in *colors*
	:when (<= (apply #'+ (mapcar (lambda (c) (/ c 255)) color)) 0.7)
	  :collect color))

(defun lightcolors ()
  (loop :for color :in *colors*
	:when (> (apply #'+ (mapcar (lambda (c) (/ c 255)) color)) 0.7)
	  :collect color))

;;; @PLANE

(defun make-plane* (resx resy)
  (list (cons 'res (cons resx resy))
	(cons 'hx (/ resx 2))
	(cons 'hy (/ resy 2))))

(defparameter *paper-sizes* `(("A0" . ,(make-plane* 9933 14043))
			      ("A1" . ,(make-plane* 7016 9933))
			      ("A3" . ,(make-plane* 3508 4961))
			      ("A4" . ,(make-plane* 2480 3508))
			      ("A5" . ,(make-plane* 1748 2480))
			      ("A6" . ,(make-plane* 1240 1748))
			      ("A7" . ,(make-plane* 874 1240))))

;; Raw coords to masked

(defun plane-x-raw (system x)
  (- x (alist-get 'hx system)))

(defun plane-y-raw (system y)
  (- y (alist-get 'hy system)))

(defun plane-raw-coords (system coords)
  (cons (plane-x-raw system (car coords))
	(plane-y-raw system (cdr coords))))

;; Masked coords to raw

(defun plane-x-mask (system x)
  (+ x (alist-get 'hx system)))

(defun plane-y-mask (system y)
  (+ y (alist-get 'hy system)))

(defun plane-masked-coords (system coords)
  (cons (plane-x-mask system (car coords))
	(plane-y-mask system (cdr coords))))

(defun plane-quadrant-signs (quadrant)
  (alist-get quadrant '((1 . (nil . t))
			(2 . (t . t))
			(3 . (nil . nil))
			(4 . (t . nil)))))

(defun plane-quadrant-constraints (system axis)
  (if (eq axis 'x)
      `((- . ,(car (alist-get 'res system))) (+ . ,(alist-get 'hx system)) (- . ,(alist-get 'hx system)) (+ 0))
      `((- . ,(cdr (alist-get 'res system))) (+ . ,(alist-get 'hy system)) (- . ,(alist-get 'hy system)) (+ 0))))

(defun plane-quadrant-raw (system coords)
  "Return SYSTEM's corresponding plane quadrant from COORDS"
  (let ((resl (cons (alist-get 'hx system)
		    (alist-get 'hy system))))
    (cond ((and (< (car coords) (car resl))
		(< (cdr coords) (cdr resl))) 1)
	  ((and (> (car coords) (car resl))
		(> (cdr coords) (cdr resl))) 2)
	  ((and (< (car coords) (car resl))
		(> (cdr coords) (cdr resl))) 3)
	  ((and (> (car coords) (car resl))
		(> (cdr coords) (cdr resl))) 4))))

(defun plane-quadrant-corners (system quadrant &optional (bias (cons 0 0)))
  "Generate SYSTEM's corresponding plane quadrant corners based on QUADRANT"
  (let ((mx (car (alist-get 'res system)))
	(my (cdr (alist-get 'res system)))

	(hx (alist-get 'hx system))
	(hy (alist-get 'hy system))
	
	(pat (plane-quadrant-signs quadrant))
	(corners '()))
    
    ;; There's an arithmetical way of generating
    ;; each plane corner instead of using conditional branching

    (cond ((= quadrant 1)
	   `((0 . ,(- hy (cdr bias)))
	     (,(- hx (car bias)) . ,(- hy (cdr bias)))
	     (0 . 0)
	     (,(- hx (car bias)) . 0)))
	  ((= quadrant 2)
	   `((,hx . ,(- hy (cdr bias)))
	     (,(- mx (car bias)) . ,(- hy (cdr bias)))
	     (,hx . 0)
	     (,(- mx (car bias)) . 0)))
	  ((= quadrant 3)
	   `((0 . ,(- my (cdr bias)))
	     (,(- hx (car bias)) . ,(- my (cdr bias)))
	     (0 . ,hy)
	     (,(- hx (car bias)) . ,hy)))
	  ((= quadrant 4)
	   `((,hx . ,(- my (cdr bias)))
	     (,(- mx (car bias)) . ,(- my (cdr bias)))
	     (,hx . ,hy)
	     (,(- mx (car bias)) . ,hy)))
	  )))

(defun plane-quadrant-constraints (system quadrant)
  (let ((corners (plane-quadrant-corners system quadrant)))
    (list :min-x (car (third corners)) :min-y (cdr (third corners))
	  :max-x (car (second corners)) :max-y (cdr (second corners)))))

;; Plane quadrant perc

(defun plane-quadrant-% (system quadrant env)
  (- 100 (* (/ (apply #'+ (loop :for ob
				:in (alist-get quadrant env)
				:nconc (remove-if-not #'numberp ob)))
	       (* (alist-get 'hx system)
		  (alist-get 'hy system))) 100)))

;; Interning quadrant objects

(defun plane-intern (system object where env &optional method)
  (push object
	(cdr (last (assoc (plane-quadrant-raw
			   system
			   (or (and method (apply method (list system where))) where))
			  env)))))

(defun my-dist (r0 r1)
  (sqrt (+ (expt (abs (- (getf r1 :x) (getf r0 :x))) 2)
	   (expt (abs (- (getf r1 :y) (getf r0 :y))) 2))))

;;; @TEXT
 
(load "options.lisp")

;; Write a more accurate function
(defun text-width (font-size len &optional spacing)
  "Text width as if it were a rectangle"
  (/ (* (/ font-size 2) len) 1.01))

(defun text-height (font-size len &optional spacing)
  "Text width as if it were a rectangle"
  font-size)

;;; @IMAGE

(defun retrieve-images (query)
  (loop :for obj :in
		 (alist-get "items"
			    (jonathan:parse
			     (dexador:get (format nil "~Acustomsearch/v1?key=~A&cx=~A&q=~A"
						  *google-api-uri*
						  *google-api-key*
						  *google-search-key*
						  query)) :as :alist))
	:collect (alist-get "og:image" (car (alist-get "metatags" (alist-get "pagemap" obj))))))

(defun generate-svg-cb ()
  (cl-svg:with-svg-to-file (scene 'cl-svg:svg-1.1-toplevel :width (car (alist-get 'res *paper-size*)) :height (cdr (alist-get 'res *paper-size*)))
      (#p"~/generative/thing.svg" :if-exists :supersede)

    (let* ((upper-right (fourth (plane-quadrant-corners plane 2 (cons (text-width font-size (length text)) 0)))))
      ;; Background
      (cl-svg:draw scene (:rect :x 0 :y 0 :width "100%" :height "100%") :fill (xmlrgb base))

      ;; Student text with top banner
      (cl-svg:draw scene (:rect :x 0 :y 0 :height "8%" :width "100%") :fill (or (and *color-complement* (xmlrgb (complrgb base))) (xmlrgb (darkrgb base)))
		   ;; :stroke-width 5 :stroke-linecap "square" :stroke-dasharray "50,40,60" :stroke (xmlrgb (darkrgb base 0.7))
		   ;; :filter "url(#f3)"
		   ;; ;; :style "border:20px solid black"
		   ;; :rx "20"
		   )
      (format t "X: ~A" (- (car upper-right) 10))
      (force-output)
      (cl-svg:text scene (:x (- (car upper-right) 10) :y font-size) (cl-svg:tspan
									(:fill "white" :font-size font-size)
									text))

      ;; (cl-svg:text scene (:x (- (car
      ;; 			   (fourth (plane-quadrant-corners plane 2 (cons (text-width font-size (length class)) 0)))
      ;; 			   ) 5) :y (* font-size 2)) (cl-svg:tspan
      ;; 							(:fill (xmlrgb (darkrgb base))
      ;; 							 :font-size font-size) class))
      ;; 160, 70
      ;; Path
      ;; 2480
      (format t "~A" (/ (cdr (alist-get 'res plane)) 0.7))
      (force-output)
      (cl-svg:draw scene (:path :d (cl-svg:path
				    (cl-svg:move-to ;; (serapeum:random-in-range -900 -300)
				     (- (/ (car (alist-get 'res plane)) 1.6))
				     (/ (cdr (alist-get 'res plane)) 1.4)
				      
				      ;; 2500
				      )
				     (cl-svg:arc-to-r 100 100 0 0 1 (* (car (alist-get 'res plane)) 2) (cdr (alist-get 'res plane)))
				     )
			   :fill (xmlrgb (darkrgb base 0.5)) :stroke (xmlrgb (lightrgb base)) :stroke-width 5))

      ;; Footer
      (cl-svg:draw scene (:rect :x 0 :y "92%" :height "8%" :width "100%" :fill (or (and *color-complement* (xmlrgb (darkrgb (complrgb base) 0.2))) (xmlrgb (darkrgb base 0.2)))))
      )
    
    ))





(defun image-viewer-cb (handle)
  (let* ((images '())
	 (index 0)
	 (imgview (iup:flat-label :image ""))
	 (okbtn (iup:button :title "Ok" :action (lambda (handle)
						  iup:+default+)))
	 (imgquery (iup:text :value "Image Query"))
	 
	 (searchbtn (iup:button :title "Search" :action (lambda (handle)
							  (setf images (retrieve-images (iup:attribute imgquery 'value)))
							  (setf index 0)
							  (format t "~A" (car images))
							  (force-output)
							  (setf (iup:attribute imgview 'image) (nth index images))
							  iup:+default+)))
	 
	 (nextbtn (iup:button :title "Next" :action (lambda (handle)
						      (incf index)
						      (setf (iup:attribute imgview 'image) (nth index images))
						      iup:+default+)))
	 
	 (prevbtn (iup:button :title "Previous" :action (lambda (handle)
							  (decf index)
							  (setf (iup:attribute imgview 'image) (nth index images))
							  iup:+default+))))
    
    (iup:show
     (iup:dialog
      (iup:vbox
       (list imgquery searchbtn nextbtn prevbtn okbtn imgview))
      :title "Muwalid Image Viewer")))
  iup:+default+)

(defun main ()
  (iup:with-iup ()
    (let* ((label (iup:label :title "مولد" :fontsize 30))
	   (work (iup:label :title "مشروع عبدالرحمن نواف الغامدي" :fontsize 10))

	   (name (iup:text :value "Your Name" :expand :horizontal :valuechanged_cb (lambda (handle)
										     (setf *student-name* (iup:attribute handle 'value))
										     iup:+default+)))
	   (class (iup:text :value "Your Class" :expand :horizontal :valuechanged_cb (lambda (handle)
										       (setf *student-class* (iup:attribute handle 'value))
										       iup:+default+)))
	   (imgviewbtn (iup:button :title "Open Image Viewer"  :action #'image-viewer-cb))

	   (colabel (iup:label :title "Colors" :fontsize 14 :padding "0x5"))

	   (basecolbtn (iup:toggle :title " Base Color"
				   :action (lambda (handle _)
					     (if (string= (iup:attribute handle 'value) "ON")
						 (unwind-protect
						      (progn
							(iup:popup
							 (iup:color-dialog :title "Color Dialog"
									   :showalpha "YES"
									   :showcolortable "YES")
							 iup:+center+
							 iup:+center+
							 )
							))
						 )
					     iup:+default+)))

	   (complbtn (iup:toggle :title " Complementary Colors"
				 :action (lambda (handle _) (setf *color-complement*
							     (not *color-complement*))
					   iup:+default+)))
	   
	   (paptypelist (iup:list :tip "Paper size"
				  :expand :yes 
				  :action (lambda (handle a b c)
					    (setf *paper-size* (cdr (nth (1- (parse-integer (iup:attribute handle 'value))) *paper-sizes*)))
					    iup:+default+)
				  ))

	   (coltypelist (iup:list :tip "Theme of the SVG"
				  :expand :yes
				  :action (lambda (handle a b c)
					    (setf *color-type* (nth (1- (parse-integer (iup:attribute handle 'value))) *color-types*))
					    iup:+default+)
				  ))
	   (genframe (iup:frame
		      (iup:vbox
		       (progn
			 (list name class))
		       )
		      :title "General"
		      ))
	   
	   (papframe (iup:frame
		      (iup:hbox
		       (progn
			 (loop :for i :from 1 :upto (length *paper-sizes*) :do
			   (setf (iup:attribute paptypelist i) (car (nth (1- i) *paper-sizes*))))
			 (list paptypelist)))
		      :title "Paper Size"))

	   (shade (iup:val  
		   :min 0 :max 1
		   :valuechanged_cb (lambda (handle) (setf *alpha-scale* (serapeum:parse-float (iup:attribute handle 'value)))
				      iup:+default+)))
	   
	   (colframe (iup:frame
		      (iup:hbox
		       (progn
			 (loop :for i :from 1 :upto (length *color-types*) :do
			   (setf (iup:attribute coltypelist i) (nth (1- i) *color-types*)))
			 (list coltypelist basecolbtn complbtn shade))
		       )
		      :title "Colors"
		       ))

	   (genbtn (iup:button :title "Generate SVG"
			       :action #'generate-svg-cb
			       ))
	   
	   (vbox
	     (iup:vbox (list label work genframe papframe colframe genbtn)
		       :gap "10"
		       :margin "10x10"
		       :alignment :acenter))
	   (dialog
             (iup:dialog vbox :title "Muwalid")))
      
      (iup:show dialog)
      (iup:main-loop))))
