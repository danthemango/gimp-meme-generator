(script-fu-register
 "script-fu-meme-generator"
 _"Meme Text"
 _"Generate Meme Text :o"
 "Eduardo Vazquez (hao)"
 "Public Domain"
 "2015-09-18"
 ""
 SF-IMAGE      "Image" 1
 SF-TEXT       "Text"  "the meme"
 SF-FONT       "Font"  "Impact Condensed"    
 SF-ADJUSTMENT "Font size (pixels)" '(150 2 1000 1 10 0 1)
 SF-ADJUSTMENT "border ratio" '(0.1 0.0 2 0.01 0.1 2 1))

(script-fu-menu-register "script-fu-meme-generator" "<Image>/File/Create/Text")

(define (gimp-image-list-count)
  (car (gimp-image-list)))

(define (gimp-image-list-items)
  (cadr (gimp-image-list)))

(define (gimp-image-latest)
  (when (= (gimp-image-list-count) 0)
    (gimp-image-new 256 256 RGB))
  (aref (gimp-image-list-items) 0))

(define (gimp-layer-hide-all from-image)
  (let ((hide-layer (lambda (layer) (gimp-layer-set-visible layer 0)))
      (layers (vector->list (cadr (gimp-image-get-layers from-image)))))
    (map hide-layer layers)))

(define (gimp-layer-unhide-all from-image)
  (let ((layers (vector->list (cadr (gimp-image-get-layers from-image)))))
    (map (lambda (layer) (gimp-layer-set-visible layer 1)) layers)))

(define (gimp-layer-unhide-all from-image)
  (let ((layers (vector->list (cadr (gimp-image-get-layers from-image)))))
    (map (lambda (layer) (gimp-layer-set-visible layer 1)) layers)))
  
(define (gimp-layer-crop-to-content layer img)
  (let* ((offsets (gimp-drawable-offsets layer))
      (x0 (list-ref offsets 0))
      (y0 (list-ref offsets 1))
      (bounds (gimp-selection-bounds img))
      (nonempty (list-ref bounds 0))
      (x1 (list-ref bounds 1))
      (y1 (list-ref bounds 2))
      (x2 (list-ref bounds 3))
      (y2 (list-ref bounds 4))
      (new-width (- x2 x1))
      (new-height (- y2 y1))
      (offx (- x0 x1))
      (offy (- y0 y1)))
      (if (= 1 nonempty) (gimp-layer-resize layer new-width new-height offx offy))))

(define (script-fu-meme-generator img text font size ratio)
  (gimp-image-undo-group-start img)
  (gimp-layer-hide-all img)
  (let* (
      (border (* size ratio 2))
      (logo-layer (car (gimp-text-fontname img -1 0 0 text border TRUE size PIXELS font)))
      (width (car (gimp-drawable-width logo-layer)))
      (height (car (gimp-drawable-height logo-layer)))
      (outline-layer (car (gimp-layer-new img width height RGBA-IMAGE text 100 NORMAL-MODE))))

     (gimp-selection-none img)
     (script-fu-util-image-add-layers img outline-layer)

    ; create inner portion
     (gimp-context-set-foreground "white")
     (gimp-layer-set-lock-alpha logo-layer TRUE)
     (gimp-edit-fill logo-layer FOREGROUND-FILL)

    ; create outline
     (gimp-edit-clear outline-layer)
     (gimp-image-select-item img CHANNEL-OP-REPLACE logo-layer)
     (gimp-selection-grow img (* size ratio))
     (gimp-context-set-background "black")
     (gimp-edit-fill outline-layer BACKGROUND-FILL)

     (let ((layer (car (gimp-image-merge-visible-layers img 1))))
      (gimp-layer-unhide-all img)
      (gimp-layer-crop-to-content layer img))
	   (gimp-selection-none img)
     (gimp-displays-flush)
     (gimp-image-undo-group-end img)))

; debug
; (script-fu-meme-generator (gimp-image-latest) "Test" "Impact Condensed" 500 0.1)
