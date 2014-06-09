(defun linked-regions/other-overlay (overlay)
  (overlay-get overlay 'other-overlay))

(defun linked-regions/overlay-text (overlay)
  (buffer-substring (overlay-start overlay) (overlay-end overlay)))

(defun linked-regions/check-matching (overlay)
  (let ((other-overlay (linked-regions/other-overlay overlay)))
    (message "Comparing overlay %s '%s' and %s '%s'"
             overlay (linked-regions/overlay-text overlay)
             other-overlay (linked-regions/overlay-text other-overlay))
    (if (not (equal (linked-regions/overlay-text overlay)
                    (linked-regions/overlay-text other-overlay)))
        (progn 
          (message "Deleting overlays as they don't match '%s' and '%s'"
                   (linked-regions/overlay-text overlay)
                   (linked-regions/overlay-text other-overlay))
          (delete-overlay overlay)
          (delete-overlay other-overlay)))))

(defun linked-regions/update-content (overlay start end range)
  (if (null (overlay-buffer overlay))
      ;; This overlay has been completely deleted, remove contents from
      ;; other overlay.
      (progn
        (let* ((other-overlay (linked-regions/other-overlay overlay))
               (other-start (overlay-start other-overlay))
               (other-end (overlay-end other-overlay))
               (inhibit-modification-hooks 't))
          (delete-region other-start other-end)))
    (let ((offset (- start (overlay-start overlay)))
          (other-overlay (linked-regions/other-overlay overlay))
          (inhibit-modification-hooks 't))
      (if (> range 0)
          ;; delete
          (let* ((del-start (+ offset (overlay-start other-overlay)))
                 (del-end (+ del-start range)))
            (delete-region del-start del-end))
        ;; insert
        (save-excursion
          (goto-char (+ (overlay-start other-overlay) offset))
          (insert (buffer-substring start end))
          (font-lock-fontify-region (overlay-start other-overlay) (overlay-end other-overlay) 't)
                  )))))

(defun linked-regions/update-front (overlay after-p start end &optional range)
  (message ">>linked-regions/update-front %s %s %s %s %s" overlay after-p start end range)
  (if after-p
      (linked-regions/update-content overlay start end range)
    (linked-regions/check-matching overlay))
  (message "<<linked-regions/update-front %s %s %s %s %s" overlay after-p start end range)
)

(defun linked-regions/update-rear (overlay after-p start end &optional range)
  (message ">>linked-regions/update-rear %s %s %s %s %s" overlay after-p start end range)
  (if after-p
      (if (and (not (null (overlay-buffer overlay)))
               (not (equal (overlay-start overlay) start)))
          (linked-regions/update-content overlay start end range))
    (linked-regions/check-matching overlay))
  (message "<<linked-regions/update-rear %s %s %s %s %s" overlay after-p start end range)
  )

(defun linked-regions/update-core (overlay after-p start end &optional range)
  (message ">>linked-regions/update-core %s %s %s %s %s" overlay after-p start end range)
  (if after-p
      (linked-regions/update-content overlay start end range)
    (linked-regions/check-matching overlay))
  (message "<<linked-regions/update-core %s %s %s %s %s" overlay after-p start end range)
  )

(defun create-linked-regions-for-ranges (s1 e1 s2 e2 &optional face)
  (if (not (equal (buffer-substring s1 e1) (buffer-substring s2 e2)))
      (error (format "Initial text does not match '%s' and '%s'" 
                     (buffer-substring s1 e1) 
                     (buffer-substring s2 e2))))

  (let (ovly-1 ovly-2)
    (setq ovly-1 (make-overlay s1 e1 (current-buffer) nil 't))
    (setq ovly-2 (make-overlay s2 e2 (current-buffer) nil 't))

    (if (not (null face))
        (progn
          ;; Change the face of the overlay.
          (overlay-put ovly-1 'face face)
          (overlay-put ovly-2 'face face)))

    ;; Store a handle to the other overlay in each overlay.
    (overlay-put ovly-1 'other-overlay ovly-2)
    (overlay-put ovly-2 'other-overlay ovly-1)

    ;; Delete automatically when they become empty.
    (overlay-put ovly-1 'evaporate 't)
    (overlay-put ovly-2 'evaporate 't)

    ;; Add change hook
    (overlay-put ovly-1 'modification-hooks '(linked-regions/update-core))
    (overlay-put ovly-2 'modification-hooks '(linked-regions/update-core))
    (overlay-put ovly-1 'insert-in-front-hooks '(linked-regions/update-front))
    (overlay-put ovly-2 'insert-in-front-hooks '(linked-regions/update-front))
    (overlay-put ovly-1 'insert-behind-hooks '(linked-regions/update-rear))
    (overlay-put ovly-2 'insert-behind-hooks '(linked-regions/update-rear))))

(defun create-linked-regions-at-points (pt1 pt2 text &optional face)
  ;; First insert text at pt1 and at pt2.
  (let (start-1 end-1 start-2 end-2)
    (save-excursion
      (goto-char pt2)
      (setq start-2 (point-marker))
      (insert text)
      (setq end-2 (point-marker))
      (goto-char pt1)
      (setq start-1 (point-marker))
      (insert text)
      (setq end-1 (point-marker)))

    ;; Create overlays to cover this new text
    (create-linked-regions-for-ranges 
     (marker-position start-1)
     (marker-position end-1)
     (marker-position start-2)
     (marker-position end-2) face)))

(provide 'linked-regions)
