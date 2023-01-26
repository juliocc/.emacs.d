(defun jccb/fabric-create-open-example-inventory ()
  (interactive)
  (beginning-of-line)
  (re-search-forward "# tftest")
  (let* ((fabric-path "~/code/cloud-foundation-fabric")
         (root-path "modules")
         (module-name (s-replace "-" "_" (nth 1 (reverse (f-split buffer-file-name)))))
         (inventories-path (f-join fabric-path "tests" root-path module-name "examples/" ))
         (cb (current-buffer))
         (eol (save-excursion (end-of-line) (point)))
         (inventory-specified (re-search-forward "inventory=\\([^[:space:]]+\\)" eol 1))
         (inventory (if inventory-specified
                        (f-join inventories-path (match-string 1))
                      (read-file-name "Inventory path: " inventories-path))))
    (unless inventory-specified
      (insert " inventory=" (f-filename inventory)))
    (find-file-other-window inventory)
    (when (zerop (buffer-size (current-buffer)))
      (with-current-buffer cb
        (write-region (save-excursion (+ (search-backward "```hcl") 6))
                      (save-excursion (- (search-forward "```") 3))
                      "~/tmp/code.tf"))
      (insert (get-register ?c))
      (insert (shell-command-to-string
               (format "python %s --example ~/tmp/code.tf"
                       (f-join fabric-path "tools/plan_summary.py")))))))
