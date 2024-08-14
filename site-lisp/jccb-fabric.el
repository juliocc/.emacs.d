(defvar jccb/fabric-boilerplate "# Copyright 2024 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the \"License\");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an \"AS IS\" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

")

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
      (insert jccb/fabric-boilerplate)
      (insert (shell-command-to-string
               (format "python %s --example ~/tmp/code.tf"
                       (f-join fabric-path "tools/plan_summary.py")))))))

(defun jccb/tf-format-region (&optional b e)
  "Run terraform fmt on the region"
  (interactive "r")
  (shell-command-on-region b e "terraform fmt -" t t))

(defun jccb/tf-format-current-block nil
  "Run terraform fmt on the current markdown fenced code block"
  (interactive)
  (save-excursion
    (let ((b (or (search-backward "```terraform" nil t) (search-backward "```hcl" nil)))
          (k (if (looking-at "```hcl") 7 13))
          (e (search-forward "```" nil t 2)))
      (jccb/tf-format-region (+ b k) (- e 3)))))

(defun jccb/tf-fabric-find-module-file nil
  (interactive)
  (let* ((path "~/code/cloud-foundation-fabric/modules")
         (modules (f-directories path))
         (module (completing-read "Module name: " (mapcar #'f-filename modules))))
    (find-file (read-file-name
                (format "Open file in module %s: " module)
                (f-slash (f-join path module))))))


(provide 'jccb-terraform)
