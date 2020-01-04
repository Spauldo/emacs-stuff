;;; paycalc.el --- Calculate and display pay rates for job searches

;; Author: Jeff Spaulding <sarnet@gmail.com>
;; Version: 0.1
;; Keywords: jobs, pay

;; This file is NOT part of GNU Emacs.

;;; License:

;; ISC license.

;; Copyright (c) 2016 Jeff Spaulding <sarnet@gmail.com>
;;
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; These functions create a table that displays the yearly, monthly, weekly,
;; and hourly pay for different pay rates.  The user specifies the lowest and
;; highest pay rates along with an interval and expected tax rate, and the
;; table will appear in a new buffer.

;; All these functions assume a 40 hour work week and 52 weeks per year.

;;; Code:

(defun paycalc-calculate-pay (pay-rate-low pay-rate-high inc pay-type tax-rate)
  "Calculate rates between PAY-RATE-LOW and PAY-RATE-HIGH in INC increments.
The value of TAX-RATE is a percentage, so `25' means 25%.
The value of PAY-TYPE must be one of the following values:
  :yearly    - Yearly pay
  :monthly   - Monthly pay
  :weekly    - Weekly pay
  :hourly    - Hourly pay
Results will display in a new buffer."
  (interactive "nLowest Pay: \nnHighest Pay: \nnIncrement: \nX:yearly, :monthly, :weekly, :hourly: \nnTax Rate: ")
  (let ((pay-calc-list (number-sequence pay-rate-low pay-rate-high inc))
	(pay-rate-list '()))
    (when (not (= pay-rate-high (car (reverse pay-calc-list))))
      (setq pay-calc-list (append pay-calc-list (list pay-rate-high))))
    (setq pay-rate-list
	  (cond ((eq pay-type :yearly)
		 (mapcar #'paycalc-calculate-pay-from-yearly pay-calc-list))
		((eq pay-type :monthly)
		 (mapcar #'paycalc-calculate-pay-from-monthly pay-calc-list))
		((eq pay-type :weekly)
		 (mapcar #'paycalc-calculate-pay-from-weekly pay-calc-list))
		((eq pay-type :hourly)
		 (mapcar #'paycalc-calculate-pay-from-hourly pay-calc-list))
		(t
		 nil)))
    (if pay-rate-list
	(paycalc-output-pay-calculation pay-rate-list tax-rate)
      (message "PAY-TYPE must be :yearly, :monthly, :weekly, or :hourly."))))

(defun paycalc-output-pay-calculation (paylist tax-rate)
  "Apply TAX-RATE to pay information in PAYLIST and display it in a new buffer.
PAYLIST should be a list of lists, each inner list containing yearly, monthly,
weekly, and hourly pay.  TAX-RATE should be a percentage, so that a 24.3% tax
rate is `24.3.'"
  (let ((take-home-ratio (/ (- 100 tax-rate) 100.0)))
    (with-current-buffer (generate-new-buffer "Pay Calculation")
      (insert (format "%-11s  %-11s  %-9s  %-9s  %-8s  %-8s  %-6s\n"
		      "Yearly" "w/tax"
		      "Monthly" "w/tax"
		      "Weekly" "w/tax"
		      "Hourly"))
      (insert (concat (make-string 74 ?-)) "\n")
      (seq-do (lambda (payrate)
		(let ((yearly (car payrate))
		      (monthly (cadr payrate))
		      (weekly (caddr payrate))
		      (hourly (cadddr payrate)))
		  (insert (format "$%10.2f  " yearly))
		  (insert (format "$%10.2f  " (* yearly take-home-ratio)))
		  (insert (format "$%8.2f  " monthly))
		  (insert (format "$%8.2f  " (* monthly take-home-ratio)))
		  (insert (format "$%7.2f  " weekly))
		  (insert (format "$%7.2f  " (* weekly take-home-ratio)))
		  (insert (format "$%5.2f\n" hourly))))
	      paylist)
      (switch-to-buffer-other-window (current-buffer)))))

(defun paycalc-calculate-pay-from-yearly (yearly-pay)
  "Given YEARLY-PAY, return a list of pay rates.
See the function PAYCALC-OUTPUT-PAY-CALCULATION for details on what this
function outputs."
  (let* ((weekly-pay (/ yearly-pay 52.0))
	 (monthly-pay (/ yearly-pay 12.0))
	 (hourly-pay (/ weekly-pay 40.0)))
    (list yearly-pay monthly-pay weekly-pay hourly-pay)))

(defun paycalc-calculate-pay-from-monthly (monthly-pay)
  "Given MONTHLY-PAY, return a list of pay rates.
See the function PAYCALC-OUTPUT-PAY-CALCULATION for details on what this
function outputs."
  (let* ((yearly-pay (* 12 monthly-pay))
	 (weekly-pay (/ yearly-pay 52.0))
	 (hourly-pay (/ weekly-pay 40.0)))
    (list yearly-pay monthly-pay weekly-pay hourly-pay)))

(defun paycalc-calculate-pay-from-weekly (weekly-pay)
  "Given WEEKLY-PAY, return a list of pay rates.
See the function PAYCALC-OUTPUT-PAY-CALCULATION for details on what this
function outputs."
  (let* ((yearly-pay (* weekly-pay 52))
	 (monthly-pay (/ yearly-pay 12.0))
	 (hourly-pay (/ weekly-pay 40.0)))
    (list yearly-pay monthly-pay weekly-pay hourly-pay)))

(defun paycalc-calculate-pay-from-hourly (hourly-pay)
  "Given HOURLY-PAY, return a list of pay rates.
See the function PAYCALC-OUTPUT-PAY-CALCULATION for details on what this
function outputs."
  (let* ((weekly-pay (* hourly-pay 40))
	 (yearly-pay (* weekly-pay 52))
	 (monthly-pay (/ yearly-pay 12.0)))
    (list yearly-pay monthly-pay weekly-pay hourly-pay)))

(provide 'paycalc)

;;; paycalc.el ends here
