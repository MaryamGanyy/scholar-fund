;; Contract: scholar-fund.clar
;; Decentralized Scholarship Allocation and Voting System (DSAVS)

(define-constant FUND_RELEASE_TIME u604800) ;; 1 week in seconds
(define-data-var current-round uint u1)
(define-data-var total-donations uint u0)
(define-data-var admin principal tx-sender)

(define-map donors principal uint) ;; donor => total donated
(define-map applications uint 
  {
    applicant: principal,
    name: (string-ascii 40),
    gpa: uint,
    docs-url: (string-ascii 100),
    round: uint,
    approved: bool
  })

(define-map student-votes { round: uint, student: uint } uint)
(define-map donor-votes { round: uint, donor: principal, student: uint } bool)
(define-map scholarship-balances principal uint)

(define-data-var last-applicant-id uint u0)

;; -----------------------------
;; Public Function: Donate STX
;; -----------------------------
(define-public (donate (amount uint))
  (begin
    (let ((sender tx-sender))
      (asserts! (> amount u0) (err u100))
      (try! (stx-transfer? amount sender (as-contract tx-sender)))
      (map-set donors tx-sender (+ amount (default-to u0 (map-get? donors tx-sender))))
      (var-set total-donations (+ (var-get total-donations) amount))
      (ok amount))))

;; -----------------------------
;; Public Function: Apply
;; -----------------------------
(define-public (apply-scholarship (name (string-ascii 40)) (gpa uint) (docs-url (string-ascii 100)))
  (begin
    (asserts! (> (len name) u0) (err u104))
    (asserts! (and (>= gpa u0) (<= gpa u400)) (err u105))
    (asserts! (> (len docs-url) u0) (err u106))
    (var-set last-applicant-id (+ (var-get last-applicant-id) u1))
    (map-set applications (var-get last-applicant-id)
      {
        applicant: tx-sender,
        name: name,
        gpa: gpa,
        docs-url: docs-url,
        round: (var-get current-round),
        approved: false
      })
    (ok (var-get last-applicant-id))))

;; -----------------------------
;; Public Function: Vote
;; -----------------------------
(define-public (vote-for-student (student-id uint))
  (let (
        (round (var-get current-round))
        (vote-key {round: round, donor: tx-sender, student: student-id})
       )
    (begin
      (asserts! (is-some (map-get? applications student-id)) (err u101))
      (asserts! (not (is-some (map-get? donor-votes vote-key))) (err u102)) ;; no double voting
      (let ((donation (default-to u0 (map-get? donors tx-sender))))
        (asserts! (> donation u0) (err u103))
        (map-set donor-votes vote-key true)
        (map-set student-votes {round: round, student: student-id} 
                 (+ (default-to u0 (map-get? student-votes {round: round, student: student-id})) donation))
        (ok true)))))

;; -----------------------------
;; Admin: Approve and Disburse
;; -----------------------------
(define-public (disburse-scholarship (student-id uint) (amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u201))
    (asserts! (> amount u0) (err u204))
    (let ((app-data (unwrap! (map-get? applications student-id) (err u203))))
      (begin
        (asserts! (is-eq (get round app-data) (var-get current-round)) (err u202))
        (map-set applications student-id (merge app-data { approved: true }))
        (map-set scholarship-balances (get applicant app-data) amount)
        (ok true)))))

;; -----------------------------
;; Student Withdraw
;; -----------------------------
(define-public (withdraw-scholarship)
  (let ((balance (default-to u0 (map-get? scholarship-balances tx-sender))))
    (begin
      (asserts! (> balance u0) (err u300))
      (map-delete scholarship-balances tx-sender)
      (try! (stx-transfer? balance (as-contract tx-sender) tx-sender))
      (ok balance))))

;; -----------------------------
;; Admin: Next Round
;; -----------------------------
(define-public (start-next-round)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u400))
    (var-set current-round (+ (var-get current-round) u1))
    (ok (var-get current-round))))
