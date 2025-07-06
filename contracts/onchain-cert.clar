;; (impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

(define-non-fungible-token certificate uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-cert (err u102))
(define-constant err-already-exists (err u103))

(define-data-var last-token-id uint u0)

(define-map certificate-data 
  uint 
  {
    issuer: principal,
    recipient: principal,
    course: (string-ascii 50),
    grade: (string-ascii 2),
    date: uint,
    institution: (string-ascii 50)
  }
)

(define-map authorized-issuers principal bool)

(define-public (add-authorized-issuer (issuer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set authorized-issuers issuer true))))

(define-public (remove-authorized-issuer (issuer principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set authorized-issuers issuer false))))

(define-read-only (is-authorized-issuer (issuer principal))
  (default-to false (map-get? authorized-issuers issuer)))

(define-read-only (get-last-token-id)
  (ok (var-get last-token-id)))

(define-read-only (get-token-uri (token-id uint))
  (ok none))

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? certificate token-id)))

(define-read-only (get-certificate-data (token-id uint))
  (ok (map-get? certificate-data token-id)))

(define-public (issue-certificate 
    (recipient principal)
    (course (string-ascii 50))
    (grade (string-ascii 2))
    (institution (string-ascii 50)))
  (let
    ((new-id (+ (var-get last-token-id) u1)))
    (asserts! (is-authorized-issuer tx-sender) err-not-authorized)
    (asserts! (is-none (nft-get-owner? certificate new-id)) err-already-exists)
    (try! (nft-mint? certificate new-id recipient))
    (map-set certificate-data new-id
      {
        issuer: tx-sender,
        recipient: recipient,
        course: course,
        grade: grade,
        date: stacks-block-height,
        institution: institution
      })
    (var-set last-token-id new-id)
    (ok new-id)))
(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (err err-not-authorized))


(define-private (check-recipient-certificates (token-id uint))
  (let
    ((cert-data (map-get? certificate-data token-id)))
    (match cert-data
      data (if (is-eq (get recipient data) tx-sender)
                  (some token-id)
                  none)
      none)))
(define-private (list-certificates (max-id uint))
  (map uint-to-int (generate-sequence max-id)))

(define-private (generate-sequence (n uint))
  (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9 u10))

(define-private (uint-to-int (n uint))
  n)



(define-map revoked-certificates uint bool)

(define-public (revoke-certificate (token-id uint))
  (let ((cert-data (unwrap! (map-get? certificate-data token-id) err-invalid-cert)))
    (asserts! (is-authorized-issuer tx-sender) err-not-authorized)
    (asserts! (is-eq (get issuer cert-data) tx-sender) err-not-authorized)
    (map-set revoked-certificates token-id true)
    (ok true)))

(define-read-only (is-certificate-revoked (token-id uint))
  (default-to false (map-get? revoked-certificates token-id)))



(define-map certificate-metadata
  uint 
  {
    course-duration: uint,
    certification-level: (string-ascii 20),
    expiration-date: uint,
    additional-notes: (string-ascii 100)
  })

(define-public (set-certificate-metadata 
    (token-id uint)
    (course-duration uint)
    (certification-level (string-ascii 20))
    (expiration-date uint)
    (additional-notes (string-ascii 100)))
  (let ((cert-data (unwrap! (map-get? certificate-data token-id) err-invalid-cert)))
    (asserts! (is-authorized-issuer tx-sender) err-not-authorized)
    (asserts! (is-eq (get issuer cert-data) tx-sender) err-not-authorized)
    (ok (map-set certificate-metadata token-id
      {
        course-duration: course-duration,
        certification-level: certification-level,
        expiration-date: expiration-date,
        additional-notes: additional-notes
      }))))

(define-read-only (get-certificate-metadata (token-id uint))
  (ok (map-get? certificate-metadata token-id)))


(define-constant err-invalid-token (err u200))
(define-constant err-verification-failed (err u201))

(define-map verification-logs
  uint
  {
    verifier: principal,
    verification-date: uint,
    verification-count: uint
  })

(define-data-var total-verifications uint u0)

(define-public (verify-certificate (token-id uint))
  (let (
    (cert-data (map-get? certificate-data token-id))
    (cert-metadata (map-get? certificate-metadata token-id))
    (is-revoked (default-to false (map-get? revoked-certificates token-id)))
    (current-height stacks-block-height)
  )
  (match cert-data
    data (let (
      (issuer (get issuer data))
      (is-issuer-authorized (default-to false (map-get? authorized-issuers issuer)))
      (is-expired (match cert-metadata
        metadata (> current-height (get expiration-date metadata))
        false))
      (owner (nft-get-owner? certificate token-id))
    )
    (begin
      (update-verification-log token-id)
      (ok {
        token-id: token-id,
        is-valid: (and is-issuer-authorized (not is-revoked) (not is-expired) (is-some owner)),
        is-revoked: is-revoked,
        is-expired: is-expired,
        issuer-authorized: is-issuer-authorized,
        owner: owner,
        issuer: issuer,
        recipient: (get recipient data),
        course: (get course data),
        grade: (get grade data),
        institution: (get institution data),
        issue-date: (get date data),
        verification-date: current-height
      })))
    err-invalid-token)))

(define-public (batch-verify-certificates (token-ids (list 10 uint)))
  (ok (map verify-single-certificate token-ids)))

(define-private (verify-single-certificate (token-id uint))
  (match (verify-certificate token-id)
    success success
    error {
      token-id: token-id,
      is-valid: false,
      is-revoked: false,
      is-expired: false,
      issuer-authorized: false,
      owner: none,
      issuer: 'SP000000000000000000002Q6VF78,
      recipient: 'SP000000000000000000002Q6VF78,
      course: "",
      grade: "",
      institution: "",
      issue-date: u0,
      verification-date: stacks-block-height
    }))

(define-private (update-verification-log (token-id uint))
  (let (
    (current-log (map-get? verification-logs token-id))
    (new-count (match current-log
      log (+ (get verification-count log) u1)
      u1))
  )
  (begin
    (map-set verification-logs token-id {
      verifier: tx-sender,
      verification-date: stacks-block-height,
      verification-count: new-count
    })
    (var-set total-verifications (+ (var-get total-verifications) u1)))))

(define-read-only (get-verification-stats (token-id uint))
  (ok (map-get? verification-logs token-id)))

(define-read-only (get-total-verifications)
  (ok (var-get total-verifications)))

(define-read-only (is-certificate-expired (token-id uint))
  (match (map-get? certificate-metadata token-id)
    metadata (ok (> stacks-block-height (get expiration-date metadata)))
    (ok false)))

(define-read-only (get-certificate-status (token-id uint))
  (let (
    (exists (is-some (map-get? certificate-data token-id)))
    (is-revoked (default-to false (map-get? revoked-certificates token-id)))
    (is-expired (unwrap-panic (is-certificate-expired token-id)))
    (owner (nft-get-owner? certificate token-id))
  )
  (ok {
    exists: exists,
    has-owner: (is-some owner),
    is-revoked: is-revoked,
    is-expired: is-expired,
    overall-valid: (and exists (is-some owner) (not is-revoked) (not is-expired))
  })))

(define-public (verify-certificate-for-recipient (token-id uint) (expected-recipient principal))
  (match (verify-certificate token-id)
    verification-result 
      (ok (and 
        (get is-valid verification-result)
        (is-eq (get recipient verification-result) expected-recipient)))
    error (ok false)))


(define-private (check-institution-match (token-id uint) (target-institution (string-ascii 50)))
  (match (map-get? certificate-data token-id)
    data (is-eq (get institution data) target-institution)
    false))

(define-private (generate-token-sequence (max-tokens uint))
  (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10))


  (define-constant err-delegation-not-found (err u300))
(define-constant err-delegation-expired (err u301))
(define-constant err-unauthorized-delegator (err u302))

(define-map certificate-delegations
  {delegator: principal, delegatee: principal, course: (string-ascii 50)}
  {
    active: bool,
    expiration-height: uint,
    max-certificates: uint,
    certificates-issued: uint,
    created-at: uint
  })

(define-public (delegate-certificate-authority 
    (delegatee principal)
    (course (string-ascii 50))
    (expiration-height uint)
    (max-certificates uint))
  (begin
    (asserts! (is-authorized-issuer tx-sender) err-not-authorized)
    (ok (map-set certificate-delegations
      {delegator: tx-sender, delegatee: delegatee, course: course}
      {
        active: true,
        expiration-height: expiration-height,
        max-certificates: max-certificates,
        certificates-issued: u0,
        created-at: stacks-block-height
      }))))

(define-public (revoke-delegation 
    (delegatee principal)
    (course (string-ascii 50)))
  (let ((delegation-key {delegator: tx-sender, delegatee: delegatee, course: course}))
    (asserts! (is-authorized-issuer tx-sender) err-not-authorized)
    (asserts! (is-some (map-get? certificate-delegations delegation-key)) err-delegation-not-found)
    (ok (map-set certificate-delegations delegation-key
      {
        active: false,
        expiration-height: u0,
        max-certificates: u0,
        certificates-issued: u0,
        created-at: u0
      }))))

(define-private (is-valid-delegation (delegator principal) (delegatee principal) (course (string-ascii 50)))
  (let ((delegation-key {delegator: delegator, delegatee: delegatee, course: course}))
    (match (map-get? certificate-delegations delegation-key)
      delegation-data (and 
        (get active delegation-data)
        (< stacks-block-height (get expiration-height delegation-data))
        (< (get certificates-issued delegation-data) (get max-certificates delegation-data)))
      false)))



(define-private (get-current-course)
  (some {course: "default"}))

(define-private (update-delegation-counter (delegator principal) (course (string-ascii 50)))
  (let ((delegation-key {delegator: delegator, delegatee: tx-sender, course: course}))
    (match (map-get? certificate-delegations delegation-key)
      delegation-data (map-set certificate-delegations delegation-key
        {
          active: (get active delegation-data),
          expiration-height: (get expiration-height delegation-data),
          max-certificates: (get max-certificates delegation-data),
          certificates-issued: (+ (get certificates-issued delegation-data) u1),
          created-at: (get created-at delegation-data)
        })
      false)))

(define-public (issue-certificate-delegated
    (recipient principal)
    (course (string-ascii 50))
    (grade (string-ascii 2))
    (institution (string-ascii 50))
    (delegator principal))
  (let ((new-id (+ (var-get last-token-id) u1)))
    (asserts! (is-authorized-issuer delegator) err-not-authorized)
    (asserts! (is-valid-delegation delegator tx-sender course) err-delegation-not-found)
    (asserts! (is-none (nft-get-owner? certificate new-id)) err-already-exists)
    (try! (nft-mint? certificate new-id recipient))
    (map-set certificate-data new-id
      {
        issuer: delegator,
        recipient: recipient,
        course: course,
        grade: grade,
        date: stacks-block-height,
        institution: institution
      })
    (var-set last-token-id new-id)
    (update-delegation-counter delegator course)
    (ok new-id)))

(define-read-only (get-delegation-info (delegator principal) (delegatee principal) (course (string-ascii 50)))
  (ok (map-get? certificate-delegations {delegator: delegator, delegatee: delegatee, course: course})))



(define-private (check-any-valid-delegation (delegator principal) (found-valid bool))
  (if found-valid
    true
    (is-valid-delegation delegator tx-sender "default")))