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


