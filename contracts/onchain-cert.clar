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



(define-private (update-delegation-counter (delegator principal) (course (string-ascii 50)))
  (let ((delegation-key {delegator: delegator, delegatee: tx-sender, course: course}))
    (match (map-get? certificate-delegations delegation-key)
      delegation-data (begin
        (map-set certificate-delegations delegation-key
          {
            active: (get active delegation-data),
            expiration-height: (get expiration-height delegation-data),
            max-certificates: (get max-certificates delegation-data),
            certificates-issued: (+ (get certificates-issued delegation-data) u1),
            created-at: (get created-at delegation-data)
          })
        true)
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

;; === CERTIFICATE SKILLS MAPPING & COMPETENCY TRACKING SYSTEM ===

;; Error constants for skills system
(define-constant err-skill-not-found (err u400))
(define-constant err-skill-exists (err u401))
(define-constant err-invalid-proficiency (err u402))
(define-constant err-no-skills-mapped (err u403))

;; Skills registry data structures
(define-map skills-registry
  (string-ascii 30) ;; skill-name
  {
    category: (string-ascii 25),
    created-by: principal,
    creation-height: uint,
    verification-count: uint
  })

;; Map certificates to their skills with proficiency levels
(define-map certificate-skills
  uint ;; token-id
  {
    primary-skills: (list 5 (string-ascii 30)),
    secondary-skills: (list 5 (string-ascii 30)),
    proficiency-levels: (list 10 uint), ;; 1-100 scale
    skills-validated: bool,
    competency-score: uint
  })

;; Track individual skill proficiencies by recipient
(define-map recipient-skills
  {recipient: principal, skill: (string-ascii 30)}
  {
    total-certificates: uint,
    average-proficiency: uint,
    highest-proficiency: uint,
    latest-certificate: uint,
    last-updated: uint
  })

;; Skills category mapping for organization
(define-map skill-categories
  (string-ascii 25) ;; category-name  
  {
    total-skills: uint,
    active-certificates: uint,
    category-weight: uint
  })

;; Data variables for tracking
(define-data-var total-skills-registered uint u0)
(define-data-var total-skill-mappings uint u0)

;; Register a new skill in the system
(define-public (register-skill 
    (skill-name (string-ascii 30))
    (category (string-ascii 25)))
  (begin
    (asserts! (is-authorized-issuer tx-sender) err-not-authorized)
    (asserts! (is-none (map-get? skills-registry skill-name)) err-skill-exists)
    (map-set skills-registry skill-name
      {
        category: category,
        created-by: tx-sender,
        creation-height: stacks-block-height,
        verification-count: u0
      })
    (update-skill-category-count category)
    (var-set total-skills-registered (+ (var-get total-skills-registered) u1))
    (ok skill-name)))

;; Map skills to an existing certificate
(define-public (map-certificate-skills
    (token-id uint)
    (primary-skills (list 5 (string-ascii 30)))
    (secondary-skills (list 5 (string-ascii 30)))
    (proficiency-levels (list 10 uint)))
  (let ((cert-data (unwrap! (map-get? certificate-data token-id) err-invalid-cert)))
    (asserts! (is-authorized-issuer tx-sender) err-not-authorized)
    (asserts! (is-eq (get issuer cert-data) tx-sender) err-not-authorized)
    (asserts! (validate-proficiency-levels proficiency-levels) err-invalid-proficiency)
    (asserts! (validate-skills-exist primary-skills secondary-skills) err-skill-not-found)
    (let ((competency-score (calculate-competency-score proficiency-levels)))
      (map-set certificate-skills token-id
        {
          primary-skills: primary-skills,
          secondary-skills: secondary-skills,
          proficiency-levels: proficiency-levels,
          skills-validated: true,
          competency-score: competency-score
        })
      (update-recipient-skills (get recipient cert-data) primary-skills secondary-skills proficiency-levels token-id)
      (var-set total-skill-mappings (+ (var-get total-skill-mappings) u1))
      (ok competency-score))))

;; Validate that proficiency levels are within acceptable range
(define-private (validate-proficiency-levels (levels (list 10 uint)))
  (fold validate-single-proficiency levels true))

(define-private (validate-single-proficiency (level uint) (valid-so-far bool))
  (and valid-so-far (and (>= level u1) (<= level u100))))

;; Validate that all listed skills exist in registry
(define-private (validate-skills-exist 
    (primary-skills (list 5 (string-ascii 30)))
    (secondary-skills (list 5 (string-ascii 30))))
  (and 
    (fold validate-skill-exists primary-skills true)
    (fold validate-skill-exists secondary-skills true)))

(define-private (validate-skill-exists (skill-name (string-ascii 30)) (valid-so-far bool))
  (if (is-eq skill-name "")
    valid-so-far
    (and valid-so-far (is-some (map-get? skills-registry skill-name)))))

;; Calculate overall competency score based on proficiency levels
(define-private (calculate-competency-score (proficiencies (list 10 uint)))
  (let ((sum (fold add-proficiency proficiencies u0))
        (count (fold count-non-zero proficiencies u0)))
    (if (> count u0)
      (/ sum count)
      u0)))

(define-private (add-proficiency (level uint) (sum uint))
  (if (> level u0)
    (+ sum level)
    sum))

(define-private (count-non-zero (level uint) (count uint))
  (if (> level u0)
    (+ count u1)
    count))

;; Update recipient's skill tracking with new certificate data
(define-private (update-recipient-skills 
    (recipient principal)
    (primary-skills (list 5 (string-ascii 30)))
    (secondary-skills (list 5 (string-ascii 30)))
    (proficiency-levels (list 10 uint))
    (token-id uint))
  (begin
    (update-skills-for-recipient recipient primary-skills proficiency-levels token-id u0)
    (update-skills-for-recipient recipient secondary-skills proficiency-levels token-id u5)
    true))

;; Helper to update skills starting from a specific index
(define-private (update-skills-for-recipient 
    (recipient principal)
    (skills (list 5 (string-ascii 30)))
    (proficiency-levels (list 10 uint))
    (token-id uint)
    (start-index uint))
  (fold process-recipient-skill 
    (zip skills (slice-proficiencies proficiency-levels start-index))
    recipient))

;; Process individual skill update for recipient
(define-private (process-recipient-skill 
    (skill-data {skill: (string-ascii 30), proficiency: uint})
    (recipient principal))
  (let ((skill-name (get skill skill-data))
        (proficiency (get proficiency skill-data)))
    (if (and (not (is-eq skill-name "")) (> proficiency u0))
      (update-single-recipient-skill recipient skill-name proficiency)
      recipient)))

;; Update a single skill entry for a recipient
(define-private (update-single-recipient-skill 
    (recipient principal)
    (skill-name (string-ascii 30))
    (new-proficiency uint))
  (let ((skill-key {recipient: recipient, skill: skill-name})
        (existing-data (map-get? recipient-skills skill-key)))
    (match existing-data
      current-skill (let ((new-total (+ (get total-certificates current-skill) u1))
                         (new-avg (/ (+ (* (get average-proficiency current-skill) 
                                          (get total-certificates current-skill))
                                       new-proficiency) new-total)))
        (map-set recipient-skills skill-key
          {
            total-certificates: new-total,
            average-proficiency: new-avg,
            highest-proficiency: (if (> new-proficiency (get highest-proficiency current-skill))
                                   new-proficiency
                                   (get highest-proficiency current-skill)),
            latest-certificate: u0, ;; Could be set to token-id if needed
            last-updated: stacks-block-height
          }))
      (map-set recipient-skills skill-key
        {
          total-certificates: u1,
          average-proficiency: new-proficiency,
          highest-proficiency: new-proficiency,
          latest-certificate: u0,
          last-updated: stacks-block-height
        }))
    recipient))

;; Helper functions for list operations
(define-private (zip (list-a (list 5 (string-ascii 30))) (list-b (list 5 uint)))
  (list 
    {skill: (unwrap-panic (element-at list-a u0)), proficiency: (unwrap-panic (element-at list-b u0))}
    {skill: (unwrap-panic (element-at list-a u1)), proficiency: (unwrap-panic (element-at list-b u1))}
    {skill: (unwrap-panic (element-at list-a u2)), proficiency: (unwrap-panic (element-at list-b u2))}
    {skill: (unwrap-panic (element-at list-a u3)), proficiency: (unwrap-panic (element-at list-b u3))}
    {skill: (unwrap-panic (element-at list-a u4)), proficiency: (unwrap-panic (element-at list-b u4))}))

(define-private (slice-proficiencies (levels (list 10 uint)) (start-index uint))
  (if (is-eq start-index u0)
    (list 
      (unwrap-panic (element-at levels u0))
      (unwrap-panic (element-at levels u1))
      (unwrap-panic (element-at levels u2))
      (unwrap-panic (element-at levels u3))
      (unwrap-panic (element-at levels u4)))
    (list 
      (unwrap-panic (element-at levels u5))
      (unwrap-panic (element-at levels u6))
      (unwrap-panic (element-at levels u7))
      (unwrap-panic (element-at levels u8))
      (unwrap-panic (element-at levels u9)))))

;; Update skill category statistics
(define-private (update-skill-category-count (category (string-ascii 25)))
  (let ((current-data (map-get? skill-categories category)))
    (match current-data
      existing (map-set skill-categories category
        {
          total-skills: (+ (get total-skills existing) u1),
          active-certificates: (get active-certificates existing),
          category-weight: (get category-weight existing)
        })
      (map-set skill-categories category
        {
          total-skills: u1,
          active-certificates: u0,
          category-weight: u50
        }))))

;; Read-only functions for querying skills data

(define-read-only (get-certificate-skills (token-id uint))
  (ok (map-get? certificate-skills token-id)))

(define-read-only (get-skill-info (skill-name (string-ascii 30)))
  (ok (map-get? skills-registry skill-name)))

(define-read-only (get-recipient-skill-summary (recipient principal) (skill-name (string-ascii 30)))
  (ok (map-get? recipient-skills {recipient: recipient, skill: skill-name})))

(define-read-only (get-skill-category-stats (category (string-ascii 25)))
  (ok (map-get? skill-categories category)))

(define-read-only (get-skills-system-stats)
  (ok {
    total-skills: (var-get total-skills-registered),
    total-mappings: (var-get total-skill-mappings),
    system-height: stacks-block-height
  }))

;; Advanced query: Find certificates by skill proficiency threshold
(define-read-only (has-skill-proficiency 
    (recipient principal)
    (skill-name (string-ascii 30))
    (min-proficiency uint))
  (match (map-get? recipient-skills {recipient: recipient, skill: skill-name})
    skill-data (ok (>= (get highest-proficiency skill-data) min-proficiency))
    (ok false)))




